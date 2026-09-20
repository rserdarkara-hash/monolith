# test-agreement-metrics.R — tests for compute_agreement_metrics(), the
# arithmetic behind the Scientific Analysis tab's "Agreement (Kappa)" table and
# the matching "Total Classification Performance" export.
#
# Every expected value here is derived from the confusion-matrix definition of
# the statistic, or from an independent implementation (stats::quantile,
# base::cut, terra::classify) — never read off compute_agreement_metrics().

# A 3-class break set whose class limits are 10 and 20, so the class centres
# 5 / 15 / 25 map cleanly onto A / B / C.
agree_params <- function(brks_inner = c(10, 20), labels = c("A", "B", "C")) {
  brks <- c(-Inf, brks_inner, Inf)
  rcl <- cbind(brks[-length(brks)], brks[-1], seq_along(labels))
  list(rcl_mat = rcl, labels = labels, brks = brks, n_c = length(labels))
}

# Turn the shared 3x3 known-answer confusion matrix into the continuous
# (actual, predicted) pair the agreement table consumes: each class becomes its
# own bin centre under agree_params().
agree_cm_values <- function(cm = make_cm_known(), centres = c(A = 5, B = 15, C = 25)) {
  d <- make_cm_pred_df(cm, target = "truth")
  list(actual = unname(centres[as.character(d$truth)]),
       predicted = unname(centres[as.character(d$.pred_class)]))
}


# ── The confusion-matrix statistics ───────────────────────────────────────

test_that("agreement statistics equal their confusion-matrix definitions", {
  v <- agree_cm_values()
  ag <- compute_agreement_metrics(v$actual, v$predicted, method = "agro",
                                  params = agree_params())

  expect_null(ag$status)
  # The bins must reproduce the fixture matrix before any statistic is checked;
  # otherwise the rest of this block would be scoring a different table.
  cm <- make_cm_known()
  obs_cm <- table(ag$actual_bin, ag$predicted_bin)
  expect_equal(dimnames(obs_cm)[[1]], rownames(cm))
  expect_equal(dimnames(obs_cm)[[2]], colnames(cm))
  expect_equal(as.vector(obs_cm), as.vector(cm))
  expect_equal(ag$n, sum(cm))

  n <- sum(cm)
  tr <- sum(diag(cm))
  rows <- rowSums(cm)   # truth totals
  cols <- colSums(cm)   # prediction totals
  k <- nrow(cm)

  # Overall accuracy = trace / n.
  expect_equal(ag$accuracy, tr / n, tolerance = 1e-10)

  # Cohen's kappa = (p_o - p_e) / (1 - p_e), p_e from the marginal product.
  p_o <- tr / n
  p_e <- sum(rows * cols) / n^2
  expect_equal(ag$kappa, (p_o - p_e) / (1 - p_e), tolerance = 1e-10)

  # Linearly weighted kappa = 1 - sum(d_ij o_ij) / sum(d_ij e_ij) with the
  # ordinal disagreement weights d_ij = |i - j| / (k - 1).
  d <- abs(outer(seq_len(k), seq_len(k), "-")) / (k - 1)
  obs_w <- sum(d * cm) / n
  exp_w <- sum(d * outer(rows, cols)) / n^2
  expect_equal(ag$kappa_linear, 1 - obs_w / exp_w, tolerance = 1e-10)

  # Gorodkin's multiclass MCC.
  mcc_ref <- (tr * n - sum(rows * cols)) /
    sqrt((n^2 - sum(cols^2)) * (n^2 - sum(rows^2)))
  expect_equal(ag$mcc, mcc_ref, tolerance = 1e-10)

  # Balanced accuracy = macro mean of (sensitivity + specificity) / 2 taken
  # one-vs-all, NOT the macro mean of recall alone.
  bal <- vapply(seq_len(k), function(i) {
    tp <- cm[i, i]; fn <- rows[i] - tp; fp <- cols[i] - tp
    tn <- n - tp - fn - fp
    (tp / (tp + fn) + tn / (tn + fp)) / 2
  }, numeric(1))
  expect_equal(ag$bal_accuracy, mean(bal), tolerance = 1e-10)

  # Sanity on the fixture itself: unbalanced classes, so macro and overall
  # agreement genuinely differ.
  expect_equal(ag$accuracy, 0.75, tolerance = 1e-10)
  expect_false(isTRUE(all.equal(ag$accuracy, ag$bal_accuracy)))
})


# ── Off-by-one accuracy ───────────────────────────────────────────────────

test_that("off-by-one accuracy counts adjacent-class predictions as agreement", {
  # Every off-diagonal cell of the known matrix is one class away, so all 20
  # predictions are within one class and off-by-one accuracy is exactly 1.
  v <- agree_cm_values()
  ag <- compute_agreement_metrics(v$actual, v$predicted, method = "agro",
                                  params = agree_params())
  expect_equal(ag$off_by_one, 1, tolerance = 1e-10)
  expect_gte(ag$off_by_one, ag$accuracy)

  # Move one A-truth point's prediction from B to C: that pair is now two
  # classes apart, so exactly one of the 20 falls outside the tolerance.
  far <- v
  first_a <- which(v$actual == 5)[1]
  far$predicted[first_a] <- 25
  ag2 <- compute_agreement_metrics(far$actual, far$predicted, method = "agro",
                                   params = agree_params())
  expect_equal(ag2$off_by_one, 19 / 20, tolerance = 1e-10)
  # Definition check on the returned classification itself.
  expect_equal(
    ag2$off_by_one,
    mean(abs(as.integer(ag2$actual_bin) - as.integer(ag2$predicted_bin)) <= 1),
    tolerance = 1e-10
  )
  expect_gte(ag2$off_by_one, ag2$accuracy)
})


# ── Quartile binning ──────────────────────────────────────────────────────

test_that("quartile binning is stats::quantile + right-closed cut", {
  x <- golden_soil("core")$ph
  p <- golden_soil("core")$som   # an unrelated column standing in for a prediction

  ag <- compute_agreement_metrics(x, p, method = "quartile")
  expect_null(ag$status)
  expect_equal(ag$levels, paste0("Q", 1:4))

  # Independent reference: the type-7 quartiles of the OBSERVED values, with
  # the outer breaks opened so nothing falls out.
  brks <- unique(stats::quantile(x, probs = seq(0, 1, 0.25)))
  b <- brks
  b[1] <- -Inf
  b[length(b)] <- Inf
  ref_act <- cut(x, breaks = b, include.lowest = TRUE, labels = paste0("Q", 1:4))
  ref_pre <- cut(p, breaks = b, include.lowest = TRUE, labels = paste0("Q", 1:4))
  expect_equal(as.character(ag$actual_bin), as.character(ref_act))
  expect_equal(as.character(ag$predicted_bin), as.character(ref_pre))

  # Closure convention, stated as counts rather than as a re-run of cut():
  # the intervals are right-closed, so Q1 holds every value at or below the
  # first quartile and Q4 every value strictly above the third.
  q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75))
  expect_equal(sum(ag$actual_bin == "Q1"), sum(x <= q[1]))
  expect_equal(sum(ag$actual_bin == "Q4"), sum(x > q[3]))

  # Quartiles of the observed values are balanced up to the ties a rounded lab
  # measurement carries (pH is recorded to 2 dp, so breaks land on repeated
  # values and the four bins cannot all hold exactly n/4).
  expect_true(all(abs(as.numeric(table(ag$actual_bin)) - length(x) / 4) <= 0.05 * length(x)))
})


# ── Agro binning ──────────────────────────────────────────────────────────

test_that("agro binning reproduces terra::classify(right = FALSE)", {
  x <- golden_soil("core")$ph
  params <- agree_params(brks_inner = stats::quantile(x, c(1 / 3, 2 / 3)))

  ag <- compute_agreement_metrics(x, x, method = "agro", params = params)
  expect_null(ag$status)

  # The map paints these classes with terra::classify(rcl_mat, right = FALSE);
  # the table must place every value in the same class the map does.
  r <- terra::rast(nrows = 1, ncols = length(x), vals = x)
  cl <- terra::classify(r, params$rcl_mat, right = FALSE)
  expect_equal(as.integer(ag$actual_bin), as.integer(terra::values(cl)[, 1]))
})


# ── A value sitting exactly on a break ────────────────────────────────────

test_that("a value on a break follows its mode's interval convention", {
  # x = 1:9 has type-7 quartiles 3 / 5 / 7, so 3, 5 and 7 are BOTH data values
  # and break values — the case where the two conventions must disagree.
  x <- 1:9

  quart <- compute_agreement_metrics(x, x, method = "quartile")
  # right = TRUE: the break belongs to the interval BELOW it.
  expect_equal(as.character(quart$actual_bin[x == 3]), "Q1")
  expect_equal(as.character(quart$actual_bin[x == 5]), "Q2")
  expect_equal(as.character(quart$actual_bin[x == 7]), "Q3")

  # Same breaks under the agro convention, right = FALSE: the break belongs to
  # the interval ABOVE it, because that is what terra::classify paints on the
  # map (docs/scientific_guide.md §5). The two modes deliberately differ here.
  agro <- compute_agreement_metrics(
    x, x, method = "agro",
    params = agree_params(brks_inner = c(3, 5, 7), labels = paste0("C", 1:4))
  )
  expect_equal(as.character(agro$actual_bin[x == 3]), "C2")
  expect_equal(as.character(agro$actual_bin[x == 5]), "C3")
  expect_equal(as.character(agro$actual_bin[x == 7]), "C4")

  # The disagreement is exactly the boundary values, nothing else.
  off <- as.integer(agro$actual_bin) != as.integer(quart$actual_bin)
  expect_equal(sort(x[off]), c(3, 5, 7))
})


# ── Degenerate guards ─────────────────────────────────────────────────────

test_that("agreement metrics refuse too few points and a variance-free target", {
  # Fewer than 3 usable pairs: NA rows are dropped before the count, so a
  # 4-row input with two NAs is still refused.
  expect_equal(compute_agreement_metrics(c(1, 2), c(1, 2), method = "quartile")$status,
               "Not enough data points for Kappa.")
  expect_equal(
    compute_agreement_metrics(c(1, 2, NA, 4), c(1, NA, 3, 4), method = "quartile")$status,
    "Not enough data points for Kappa."
  )

  # A constant target collapses every quartile onto one value, leaving a single
  # unique break and no intervals to cut on.
  const <- compute_agreement_metrics(rep(7, 20), runif(20), method = "quartile")
  expect_equal(const$status, "Not enough variance for quartiles.")
  expect_null(const$accuracy)

  # A constant PREDICTION is not degenerate — the breaks come from the observed
  # values, so it scores as a (poor) single-class prediction.
  ok <- compute_agreement_metrics(seq_len(20), rep(7, 20), method = "quartile")
  expect_null(ok$status)
  expect_equal(ok$n, 20)
})


# ── agreement_metrics_df ──────────────────────────────────────────────────
# The Classification Performance card and its export read one builder, so the
# exported sheet cannot list fewer statistics than the screen it came from.

test_that("agreement_metrics_df reports all six statistics, numerically", {
  set.seed(21)
  obs <- runif(60, 0, 10)
  pre <- obs + rnorm(60, 0, 1)
  ag <- compute_agreement_metrics(obs, pre, method = "quartile")

  out <- agreement_metrics_df(ag)

  expect_equal(out$Metric, c("Overall Accuracy", "Balanced Accuracy",
                             "Off-by-one Accuracy", "Matthews Corr. Coef. (MCC)",
                             "Kappa (Unweighted)", "Weighted Kappa (Linear)"))
  expect_true(is.numeric(out$Value))
  val <- function(nm) out$Value[out$Metric == nm]
  # the export flavour is full precision
  expect_equal(val("Overall Accuracy"), ag$accuracy)
  expect_equal(val("Balanced Accuracy"), ag$bal_accuracy)
  expect_equal(val("Off-by-one Accuracy"), ag$off_by_one)
  expect_equal(val("Matthews Corr. Coef. (MCC)"), ag$mcc)
  expect_equal(val("Kappa (Unweighted)"), ag$kappa)
  expect_equal(val("Weighted Kappa (Linear)"), ag$kappa_linear)

  # One flavour only: the card displays these very numbers at four significant
  # digits (mnFormatSig / format_sig), so the sheet and the screen agree.
  expect_equal(format_sig(out$Value[out$Metric == "Overall Accuracy"]),
               format_sig(ag$accuracy))
})

test_that("agreement_metrics_df returns NULL for a refused comparison", {
  ag <- compute_agreement_metrics(c(1, 2), c(1, 2), method = "quartile")
  expect_false(is.null(ag$status))
  expect_null(agreement_metrics_df(ag))
  expect_null(agreement_metrics_df(NULL))
})
