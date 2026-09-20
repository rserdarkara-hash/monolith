# test-desc-statistics.R — tests for the Descriptive Suite's pure statistics:
# desc_summary_table(), desc_group_fit_stats() and desc_pca_fit()
# (ui_formatting.R). These back the module's summary table, its per-group trend
# columns and the PCA panel.
#
# References are recomputed inside each test from the definition or from an
# independent base-R route (split + sapply, summary(lm), eigen of the
# correlation matrix) — never read off the function under test.


# ── desc_summary_table ────────────────────────────────────────────────────

test_that("the summary table reports per-group statistics and a TOTAL row", {
  d <- golden_soil("core")
  res <- desc_summary_table(d$ph, d$locality)

  # Independent reference: split the vector and summarise each piece.
  sp <- split(d$ph, d$locality)
  expect_equal(as.character(res$Group), c(names(sp), "TOTAL"))
  expect_equal(names(res), c("Group", "Count", DESC_SUMMARY_STATS))

  g <- res[res$Group != "TOTAL", ]
  expect_equal(g$Count, unname(vapply(sp, length, integer(1))))
  # Values are the statistics themselves: the display formats them, so a
  # small-unit variable is not quantized before it reaches the reader.
  expect_equal(g$Mean, unname(vapply(sp, mean, numeric(1))))
  expect_equal(g$SD, unname(vapply(sp, stats::sd, numeric(1))))
  expect_equal(g$Min, unname(vapply(sp, min, numeric(1))))
  expect_equal(g$Max, unname(vapply(sp, max, numeric(1))))

  # The TOTAL row summarises the pooled vector, not the group summaries: its
  # SD carries the between-group spread that a mean of group SDs would lose.
  tot <- res[res$Group == "TOTAL", ]
  expect_equal(tot$Count, length(d$ph))
  expect_equal(tot$Mean, mean(d$ph))
  expect_equal(tot$SD, stats::sd(d$ph))
  expect_equal(tot$Min, min(d$ph))
  expect_equal(tot$Max, max(d$ph))
  expect_equal(sum(g$Count), tot$Count)
  expect_false(isTRUE(all.equal(tot$SD, mean(g$SD))))

  # Grouped statistics are computed on complete pairs (aggregate()'s formula
  # interface drops NA rows) while TOTAL counts every non-NA value.
  x <- d$ph
  x[c(1, 2, 3)] <- NA
  res_na <- desc_summary_table(x, d$locality)
  expect_equal(res_na$Count[nrow(res_na)], sum(!is.na(x)))
  expect_equal(sum(res_na$Count[-nrow(res_na)]), sum(!is.na(x)))
  expect_equal(res_na$Mean[nrow(res_na)], mean(x, na.rm = TRUE))

  # Row names must stay the default integer sequence. DT renders them, so a
  # name inherited from the statistics matrix would show up as a leading column
  # in the table - and the single-group case is the DEFAULT path, since the
  # module sets group_id to "All" when no grouping variable is chosen.
  one <- desc_summary_table(d$ph, factor(rep("All", nrow(d))))
  expect_equal(rownames(one), as.character(seq_len(nrow(one))))
  expect_equal(rownames(res), as.character(seq_len(nrow(res))))
  expect_equal(one$Count, c(nrow(d), nrow(d)))
})

test_that("the summary table carries the robust statistics beside the moments", {
  # A vector whose median, quartiles and MAD can be written down by hand.
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  res <- desc_summary_table(x, rep("g", length(x)))
  g <- res[res$Group == "g", ]

  expect_equal(g$Median, 5)
  expect_equal(g$Q1, 3)                       # quantile type 7, R's default
  expect_equal(g$Q3, 7)
  expect_equal(g$IQR, 4)
  expect_equal(g$IQR, g$Q3 - g$Q1)            # exactly, by construction
  # stats::mad is the median absolute deviation SCALED by 1.4826, so it reads
  # on the same scale as the SD next to it: median|x - 5| = 2 here.
  expect_equal(g$MAD, 2 * 1.4826)
  expect_equal(g$MAD, stats::mad(x))

  # Contamination: three planted outliers move the SD a long way and leave the
  # median and the MAD where they were. That is the whole reason the robust
  # columns are reported beside the moments.
  y <- c(x, 40, 45, 50)
  cont <- desc_summary_table(y, rep("g", length(y)))
  cont <- cont[cont$Group == "g", ]
  expect_gt(cont$SD, g$SD * 1.4)
  expect_equal(cont$Median, stats::median(y))
  expect_lt(abs(cont$MAD - g$MAD), g$MAD)     # MAD barely moves
  expect_equal(cont$IQR, cont$Q3 - cont$Q1)
})

test_that("a near-constant column is not reported as a constant one", {
  # 7 + N(0, 1e-7) against exactly 7: rounded to three decimals the two
  # columns printed byte-identical rows (Mean 7, SD 0, Min 7, Max 7).
  set.seed(5)
  near <- 7 + rnorm(60, 0, 1e-7)
  const <- rep(7, 60)
  res <- desc_summary_table(c(near, const), rep(c("near", "const"), each = 60))
  rn <- res[res$Group == "near", ]
  rc <- res[res$Group == "const", ]

  expect_gt(rn$SD, 0)
  expect_equal(rc$SD, 0)
  expect_gt(rn$MAD, 0)
  expect_equal(rc$MAD, 0)
  expect_false(isTRUE(all.equal(unlist(rn[, DESC_SUMMARY_STATS]),
                                unlist(rc[, DESC_SUMMARY_STATS]))))
  # and the display keeps them apart rather than collapsing the small one
  expect_match(format_sig(rn$SD), "e-0[0-9]$")
  expect_identical(format_sig(rc$SD), "0")
})


# ── desc_group_fit_stats: linear and polynomial ───────────────────────────

test_that("group trend R2 equals summary(lm)$r.squared and its F-test p", {
  d <- golden_soil("core")
  d$group_id <- d$locality
  groups <- c(sort(unique(d$locality)), "TOTAL")

  fits <- desc_group_fit_stats(d, "som", "ph", "linear", groups)
  expect_equal(fits$Group, groups)

  for (g in groups) {
    sub <- if (g == "TOTAL") d else d[d$group_id == g, ]
    s <- summary(stats::lm(ph ~ som, data = sub))
    row <- fits[fits$Group == g, ]
    expect_equal(row$r2, s$r.squared, tolerance = 1e-10)
    # For a simple linear regression R2 is the squared Pearson correlation —
    # an identity independent of summary.lm's own bookkeeping.
    expect_equal(row$r2, stats::cor(sub$som, sub$ph)^2, tolerance = 1e-10)
    expect_equal(row$p,
                 unname(stats::pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3],
                                  lower.tail = FALSE)),
                 tolerance = 1e-10)
  }

  # The quadratic fit explains at least as much as the linear one nested in it.
  poly_fits <- desc_group_fit_stats(d, "som", "ph", "polynomial", groups)
  expect_true(all(poly_fits$r2 >= fits$r2 - 1e-12))

  # Groups below the 5-row minimum report NA rather than an unstable fit.
  small <- d[seq_len(4), ]
  small$group_id <- "tiny"
  expect_true(all(is.na(unlist(desc_group_fit_stats(small, "som", "ph", "linear", "tiny")[, c("r2", "p")]))))

  # An unknown fit type is not silently treated as linear.
  expect_true(all(is.na(desc_group_fit_stats(d, "som", "ph", "none", "TOTAL")[, c("r2", "p")])))
})


# ── desc_group_fit_stats: loess ───────────────────────────────────────────

test_that("the loess column is cor(y, fitted)^2 and carries no p-value", {
  d <- golden_soil("core")
  d$group_id <- d$locality
  groups <- c(sort(unique(d$locality)), "TOTAL")

  fits <- desc_group_fit_stats(d, "som", "ph", "loess", groups)

  for (g in groups) {
    sub <- if (g == "TOTAL") d else d[d$group_id == g, ]
    mod <- stats::loess(ph ~ som, data = sub, span = 0.7)
    expect_equal(fits$r2[fits$Group == g],
                 stats::cor(sub$ph, stats::fitted(mod))^2,
                 tolerance = 1e-8)
  }

  # A loess has no F test, so no p-value is reported for it — the column must
  # be NA rather than borrowing the linear model's test.
  expect_true(all(is.na(fits$p)))

  # A local regression is at least as flexible as the global straight line, so
  # its squared correlation cannot be lower. It is NOT an R2 (the residuals are
  # not orthogonal to the fit), which is why the module labels it differently.
  lin <- desc_group_fit_stats(d, "som", "ph", "linear", groups)
  expect_true(all(fits$r2 >= lin$r2 - 1e-12))

  # A group carrying missing values still reports a number. loess drops the
  # incomplete rows, so the statistic must be formed on the rows the fit
  # actually used - comparing against the unfiltered column is a length
  # mismatch, and the guard would turn the whole column blank.
  dna <- d
  dna$ph[c(2, 7)] <- NA
  dna$som[11] <- NA
  na_fit <- desc_group_fit_stats(dna, "som", "ph", "loess", "TOTAL")
  cc <- stats::complete.cases(dna[, c("som", "ph")])
  ref <- stats::loess(ph ~ som, data = dna[cc, ], span = 0.7)
  expect_equal(na_fit$r2, stats::cor(dna$ph[cc], stats::fitted(ref))^2, tolerance = 1e-8)
  expect_false(is.na(na_fit$r2))
  expect_true(is.na(na_fit$p))
})


# ── desc_pca_fit ──────────────────────────────────────────────────────────

test_that("PCA eigenvalues and variance shares match the correlation/covariance spectrum", {
  d <- golden_soil("core")
  vars <- c("ph", "som", "caco3", "sand", "tn")
  labs <- paste0("L_", vars)
  X <- as.matrix(d[, vars])

  # Scaled (correlation PCA): eigenvalues of cor(X), summing to the number of
  # variables because every standardised variable contributes variance 1.
  sc <- desc_pca_fit(d, vars, labs, scale = TRUE)
  ev_cor <- eigen(stats::cor(X), symmetric = TRUE)$values
  expect_equal(sc$res$sdev^2, ev_cor, tolerance = 1e-8)
  expect_equal(sum(sc$res$sdev^2), length(vars), tolerance = 1e-8)
  expect_equal(sc$res$sdev^2 / sum(sc$res$sdev^2), ev_cor / sum(ev_cor), tolerance = 1e-8)

  # Unscaled (covariance PCA): eigenvalues of cov(X), summing to the total
  # variance in the variables' own units — a different, scale-dependent answer.
  un <- desc_pca_fit(d, vars, labs, scale = FALSE)
  ev_cov <- eigen(stats::cov(X), symmetric = TRUE)$values
  expect_equal(un$res$sdev^2, ev_cov, tolerance = 1e-8)
  expect_equal(sum(un$res$sdev^2), sum(apply(X, 2, stats::var)), tolerance = 1e-8)
  expect_false(isTRUE(all.equal(sc$res$sdev^2, un$res$sdev^2)))

  # Variance shares are a proper partition in both modes.
  for (fit in list(sc, un)) {
    shares <- fit$res$sdev^2 / sum(fit$res$sdev^2)
    expect_equal(sum(shares), 1, tolerance = 1e-10)
    expect_true(all(shares >= 0))
    expect_true(all(diff(shares) <= 1e-12))   # components ordered by variance
  }

  # Display labels reach the fitted object, which is what the biplot and the
  # loadings table read their variable names from.
  expect_equal(colnames(sc$data), labs)
  expect_equal(rownames(sc$res$rotation), labs)
  expect_equal(sc$dropped, 0L)
  expect_equal(nrow(sc$data), nrow(d))

  # Complete-case filtering: the mask must select exactly the rows kept, so a
  # grouping vector subset by it stays aligned with the scores.
  dna <- d
  dna$ph[c(2, 5, 9)] <- NA
  dna$tn[5] <- NA
  na_fit <- desc_pca_fit(dna, vars, labs, scale = TRUE)
  expect_equal(na_fit$dropped, 3)
  expect_equal(nrow(na_fit$res$x), nrow(d) - 3)
  expect_equal(which(!na_fit$keep), c(2L, 5L, 9L))
  expect_equal(unname(na_fit$data[, 1]), d$ph[na_fit$keep])
})

test_that("a zero-variance column is excluded and named, and scaling is kept", {
  d <- golden_soil("core")
  vars <- c("ph", "som", "caco3")
  d$flat <- 7                                   # constant: cannot be standardised
  fit <- desc_pca_fit(d, c(vars, "flat"), c(vars, "Flat (const)"), scale = TRUE)
  expect_equal(fit$dropped_constant, "Flat (const)")
  expect_null(fit$refusal)
  # The PCA is the correlation PCA of the informative columns alone, unscaled
  # PCA is not substituted.
  ev <- eigen(stats::cor(as.matrix(d[, vars])), symmetric = TRUE)$values
  expect_equal(fit$res$sdev^2, ev, tolerance = 1e-8)
  expect_equal(rownames(fit$res$rotation), vars)
  expect_equal(colnames(fit$data), vars)
  expect_true(is.numeric(fit$res$scale))        # prcomp stores FALSE when unscaled
  # `dropped` keeps its meaning: rows removed by the complete-case filter.
  expect_equal(fit$dropped, 0L)
})

test_that("PCA is refused, not errored, below two informative variables", {
  d <- data.frame(a = c(1.2, 3.4, 2.2, 5.1, 4.0), b = 7, c = 2)
  fit <- desc_pca_fit(d, c("a", "b", "c"), scale = TRUE)
  expect_null(fit$res)
  expect_equal(fit$dropped_constant, c("b", "c"))
  expect_match(fit$refusal, "at least two variables with variance")
  expect_match(fit$refusal, "b, c", fixed = TRUE)
  expect_match(fit$refusal, "only one usable variable remains", fixed = TRUE)
})

# ── summary_stats_df ──────────────────────────────────────────────────────
# The descriptive-statistics card and its export. The fixed row set is the
# point: summary() appends an "NA's" element only for a vector that has
# missing values, and building the second column by assignment onto a frame
# sized from the first raised whenever exactly one side had them.

test_that("summary_stats_df reproduces summary() and stays numeric", {
  x <- c(2, 4, 4, 5, 9, 12)
  out <- summary_stats_df(x)
  s <- summary(x)

  expect_equal(names(out), c("Metric", "Value"))
  expect_true(is.numeric(out$Value))
  expect_equal(out$Metric, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
  expect_equal(out$Value, unname(as.numeric(s)))
})

test_that("summary_stats_df keeps small-unit statistics at full precision", {
  x <- c(0.0175, 0.031, 0.052, 0.0874, 0.214)   # total N, %
  out <- summary_stats_df(x)
  expect_equal(out$Value[out$Metric == "Min."], 0.0175)
  expect_equal(out$Value[out$Metric == "Mean"], mean(x))

  # One flavour only: the frame carries the computed values and the card
  # displays them at four significant digits.
  expect_equal(format_sig(out$Value[out$Metric == "Mean"]),
               format_sig(mean(x)))
  big <- summary_stats_df(c(2, 1234.5678))
  expect_equal(big$Value[big$Metric == "Max."], 1234.5678)
  expect_equal(format_sig(1234.5678), "1235")
})

test_that("summary_stats_df pairs the columns when only the SECOND carries NAs", {
  a <- c(2, 3, 4, 5, 6, 7)
  b <- c(1, 2, 3, NA, 5, 6)
  out <- summary_stats_df(a, b, labels = c("Actual", "Predicted"))

  expect_equal(nrow(out), 7)
  expect_equal(out$Actual[out$Metric == "NA's"], 0)
  expect_equal(out$Predicted[out$Metric == "NA's"], 1)
  expect_equal(out$Predicted[out$Metric == "Mean"], mean(b, na.rm = TRUE))
  expect_equal(out$Actual[out$Metric == "Max."], 7)
})

test_that("summary_stats_df keeps an all-missing second column instead of dropping it", {
  out <- summary_stats_df(1:4, rep(NA_real_, 4), labels = c("Actual", "Predicted"))
  expect_true("Predicted" %in% names(out))
  expect_true(all(is.na(out$Predicted[out$Metric != "NA's"])))
  expect_equal(out$Predicted[out$Metric == "NA's"], 4)
  expect_equal(out$Actual[out$Metric == "Mean"], 2.5)
})

test_that("stats_table_vectors reads the uploaded rows the card reads", {
  # rows 1-2 are co-located: the run's point set de-duplicates them, the
  # descriptive table (card and export alike) must not.
  df <- data.frame(site = c("A", "A", "A", "B"), x = c(1, 1, 2, 3), y = c(1, 1, 2, 3),
                   tn = c(0.1, 0.3, 0.2, 0.9), tn_cve = c(0.12, 0.28, 0.21, 0.8),
                   tn_ss = c(9, 9, 9, 7))
  meta <- list(actual = "tn", pred = "tn_cve", pred_ss = "tn_ss",
               comp_mode = FALSE, value_type = "actual")

  sv <- stats_table_vectors(df, meta, "site", "A")
  expect_equal(sv$act, c(0.1, 0.3, 0.2))
  expect_null(sv$pre)                     # the run mapped no predictions

  meta$comp_mode <- TRUE
  expect_equal(stats_table_vectors(df, meta, "site", NULL)$pre, df$tn_cve)
  meta$value_type <- "pred_ss"            # a Single-Split run describes _ss
  expect_equal(stats_table_vectors(df, meta, "site", "B")$pre, 7)

  meta$actual <- "absent"
  expect_null(stats_table_vectors(df, meta, "site", NULL))
})

test_that("summary_stats_df pairs two columns even when only one carries NAs", {
  a <- c(1, 2, 3, NA, 5, 6)
  b <- c(2, 3, 4, 5, 6, 7)

  out <- summary_stats_df(a, b, labels = c("Actual", "Predicted"))

  expect_equal(names(out), c("Metric", "Actual", "Predicted"))
  expect_equal(nrow(out), 7)
  expect_true("NA's" %in% out$Metric)
  expect_equal(out$Actual[out$Metric == "NA's"], 1)
  expect_equal(out$Predicted[out$Metric == "NA's"], 0)
  # the statistics themselves are summary()'s, computed on the complete values
  # and at full precision - the table rounds for display, the frame does not.
  expect_equal(out$Actual[out$Metric == "Mean"], mean(a, na.rm = TRUE))
  expect_equal(out$Predicted[out$Metric == "Max."], max(b))
})

test_that("summary_stats_df hides the missing-value row when there is none", {
  out <- summary_stats_df(1:10, 2:11, labels = c("A", "B"))
  expect_false("NA's" %in% out$Metric)
  expect_equal(nrow(out), 6)
})

test_that("summary_stats_df returns NULL for an all-missing or empty vector", {
  expect_null(summary_stats_df(c(NA, NA)))
  expect_null(summary_stats_df(numeric(0)))
  expect_null(summary_stats_df(NULL))
})
