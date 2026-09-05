# test-desc-statistics.R — tests for the Descriptive Suite's pure statistics:
# desc_summary_table(), desc_group_fit_stats() and desc_pca_fit()
# (ui_formatting.R). These back the module's summary table, its per-group trend
# columns and the PCA panel.
#
# References are recomputed inside each test from the definition or from an
# independent base-R route (split + sapply, summary(lm), eigen of the
# correlation matrix) — never read off the function under test.


# ── desc_summary_table ────────────────────────────────────────────────────

test_that("the summary table reports per-group n/mean/sd/min/max and a TOTAL row", {
  d <- golden_soil("core")
  res <- desc_summary_table(d$ph, d$locality)

  # Independent reference: split the vector and summarise each piece.
  sp <- split(d$ph, d$locality)
  expect_equal(as.character(res$Group), c(names(sp), "TOTAL"))

  g <- res[res$Group != "TOTAL", ]
  expect_equal(g$Count, unname(vapply(sp, length, integer(1))))
  expect_equal(g$Mean, unname(round(vapply(sp, mean, numeric(1)), 3)))
  expect_equal(g$SD, unname(round(vapply(sp, stats::sd, numeric(1)), 3)))
  expect_equal(g$Min, unname(round(vapply(sp, min, numeric(1)), 3)))
  expect_equal(g$Max, unname(round(vapply(sp, max, numeric(1)), 3)))

  # The TOTAL row summarises the pooled vector, not the group summaries: its
  # SD carries the between-group spread that a mean of group SDs would lose.
  tot <- res[res$Group == "TOTAL", ]
  expect_equal(tot$Count, length(d$ph))
  expect_equal(tot$Mean, round(mean(d$ph), 3))
  expect_equal(tot$SD, round(stats::sd(d$ph), 3))
  expect_equal(tot$Min, round(min(d$ph), 3))
  expect_equal(tot$Max, round(max(d$ph), 3))
  expect_equal(sum(g$Count), tot$Count)
  expect_false(isTRUE(all.equal(tot$SD, mean(g$SD))))

  # Grouped statistics are computed on complete pairs (aggregate()'s formula
  # interface drops NA rows) while TOTAL counts every non-NA value.
  x <- d$ph
  x[c(1, 2, 3)] <- NA
  res_na <- desc_summary_table(x, d$locality)
  expect_equal(res_na$Count[nrow(res_na)], sum(!is.na(x)))
  expect_equal(sum(res_na$Count[-nrow(res_na)]), sum(!is.na(x)))
  expect_equal(res_na$Mean[nrow(res_na)], round(mean(x, na.rm = TRUE), 3))

  # Row names must stay the default integer sequence. DT renders them, so a
  # name inherited from the statistics matrix would show up as a leading column
  # in the table - and the single-group case is the DEFAULT path, since the
  # module sets group_id to "All" when no grouping variable is chosen.
  one <- desc_summary_table(d$ph, factor(rep("All", nrow(d))))
  expect_equal(rownames(one), as.character(seq_len(nrow(one))))
  expect_equal(rownames(res), as.character(seq_len(nrow(res))))
  expect_equal(one$Count, c(nrow(d), nrow(d)))
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
