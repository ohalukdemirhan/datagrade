# Inputs that broke the reference implementation: empty frames, one column,
# all-NA columns, no numeric columns, perfect collinearity.

test_that("a zero-row data frame is assessed without error", {
  df <- data.frame(a = numeric(0), b = character(0), stringsAsFactors = FALSE)
  expect_no_error(report <- dg_assess(df, verbose = FALSE))
  expect_equal(report$n_row, 0L)
})

test_that("a single numeric column yields no correlation, not an error", {
  # data[, sapply(data, is.numeric)] drops to a vector when exactly one column
  # matches, and cor() then fails on a vector.
  df <- data.frame(only = rnorm(50))
  expect_no_error(report <- dg_assess(df, verbose = FALSE))
  expect_null(report$redundancy$matrix)
  expect_true(is.na(report$scores[["consistency"]]))
  expect_length(dg_vif(df), 0L)
})

test_that("an entirely missing column does not poison downstream statistics", {
  df <- data.frame(empty = rep(NA_real_, 30), value = rnorm(30))
  report <- dg_assess(df, verbose = FALSE)
  expect_equal(report$missing$columns$rate[report$missing$columns$column == "empty"], 1)
  expect_false(is.na(report$scores[["completeness"]]))
  # Mean of an all-NA column is NaN; imputation must leave the column alone
  # rather than write NaN into every row.
  imputed <- impute_missing_values(df, "mean")
  expect_true(all(is.na(imputed$empty)))
  expect_false(any(is.nan(imputed$empty)))
})

test_that("a data frame with no numeric columns scores only what applies", {
  df <- data.frame(a = c("x", "y", "z"), b = c("p", "q", "r"),
                   stringsAsFactors = FALSE)
  report <- dg_assess(df, verbose = FALSE)
  expect_true(is.na(report$scores[["accuracy"]]))
  expect_true(is.na(report$plausibility))
  # CONS_FORM still applies to text columns, so consistency is measurable even
  # with nothing numeric to compute a VIF from.
  expect_true(is.na(report$measures$value[report$measures$code == "RIES_INCO"]))
  expect_false(is.na(report$scores[["consistency"]]))
})

test_that("perfectly collinear columns are reported as infinite VIF, not an error", {
  set.seed(7)
  df <- data.frame(a = rnorm(100), b = rnorm(100))
  df$c <- df$a + df$b            # exact linear combination
  vif <- dg_vif(df)
  expect_true(any(is.infinite(vif)))
  expect_length(vif, 3L)
  res <- calculate_consistency_score(df, vif_threshold = 5)
  expect_false(is.na(res$score))
  expect_lt(res$score, 1)
})

test_that("VIF matches 1 / (1 - R squared) from an explicit regression", {
  set.seed(11)
  df <- data.frame(a = rnorm(500), b = rnorm(500))
  df$c <- 0.6 * df$a + 0.3 * df$b + rnorm(500)
  ours <- dg_vif(df)
  by_regression <- vapply(names(df), function(col) {
    others <- setdiff(names(df), col)
    r2 <- summary(lm(df[[col]] ~ ., data = df[others]))$r.squared
    1 / (1 - r2)
  }, numeric(1L))
  expect_equal(unname(ours[names(by_regression)]), unname(by_regression),
               tolerance = 1e-8)
})

test_that("grouped imputation falls back for groups that are entirely missing", {
  df <- data.frame(grp = rep(c("a", "b"), each = 5),
                   value = c(rep(NA_real_, 5), 1:5))
  imputed <- impute_missing_values(df, "mean", group_by = "grp")
  expect_false(any(is.na(imputed$value)))
  expect_equal(unique(imputed$value[df$grp == "a"]), mean(1:5))
})

test_that("weights renormalise over the dimensions that apply", {
  df <- data.frame(a = c("x", "y", "z"), stringsAsFactors = FALSE)
  report <- dg_assess(df, verbose = FALSE,
                      weights = c(completeness = 2, accuracy = 1,
                                  consistency = 1, timeliness = 1))
  # Only completeness applies, so it must be the whole of the overall score
  # regardless of the weights attached to the inapplicable dimensions.
  expect_equal(report$overall, report$scores[["completeness"]])
})

test_that("unsupported input is rejected with a clear message", {
  expect_error(dg_assess(), "Supply either")
  expect_error(dg_assess(data = 1:10), "must be a data frame")
  expect_error(dg_assess(path = "no-such-file.csv"), "File not found")
})

test_that("identifier columns are excluded from the statistics", {
  set.seed(3)
  df <- data.frame(row_id = 1:200, x = rnorm(200))
  df$y <- df$x * 0.5 + rnorm(200)
  report <- dg_assess(df, verbose = FALSE)
  expect_equal(report$identifiers, "row_id")
  # A surrogate key is perfectly correlated with row order and would otherwise
  # dominate the correlation and multicollinearity results.
  expect_false("row_id" %in% colnames(report$redundancy$matrix))
  expect_false("row_id" %in% report$outliers$by_column$column)
})
