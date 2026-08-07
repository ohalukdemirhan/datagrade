# One test per defect found in the dissertation's reference implementation.
# Each name states the defect, so a future regression reports what broke rather
# than which line number failed.

test_that("redundancy never reports the literal strings 'row' and 'col'", {
  # which(arr.ind = TRUE) returns a matrix whose *columns* are named "row" and
  # "col". Taking colnames() of it leaked those two strings into the result as
  # if they were data columns -- including on inputs with no redundancy at all.
  res <- flag_redundant_columns(iris, 0.5)
  expect_false(any(c("row", "col") %in% res$columns))
  expect_false(any(c("row", "col") %in% res$pairs$column_1))
  expect_true(all(res$columns %in% names(iris)))

  no_redundancy <- flag_redundant_columns(iris, 0.99)
  expect_length(no_redundancy$columns, 0L)
  expect_equal(nrow(no_redundancy$pairs), 0L)
})

test_that("a single redundant pair does not collapse the index matrix", {
  set.seed(1)
  df <- data.frame(a = rnorm(100), b = rnorm(100))
  df$c <- df$a * 2 + rnorm(100, sd = 0.01)
  res <- flag_redundant_columns(df, 0.95)
  expect_equal(nrow(res$pairs), 1L)
  expect_setequal(c(res$pairs$column_1, res$pairs$column_2), c("a", "c"))
  expect_length(res$columns, 1L)
})

test_that("consistency is scored on the same 0-10 scale as every other dimension", {
  # The original returned a 0-1 proportion that the caller multiplied by 10 as
  # though it were already 0-10, which is why the dissertation reports 2.5%
  # for iris where 25% was meant.
  res <- calculate_consistency_score(iris, vif_threshold = 5)
  expect_gte(res$score, 0)
  expect_lte(res$score, 10)
  expect_equal(as_pct(res$score), res$score * 10)

  report <- dg_assess(iris, verbose = FALSE)
  expect_true(all(report$scores[!is.na(report$scores)] <= 10))
  expect_true(all(report$scores[!is.na(report$scores)] >= 0))
})

test_that("a column with zero variance does not turn the outlier count into NA", {
  df <- data.frame(constant = rep(5, 50), varying = c(rnorm(49), 100))
  res <- detect_outliers(df)
  expect_false(is.na(res$total))
  expect_equal(res$by_column$n_outliers[res$by_column$column == "constant"], 0L)

  report <- dg_assess(df, verbose = FALSE)
  expect_false(is.na(report$scores[["accuracy"]]))
})

test_that("identifier columns are detected before storage type is consulted", {
  # Appendix A.3 tested uniqueness last, so the "ID" branch was unreachable:
  # a character key matched is.character() and returned "Text" first.
  df <- data.frame(id = sprintf("CUST%03d", 1:20), n = rep(1:10, 2),
                   grp = rep(letters[1:4], 5), stringsAsFactors = FALSE)
  types <- detect_data_types(df)
  expect_equal(unname(types[["id"]]), "ID")
  expect_equal(unname(types[["grp"]]), "Categorical")
  expect_equal(check_unique_identifiers(df), "id")
})

test_that("a continuous numeric column is never mistaken for an identifier", {
  # Distinctness alone cannot identify a key: rnorm() is distinct in every row,
  # so a bare uniqueness test silently removed real measurement columns from
  # the analysis and inflated the consistency score to a perfect 10.
  set.seed(5)
  df <- data.frame(a = rnorm(500), b = rnorm(500), row_id = 1:500)
  df$d <- df$b * 1.0001
  types <- detect_data_types(df)
  expect_equal(unname(types[["a"]]), "Numeric")
  expect_equal(unname(types[["b"]]), "Numeric")
  expect_equal(unname(types[["d"]]), "Numeric")
  expect_equal(unname(types[["row_id"]]), "ID")

  report <- dg_assess(df, verbose = FALSE)
  expect_equal(report$identifiers, "row_id")
  expect_true(any(is.infinite(report$consistency$vif)))
  expect_lt(report$scores[["consistency"]], 10)
})

test_that("a mostly empty column is not mistaken for an identifier", {
  df <- data.frame(sparse = c("a", "b", rep(NA, 18)), n = 1:20,
                   stringsAsFactors = FALSE)
  expect_false(unname(detect_data_types(df)[["sparse"]]) == "ID")
})

test_that("text normalisation preserves non-ASCII letters, dates and e-mail", {
  # gsub("[^a-zA-Z0-9 ]", "") deleted every accent, every date separator and
  # every "@", silently corrupting the data it was supposed to assess.
  df <- data.frame(
    city = c("  Ağrı ", "Çorum"),
    email = c("a@b.com", "c@d.org"),
    when = c("2024-01-15", "2024-02-20"),
    stringsAsFactors = FALSE)

  gentle <- clean_text_columns(df)
  expect_equal(gentle$city, c("Ağrı", "Çorum"))
  expect_equal(gentle$email, df$email)
  expect_equal(gentle$when, df$when)

  # Even with punctuation stripping on, Unicode letters survive.
  strict <- clean_text_columns(df, strip_punctuation = TRUE)
  expect_equal(strict$city, c("Ağrı", "Çorum"))
  expect_equal(strict$email, c("abcom", "cdorg"))
})

test_that("dates are parsed before text cleaning, so timeliness is measurable", {
  df <- data.frame(when = c("2024-01-15", "2024-02-20", "2024-03-25"),
                   value = 1:3, stringsAsFactors = FALSE)
  report <- dg_assess(df, verbose = FALSE, strip_punctuation = TRUE)
  expect_equal(report$parsed_dates, "when")
  expect_equal(nrow(report$dates), 1L)
  expect_false(is.na(report$scores[["timeliness"]]))
})

test_that("kurtosis is excess kurtosis, so a normal sample scores near zero", {
  # The dissertation's Figure 7 subtracts 3; moments::kurtosis() does not, so
  # the published formula and the published code disagreed by exactly 3.
  set.seed(42)
  df <- data.frame(x = rnorm(20000))
  res <- analyze_distribution(df)
  expect_lt(abs(res$excess_kurtosis), 0.25)
  expect_lt(abs(res$skewness), 0.1)
})

test_that("every figure is a real ggplot object, not a discarded theme", {
  # `ggplot(...) + theme_minimal() %>% print()` parses as
  # `ggplot(...) + (theme_minimal() %>% print())` because %>% binds tighter
  # than +. The theme was printed and the plot silently dropped, so none of
  # the figures in the dissertation could be produced by the code that ships
  # with it.
  report <- dg_assess(iris, verbose = FALSE)
  figures <- dg_plots(report)
  expect_true(length(figures) >= 4L)
  for (nm in names(figures)) {
    expect_s3_class(figures[[nm]], "ggplot")
    expect_false(inherits(figures[[nm]], "theme"))
  }
})

test_that("assessment writes nothing to the global environment", {
  # The original called assign("imported_data", data, envir = .GlobalEnv),
  # which clobbers the user's workspace and fails R CMD check outright.
  before <- ls(envir = globalenv())
  dg_assess(iris, verbose = FALSE)
  expect_setequal(ls(envir = globalenv()), before)
})

test_that("the caller's data frame is never modified", {
  df <- data.frame(x = c(1, NA, 3), label = c(" A ", "b", "C"),
                   stringsAsFactors = FALSE)
  snapshot <- df
  dg_assess(df, verbose = FALSE)
  expect_identical(df, snapshot)
})
