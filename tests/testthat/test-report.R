test_that("dg_assess returns a structured object, not console text", {
  # The reference implementation printed everything with cat() and returned
  # NULL, so no result could be used programmatically.
  report <- dg_assess(iris, verbose = FALSE)
  expect_s3_class(report, "dg_report")
  expect_named(report$scores,
               c("completeness", "accuracy", "consistency", "timeliness"))
  expect_type(report$overall, "double")
  expect_s3_class(report$distribution, "data.frame")
  expect_s3_class(report$outliers$by_column, "data.frame")
})

test_that("the data argument works, as the dissertation documented", {
  # Documented in three places but implemented nowhere: only `path` existed.
  expect_no_error(dg_assess(data = mtcars, verbose = FALSE))
  from_frame <- dg_assess(data = iris, verbose = FALSE)
  expect_equal(from_frame$n_row, nrow(iris))
})

test_that("path and data give the same scores for the same table", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(mtcars, tmp, row.names = FALSE)
  from_path <- dg_assess(path = tmp, verbose = FALSE)
  from_data <- dg_assess(data = read.csv(tmp), verbose = FALSE)
  expect_equal(from_path$scores, from_data$scores)
})

test_that("data_quality_assessment is kept as an alias", {
  expect_equal(data_quality_assessment(iris, verbose = FALSE)$scores,
               dg_assess(iris, verbose = FALSE)$scores)
})

test_that("summary returns tables and as.data.frame returns one stackable row", {
  report <- dg_assess(iris, verbose = FALSE)
  s <- summary(report)
  expect_s3_class(s$scores, "data.frame")
  expect_equal(nrow(s$scores), 5L)          # four dimensions plus overall
  expect_equal(nrow(s$columns), ncol(iris))

  row <- as.data.frame(report)
  expect_equal(nrow(row), 1L)
  expect_true(all(c("completeness", "accuracy", "consistency", "timeliness",
                    "overall", "rows", "columns") %in% names(row)))

  # Stacking many assessments is the point of the one-row form.
  stacked <- do.call(rbind, lapply(list(iris, mtcars, airquality),
                                   function(d) as.data.frame(dg_assess(d, verbose = FALSE))))
  expect_equal(nrow(stacked), 3L)
})

test_that("printing is quiet when asked and loud when not", {
  expect_silent(dg_assess(iris, verbose = FALSE))
  report <- dg_assess(iris, verbose = FALSE)
  # cli may route to stdout or stderr depending on how it is configured, so
  # capture both rather than assuming one.
  printed <- c(utils::capture.output(print(report), type = "output"),
               utils::capture.output(print(report), type = "message"))
  expect_match(paste(printed, collapse = "\n"), "Data quality report")
})

test_that("results are reproducible across runs", {
  a <- dg_assess(airquality, verbose = FALSE)
  b <- dg_assess(airquality, verbose = FALSE)
  expect_equal(a$scores, b$scores)
  expect_equal(a$distribution, b$distribution)
})

test_that("a clean data set scores near ten and a dirty one clearly lower", {
  set.seed(5)
  clean <- data.frame(a = rnorm(500), b = rnorm(500), c = rnorm(500))
  dirty <- clean
  dirty$a[1:150] <- NA
  dirty$d <- dirty$b * 1.0001          # near-perfect redundancy
  dirty$c[1:20] <- 5000                # gross outliers

  clean_report <- dg_assess(clean, verbose = FALSE)
  dirty_report <- dg_assess(dirty, verbose = FALSE)

  expect_gt(clean_report$overall, 9)
  expect_lt(dirty_report$overall, clean_report$overall)
  # Completeness is the mean missing rate *across columns*, per the
  # dissertation's formula, so one 30%-empty column out of four costs 0.75
  # points rather than 3. The dilution is a property of the published formula,
  # not of this implementation.
  expect_lt(dirty_report$scores[["completeness"]], 9.5)
  expect_lt(dirty_report$scores[["consistency"]], 10)
})

test_that("duplicates are counted without materialising every duplicate row", {
  df <- do.call(rbind, replicate(50, mtcars, simplify = FALSE))
  res <- detect_duplicates(df)
  expect_equal(res$n, nrow(df) - nrow(mtcars))
  # The examples slot is a small sample, never the full duplicate set, which
  # on a large input can be bigger than the input itself.
  expect_lte(nrow(res$examples), 5L)
})
