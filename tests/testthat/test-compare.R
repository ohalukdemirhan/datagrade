test_that("identical deliveries compare as unchanged", {
  cmp <- dg_compare(iris, iris)
  expect_s3_class(cmp, "dg_comparison")
  expect_equal(nrow(cmp$measures), 15L)
  applicable <- !is.na(cmp$measures$old)
  expect_true(all(cmp$measures$status[applicable] == "unchanged"))
  expect_true(all(cmp$measures$status[!applicable] == "not applicable"))
  expect_equal(cmp$overall$status, "unchanged")
  expect_length(cmp$schema$added, 0L)
  expect_length(cmp$schema$removed, 0L)
  expect_equal(nrow(cmp$schema$retyped), 0L)
  expect_equal(cmp$rows, c(old = 150L, new = 150L))
})

test_that("new missing values show up as a completeness decline", {
  new <- airquality
  new$Ozone[1:60] <- NA
  cmp <- dg_compare(airquality, new)
  expect_equal(cmp$scores$status[cmp$scores$characteristic == "completeness"],
               "declined")
  ozone <- cmp$missing[cmp$missing$column == "Ozone", ]
  expect_equal(ozone$status, "declined")
  expect_gt(ozone$new, ozone$old)
  # The most changed column is listed first.
  expect_equal(cmp$missing$column[1], "Ozone")
  expect_equal(cmp$measures$status[cmp$measures$code == "COMP_VAL_ESP"],
               "declined")
})

test_that("repairs register as improvements", {
  broken <- airquality
  broken$Solar.R[1:50] <- NA
  cmp <- dg_compare(broken, airquality)
  expect_equal(cmp$scores$status[cmp$scores$characteristic == "completeness"],
               "improved")
  expect_equal(cmp$missing$status[cmp$missing$column == "Solar.R"], "improved")
})

test_that("schema changes are reported", {
  new <- airquality
  new$Wind <- NULL
  new$Station <- "A"
  new$Month <- as.character(new$Month)
  cmp <- dg_compare(airquality, new)
  expect_equal(cmp$schema$removed, "Wind")
  expect_equal(cmp$schema$added, "Station")
  expect_true("Month" %in% cmp$schema$retyped$column)
  expect_false("Wind" %in% cmp$missing$column)
})

test_that("a measure that stops being computable is lost, not zero", {
  old <- data.frame(id = 1:20, when = as.Date("2026-01-01") + 0:19,
                    x = rnorm(20))
  new <- old[, c("id", "x")]
  cmp <- dg_compare(old, new)
  conv <- cmp$measures[cmp$measures$code == "CONV_ACT", ]
  expect_equal(conv$status, "lost")
  expect_true(is.na(conv$new))
  expect_true(is.na(conv$change))
  expect_equal(cmp$scores$status[cmp$scores$characteristic == "currentness"],
               "lost")
  back <- dg_compare(new, old)
  expect_equal(back$measures$status[back$measures$code == "CONV_ACT"], "gained")
})

test_that("both versions are scored against the same spec", {
  spec <- dg_spec(ranges = list(Temp = c(50, 100)))
  hot <- airquality
  hot$Temp[1:20] <- 130
  cmp <- dg_compare(airquality, hot, spec = spec)
  ran <- cmp$measures[cmp$measures$code == "RAN_EXAC", ]
  expect_false(is.na(ran$old))
  expect_equal(ran$status, "declined")
  expect_equal(cmp$scores$status[cmp$scores$characteristic == "accuracy"],
               "declined")
})

test_that("changes within the tolerance are not reported", {
  new <- airquality
  new$Ozone[1] <- NA # 1 cell of 918
  cmp <- dg_compare(airquality, new)
  expect_equal(cmp$scores$status[cmp$scores$characteristic == "completeness"],
               "unchanged")
  strict <- dg_compare(airquality, new, tolerance = 0)
  expect_equal(strict$scores$status[strict$scores$characteristic == "completeness"],
               "declined")
})

test_that("duplicate rows are compared", {
  new <- rbind(mtcars, mtcars[1:10, ])
  cmp <- dg_compare(mtcars, new)
  expect_equal(cmp$duplicates$status, "declined")
  expect_equal(cmp$rows[["new"]], 42L)
  no_scan <- dg_compare(mtcars, new, check_duplicates = FALSE)
  expect_null(no_scan$duplicates)
})

test_that("existing reports can be compared and mismatches are flagged", {
  a <- dg_assess(iris, verbose = FALSE)
  b <- dg_assess(iris, verbose = FALSE)
  expect_no_warning(cmp <- dg_compare(a, b))
  expect_equal(cmp$overall$status, "unchanged")
  c_spec <- dg_assess(iris, spec = dg_spec(ranges = list(Sepal.Length = c(4, 8))),
                      verbose = FALSE)
  expect_warning(dg_compare(a, c_spec), "different specification")
  c_set <- dg_assess(iris, outlier_method = "iqr", verbose = FALSE)
  expect_warning(dg_compare(a, c_set), "different settings")
  expect_warning(dg_compare(a, b, spec = dg_spec()), "ignored")
})

test_that("inputs are validated", {
  r <- dg_assess(iris, verbose = FALSE)
  expect_error(dg_compare(iris, r), "both be data frames")
  expect_error(dg_compare(iris, 1:3), "both be data frames")
  expect_error(dg_compare(iris, iris, tolerance = -1), "tolerance")
  expect_error(dg_compare(iris, iris, tolerance = c(0.1, 0.2)), "tolerance")
})

test_that("the comparison prints and coerces to a data frame", {
  new <- airquality
  new$Ozone[1:60] <- NA
  new$Wind <- NULL
  cmp <- dg_compare(airquality, new)
  out <- cli::cli_fmt(print(cmp))
  expect_true(any(grepl("Data quality comparison", out)))
  expect_true(any(grepl("Columns removed", out)))
  expect_true(any(grepl("COMP_VAL_ESP", out)))
  same <- cli::cli_fmt(print(dg_compare(iris, iris)))
  expect_true(any(grepl("No structural change", same)))
  df <- as.data.frame(cmp)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("code", "property", "characteristic", "old", "new",
                     "change", "status"))
})

test_that("the caller's data is not modified", {
  old <- airquality
  new <- airquality
  new$Ozone[1:5] <- NA
  before <- new
  dg_compare(old, new)
  expect_identical(new, before)
  expect_identical(old, airquality)
})
