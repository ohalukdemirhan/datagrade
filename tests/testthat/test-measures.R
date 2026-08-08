test_that("the fifteen ISO/IEC 25012 properties are all reported, applicable or not", {
  report <- dg_assess(iris, verbose = FALSE)
  expect_equal(nrow(report$measures), 15L)
  expect_setequal(
    report$measures$code,
    c("EXAC_SINT", "EXAC_SEMAN", "RAN_EXAC",
      "COMP_REG", "COMP_VAL_ESP", "COMP_FICH", "FAL_COMP_FICH",
      "INT_REF", "RIES_INCO", "CONS_SEMAN", "CONS_FORM",
      "CRED_VAL_DAT", "CRED_FUEN", "FREC_ACT", "CONV_ACT"))
  expect_setequal(levels(report$measures$characteristic),
                  c("accuracy", "completeness", "consistency",
                    "credibility", "currentness"))
})

test_that("a measure with no declared expectation is NA, never zero", {
  # The distinction matters: zero is a claim about the data, NA is a statement
  # that the standard's denominator could not be counted. Scoring the second as
  # the first would punish every undocumented data set for being undocumented.
  report <- dg_assess(iris, verbose = FALSE)
  needs_spec <- report$measures[!report$measures$applicable, ]
  expect_true(all(is.na(needs_spec$value)))
  expect_true(all(!is.na(needs_spec$note)))
  expect_true(is.na(report$scores[["accuracy"]]))
})

test_that("declaring an expectation makes the accuracy measures computable", {
  spec <- dg_spec(ranges  = list(Sepal.Length = c(4, 8)),
                  domains = list(Species = levels(iris$Species)))
  report <- dg_assess(iris, spec = spec, verbose = FALSE)
  m <- report$measures
  expect_false(is.na(m$value[m$code == "RAN_EXAC"]))
  expect_false(is.na(m$value[m$code == "EXAC_SINT"]))
  expect_false(is.na(report$scores[["accuracy"]]))
  # Every Sepal.Length is inside [4, 8] and every Species is in the declared
  # domain, so both measures are exactly 1.
  expect_equal(m$value[m$code == "RAN_EXAC"], 1)
  expect_equal(m$value[m$code == "EXAC_SINT"], 1)
})

test_that("every measure is A / B over counted data items", {
  d <- data.frame(x = c(1, 2, 3, 40), stringsAsFactors = FALSE)
  spec <- dg_spec(ranges = list(x = c(0, 10)))
  m <- dg_assess(d, spec = spec, verbose = FALSE)$measures
  applicable <- m[m$applicable, ]
  expect_equal(applicable$value, applicable$a / applicable$b)
  ran <- m[m$code == "RAN_EXAC", ]
  expect_equal(c(ran$a, ran$b), c(3, 4))
})

test_that("range accuracy counts only values with a declared interval", {
  d <- data.frame(declared = c(1, 2, 99), undeclared = c(1000, 2000, 3000))
  m <- dg_assess(d, spec = dg_spec(ranges = list(declared = c(0, 10))),
                 verbose = FALSE)$measures
  ran <- m[m$code == "RAN_EXAC", ]
  # B is 3, not 6: the second column has no declared interval, so its values
  # are not data items for which the property is defined.
  expect_equal(ran$b, 3)
  expect_equal(ran$a, 2)
})

test_that("record and value completeness count different things", {
  d <- data.frame(a = c(1, NA, 3, 4), b = c(1, 2, NA, 4))
  m <- dg_assess(d, verbose = FALSE)$measures
  # Two of four records have a hole; two of eight values are missing.
  expect_equal(m$value[m$code == "COMP_REG"], 0.5)
  expect_equal(m$value[m$code == "COMP_VAL_ESP"], 0.75)
})

test_that("required columns narrow both completeness denominators", {
  d <- data.frame(keep = c(1, 2, 3, 4), ignore = c(NA, NA, NA, NA))
  m <- dg_assess(d, spec = dg_spec(required = "keep"), verbose = FALSE)$measures
  expect_equal(m$value[m$code == "COMP_REG"], 1)
  expect_equal(m$value[m$code == "COMP_VAL_ESP"], 1)
})

test_that("a placeholder string is caught as false completeness", {
  d <- data.frame(x = c("real", "N/A", "unknown", "also real"),
                  stringsAsFactors = FALSE)
  m <- dg_assess(d, verbose = FALSE)$measures
  expect_equal(m$value[m$code == "FAL_COMP_FICH"], 0.5)
  # Value completeness sees nothing wrong: the cells are occupied.
  expect_equal(m$value[m$code == "COMP_VAL_ESP"], 1)
})

test_that("null-equivalent matching ignores case and surrounding whitespace", {
  d <- data.frame(x = c(" N/A ", "n/a", "NA", "real"), stringsAsFactors = FALSE)
  m <- dg_assess(d, verbose = FALSE)$measures
  expect_equal(m$value[m$code == "FAL_COMP_FICH"], 0.25)
})

test_that("free text is excluded from format consistency rather than failing it", {
  # Every sentence is its own format signature. Scoring this column would
  # report near-zero conformity for a column that has no format to conform to.
  set.seed(2)
  prose <- vapply(1:60, function(i) {
    words <- vapply(seq_len(sample(2:12, 1)), function(j)
      paste(sample(letters, sample(3:9, 1), TRUE), collapse = ""), character(1L))
    paste(words, collapse = " ")
  }, character(1L))
  d <- data.frame(note = prose, code = sprintf("AB-%03d", 1:60),
                  stringsAsFactors = FALSE)
  m <- dg_assess(d, verbose = FALSE)$measures
  cons_form <- m[m$code == "CONS_FORM", ]
  expect_equal(cons_form$b, 60)          # the code column only
  expect_equal(cons_form$value, 1)
  expect_match(cons_form$note, "note")
})

test_that("format consistency flags a column that mixes two shapes", {
  d <- data.frame(ref = c(rep("AB-1234", 8), "1234-AB", "XX/99"),
                  stringsAsFactors = FALSE)
  m <- dg_assess(d, verbose = FALSE)$measures
  expect_equal(m$value[m$code == "CONS_FORM"], 0.8)
})

test_that("a declared format overrides the dominant observed one", {
  # Eight of ten values share a shape, so the inferred measure would say 0.8.
  # Against a declared format that none of them match, the answer is 0.
  d <- data.frame(ref = c(rep("AB-1234", 8), "1234-AB", "XX/99"),
                  stringsAsFactors = FALSE)
  m <- dg_assess(d, spec = dg_spec(formats = list(ref = "^[0-9]{6}$")),
                 verbose = FALSE)$measures
  expect_equal(m$value[m$code == "CONS_FORM"], 0)
})

test_that("semantic consistency counts rule evaluations, not records", {
  d <- data.frame(start = as.Date("2024-01-01") + 0:3,
                  end   = as.Date("2024-01-01") + c(1, 2, -1, 4))
  m <- dg_assess(d, spec = dg_spec(rules = list(order = ~ start <= end)),
                 verbose = FALSE)$measures
  cons <- m[m$code == "CONS_SEMAN", ]
  expect_equal(c(cons$a, cons$b), c(3, 4))
})

test_that("a rule that cannot be evaluated is reported, not counted as failure", {
  d <- data.frame(x = 1:4)
  m <- dg_assess(d, spec = dg_spec(rules = list(
    fine = ~ x > 0, broken = ~ nonexistent_column > 0)), verbose = FALSE)$measures
  cons <- m[m$code == "CONS_SEMAN", ]
  expect_equal(cons$b, 4)                # the working rule only
  expect_equal(cons$value, 1)
  expect_match(cons$note, "broken")
})

test_that("referential integrity counts values outside the permitted set", {
  d <- data.frame(dept = c("A", "B", "ZZ", "A"), stringsAsFactors = FALSE)
  m <- dg_assess(d, spec = dg_spec(foreign_keys = list(dept = c("A", "B"))),
                 verbose = FALSE)$measures
  expect_equal(m$value[m$code == "INT_REF"], 0.75)
})

test_that("semantic accuracy compares against the reference and ignores unmatched records", {
  d <- data.frame(id = c("a", "b", "c", "d"), value = c(1, 2, 3, 4),
                  stringsAsFactors = FALSE)
  truth <- data.frame(id = c("a", "b", "c"), value = c(1, 2, 99),
                      stringsAsFactors = FALSE)
  m <- dg_assess(d, spec = dg_spec(reference = truth, reference_key = "id"),
                 verbose = FALSE)$measures
  exac <- m[m$code == "EXAC_SEMAN", ]
  # Record "d" has no counterpart, so it is unverifiable rather than inaccurate.
  expect_equal(c(exac$a, exac$b), c(2, 3))
})

test_that("currentness counts future dates as not current", {
  d <- data.frame(when = as.Date(c("2020-01-01", "2099-01-01", "1900-01-01")))
  m <- dg_assess(d, spec = dg_spec(as_of = as.Date("2024-01-01"),
                                   max_age = 3652.5), verbose = FALSE)$measures
  conv <- m[m$code == "CONV_ACT", ]
  expect_equal(c(conv$a, conv$b), c(1, 3))
})

test_that("update frequency measures the gaps between distinct update dates", {
  d <- data.frame(when = as.Date("2024-01-01") + c(0, 5, 10, 40))
  m <- dg_assess(d, spec = dg_spec(update_column = "when", update_interval = 7),
                 verbose = FALSE)$measures
  frec <- m[m$code == "FREC_ACT", ]
  expect_equal(c(frec$a, frec$b), c(2, 3))
})

test_that("credibility separates the believable from the merely valid", {
  d <- data.frame(age = c(30, 40, 900))
  m <- dg_assess(d, spec = dg_spec(ranges = list(age = c(0, 1000)),
                                   credible_ranges = list(age = c(0, 120))),
                 verbose = FALSE)$measures
  expect_equal(m$value[m$code == "RAN_EXAC"], 1)
  expect_equal(m$value[m$code == "CRED_VAL_DAT"], 2 / 3)
})

test_that("source credibility counts columns whose provenance is trusted", {
  m <- dg_assess(iris, spec = dg_spec(
    source_trust = list(Sepal.Length = "registry", Species = "guess"),
    trusted_sources = "registry"), verbose = FALSE)$measures
  expect_equal(m$value[m$code == "CRED_FUEN"], 0.5)
})

test_that("plausibility is reported separately and kept out of the overall score", {
  spec <- dg_spec(ranges = list(Sepal.Length = c(4, 8)))
  report <- dg_assess(iris, spec = spec, verbose = FALSE)
  expect_false(is.na(report$plausibility))
  expect_false("plausibility" %in% names(report$scores))
  expect_equal(report$overall, dg_overall(report$scores, report$weights))
})

test_that("a characteristic aggregates only the measures that apply to it", {
  report <- dg_assess(iris, verbose = FALSE)
  m <- report$measures
  for (ch in names(report$scores)) {
    vals <- m$value[m$characteristic == ch & m$applicable]
    expected <- if (length(vals) == 0L) NA_real_ else mean(vals)
    expect_equal(unname(report$scores[[ch]]), expected)
  }
})

test_that("timeliness is kept as an alias for currentness", {
  d <- data.frame(when = as.Date("2024-01-01") + 0:3)
  report <- dg_assess(d, verbose = FALSE)
  expect_equal(report$timeliness, unname(report$scores[["currentness"]]))
  # And a v1 weight vector naming timeliness still addresses the right thing.
  weighted <- dg_assess(d, weights = c(currentness = 3, completeness = 1),
                        verbose = FALSE)
  aliased <- dg_assess(d, weights = c(timeliness = 3, completeness = 1),
                       verbose = FALSE)
  expect_equal(aliased$overall, weighted$overall)
})

test_that("dg_spec rejects a malformed declaration at the point of declaration", {
  expect_error(dg_spec(ranges = list(x = c(10, 1))), "min <= max")
  expect_error(dg_spec(ranges = list(c(1, 2))), "named list")
  expect_error(dg_spec(patterns = list(x = "[unclosed")), "valid regular expression")
  expect_error(dg_spec(rules = list(bad = "not a formula")), "one-sided formula")
  expect_error(dg_spec(expected_records = -1), "positive number")
  expect_error(dg_spec(reference = data.frame(a = 1)), "reference_key")
})

test_that("dg_assess rejects a spec that did not come from dg_spec", {
  expect_error(dg_assess(iris, spec = list(required = "Species"), verbose = FALSE),
               "dg_spec")
})

test_that("an empty spec assesses exactly as no spec does", {
  expect_equal(dg_assess(iris, verbose = FALSE)$measures,
               dg_assess(iris, spec = dg_spec(), verbose = FALSE)$measures)
})
