# The ISO/IEC 25012 quality model, as fifteen quality properties grouped under
# five inherent characteristics. Every measure below is a ratio X = A / B over
# counted data items, which is the form ISO/IEC 25024 measurement functions
# take. A is the count of items that satisfy the property, B the count of items
# for which the property is *defined at all* — and B is where the metadata
# lives. Nine of the fifteen have a B that cannot be counted without a declared
# expectation, which is what dg_spec() supplies and why they report NA without
# one rather than being approximated.

# One measure. `applicable = FALSE` means B could not be counted, which is a
# different statement from a score of zero and is never aggregated as one.
measure <- function(code, property, characteristic, a = NA_real_, b = NA_real_,
                    note = NULL) {
  applicable <- !is.na(b) && b > 0
  list(code = code, property = property, characteristic = characteristic,
       value = if (applicable) clamp(a / b) else NA_real_,
       a = if (applicable) a else NA_real_,
       b = if (applicable) b else NA_real_,
       applicable = applicable,
       note = note %||% NA_character_)
}

as_chr <- function(x) if (is.factor(x)) as.character(x) else x

# Present in the file but carrying no information. Trimmed and case-folded
# before matching, because " N/A " and "n/a" are the same non-value.
is_null_equivalent <- function(x, null_equivalents) {
  if (length(null_equivalents) == 0L) return(rep(FALSE, length(x)))
  tolower(trimws(as.character(x))) %in% tolower(null_equivalents)
}

chr_cols <- function(data) {
  which_cols(data, function(col) is.character(col) || is.factor(col))
}

# The shape of a value with its content removed: runs of digits become 9, runs
# of letters A, everything else is kept. "2024-01-15" and "1999-12-31" share the
# signature "9-9-9"; "AB-1234" and "ZZ-0001" share "A-9". Runs are collapsed so
# that values differing only in length still count as one format.
format_signature <- function(x) {
  s <- trimws(as.character(x))
  s <- gsub("[0-9]+", "9", s)
  s <- gsub("[[:alpha:]]+", "A", s)
  gsub("[[:space:]]+", " ", s)
}

# A column has a *format* only if its values collapse to a handful of shapes.
# Free text does not: every sentence is its own signature, and scoring it for
# format consistency would report near-zero conformity for data that has no
# format to conform to. Such columns are excluded from the denominator instead.
has_discernible_format <- function(sigs, values) {
  n_sig <- length(unique(sigs))
  n_val <- length(unique(values))
  n_sig <= max(3L, ceiling(0.05 * n_val))
}

# ---------------------------------------------------------------- completeness

m_comp_reg <- function(data, spec) {
  cols <- intersect(spec$required %||% names(data), names(data))
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(measure("COMP_REG", "Record completeness", "completeness",
                   note = "no required columns present"))
  }
  complete <- stats::complete.cases(data[, cols, drop = FALSE])
  measure("COMP_REG", "Record completeness", "completeness",
          a = sum(complete), b = nrow(data))
}

m_comp_val_esp <- function(data, spec) {
  cols <- intersect(spec$required %||% names(data), names(data))
  if (length(cols) == 0L || nrow(data) == 0L) {
    return(measure("COMP_VAL_ESP", "Data value completeness", "completeness",
                   note = "no required columns present"))
  }
  n_missing <- sum(vapply(data[cols], function(col) sum(is.na(col)), numeric(1L)))
  total <- nrow(data) * length(cols)
  measure("COMP_VAL_ESP", "Data value completeness", "completeness",
          a = total - n_missing, b = total)
}

m_comp_fich <- function(data, spec) {
  if (is.null(spec$expected_records)) {
    return(measure("COMP_FICH", "File completeness", "completeness",
                   note = "needs spec$expected_records"))
  }
  measure("COMP_FICH", "File completeness", "completeness",
          a = nrow(data), b = spec$expected_records)
}

m_fal_comp_fich <- function(data, spec) {
  cols <- intersect(spec$required %||% names(data), chr_cols(data))
  if (length(cols) == 0L || length(spec$null_equivalents) == 0L) {
    return(measure("FAL_COMP_FICH", "False completeness of file", "completeness",
                   note = if (length(cols) == 0L) "no text columns to inspect"
                   else "null-equivalent list is empty"))
  }
  counts <- vapply(data[cols], function(col) {
    present <- !is.na(col)
    c(present = sum(present),
      false = sum(is_null_equivalent(col[present], spec$null_equivalents)))
  }, numeric(2L))
  measure("FAL_COMP_FICH", "False completeness of file", "completeness",
          a = sum(counts["present", ]) - sum(counts["false", ]),
          b = sum(counts["present", ]))
}

# -------------------------------------------------------------------- accuracy

m_exac_sint <- function(data, spec) {
  cols <- union(names(spec$patterns), names(spec$domains))
  cols <- intersect(cols, names(data))
  if (length(cols) == 0L) {
    return(measure("EXAC_SINT", "Syntactic accuracy", "accuracy",
                   note = "needs spec$patterns or spec$domains"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    col <- as_chr(data[[nm]])
    present <- !is.na(col)
    vals <- col[present]
    if (length(vals) == 0L) next
    ok <- rep(TRUE, length(vals))
    if (!is.null(spec$patterns[[nm]])) {
      ok <- ok & grepl(spec$patterns[[nm]], as.character(vals), perl = TRUE)
    }
    if (!is.null(spec$domains[[nm]])) {
      ok <- ok & vals %in% spec$domains[[nm]]
    }
    a <- a + sum(ok); b <- b + length(vals)
  }
  measure("EXAC_SINT", "Syntactic accuracy", "accuracy", a = a, b = b)
}

m_ran_exac <- function(data, spec) {
  cols <- intersect(names(spec$ranges), names(data))
  cols <- cols[vapply(data[cols], is.numeric, logical(1L))]
  if (length(cols) == 0L) {
    return(measure("RAN_EXAC", "Accuracy range", "accuracy",
                   note = "needs spec$ranges on numeric columns"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    vals <- data[[nm]][!is.na(data[[nm]])]
    if (length(vals) == 0L) next
    lim <- spec$ranges[[nm]]
    a <- a + sum(vals >= lim[1L] & vals <= lim[2L]); b <- b + length(vals)
  }
  measure("RAN_EXAC", "Accuracy range", "accuracy", a = a, b = b)
}

m_exac_seman <- function(data, spec) {
  if (is.null(spec$reference)) {
    return(measure("EXAC_SEMAN", "Semantic accuracy", "accuracy",
                   note = "needs spec$reference and spec$reference_key"))
  }
  key <- spec$reference_key
  if (!all(key %in% names(data))) {
    return(measure("EXAC_SEMAN", "Semantic accuracy", "accuracy",
                   note = "reference key absent from the data"))
  }
  compare <- setdiff(intersect(names(data), names(spec$reference)), key)
  if (length(compare) == 0L) {
    return(measure("EXAC_SEMAN", "Semantic accuracy", "accuracy",
                   note = "no columns shared with the reference"))
  }
  # Joined on the declared key, so only records the reference actually knows
  # about enter the denominator. A record with no counterpart is unverifiable,
  # not inaccurate.
  joined <- merge(data[, c(key, compare), drop = FALSE],
                  spec$reference[, c(key, compare), drop = FALSE],
                  by = key, suffixes = c(".dg", ".ref"))
  if (nrow(joined) == 0L) {
    return(measure("EXAC_SEMAN", "Semantic accuracy", "accuracy",
                   note = "no records matched the reference"))
  }
  a <- 0; b <- 0
  for (nm in compare) {
    got <- as_chr(joined[[paste0(nm, ".dg")]])
    want <- as_chr(joined[[paste0(nm, ".ref")]])
    checkable <- !is.na(got) & !is.na(want)
    a <- a + sum(got[checkable] == want[checkable]); b <- b + sum(checkable)
  }
  measure("EXAC_SEMAN", "Semantic accuracy", "accuracy", a = a, b = b)
}

# ----------------------------------------------------------------- consistency

m_ries_inco <- function(consistency, vif_threshold) {
  vif <- consistency$vif
  if (length(vif) == 0L) {
    return(measure("RIES_INCO", "Risk of inconsistency", "consistency",
                   note = "fewer than two usable numeric columns"))
  }
  # Information held more than once can contradict itself. A column whose
  # variance is largely explained by the others carries no independent content,
  # so it is counted as at risk. This is a proxy for the ISO property, not a
  # contradiction count, and is the one consistency measure computable without
  # declared rules.
  measure("RIES_INCO", "Risk of inconsistency", "consistency",
          a = sum(vif <= vif_threshold), b = length(vif))
}

m_cons_form <- function(data, spec) {
  cols <- chr_cols(data)
  if (length(cols) == 0L) {
    return(measure("CONS_FORM", "Format consistency", "consistency",
                   note = "no text columns to inspect"))
  }
  a <- 0; b <- 0; skipped <- character(0)
  for (nm in cols) {
    col <- as_chr(data[[nm]])
    vals <- col[!is.na(col)]
    if (length(vals) == 0L) next
    if (!is.null(spec$formats[[nm]])) {
      a <- a + sum(grepl(spec$formats[[nm]], vals, perl = TRUE))
      b <- b + length(vals)
      next
    }
    sigs <- format_signature(vals)
    if (!has_discernible_format(sigs, vals)) {
      skipped <- c(skipped, nm)
      next
    }
    # No format declared, so the column is measured against its own dominant
    # shape: the question becomes whether the column is internally consistent,
    # not whether it matches an external rule.
    a <- a + max(table(sigs)); b <- b + length(vals)
  }
  measure("CONS_FORM", "Format consistency", "consistency", a = a, b = b,
          note = if (length(skipped))
            paste("free-text columns excluded:", paste(skipped, collapse = ", ")))
}

m_int_ref <- function(data, spec) {
  cols <- intersect(names(spec$foreign_keys), names(data))
  if (length(cols) == 0L) {
    return(measure("INT_REF", "Referential integrity", "consistency",
                   note = "needs spec$foreign_keys"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    fk <- spec$foreign_keys[[nm]]
    permitted <- if (is.list(fk) && !is.null(fk$data)) {
      as_chr(fk$data[[fk$column %||% nm]])
    } else as_chr(fk)
    col <- as_chr(data[[nm]])
    vals <- col[!is.na(col)]
    if (length(vals) == 0L) next
    a <- a + sum(vals %in% permitted); b <- b + length(vals)
  }
  measure("INT_REF", "Referential integrity", "consistency", a = a, b = b)
}

m_cons_seman <- function(data, spec) {
  if (is.null(spec$rules) || length(spec$rules) == 0L) {
    return(measure("CONS_SEMAN", "Semantic consistency", "consistency",
                   note = "needs spec$rules"))
  }
  a <- 0; b <- 0; failed <- character(0)
  for (nm in names(spec$rules)) {
    result <- tryCatch(
      eval(spec$rules[[nm]][[2L]], envir = data, enclos = environment(spec$rules[[nm]])),
      error = function(e) e)
    # A rule naming a column the data does not have is a specification error,
    # not a data failure, so it is excluded from the denominator and reported.
    if (inherits(result, "error") || !is.logical(result)) {
      failed <- c(failed, nm)
      next
    }
    result <- rep_len(result, nrow(data))
    evaluable <- !is.na(result)
    a <- a + sum(result[evaluable]); b <- b + sum(evaluable)
  }
  measure("CONS_SEMAN", "Semantic consistency", "consistency", a = a, b = b,
          note = if (length(failed))
            paste("rules that could not be evaluated:", paste(failed, collapse = ", ")))
}

# ----------------------------------------------------------------- credibility

m_cred_val_dat <- function(data, spec) {
  cols <- union(names(spec$credible_ranges), names(spec$credible_domains))
  cols <- intersect(cols, names(data))
  if (length(cols) == 0L) {
    return(measure("CRED_VAL_DAT", "Data values credibility", "credibility",
                   note = "needs spec$credible_ranges or spec$credible_domains"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    col <- data[[nm]]
    vals <- col[!is.na(col)]
    if (length(vals) == 0L) next
    ok <- rep(TRUE, length(vals))
    lim <- spec$credible_ranges[[nm]]
    if (!is.null(lim) && is.numeric(vals)) {
      ok <- ok & vals >= lim[1L] & vals <= lim[2L]
    }
    if (!is.null(spec$credible_domains[[nm]])) {
      ok <- ok & as_chr(vals) %in% spec$credible_domains[[nm]]
    }
    a <- a + sum(ok); b <- b + length(vals)
  }
  measure("CRED_VAL_DAT", "Data values credibility", "credibility", a = a, b = b)
}

m_cred_fuen <- function(data, spec) {
  trust <- as.list(spec$source_trust %||% list())
  trust <- trust[intersect(names(trust), names(data))]
  if (length(trust) == 0L || is.null(spec$trusted_sources)) {
    return(measure("CRED_FUEN", "Source credibility", "credibility",
                   note = "needs spec$source_trust and spec$trusted_sources"))
  }
  measure("CRED_FUEN", "Source credibility", "credibility",
          a = sum(vapply(trust, function(s) all(s %in% spec$trusted_sources),
                         logical(1L))),
          b = length(trust))
}

# ------------------------------------------------------------------ currentness

# The columns the currentness measures read. A declared update column wins; with
# none, every parsed date column counts, which is what makes CONV_ACT the one
# currentness measure computable with no spec.
currentness_cols <- function(data, spec) {
  if (!is.null(spec$update_column)) {
    return(intersect(spec$update_column, date_cols(data)))
  }
  date_cols(data)
}

m_conv_act <- function(data, spec) {
  cols <- currentness_cols(data, spec)
  if (length(cols) == 0L) {
    return(measure("CONV_ACT", "Timeliness of update", "currentness",
                   note = "no date columns"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    vals <- as.Date(data[[nm]])
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0L) next
    age <- as.numeric(spec$as_of - vals)
    # Future-dated records are not current either: they are impossible, and
    # counting them as fresh would let a corrupt timestamp raise the score.
    a <- a + sum(age >= 0 & age <= spec$max_age); b <- b + length(vals)
  }
  measure("CONV_ACT", "Timeliness of update", "currentness", a = a, b = b)
}

m_frec_act <- function(data, spec) {
  if (is.null(spec$update_interval)) {
    return(measure("FREC_ACT", "Update frequency", "currentness",
                   note = "needs spec$update_interval"))
  }
  cols <- currentness_cols(data, spec)
  if (length(cols) == 0L) {
    return(measure("FREC_ACT", "Update frequency", "currentness",
                   note = "no date column to measure intervals on"))
  }
  a <- 0; b <- 0
  for (nm in cols) {
    vals <- sort(unique(as.Date(data[[nm]][!is.na(data[[nm]])])))
    if (length(vals) < 2L) next
    gaps <- as.numeric(diff(vals))
    a <- a + sum(gaps <= spec$update_interval); b <- b + length(gaps)
  }
  measure("FREC_ACT", "Update frequency", "currentness", a = a, b = b,
          note = if (b == 0) "fewer than two distinct update dates")
}

# ----------------------------------------------------------------- aggregation

dg_measure_table <- function(data, spec, consistency, vif_threshold) {
  ms <- list(
    m_comp_reg(data, spec), m_comp_val_esp(data, spec),
    m_comp_fich(data, spec), m_fal_comp_fich(data, spec),
    m_exac_sint(data, spec), m_exac_seman(data, spec), m_ran_exac(data, spec),
    m_int_ref(data, spec), m_ries_inco(consistency, vif_threshold),
    m_cons_seman(data, spec), m_cons_form(data, spec),
    m_cred_val_dat(data, spec), m_cred_fuen(data, spec),
    m_frec_act(data, spec), m_conv_act(data, spec)
  )
  out <- data.frame(
    code           = vapply(ms, `[[`, character(1L), "code"),
    property       = vapply(ms, `[[`, character(1L), "property"),
    characteristic = vapply(ms, `[[`, character(1L), "characteristic"),
    value          = vapply(ms, `[[`, numeric(1L), "value"),
    a              = vapply(ms, `[[`, numeric(1L), "a"),
    b              = vapply(ms, `[[`, numeric(1L), "b"),
    applicable     = vapply(ms, `[[`, logical(1L), "applicable"),
    note           = vapply(ms, `[[`, character(1L), "note"),
    stringsAsFactors = FALSE)
  out$characteristic <- factor(out$characteristic, levels = dg_characteristics)
  out[order(out$characteristic), , drop = FALSE]
}

dg_characteristics <- c("accuracy", "completeness", "consistency",
                        "credibility", "currentness")

# ISO/IEC 25024 defines the measures but not how they combine into a score for
# the characteristic above them, so the unweighted mean over the properties that
# apply is this package's choice and is stated as such. Properties that do not
# apply are dropped rather than scored zero.
dg_characteristic_scores <- function(measures) {
  vapply(dg_characteristics, function(ch) {
    vals <- measures$value[measures$characteristic == ch & measures$applicable]
    if (length(vals) == 0L) NA_real_ else mean(vals)
  }, numeric(1L))
}
