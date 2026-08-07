# datagrade 0.1.0

First release. Implements the data quality framework from Demirhan (2024), MSc
dissertation, University of Greenwich.

## Interface

* `dg_assess()` assesses a data frame or file and returns a `dg_report` object,
  with `print()`, `summary()`, `plot()` and `as.data.frame()` methods.
  `data_quality_assessment()` is kept as an alias.
* The `data` argument works. The dissertation documented it in three places and
  implemented it in none.
* `dg_plots()` returns figures as a named list of ggplot objects;
  `dg_save_plots()` writes them to disk.
* All thirteen checks are exported and usable on their own.
* Dependencies reduced to `cli`, `ggplot2` and base R. `dplyr`, `car`,
  `moments`, `reshape2`, `DescTools`, `mice` and `caret` are no longer needed;
  `mice` and `caret` were never used by the original code at all.

## Correctness

* Consistency is scored on 0--10 like every other dimension. It previously
  returned 0--1 and was multiplied by ten as though it were 0--10, which is why
  the dissertation reports 2.5% for `iris` where 25% was meant.
* `flag_redundant_columns()` no longer reports the literal strings `"row"` and
  `"col"` as redundant data columns.
* Figures are actually produced. `ggplot(...) + theme_minimal() %>% print()`
  printed the theme and discarded the plot.
* Kurtosis is excess kurtosis, matching the dissertation's stated formula rather
  than `moments::kurtosis()`.
* Identifier detection runs before the storage-type tests, so the `"ID"` branch
  is reachable. A numeric column qualifies only if its values are whole numbers,
  so continuous measurements are not mistaken for keys and dropped.
* Dates are parsed before text normalisation, so timeliness is measurable from a
  CSV.
* Text normalisation is Unicode-aware and non-destructive by default.
* Outliers are counted on observed values rather than mean-imputed ones.
* Nothing is written to the global environment.
* The caller's data frame is never modified.

## Robustness

Regression tests cover constant columns, single-column frames, all-`NA` columns,
zero-row frames, perfect collinearity and single redundant pairs — each of which
produced an `NA`, an error or a silently wrong answer before.

## Performance

* Duplicate detection rewritten from `duplicated.data.frame` to integer-coded
  row hashing: 10 million rows went from 494 s to 38 s, bit-identical result.
* VIF computed as `diag(solve(cor(X)))`, which is independent of row count.
* Figures built from precomputed bin counts and five-number summaries, so they
  render in constant time and the report object stays at ~83 KB regardless of
  input size.
* Shapiro-Wilk and the anomaly model use deterministic subsampling that neither
  depends on nor disturbs the caller's random state.

## Known limitations

* Not ISO/IEC 25024 measure-conformant. Dimension names follow ISO/IEC 25012;
  the measures are this package's own. Genuine `X = A/B` measures require
  metadata that zero-configuration assessment does not have. See
  `vignette("methodology")`.
* `credibility`, the fifth ISO/IEC 25012 inherent characteristic, is not
  measured.
* The `accuracy` score substitutes a statistically derived interval for a
  specified one, so it measures plausibility rather than accuracy in the ISO
  sense.
* Cost grows as `O(p^3)` in the *column* count through the VIF matrix inversion.
  Fine for hundreds of columns, not for tens of thousands.
