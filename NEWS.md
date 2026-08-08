# datagrade 0.2.0

Scoring now follows the ISO/IEC 25012 data quality model: five inherent
characteristics, fifteen quality properties, each measured as a ratio `A / B`
over counted data items in the form ISO/IEC 25024 measurement functions take.
Every statistic the package computes is unchanged; what changed is how those
statistics are organised into scores.

## New

* `dg_spec()` declares what the data is supposed to look like — required
  columns, permitted domains, valid ranges, regular expressions, referential
  sets, per-record rules, credibility criteria, provenance and update
  expectations. Nine of the fifteen properties have a denominator that counts
  *the items for which an expectation is defined*, which no data set contains;
  this is where it comes from. Every argument is optional.
* `report$measures` is a fifteen-row data frame giving each property's ISO code,
  characteristic, value, `A`, `B`, whether it applied, and — when it did not —
  which `dg_spec()` argument would make it apply.
* Six properties are computable with no spec at all: `COMP_REG`,
  `COMP_VAL_ESP`, `FAL_COMP_FICH`, `RIES_INCO`, `CONS_FORM` and `CONV_ACT`.
  `FAL_COMP_FICH` catches cells that are occupied but empty (`"N/A"`,
  `"unknown"`, `"-999"`); `CONS_FORM` measures each column against its own
  dominant format, excluding free text rather than scoring it near zero.

## Breaking

* `report$scores` now holds the five ISO characteristics — `accuracy`,
  `completeness`, `consistency`, `credibility`, `currentness` — where it held
  four dimensions.
* `timeliness` is renamed `currentness`, which is ISO's word for it.
  `report$timeliness` still resolves and `weights` still accepts the old name.
* **`accuracy` is `NA` unless a spec declares an interval, pattern or
  reference.** All three ISO accuracy properties require a *declared*
  expectation. The old measure — outliers against an interval derived from the
  data — is unchanged, reported as `report$plausibility`, and excluded from the
  overall score, because an outlier need not be wrong and a wrong value can sit
  exactly on the mean.
* Completeness counts cells, not the average of per-column missing rates. The
  old quantity remains derivable from `report$missing$columns`.
* Scores are **ratios in 0--1**, where they were on the dissertation's 0--10
  scale. Percentages are unchanged, so every reported percentage reads exactly
  as before. Code comparing raw scores against 0--10 thresholds needs its
  cut-offs divided by ten; the `good`/`warning`/`serious`/`critical` bands now
  break at 0.9, 0.7 and 0.5.
* A characteristic with no applicable property is `NA` and is dropped from the
  aggregate rather than scored zero. `NA` is a statement that `B` could not be
  counted, not a claim about the data.
* `summary()$scores` names its first column `characteristic`, not `dimension`,
  and `summary()` gains a `measures` table.

## Correctness

* `dg_assess()` no longer aborts on its own default of `verbose = TRUE`. Progress
  messages are interpolated by `cli` in the frame of the caller, which was the
  internal `say()` helper rather than `dg_assess()`, so every message referring
  to a local variable failed.

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

* Consistency is scored as a ratio in 0--1 like every other dimension. It was
  previously multiplied by ten as though it were on a 0--10 scale, which is why
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
