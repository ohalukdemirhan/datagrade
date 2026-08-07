# datagrade

Point it at a data frame. Get a score.

`datagrade` assesses the quality of a tabular data set and scores it on four
dimensions — completeness, accuracy, consistency and timeliness — then returns a
report object with tables and figures. No configuration, no metadata file, no
rules to write first.

That last part is the point. The existing R packages for data quality
(`DQA`, `DQAstats`, `dataquieR`) all require a metadata repository or data
dictionary before they will run. `datagrade` runs on anything with columns.

## Install

```r
# install.packages("remotes")
remotes::install_github("ohalukdemirhan/datagrade")
```

## Use

```r
library(datagrade)

report <- dg_assess(ggplot2::diamonds)
```

```
── Data quality report ─────────────────────────────────────────────────
Source: in-memory data frame
Size: 53940 rows x 10 columns
Elapsed: 0.15s

── Scores ──

• completeness: 10.0/10 (100%) - good
• accuracy: 9.9/10 (99.3%) - good
• consistency: 2.9/10 (28.6%) - critical
• timeliness: not applicable

Overall: 7.6/10 (75.9%) - warning

── Findings ──

• Missing values: 0 across 0 columns
• Duplicate rows: 146 (0.27%)
• Outliers (z_score): 2798 of 377580 numeric values
• Redundant pairs above |r| > 0.5: 10 (strongest carat~x at 0.98)
• Suggested to drop: carat, price, x, and y
• VIF above 5: carat, price, x, y, and z
```

The printout is a side effect. The object is the product:

```r
report$scores            # named vector, 0-10
report$redundancy$pairs  # data frame of correlated column pairs
summary(report)$columns  # per-column type, missingness, outliers, VIF
as.data.frame(report)    # one row, for stacking across many data sets
```

Figures come from `dg_plots(report)`:

![Quality scores](man/figures/scores.png)

![Correlation](man/figures/correlation.png)

Also works on files — `dg_assess(path = "data.csv")` — for `.csv`, `.tsv`,
`.txt`, `.xls` and `.xlsx`.

## It scales

Benchmarked on `ggplot2::diamonds` (53,940 × 10, MIT licensed), then the same
real data replicated with jitter. R 4.5.2, macOS arm64, single core.

| Rows | Input size | Full assessment | Figures | Report object |
|---:|---:|---:|---:|---:|
| 53,940 | 3.5 MB | 0.12 s | 0.11 s | 82 KB |
| 500,000 | 34 MB | 1.40 s | 0.10 s | 84 KB |
| 1,000,000 | 68 MB | 2.99 s | 0.10 s | 83 KB |
| 5,000,000 | 340 MB | 17.08 s | 0.11 s | 83 KB |
| **10,000,000** | **680 MB** | **38.04 s** | **0.12 s** | **83 KB** |

Cost per row is 1.37 µs at 54 thousand rows and 0.94 µs at 10 million — it goes
*down*, because fixed startup amortises. The input grows 185-fold; the report
object grows 1%, because it holds statistics and never the data. Figures render
in constant time at every size. Scores are identical at every size, which is how
you know the subsampling isn't biasing anything.

Reproduce it yourself: `Rscript inst/bench/benchmark.R`.

Three dependencies: `cli`, `ggplot2`, base R.

## Documentation

Everything technical lives in the vignettes, not here.

- `vignette("datagrade")` — the full walkthrough
- `vignette("methodology")` — every formula, written out
- `vignette("scalability")` — how the numbers above are achieved
- `vignette("deviations")` — how this differs from the dissertation it implements

## Background

This implements the framework from:

> Demirhan, O. H. (2024). *Assessing Data Quality Management Issues in
> Open-Sourced Data Sets with Statistical Methodology.* MSc dissertation,
> University of Greenwich.

The four dimension **names** follow the ISO/IEC 25012 data quality model. The
**measures** are this package's own — they are not ISO/IEC 25024 conformant
measures, and the package does not claim to be. `vignette("methodology")`
explains exactly what conformance would require and why zero-configuration
assessment cannot deliver it.

Numbers here will not always match numbers in the dissertation.
`vignette("deviations")` lists every difference and the reason for it.

## Licence

MIT © Omer Haluk Demirhan
