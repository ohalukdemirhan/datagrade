---
title: 'datagrade: Computable ISO/IEC 25012 Data Quality Measures for R'
tags:
  - R
  - data quality
  - data governance
  - ISO/IEC 25012
  - reproducibility
  - data validation
authors:
  - name: Omer Haluk Demirhan
    orcid: 0009-0006-6188-8682
    affiliation: 1
affiliations:
  - name: Independent Researcher, London, United Kingdom
    index: 1
date: 23 August 2026
bibliography: paper.bib
---

# Summary

`datagrade` assesses the quality of a tabular dataset against the data quality
model defined in ISO/IEC 25012 [@iso25012] and returns a structured report
rather than console output. A single call, `dg_assess(data)`, computes fifteen
measured properties, aggregates them into five characteristics -- accuracy,
completeness, consistency, credibility and currentness -- and reports the
diagnostics that produced them: missing values, duplicate records, outliers,
redundant column pairs and variance inflation factors.

Every measure takes the ratio form `A / B`, where `A` counts the items
satisfying the property and `B` counts the items for which the property is
defined at all. Properties that cannot be measured from the data alone return
`NA` rather than zero, and are dropped from aggregation rather than scored as
failures. This distinction between *not measurable* and *measurably bad* is the
central design commitment of the package: nine of the fifteen properties require
the user to declare expectations through `dg_spec()`, and in the absence of a
declaration `datagrade` reports that it does not know, not that the data is
poor.

The report object is designed for logging and audit trails. `report$scores`
gives the characteristic ratios, `report$measures` gives every property with its
ISO code, its `A` and `B` counts and its applicability, and
`as.data.frame(report)` returns a single row so that assessments of many
datasets can be stacked and compared over time.

# Statement of need

Data quality obligations appear throughout contemporary data governance and AI
assurance frameworks, but the standards that define the vocabulary do not ship
executable measures. ISO/IEC 25012 specifies *characteristics*; ISO/IEC 25024
[@iso25024] names *properties* and their measurement functions, but the document
is paywalled and, critically, does not specify how properties aggregate into
characteristic-level or dataset-level scores. Practitioners are therefore asked
to evidence data quality using a vocabulary that has no reference
implementation.

Existing R tooling addresses adjacent problems. `validate` [@vanderloo2021]
provides a rich infrastructure for expressing and checking user-defined rules,
and `pointblank` offers agent-based validation workflows; both are excellent at
answering *did this data satisfy the constraints I wrote?* Neither answers
*where does this dataset sit against a published quality model, and which parts
of that model could I not evaluate?* Outside R, `Deequ` [@schelter2018] and
TensorFlow Data Validation [@breck2019] compute constraint- and schema-based
quality signals at scale, but are similarly organised around user constraints
rather than around a standard's characteristic model.

`datagrade` fills that gap: it maps a dataset onto the ISO/IEC 25012 model,
computes what is computable from structure alone, and states explicitly what
additional declarations would be required to compute the rest. Six properties
require no specification at all, which means a dataset can be screened
immediately and the screening result carries an honest account of its own
coverage. This makes the output usable as governance documentation, in model
risk assessment, and as a repeatable pre-analysis check on third-party or
open-licensed data, a setting where the recipient has no control over
collection and often no metadata at all [@sambasivan2021; @gebru2021].

# Design and implementation

The package (version 0.2.0) has two non-base dependencies, `cli` and `ggplot2`. It accepts data frames
or file paths (CSV, TSV, TXT, XLS, XLSX). `dg_spec()` collects declarative
expectations -- required columns, regular expression patterns, categorical
domains, numeric ranges, cross-column rules, foreign keys, credibility
thresholds, trusted sources, expected update intervals and reference tables --
and each declaration switches on the properties it makes measurable.

Aggregation is deliberately simple and documented as a package choice rather
than as standard conformance: an unweighted mean of applicable properties within
a characteristic, and a weighted mean across applicable characteristics with
weights renormalised over whatever remains applicable. The default weights are
equal, so the default overall score is itself an unweighted mean. A dataset with no date
columns drops currentness entirely rather than scoring zero for it.

One measure, *plausibility*, is reported separately and excluded from the
overall score. It flags outliers using a data-derived interval (z-score
threshold 3, or the 1.5 IQR fence), which is not what ISO accuracy means:
accuracy properties require a *declared* range, pattern or reference. Keeping
plausibility visible but outside the score preserves the diagnostic without
misrepresenting it as a standards measure.

Assessment is single-pass and linear in table size. On a commodity laptop
(R 4.5.2, macOS arm64, single core) a full assessment of a 10,000,000 x 10
table (680 MB) completes in 38.0 s, or 9.4 s with duplicate detection disabled, with per-row cost falling from 1.37 to
0.94 microseconds between the smallest and largest benchmark as fixed startup
cost amortises. The report object stays at roughly 83 KB across all scales,
which is what makes retaining assessments as audit records practical.

# Reproducibility of the originating research

The methodology originates in the author's MSc dissertation [@demirhan2024]. In
re-implementing it as a distributable package, several defects in the original
implementation were identified and corrected, including two scores computed on
inconsistent scales, an excess-kurtosis term that omitted the constant, outlier
detection applied to imputed rather than observed values, and an identifier
check made unreachable by an earlier type test. Each correction, together with
the resulting numerical differences from the figures reported in the
dissertation, is documented in `vignette("deviations")`. The package is
therefore the corrected reference implementation of that work, and the
divergence is recorded rather than quietly absorbed.

# Limitations

Aggregation weights, the collinearity-as-inconsistency proxy, the default
thresholds and the format-signature heuristic are package design decisions, not
derivations from ISO/IEC 25024. The measurement functions implement named
properties but are not reproductions of the standard's own functions, and the
package should be treated as a screening instrument rather than as evidence of
conformance. Specification-based, conformance-oriented measurement is the
subject of ongoing work.

# Acknowledgements

The underlying dissertation was supervised by Dr Solomon Ebenuwa at the
University of Greenwich.

# References
