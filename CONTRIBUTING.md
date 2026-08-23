# Contributing to datagrade

Thanks for considering a contribution.

## Reporting problems

Open an issue at https://github.com/ohalukdemirhan/datagrade/issues. For a bug,
please include a minimal reproducible example, the output of
`sessionInfo()`, and the `datagrade` version.

Measurement disagreements are especially welcome. If a property returns a value
you believe is wrong, or returns `NA` where you think it should be measurable,
that is a substantive issue rather than a nuisance one, and it is the kind of
report most likely to improve the package.

## Proposing changes

1. Open an issue before starting work on anything non-trivial, so we can agree
   on the approach.
2. Fork, branch from `main`, and keep the change focused.
3. Add or update tests under `tests/testthat/`. Any change to a measurement
   function must come with a test that pins the new numeric behaviour.
4. Run `devtools::check()` and make sure it passes with no errors, warnings or
   notes.
5. If the change alters a documented formula, update `vignette("methodology")`
   in the same pull request.
6. If the change alters a number reported in the originating dissertation,
   record it in `vignette("deviations")`.

## Scope

`datagrade` measures. It does not clean, repair or impute as part of assessment.
Contributions that add remediation to `dg_assess()` will be declined; the
separation is deliberate, because a tool that silently repairs data cannot
honestly report on it.

## Code style

Follow the existing style in `R/`. Base R and the three declared dependencies
only; new dependencies need a strong justification.

## Conduct

Be civil and assume good faith.
