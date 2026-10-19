## Resubmission

This is a resubmission. In response to the review of 3 September 2026:

* Removed the single quotes around ISO/IEC in the Title and Description; only
  package, software and API names would be quoted, and there are none.
* Added links to the references in the Description, in the form
  authors (year) <https:...>: the two ISO/IEC standards and the dissertation
  the methodology comes from. None of the three has a DOI or ISBN.
* `dg_save_plots()` no longer has a default output directory, so nothing is
  written unless the user supplies a path. Its new example writes to
  `tempdir()` and removes the files afterwards. No other function, example,
  vignette or test writes outside `tempdir()`.

## Test environments

* local macOS 26.1 (aarch64-apple-darwin20), R 4.5.2 -- `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note.

```
New submission

Found the following (possibly) invalid URLs:
  URL: https://www.iso.org/standard/35736.html
  URL: https://www.iso.org/standard/35749.html
    Status: 403
    Message: Forbidden
```

These are the official ISO catalogue pages for ISO/IEC 25012 and ISO/IEC 25024,
the standards this package implements. They resolve correctly in a browser;
iso.org returns 403 to automated requests, so the note is a false positive.
The first submission's win-builder and Debian pretests did not report them.

The first submission's pretest also flagged "computable", "currentness" and
"multicollinearity" as possibly misspelled. All three are spelled correctly;
"currentness" is the name ISO/IEC 25012 gives to one of the five inherent data
quality characteristics, so it is used deliberately rather than replaced with
"currency" or "timeliness". "Demirhan" is the author's surname.
