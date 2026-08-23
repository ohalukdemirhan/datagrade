## Test environments

* local macOS 26.1 (aarch64-apple-darwin20), R 4.5.2 -- `R CMD check --as-cran`
* win-builder, Windows Server 2022 x64, R-devel (2026-08-22 r90443 ucrt)

## R CMD check results

0 errors | 0 warnings | 1 note on each platform.

This is a new submission, so "New submission" is part of the note everywhere.
The rest of the note differs by platform and is addressed below.

### win-builder R-devel

```
Possibly misspelled words in DESCRIPTION:
  MSc (18:37)
  computable (12:30)
  currentness (9:58)
  multicollinearity (16:5)
```

All four are spelled correctly:

* "MSc" is the degree abbreviation in the dissertation citation.
* "computable" and "multicollinearity" are ordinary English words.
* "currentness" is the name ISO/IEC 25012 gives to one of the five inherent
  data quality characteristics this package scores, so it is used deliberately
  rather than substituted with "currency" or "timeliness".

### local macOS

```
Found the following (possibly) invalid URLs:
  URL: https://www.iso.org/standard/35736.html
  URL: https://www.iso.org/standard/35749.html
    Status: 403
    Message: Forbidden
```

These are the official ISO catalogue pages for ISO/IEC 25012 and ISO/IEC 25024,
the standards this package implements. They resolve correctly in a browser;
iso.org returns 403 to automated requests, so the note is a false positive and
the links are left as they are. win-builder did not report them.

## Notes on the DESCRIPTION

Standard names are quoted as 'ISO/IEC' throughout, and the dissertation title
is given in double quotes as the source of the methodology.
