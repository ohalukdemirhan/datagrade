## Update

This is an update from 0.2.0 to 0.3.0. It adds one exported function,
`dg_compare()`, which compares two deliveries of the same data set on the
ISO/IEC 25012 measures, with its tests, an example and a vignette section. It also fixes two
date-parsing bugs found while testing on public city data sets; see NEWS.md.

## Test environments

* local macOS 26.1 (aarch64-apple-darwin20), R 4.5.2 -- `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 1 note.

```
Found the following (possibly) invalid URLs:
  URL: https://www.iso.org/standard/35736.html
  URL: https://www.iso.org/standard/35749.html
    Status: 403
    Message: Forbidden
```

These are the official ISO catalogue pages for ISO/IEC 25012 and ISO/IEC 25024,
the standards this package implements. They resolve in a browser; iso.org
returns 403 to automated requests. The same URLs were accepted with 0.2.0.

## Reverse dependencies

There are no reverse dependencies.
