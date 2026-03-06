## R CMD check results

0 errors | 0 warnings | 2 NOTEs

* NOTE: "New submission" — this is the first CRAN submission of earthUI.
* NOTE: "Skipping checking math rendering: package 'V8' unavailable" — V8 is
  not required; math rendering uses MathJax in the Shiny app.

## Test environments

* Local: macOS Tahoe (aarch64), R 4.5.2
* GitHub Actions: ubuntu-latest (R release, R devel, R oldrel-1),
  macOS-latest (R release), windows-latest (R release)

## Resubmission

Addresses reviewer feedback from initial submission:

* Single-quoted all software/package names in Title and Description
* Replaced `\dontrun{}` with `\donttest{}` in examples
* Examples now write to `tempdir()` instead of user filespace
