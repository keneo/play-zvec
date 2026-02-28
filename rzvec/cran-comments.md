## R CMD check results

0 errors | 0 warnings | 1 note

* This is the first submission.

## Notes

* rzvec wraps the Python `zvec` package via `reticulate`.
  The Python dependency is **not** installed automatically;
  users must call `rzvec_install()` to set it up.
  All tests and examples skip gracefully when `zvec` is not available.
