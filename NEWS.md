# mlrMBO 1.1.1

* `makeMBOControl()` has `on.surrogate.error` argument which enables random proposals if the surrogate model fails.
* With `initSMBO()`, `updateSMBO()` and `finalizeSMBO()` it is now possible to do a human-in-the-loop MBO.
* The result now contains the `final.opt.state`.
* Plot method for `OptState` objects.

# mlrMBO 1.1.0

* Fixed bug in focus search that affected discrete search spaces.
* Numerics will be auto converted to integers where integers are expected.
* Package can now be cited with `citation("mlrMBO")`.

# mlrMBO 1.0.0

* Initial CRAN release
