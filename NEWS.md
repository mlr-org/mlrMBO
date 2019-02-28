# mlrMBO 1.1.3

* Bugfix: `plot(opt.state)` now also works for Param Sets with transformations.
* Bugfix: Infill optimization with CMAES now supports restarts.

# mlrMBO 1.1.2

* Adaptive infill criterions. Infill criterions now have to support an `progress` argument. Termination criterions now can supply a `progress` return value.
* Fix for parEGO + EI (Issue #407)
* `save.on.disk` now can take arbitrary numeric vectors to specify iterations, when to save on disk.
* Spelling mistakes for infill criterions will now be cought. (Issue #417)

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
