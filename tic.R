do_package_checks(error_on = "warning")

if (ci_has_env("BUILD_PKGDOWN")) {
  get_stage("install") %>%
    add_step(step_install_github("mlr-org/mlr3pkgdowntemplate")) %>%
    add_step(step_install_cran("animation")) %>%
    add_step(step_install_cran("e1071")) %>%
    add_step(step_install_cran("reshape2"))

  do_pkgdown(orphan = TRUE)
}
