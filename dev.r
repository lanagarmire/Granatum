# A collection of miscellanous lines you can run in your development of this
# package. Nothing in here is exported or used anywhere else. Purely
# helpers for development.

granatum.other = function () {
  library(devtools)
  # Check package
  devtools::check()

  # Update datasets
  # Saving datasets
  use_data(someData, overwrite = TRUE)

  install.packages("roxygen2")
  install.packages("testthat")
  install.packages("usethis")
  install.packages("covr")
  install.packages("DT")

  usethis::use_test("nodesDiffExp")

  devtools::test()
  granatum.codeCoverage()

  devtools::build_win()
  create("cats")
  devtools::release()

  # devtools::revdep_check()

  install.packages("granatum_0.1.0.tar.gz", repos = NULL, type ="source")
}

granatum.makeDoc = function (dataFrame, title = substitute(dataFrame)) {
  output = c(paste("#'", title), "#' @format data.frame", gsub("^","#'",capture.output(str(dataFrame))), dQuote(title))
  cat(output, sep="\n")
}

granatum.codeCoverage = function () {
  library(covr)
  report()
}

# R CMD build .
# R CMD check --as-cran granatum.tar.gz
