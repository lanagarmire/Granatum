# Rscript installDependencies.r

installIfNeeded = function (packages, installFn = install.packages) {
	newPackages <- packages[!(packages %in% installed.packages()[, "Package"])]
	if (length(newPackages)) installFn(newPackages)
}

cranPackages = c(
	"devtools", "Rtsne", "igraph", "visNetwork", "shiny", "htmltools", "shinyjs", "htmlwidgets", "plotly", "Rlof",
	"scales", "reshape2", "purrr", "markdown", "shinythemes", "readr", "stringr", "tidyr", "tibble", "forcats",
	"dplyr", "NMF", "dplyr")

installIfNeeded(cranPackages)

source("https://bioconductor.org/biocLite.R")
bioPackages = c(
	"monocle", "preprocessCore", "edgeR", "scde", "sva", "fgsea", "KEGG.db", "limma",
	"GO.db", "org.Hs.eg.db", "org.Mm.eg.db", "SummarizedExperiment", "impute"
)

installIfNeeded(bioPackages, biocLite)

library(devtools)

installIfNeeded("scImpute", function (new) install_github("Vivianstats/scImpute"))
installIfNeeded("SAVER", function (new) install_github("mohuangx/SAVER@*release"))

# install.packages("MetaDE", repos = "http://cran.us.r-project.org")
installIfNeeded("MetaDE", function (new) devtools::install_url("https://cran.r-project.org/src/contrib/Archive/MetaDE/MetaDE_1.0.5.tar.gz"))


# Linux packages:
# Fedora: sudo dnf install libXt-devel