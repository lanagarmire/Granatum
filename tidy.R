#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
formatR::tidy_source(args[1], file = args[1], indent = 2, width.cutoff = 100, arrow = T)
