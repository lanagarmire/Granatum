#!/bin/bash

Rscript -e 'source("load_dependencies.R"); enableBookmarking("server"); runApp(".", host="0.0.0.0", port=8100)'
