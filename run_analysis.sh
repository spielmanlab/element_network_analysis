#! /bin/bash


# This bash script simply knits the Rmarkdown file, 
# and copies the knitted HTML _and_ associated libs into `docs/`
# so the Rmd can be rendered by gh-pages. Safer than htmlpreview thing.

# Render
Rscript -e "rmarkdown::render('analyze_element_networks.Rmd', clean = TRUE)"

# Copy
cp analyze_element_networks.html docs/index.html



