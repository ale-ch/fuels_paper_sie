# Use a specific R version tag for reproducibility (e.g., 4.4.1)
# FROM rocker/geospatial:latest

FROM --platform=linux/amd64 rocker/geospatial:4.5.1
# FROM rocker/geospatial:4.5.1

COPY requirements.csv /tmp/requirements.csv

RUN Rscript -e " \
  if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes'); \
  reqs <- read.csv('/tmp/requirements.csv'); \
  for(i in 1:nrow(reqs)) { \
    remotes::install_version(reqs\$Package[i], version = reqs\$Version[i], upgrade = 'never') \
  } \
"

WORKDIR /workspace
