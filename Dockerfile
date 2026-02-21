# syntax=docker/dockerfile:1
FROM rocker/r-ver:4.2.0

ENV DEBIAN_FRONTEND=noninteractive
ENV _R_CHECK_CRAN_INCOMING_=false
ENV _R_CHECK_BUILD_VIGNETTES_=false
ENV R_BUILD_VIGNETTES=false
# keep suggests required for tests (set to false only if you want to allow missing suggests)
ENV _R_CHECK_FORCE_SUGGESTS_=true

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential gfortran make git pkg-config \
    libcurl4-openssl-dev libssl-dev libxml2-dev libgit2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libtiff5-dev libjpeg-dev \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages('remotes', repos='https://cloud.r-project.org')"

WORKDIR /pkg

CMD bash -lc "set -euo pipefail; \
  test -f DESCRIPTION; \
  R -q -e \"options(repos=c(CRAN='https://cloud.r-project.org')); remotes::install_deps(dependencies=TRUE, upgrade='never')\"; \
  rm -f *.tar.gz; \
  R -q -e \"cat('R version: ', R.version.string, '\\n');\"; \
  R CMD build --no-build-vignettes .; \
  TARBALL=\$(ls -1t *.tar.gz | head -n 1); \
  echo \"Running: R CMD check --as-cran --no-manual \$TARBALL\"; \
  R CMD check --as-cran --no-manual \"\$TARBALL\""