# craibm cloud execution image
#
# Built automatically by GitHub Actions and pulled by Cloud Batch. Everything
# the simulation needs is compiled in advance, so a run only has to fetch its
# payload and start work.

FROM rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive

# System libraries: OpenMP for the individual-level parallelism, linear algebra
# for RcppArmadillo, plus the tools used to fetch and package data.
RUN apt-get update && apt-get install -y --no-install-recommends \
      build-essential \
      gfortran \
      libomp-dev \
      liblapack-dev \
      libblas-dev \
      libcurl4-openssl-dev \
      libssl-dev \
      libxml2-dev \
      libicu-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libpng-dev \
      libjpeg-dev \
      libtiff-dev \
      zip \
      unzip \
      curl \
      python3 \
      python3-pip \
    && rm -rf /var/lib/apt/lists/*

# The Cloud SDK provides gsutil, which the entry point uses to move files.
RUN curl -sSL https://sdk.cloud.google.com | bash -s -- --disable-prompts --install-dir=/opt \
    && ln -s /opt/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil \
    && ln -s /opt/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud

# R dependencies, installed before the package itself so that changing the
# model source does not force them all to be rebuilt.
RUN install2.r --error --skipinstalled \
      Rcpp \
      RcppArmadillo \
      shiny \
      bs4Dash \
      DT \
      foreach \
      doParallel \
      data.table \
      dplyr \
      tidyr \
      ggplot2 \
      scales \
      readr \
      readxl \
      car \
      lme4 \
      FSA \
      stringr \
      purrr \
      shinyFiles \
      fs \
      waiter \
      shinyjs \
      callr \
      lmeresampler \
      ps \
      jsonlite \
      httr \
      openssl

COPY . /opt/craibm_src

RUN R CMD INSTALL --no-multiarch /opt/craibm_src \
    && mkdir -p /opt/craibm \
    && cp /opt/craibm_src/inst/cloud/entrypoint.R /opt/craibm/entrypoint.R \
    && chmod +x /opt/craibm/entrypoint.R

WORKDIR /workspace

ENTRYPOINT ["/usr/local/bin/Rscript", "/opt/craibm/entrypoint.R"]
