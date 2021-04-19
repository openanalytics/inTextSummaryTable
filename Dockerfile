# Generated by packamon: do not edit by hand
# Instead of modifying this file, you can modify a template. See ?init for details.
# Generated on: 2021-03-05 13:00:03 CET

FROM openanalytics/r-ver:3.6.1

# System libraries (incl. system requirements for R packages)
RUN apt-get update && apt-get install --no-install-recommends -y \
    libssl-dev \
    zlib1g-dev \
    pandoc pandoc-citeproc \
    libxml2-dev \
    libcairo2 \
    libcairo2-dev \
    libicu-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "cat(\"local(options(repos = c(CRAN = 'https://cloud.r-project.org')))\n\", file = R.home('etc/Rprofile.site'), append = TRUE)"

# install dependencies
RUN R -e "install.packages('remotes')" && \
    R -e "remotes::install_version('assertthat', version = '0.2.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('base64enc', version = '0.1-3', upgrade = FALSE)" && \
    R -e "remotes::install_version('brew', version = '1.0-6', upgrade = FALSE)" && \
    R -e "remotes::install_version('brio', version = '1.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('colorspace', version = '2.0-0', upgrade = FALSE)" && \
    R -e "remotes::install_version('commonmark', version = '1.7', upgrade = FALSE)" && \
    R -e "remotes::install_version('cpp11', version = '0.2.6', upgrade = FALSE)" && \
    R -e "remotes::install_version('crayon', version = '1.4.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('curl', version = '4.3', upgrade = FALSE)"
RUN R -e "remotes::install_version('data.table', version = '1.14.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('digest', version = '0.6.27', upgrade = FALSE)" && \
    R -e "remotes::install_version('evaluate', version = '0.14', upgrade = FALSE)" && \
    R -e "remotes::install_version('fansi', version = '0.4.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('farver', version = '2.1.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('generics', version = '0.1.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('glue', version = '1.4.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('gtable', version = '0.3.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('highr', version = '0.8', upgrade = FALSE)" && \
    R -e "remotes::install_version('isoband', version = '0.2.4', upgrade = FALSE)"
RUN R -e "remotes::install_version('jsonlite', version = '1.7.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('labeling', version = '0.4.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('lattice', version = '0.20-41', upgrade = FALSE)" && \
    R -e "remotes::install_version('lazyeval', version = '0.2.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('magrittr', version = '2.0.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('MASS', version = '7.3-53.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('mime', version = '0.10', upgrade = FALSE)" && \
    R -e "remotes::install_version('pkgconfig', version = '2.0.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('praise', version = '1.0.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('prettyunits', version = '1.1.1', upgrade = FALSE)"
RUN R -e "remotes::install_version('ps', version = '1.6.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('R6', version = '2.5.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('RColorBrewer', version = '1.1-2', upgrade = FALSE)" && \
    R -e "remotes::install_version('Rcpp', version = '1.0.6', upgrade = FALSE)" && \
    R -e "remotes::install_version('rlang', version = '0.4.10', upgrade = FALSE)" && \
    R -e "remotes::install_version('rprojroot', version = '2.0.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('rstudioapi', version = '0.13', upgrade = FALSE)" && \
    R -e "remotes::install_version('stringi', version = '1.5.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('sys', version = '3.4', upgrade = FALSE)" && \
    R -e "remotes::install_version('utf8', version = '1.1.4', upgrade = FALSE)"
RUN R -e "remotes::install_version('uuid', version = '0.1-4', upgrade = FALSE)" && \
    R -e "remotes::install_version('viridisLite', version = '0.3.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('withr', version = '2.4.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('xfun', version = '0.21', upgrade = FALSE)" && \
    R -e "remotes::install_version('xml2', version = '1.3.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('yaml', version = '2.2.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('zip', version = '2.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('askpass', version = '1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('cli', version = '2.3.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('desc', version = '1.2.0', upgrade = FALSE)"
RUN R -e "remotes::install_version('diffobj', version = '0.3.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('ellipsis', version = '0.3.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('htmltools', version = '0.5.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('lifecycle', version = '1.0.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('markdown', version = '1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('Matrix', version = '1.3-2', upgrade = FALSE)" && \
    R -e "remotes::install_version('munsell', version = '0.5.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('nlme', version = '3.1-152', upgrade = FALSE)" && \
    R -e "remotes::install_version('officer', version = '0.3.17', upgrade = FALSE)" && \
    R -e "remotes::install_version('pander', version = '0.6.3', upgrade = FALSE)"
RUN R -e "remotes::install_version('plyr', version = '1.8.6', upgrade = FALSE)" && \
    R -e "remotes::install_version('processx', version = '3.4.5', upgrade = FALSE)" && \
    R -e "remotes::install_version('purrr', version = '0.3.4', upgrade = FALSE)" && \
    R -e "remotes::install_version('rex', version = '1.2.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('stringr', version = '1.4.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('systemfonts', version = '1.0.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('tinytex', version = '0.30', upgrade = FALSE)" && \
    R -e "remotes::install_version('callr', version = '3.5.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('gdtools', version = '0.2.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('knitr', version = '1.31', upgrade = FALSE)"
RUN R -e "remotes::install_version('mgcv', version = '1.8-34', upgrade = FALSE)" && \
    R -e "remotes::install_version('openssl', version = '1.4.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('reshape2', version = '1.4.4', upgrade = FALSE)" && \
    R -e "remotes::install_version('scales', version = '1.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('vctrs', version = '0.3.6', upgrade = FALSE)" && \
    R -e "remotes::install_version('httr', version = '1.4.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('pillar', version = '1.5.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('pkgbuild', version = '1.2.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('rmarkdown', version = '2.7', upgrade = FALSE)" && \
    R -e "remotes::install_version('tidyselect', version = '1.1.0', upgrade = FALSE)"
RUN R -e "remotes::install_version('covr', version = '3.5.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('flextable', version = '0.6.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('pkgload', version = '1.2.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('tibble', version = '3.1.0', upgrade = FALSE)" && \
    R -e "remotes::install_version('dplyr', version = '1.0.4', upgrade = FALSE)" && \
    R -e "remotes::install_version('ggplot2', version = '3.3.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('rematch2', version = '2.1.2', upgrade = FALSE)" && \
    R -e "remotes::install_version('roxygen2', version = '7.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('cowplot', version = '1.1.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('ggrepel', version = '0.9.1', upgrade = FALSE)"
RUN R -e "remotes::install_version('waldo', version = '0.2.4', upgrade = FALSE)" && \
    R -e "remotes::install_version('testthat', version = '3.0.2', upgrade = FALSE)"

# extra dependencies of clinUtils
RUN R -e "remotes::install_version('haven', version = '2.3.1', upgrade = FALSE)" && \
    R -e "remotes::install_version('png', version = '0.1-7', upgrade = FALSE)" && \
    R -e "remotes::install_version('htmlwidgets', version = '1.5.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('plotly', version = '4.9.3', upgrade = FALSE)" && \
    R -e "remotes::install_version('DT', version = '0.17', upgrade = FALSE)" && \
    R -e "remotes::install_version('crosstalk', version = '1.1.1', upgrade = FALSE)"

# clinUtils
COPY clinUtils_*.tar.gz /tmp/clinUtils.tar.gz
RUN R -e "install.packages('/tmp/clinUtils.tar.gz', repos = NULL, dependencies = FALSE)"

