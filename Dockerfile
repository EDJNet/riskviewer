FROM rocker/r-ver:4.0.4
RUN apt-get update && apt-get install -y  git-core imagemagick libcurl4-openssl-dev libfribidi-dev libgit2-dev libharfbuzz-dev libicu-dev libmagic-dev libmagick++-dev libpng-dev libssl-dev libtiff-dev libxml2-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.1")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.33")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.5")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.7")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("cowplot",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("shades",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("patchwork",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("ragg",upgrade="never", version = "1.1.2")'
RUN Rscript -e 'remotes::install_version("magick",upgrade="never", version = "2.7.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');riskviewer::rv_explorer()"
