FROM rocker/r-ver:4.1.3
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev make pandoc pandoc-citeproc && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.2")'
RUN Rscript -e 'remotes::install_version("httr",upgrade="never", version = "1.4.3")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("tibble",upgrade="never", version = "3.1.7")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.39")'
RUN Rscript -e 'remotes::install_version("gargle",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.14")'
RUN Rscript -e 'remotes::install_version("RSQLite",upgrade="never", version = "2.2.14")'
RUN Rscript -e 'remotes::install_version("usethis",upgrade="never", version = "2.1.6")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("googledrive",upgrade="never", version = "2.0.0")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
