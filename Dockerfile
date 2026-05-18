FROM rocker/geospatial:4.4.1

RUN apt-get update -y && apt-get install -y \
    make pandoc libpq-dev libcurl4-openssl-dev libssl-dev libicu-dev xz-utils \
    libgsl0-dev zlib1g-dev cmake libgdal-dev gdal-bin libgeos-dev libpng-dev \
    libproj-dev libsqlite3-dev libudunits2-dev git && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/

# Switched the CRAN repo URL to the Posit Jammy Binary Mirror
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'), download.file.method = 'libcurl', Ncpus = 4)" \
    | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site

RUN R -e 'install.packages("remotes")'
RUN R -e 'install.packages("renv")'

COPY renv.lock renv.lock
ENV R_REMOTES_UPGRADE="never"
RUN R -e "options(timeout = 300)"

# This step will now pull pre-compiled .so files for Ubuntu 22.04 
RUN --mount=type=cache,id=renv-cache,target=/root/.cache/R/renv R -e 'renv::restore()'

WORKDIR /srv/shiny-server/
COPY . /srv/shiny-server/

EXPOSE 3838
CMD R -e 'shiny::runApp("/srv/shiny-server",host="0.0.0.0",port=3838)'