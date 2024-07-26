# TODO: update version
FROM rocker/r-ver:4.4.1

RUN apt-get update &&  apt-get install -y --no-install-recommends \
        libcurl4-openssl-dev \
        libssl-dev \
        libv8-dev \
        zlib1g-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

# NOTE: manual install stringi to link against libicu70 over libicu66
RUN install2.r --error stringi

# Without this, we are unable to pick up more recent packages
COPY docker/Rprofile.site /usr/local/lib/R/etc/Rprofile.site

# More cache-friendly installation of some dependencies:
# NOTE: need to convert to using r-universe instead
RUN install2.r --error \
        --repos=https://mrc-ide.r-universe.dev \
        --repos=https://packagemanager.posit.co/all/__linux__/focal/latest \
        docopt \
        jsonlite \
        logr \
        pkgbuild \
        porcelain

COPY DESCRIPTION /tmp/DESCRIPTION

# Then get the full list via remotes
RUN install2.r --error remotes &&  \
        Rscript -e 'remotes::install_deps("/tmp")'

EXPOSE 8001

COPY . /src
RUN R CMD INSTALL --install-tests /src && \
        cp /src/docker/daedalus_api /usr/local/bin && \
        rm -rf /src

ENTRYPOINT ["/usr/local/bin/daedalus_api"]
