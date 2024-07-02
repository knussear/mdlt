# Filename: Dockerfile
FROM rocker/r-base:latest

LABEL maintainer="Ken Nussear <knussear@unr.edu>"

# RUN /rocker_scripts/setup_R.sh https://packagemanager.posit.co/cran/__linux__/jammy/latest
RUN echo "\noptions(shiny.port=3838, shiny.host='0.0.0.0')" >> /usr/local/lib/R/etc/Rprofile.site

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcairo2-dev \
    libxt-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install --no-install-recommends -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -q -e "options(warn=2); install.packages(c('shiny'))"

# install dependencies of the mdlt app
RUN R -q -e "options(warn=2); install.packages('Rmpfr')"

RUN R -q -e "options(warn=2); install.packages('terra')"
RUN R -q -e "options(warn=2); install.packages('shiny')"
RUN R -q -e "options(warn=2); install.packages('shinydashboard')"
RUN R -q -e "options(warn=2); install.packages('dplyr')"
RUN R -q -e "options(warn=2); install.packages('sf')"
RUN R -q -e "options(warn=2); install.packages('rasterVis')"
RUN R -q -e "options(warn=2); install.packages('stringr')"
RUN R -q -e "options(warn=2); install.packages('ggplot2')"
RUN R -q -e "options(warn=2); install.packages('tidyterra')"
RUN R -q -e "options(warn=2); install.packages('patchwork')"
RUN R -q -e "options(warn=2); install.packages('purrr')"
RUN R -q -e "options(warn=2); install.packages('readr')"
RUN R -q -e "options(warn=2); install.packages('shinyBS')"
RUN R -q -e "options(warn=2); install.packages('leaflet')"
RUN R -q -e "options(warn=2); install.packages('leaflet.extras')"
RUN R -q -e "options(warn=2); install.packages('DT')"
RUN R -q -e "options(warn=2); install.packages('data.table')"
RUN R -q -e "options(warn=2); install.packages('gridExtra')"
RUN R -q -e "options(warn=2); install.packages('ggpubr')"
RUN R -q -e "options(warn=2); install.packages('cowplot')"
RUN R -q -e "options(warn=2); install.packages('htmltools')"
RUN R -q -e "options(warn=2); install.packages('zip')"
RUN R -q -e "options(warn=2); install.packages('bslib')"
RUN R -q -e "options(warn=2); install.packages('rlist')"
RUN R -q -e "options(warn=2); install.packages('scales')"


# install R code
COPY MDLTV4 /app
WORKDIR /app

EXPOSE 3838

# create user
RUN groupadd -g 1000 shiny && useradd -c 'shiny' -u 1000 -g 1000 -m -d /home/shiny -s /sbin/nologin shiny
USER shiny

CMD ["R", "-q", "-e", "shiny::runApp('/app')"]
