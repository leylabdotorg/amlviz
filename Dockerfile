FROM rocker/shiny-verse:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev

RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyjs', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('plotly', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('devtools', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('eclarke/ggbeeswarm', ref='v0.6.1')"
RUN R -e "install.packages('DBI', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RSQLite', repos='http://cran.rstudio.com/')"

COPY . /srv/shiny-server/shiny/

EXPOSE 3838

CMD R -e "shiny::runApp('/srv/shiny-server/shiny/', port = 3838, host='0.0.0.0')"
