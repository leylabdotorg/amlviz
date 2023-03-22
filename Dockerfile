FROM rocker/shiny-verse:latest

RUN R -e "install.packages(c(\
  'shinyjs', \
  'shinythemes', \
  'plotly', \
  'RMySQL' \
  ), repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('eclarke/ggbeeswarm', ref='v0.6.1')"

RUN rm -rf /srv/shiny-server/*
COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', port = 3838, host='0.0.0.0')"]
