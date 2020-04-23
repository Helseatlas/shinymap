FROM hnskde/helseatlas-base-r

LABEL maintainer "Arnfinn Hykkerud Steindal <arnfinn.steindal@gmail.com>"
LABEL no.mongr.cd.enable="true"

ARG GITHUB_PAT

# Install missing dependencies not yet in helseatlas-base-r
RUN R -e "install.packages(c('htmltools', 'leaflet.extras', 'methods'))"

RUN R -e "remotes::install_github('helseatlas/kart')"
RUN R -e "remotes::install_github('helseatlas/data')"

# Install the current local version of helseatlas
COPY *.tar.gz .
RUN R CMD INSTALL --clean *.tar.gz
RUN rm *.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); helseatlas::run_app()"]
