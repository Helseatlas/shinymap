FROM hnskde/helseatlas-base-r

LABEL maintainer "Arnfinn Hykkerud Steindal <arnfinn.steindal@gmail.com>"
LABEL no.mongr.cd.enable="true"

RUN R -e "remotes::install_github('helseatlas/kart')"
RUN R -e "remotes::install_github('helseatlas/data')"

# Install the current local version of shinymap
COPY *.tar.gz .
RUN R CMD INSTALL --clean *.tar.gz
RUN rm *.tar.gz

EXPOSE 3838

CMD ["R", "-e", "options(shiny.port=3838,shiny.host='0.0.0.0'); shinymap::run_app()"]
