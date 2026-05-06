FROM islasgeci/base:22.04
COPY . /workdir
RUN R -e "remotes::install_github('nepito/wyscout_tools', build_vignettes=FALSE, upgrade = FALSE)"
RUN R -e "install.packages(c('comprehenr', 'janitor', 'zoo'), repos='http://cran.rstudio.com')"
RUN pip install typer
