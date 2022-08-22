FROM rocker/verse:4.1.0
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libicu-dev libmariadb-dev libxml2-dev cron make && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("purrr",upgrade="never", version = "0.3.4")'
RUN Rscript -e 'remotes::install_version("DBI",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("xml2",upgrade="never", version = "1.3.2")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("RMySQL",upgrade="never", version = "0.10.22")'
RUN Rscript -e 'remotes::install_version("RCurl",upgrade="never", version = "1.98-1.5")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.9")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN rm -rf /build_zone
RUN touch /var/log/cron.log
COPY script_actualizare_sentinte.R /script_actualizare_sentinte.R
RUN (crontab -l ; echo "* 16 * * * Rscript /script_actualizare_sentinte.R  >> /var/log/cron.log") | crontab
# Run the command on container startup
 CMD cron && tail -f /var/log/cron.log
