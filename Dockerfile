FROM rocker/shiny:latest
ARG DEBIAN_FRONTEND="noninteractive"
ARG CONTAINER_TIMEZONE="GMT"
ENV LANG en_US.UTF-8 
ENV LC_ALL en_US.UTF-8
ENV _R_SHLIB_STRIP_=true

RUN ln -snf /usr/share/zoneinfo/${CONTAINER_TIMEZONE} /etc/localtime && \
    echo ${CONTAINER_TIMEZONE} > /etc/timezone && \
    apt update && \
    apt install -y --no-install-recommends git-core libssl-dev \
                                           libcurl4-gnutls-dev curl \
                                           libsodium-dev libxml2-dev \
                                           libicu-dev locales && \
    echo "${LC_ALL} UTF-8" >> /etc/locale.gen && \
	locale-gen "${LC_ALL}" && \
	/usr/sbin/update-locale LANG="${LC_ALL}" && \
    install2.r --error --skipinstalled shiny shinyWidgets shinydashboard shinyjs DT \
                                       ggplot2 RColorBrewer ggpubr ggrepel plotly \
                                       readr sass httr2 shinycssloaders openxlsx dplyr tidyr && \
    apt autoremove --purge -y && \
    apt autoclean && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* && \
    rm -rf /srv/shiny-server/*

COPY app /srv/shiny-server

RUN chown -R shiny:shiny /srv/shiny-server && \
    mkdir -p /srv/shiny-server/app_cache && \
    chmod 777 /srv/shiny-server/app_cache

COPY Rprofile.site /usr/local/lib/R/etc/Rprofile.site
USER shiny
EXPOSE 3838
WORKDIR /srv/shiny-server
CMD ["/usr/bin/shiny-server"]
