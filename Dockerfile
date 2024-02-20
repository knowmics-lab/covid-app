FROM ubuntu:jammy
ARG DEBIAN_FRONTEND="noninteractive"
ARG CONTAINER_TIMEZONE="GMT"
ENV LANG en_US.UTF-8 
ENV LC_ALL en_US.UTF-8

RUN useradd docker && \
    mkdir /home/docker && \
    chown docker:docker /home/docker && \
    addgroup docker staff && \
    ln -snf /usr/share/zoneinfo/${CONTAINER_TIMEZONE} /etc/localtime && \
    echo ${CONTAINER_TIMEZONE} > /etc/timezone && \
    apt update --fix-missing && \
    apt install -y perl dialog software-properties-common wget \
                   locales apt-utils && \
    echo "${LC_ALL} UTF-8" >> /etc/locale.gen && \
	locale-gen "${LC_ALL}" && \
	/usr/sbin/update-locale LANG="${LC_ALL}" && \
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc && \
    add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/" && \
    add-apt-repository ppa:c2d4u.team/c2d4u4.0+ && \
    apt update && \
    apt dist-upgrade -y && \
    apt install -y less unzip gsfonts git curl cmake nano jq \
                   zip r-base r-cran-shiny r-cran-shinyjs r-cran-tidyverse \
                   r-cran-psych r-cran-dt r-cran-ggplot2 r-cran-ggpubr \
                   r-cran-ggrepel r-cran-radiant.data r-cran-scales \
                   r-cran-data.table r-cran-bslib r-cran-littler \
                   r-cran-shinybs && \
    apt remove -y software-properties-common && \
    apt autoremove --purge -y && \
    apt autoclean && \
    apt clean && \
    rm -rf /var/lib/apt/lists/* && \
    mkdir -p /app/shiny

USER docker
COPY app /app/shiny
COPY Rprofile.site /usr/lib/R/etc/Rprofile.site
EXPOSE 3838
WORKDIR /app/shiny

# CMD ["R", "-e", "shiny::runApp('/app/shiny')"] # Uncomment this to run the app without shinyproxy