
FROM continuumio/miniconda3

RUN apt-get update \
    && apt-get install -y texlive-latex-base \
                          texlive-latex-recommended \
                          texlive-science \
                          texlive-latex-extra \
                          texlive-fonts-recommended \
                          texlive-xetex \
                          dvipng \
                          ghostscript \
                          wget \
                          gdebi-core \
    && apt-get clean && rm -rf /var/cache/apt/* && rm -rf /var/lib/apt/lists/*

RUN cd /tmp && wget https://download2.rstudio.org/rstudio-server-1.1.463-amd64.deb
RUN gdebi /tmp/rstudio-server-1.1.463-amd64.deb

ADD environment.yml /tmp/environment.yml
RUN conda env create -f /tmp/environment.yml

# Conda Update All
RUN conda update --all -y

ADD postBuild /tmp/postBuild
RUN /tmp/postBuild

# Cleanup
RUN rm -rf /tmp/*
