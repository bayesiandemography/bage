FROM debian:bookworm

ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /opt

# System dependencies for building R and most CRAN/GitHub packages
RUN apt-get update && apt-get install -y \
    build-essential \
    gfortran \
    libreadline-dev \
    libx11-dev \
    libxt-dev \
    libpng-dev \
    libjpeg-dev \
    libcairo2-dev \
    libbz2-dev \
    liblzma-dev \
    libpcre2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    texinfo \
    wget \
    curl \
    git \
    valgrind \
    ca-certificates \
    && apt-get clean

# Download and build R from source with Valgrind instrumentation
RUN wget https://cran.r-project.org/src/base/R-4/R-4.5.0.tar.gz && \
    tar -xf R-4.5.0.tar.gz && \
    cd R-4.5.0 && \
    ./configure --with-valgrind-instrumentation=2 && \
    make -j4 && \
    make install && \
    cd .. && rm -rf R-4.5.0*

# Install base R packages
RUN Rscript -e "install.packages(c('remotes', 'TMB', 'testthat'), repos = 'https://cloud.r-project.org')"

# Clone and install the 'bage' package from GitHub
RUN Rscript -e "remotes::install_github('bayesiandemography/bage', upgrade = 'never')"

# Default workdir for user-mounted code
WORKDIR /home/code