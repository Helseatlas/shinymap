
# setup to run docker
# Did not find the correct INSTANCE for R, but rather close
BUILDID="build-$RANDOM"
INSTANCE="travisci/ci-amethyst:packer-1512508255-986baf0"
docker pull travisci/ci-amethyst:packer-1512508255-986baf0

# run docker
docker run --name $BUILDID -dit $INSTANCE /sbin/init
docker exec -it $BUILDID bash -l

# Go into sudo
su - travis

# Run a lot of commands
sudo add-apt-repository -y "ppa:marutter/rrutter3.5"
sudo add-apt-repository -y "ppa:marutter/c2d4u3.5"
sudo add-apt-repository -y "ppa:ubuntugis/ppa"
sudo add-apt-repository -y "ppa:opencpu/jq"
sudo add-apt-repository -y "ppa:kirillshkrogalev/ffmpeg-next"
sudo apt-get update
sudo apt-get install -y --no-install-recommends build-essential gcc g++ libblas-dev liblapack-dev libncurses5-dev libreadline-dev libjpeg-dev libpcre3-dev libpng-dev zlib1g-dev libbz2-dev liblzma-dev libicu-dev cdbs qpdf texinfo libssh2-1-dev gfortran
curl -fLo /tmp/R-3.5.2-$(lsb_release -cs).xz https://travis-ci.rstudio.org/R-3.5.2-$(lsb_release -cs).xz
tar xJf /tmp/R-3.5.2-$(lsb_release -cs).xz -C ~
rm /tmp/R-3.5.2-$(lsb_release -cs).xz
sudo mkdir -p /usr/local/lib/R/site-library $R_LIBS_USER
sudo chmod 2777 /usr/local/lib/R /usr/local/lib/R/site-library $R_LIBS_USER
echo 'options(repos = c(CRAN = "http://cloud.r-project.org"))' > ~/.Rprofile.site
curl -fLo /tmp/texlive.tar.gz https://github.com/jimhester/ubuntu-bin/releases/download/latest/texlive.tar.gz
tar xzf /tmp/texlive.tar.gz -C ~
export PATH=${TRAVIS_HOME}/texlive/bin/x86_64-linux:$PATH
sudo apt-get install texlive-base
sudo tlmgr update --self
curl -fLo /tmp/pandoc-2.2-1-amd64.deb https://github.com/jgm/pandoc/releases/download/2.2/pandoc-2.2-1-amd64.deb
sudo dpkg -i /tmp/pandoc-2.2-1-amd64.deb
sudo apt-get install -f
rm /tmp/pandoc-2.2-1-amd64.deb
git clone --depth=50 --branch=master https://github.com/helseatlas/helseatlas.git helseatlas/helseatlas
cd helseatlas/helseatlas
git checkout -qf eda6b53d905564f345a43f8a677714f069ccc3c1
export CASHER_DIR=${TRAVIS_HOME}/.casher
sudo apt-get install r-base-core
Rscript -e 'sessionInfo()'

# Taken from "before_install" in the .travis.yml file
sudo apt-get install -y libgdal-dev libspatialite5 libgeos-c1v5
sudo apt-get install -y libprotobuf-dev libudunits2-dev libv8-dev libgdal1-dev
sudo apt-get install -y libprotobuf-dev protobuf-compiler
sudo add-apt-repository -y ppa:opencpu/jq
sudo apt-get install -y libjq-dev

Rscript -e "install.packages('covr')"
Rscript -e "install.packages('roxygen2')"
Rscript -e "install.packages('devtools')"
Rscript -e "devtools::install_github('r-lib/pkgdown')"

# Install packages from DESCRIPTION file
Rscript -e "install.packages('testthat')"
Rscript -e "install.packages(c('shiny', 'shinythemes', 'rsconnect', 'ggplot2', 'leaflet', 'dplyr', 'ggthemes', 'maps'))"
R CMD build .
R CMD check *tar.gz
