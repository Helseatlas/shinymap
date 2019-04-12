
# setup to run r-base docker

BUILDID="build-$RANDOM"
INSTANCE="r-base"
docker pull r-base

# run docker
docker run --name $BUILDID -dit $INSTANCE
docker exec -it $BUILDID bash -l # Enter docker image

# Go into sudo
#su

# Run a lot of commands
apt-get update
apt-get install git

cd /home/docker
git clone https://github.com/Helseatlas/shinymap.git
cd shinymap

# Install apt-get packages
apt-get install libcurl4-gnutls-dev libgit2-dev libssl-dev libudunits2-dev libv8-dev libjq-dev protobuf-compiler libgdal-dev
# curl-config libprotobuf-dev 

# Install packages from DESCRIPTION file
Rscript -e "install.packages(c('shiny', 'sp', 'rsconnect', 'ggplot2', 'leaflet', 'magrittr', 'dplyr', 'ggthemes', 'tibble', 'jsonlite', 'geojsonio', 'rmapshaper', 'rgdal'))"
Rscript -e "install.packages('testthat', 'shinytest', 'xml2', 'covr')"

# Build and test package
R CMD build .
R CMD check *tar.gz

exit # Leave docker image

docker ps # list of (active) images


# Cleanup

docker kill $(docker ps -q) # kill all running containers
docker rm $(docker ps -a -q) # delete all stopped containers
docker rmi $(docker images -q) # delete all images
