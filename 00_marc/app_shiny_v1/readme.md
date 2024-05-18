## Build de image
docker build --tag proyecto2_app:1.0 . --platform=linux/amd64 --no-cache

## All the available images
docker images

## Save image (take long time and create a big file)
docker save -o docker_app.tar proyecto2_app
### or
docker save proyecto2_app > docker_app.tar
### or export if you have already a container
docker export proyecto2_app > docker_app.tar

### Export and save are a bit different
"docker export" export a filesystem from a container, "docker save" saves the image with all layers and metadata.

## Save compressed image (you must have gzip installed)
docker save proyecto2_app | gzip > docker_app.tgz

## Load/import image
docker load -i ./docker_app.tar
### or
docker import ./docker_app.tar



## Run the image and create a new container
docker run -p 8080:3838 proyecto2_app:1.0

### you can assign a name to the contaniner
docker run --name app_container -p 8080:3838 -d proyecto2_app:1.0

### you can also mount a local directory (volume)
docker run -v path_to_the_local_dir:/app_shiny --name app_container -p 8080:3838 -d proyecto2_app:1.0

### you can access the app at:
http://localhost:8080/

## Info about running dockers (containers)
docker ps

## Info about all dockers
docker ps -a

## Stop the docker (container) by name
docker stop app_container

## Start/run the existing docker
docker start app_container

