#!/bin/sh

docker pull rocker/tidyverse:4.3
# docker pull rocker/tidyverse:4.4
DOCKER_BUILDKIT=1 docker build . --platform=linux/amd64 -t cfb-team-summaries:latest

docker stop cfb-team-summaries
docker remove cfb-team-summaries
docker run -it -p 3000:3000 --name=cfb-team-summaries --platform=linux/amd64 --env-file=./.env cfb-team-summaries:latest
