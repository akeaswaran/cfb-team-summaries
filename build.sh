#!/bin/sh

DOCKER_BUILDKIT=1 docker build --secret id=CFBD_API_KEY,src=./secrets/CFBD_API_KEY --platform=linux/amd64 -t cfb-team-summaries:latest .

docker stop cfb-team-summaries
docker rm cfb-team-summaries
docker run -it -p 3000:3000 --name=cfb-team-summaries --platform=linux/amd64 cfb-team-summaries:latest
