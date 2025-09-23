#!/bin/sh
DOCKER_BUILDKIT=1 docker build --secret type=file,id=CFB_API_KEY_FILE,src=.env --platform=linux/amd64 -t cfb-team-summaries:latest .

docker stop cfb-team-summaries
docker remove cfb-team-summaries
docker run -it -p 3000:3000 --name=cfb-team-summaries --platform=linux/amd64 cfb-team-summaries:latest