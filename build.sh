#!/bin/sh

# docker pull rocker/tidyverse:4.3
# docker pull rocker/tidyverse:4.4
while IFS= read -r line; do
  # echo $line
  array=(${line//=/ })
  opts+=("id=${array[0]},env=${array[0]}")
done < ./.env

echo "${opts}"
DOCKER_BUILDKIT=1 docker build --secret "${opts[@]}" --platform=linux/amd64 -t cfb-team-summaries:latest .

docker stop cfb-team-summaries
docker rm cfb-team-summaries
docker run -it -p 3000:3000 --name=cfb-team-summaries --platform=linux/amd64 cfb-team-summaries:latest
