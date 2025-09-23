FROM rocker/tidyverse:4.3 AS rbase

WORKDIR /src

# Install R packages
RUN install2.r --error \
    stringr \
    glue \
    devtools

RUN Rscript -e 'devtools::install_github(repo = "sportsdataverse/cfbfastR")'

COPY ./team_agg.R .

RUN --mount=type=secret,id=CFB_API_KEY_FILE,target=/src/.Renviron Rscript ./team_agg.R skipcache

FROM ghcr.io/astral-sh/uv:python3.12-alpine AS pybase
WORKDIR /src
# scikit-learn needs a C compiler so we install GCC
RUN apk update && apk upgrade && apk add build-base 
COPY --from=rbase /src/data ./data
COPY ./adjust_epa.py .
RUN uv run adjust_epa.py skipcache

FROM node:lts AS nodebase
WORKDIR /root/src

COPY ./server ./
RUN npm set progress=false && npm config set depth 0
RUN npm install -g typescript
RUN cd src && npm install && tsc

FROM nodebase
WORKDIR /code

COPY --from=pybase /src/data ./data
COPY --from=nodebase /root/src/build ./
COPY --from=nodebase /root/src/src/node_modules ./node_modules

ENV PORT=3000
EXPOSE 3000

CMD ["node", "./app.js"]
