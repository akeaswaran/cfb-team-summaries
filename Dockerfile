FROM rocker/tidyverse:4.1.0 as rbase

WORKDIR /src

# Install R packages
RUN install2.r --error \
    stringr \
    glue \
    devtools

RUN Rscript -e 'devtools::install_github(repo = "saiemgilani/cfbfastR")'

COPY ./team_agg.R .

RUN Rscript ./team_agg.R

FROM node:17 as nodebase
WORKDIR /root/src

COPY ./server ./
RUN npm set progress=false && npm config set depth 0
RUN npm install -g typescript
RUN cd src && npm install && tsc

FROM nodebase
WORKDIR /code

COPY --from=rbase /src/data ./data
COPY --from=nodebase /root/src/build ./
COPY --from=nodebase /root/src/src/node_modules ./node_modules

ENV PORT=3000
EXPOSE 3000

CMD node ./app.js

