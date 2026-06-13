FROM rocker/tidyverse:4.3 AS rbase

WORKDIR /src

# team_agg.R deps beyond the rocker/tidyverse base.
# Base already provides: dplyr (>= 1.1, required for .by / reframe() / join_by()),
# cli, glue, purrr, readr, stringr, tidyr, ggplot2, lubridate, devtools.
# Extras: glmnet (ridge opponent adjustments), janitor (clean_names).
# httr2 is updated from CRAN because the base image's version predates
# httr2::url_modify(), which current cfbfastR requires to install.
RUN install2.r --error \
    devtools \
    glmnet \
    httr2 \
    janitor

RUN Rscript -e 'devtools::install_github(repo = "sportsdataverse/cfbfastR")'

COPY ./team_agg.R .

RUN --mount=type=secret,id=CFBD_API_KEY  echo "CFBD_API_KEY=$(cat /run/secrets/CFBD_API_KEY)" >> .Renviron && Rscript ./team_agg.R skipcache

FROM node:lts AS nodebase
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

CMD ["node", "./app.js"]
