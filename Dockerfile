FROM rocker/tidyverse:4.5 AS rbase

WORKDIR /src

# team_agg.R deps beyond the rocker/tidyverse base.
# Base already provides: dplyr (>= 1.1, required for .by / reframe() / join_by()),
# cli, glue, purrr, readr, stringr, tidyr, ggplot2, lubridate.
# Extras: glmnet (ridge opponent adjustments), janitor (clean_names),
# httr2 (cfbfastR needs url_modify()), remotes (to install cfbfastR from GitHub --
# devtools 2.5+ deprecated install_github and requires remotes anyway).
RUN install2.r --error \
    glmnet \
    httr2 \
    janitor \
    remotes

RUN Rscript -e 'remotes::install_github("sportsdataverse/cfbfastR")'

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
