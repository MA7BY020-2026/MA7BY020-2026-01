
url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

url_unemployment <- "https://ourworldindata.org/grapher/unemployment-rate.csv?v=1&csvType=full&useColumnShortNames=true"
unemployment <-  read.csv(url_unemployment) |> dplyr::select(entity, year,sl_uem_totl_zs) |> dplyr::rename(country = entity, unemployment = sl_uem_totl_zs)

url_gdp <- "https://ourworldindata.org/grapher/annual-working-hours-vs-gdp-per-capita-pwt.csv?v=1&csvType=full&useColumnShortNames=true"
gdp <-  read.csv(url_gdp) |> dplyr::select(entity, year,rgdpo_pc) |> dplyr::rename(country = entity, gdp = rgdpo_pc)

productivity_unemployment <- dplyr::inner_join(x=productivity, y=unemployment, by=dplyr::join_by(country, year))

df <- dplyr::inner_join(x=productivity_unemployment, y=gdp, by=dplyr::join_by(country, year))


