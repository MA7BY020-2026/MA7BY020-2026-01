
url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

url_unemployment <- "https://ourworldindata.org/grapher/unemployment-rate.csv?v=1&csvType=full&useColumnShortNames=true"
unemployment <-  read.csv(url_unemployment) |> dplyr::select(entity, year,sl_uem_totl_zs) |> dplyr::rename(country = entity, unemployment = sl_uem_totl_zs)


