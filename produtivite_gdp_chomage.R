library(dplyr)

url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

url_unemployment <- "https://ourworldindata.org/grapher/unemployment-rate.csv?v=1&csvType=full&useColumnShortNames=true"
unemployment <-  read.csv(url_unemployment) |> dplyr::select(entity, year,sl_uem_totl_zs) |> dplyr::rename(country = entity, unemployment = sl_uem_totl_zs)

url_gdpparpersonne <- "https://ourworldindata.org/grapher/annual-working-hours-vs-gdp-per-capita-pwt.csv?v=1&csvType=full&useColumnShortNames=true"
gdpparpersonne <-  read.csv(url_gdpparpersonne) |> dplyr::select(entity, year,rgdpo_pc) |> dplyr::rename(country = entity, gdpparpersonne = rgdpo_pc)

productivity_unemployment <- dplyr::inner_join(x=productivity, y=unemployment, by=dplyr::join_by(country, year))

df <- dplyr::inner_join(x=productivity_unemployment, y=gdpparpersonne, by=dplyr::join_by(country, year))

#Find the most recent year in the dataset
latest_common_year <- max(df$year, na.rm = TRUE)

#Rank the top 30 countries by GDP personnal
top_30_countries <- df %>%
  filter(year == latest_common_year) %>%
  arrange(desc(gdpparpersonne)) %>%
  slice(1:30) %>%
  pull(country)

#Create a new DataFrame for the top 30 countries
df_top30 <- df |> 
 dplyr::filter(country %in% top_30_countries)
