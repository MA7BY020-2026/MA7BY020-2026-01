library(dplyr)

url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

url_unemployment <- "https://ourworldindata.org/grapher/unemployment-rate.csv?v=1&csvType=full&useColumnShortNames=true"
unemployment <-  read.csv(url_unemployment) |> dplyr::select(entity, year,sl_uem_totl_zs) |> dplyr::rename(country = entity, unemployment = sl_uem_totl_zs)

url_population <- "https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true"
population <-  read.csv(url_population) |> dplyr::select(entity, year,population_historical) |> dplyr::rename(country = entity, population = population_historical)

productivity_unemployment <- dplyr::inner_join(x=productivity, y=unemployment, by=dplyr::join_by(country, year))

df <- dplyr::inner_join(x=productivity_unemployment, y=population, by=dplyr::join_by(country, year))

#Find the most recent year in the dataset
#latest_common_year <- max(df$year, na.rm = TRUE)

#Rank the top 30 countries by GDP personnal
#top_30_countries <- df %>%
#  filter(year == latest_common_year) %>%
#  arrange(desc(gdpparpersonne)) %>%
#  slice(1:30) %>%
#  pull(country)

#Create a new DataFrame for the top 30 countries
#df_top30 <- df |> 
#  dplyr::filter(country %in% top_30_countries)
