library(dplyr)
library(ggplot2)
library(countrycode)
library(gapminder)
library(plotly)

url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

url_workhour <- "https://ourworldindata.org/grapher/annual-working-hours-per-worker.csv?v=1&csvType=full&useColumnShortNames=true"
workhour <-read.csv(url_workhour) |> dplyr::select(entity, year,working_hours_omm) |> dplyr::rename(country = entity, workhour = working_hours_omm)

url_population <- "https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true"
population <-  read.csv(url_population) |> dplyr::select(entity, year,population_historical) |> dplyr::rename(country = entity, population = population_historical)

productivity_workhour <- dplyr::inner_join(x=productivity, y=workhour, by=dplyr::join_by(country, year))

df <- dplyr::inner_join(x=productivity_workhour, y=population, by=dplyr::join_by(country, year))

df$continent <- countrycode(sourcevar = df$country,
                            origin = "country.name",
                            destination = "continent")

figure <- df |>
  filter(year == 2023) |>
  ggplot(aes(x = productivity, y = workhour, 
             color = continent,
             size = log10(population),   
             text = paste("Country:", country,
                          "<br>Productivity:", productivity,
                          "<br>Workhour:", workhour,
                          "<br>Population:", population))) +
  geom_point(alpha = 0.7) +       
  scale_size(range = c(2, 3)) +  
  theme_minimal() +
  labs(title = "Productivity vs Annual Working Hour (2023)",
       x = "Productivity",
       y = "Annual Working Hour",
       color = "Continent",
       size = "Population (log10)") 

figure_interactive <- ggplotly(figure, tooltip = "text")
print(figure_interactive)
