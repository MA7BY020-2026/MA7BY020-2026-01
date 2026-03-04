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

df <- dplyr::inner_join(x=productivity_workhour, y=population, 
                        by=dplyr::join_by(country, year)) |>
     dplyr::mutate(
     continent = countrycode(country, origin = "country.name", destination = "continent")
     ) |>
     dplyr::filter(!is.na(continent)) |>           # ← 
     dplyr::group_by(year) |>
     dplyr::filter(n_distinct(continent) == 5) |>
     dplyr::ungroup() |>
     dplyr::arrange(country, year) 

figure <- df |>
  arrange(year, desc(population)) |> 
  ggplot(aes(x = productivity, 
             y = workhour, 
             color = continent,
             size = population,   
             text = paste(
               "Country:", country,
               "Productivity:", productivity,
               "Workhour:", workhour,
               "Population:", population),
             frame = year,
             ids = country,
             group = country)) +
  geom_point(alpha = 0.6, stroke = 1.0) +   
  scale_color_manual(values = c(
    "Africa"   = "#fc5173",  
    "Americas" = "#fde803",  
    "Asia"     = "#01d4e5",  
    "Europe"   = "#7dea01",  
    "Oceania"  = "#9B6BB5"
  )) +
  scale_size_area(
    max_size = 28,
    labels = scales::label_number(scale = 1/1e6, suffix = "B")
  ) +
  scale_x_log10() + 
  theme_minimal(base_size = 13) +
  theme(
  legend.position = "right",
  plot.title = element_text(face = "bold", size = 15)
  ) +
  guides(
    color = guide_legend(title = "Continent", override.aes = list(size = 4)),
    size  = guide_legend(title = "Population")
  )+
  labs(title = "Labor Productivity vs. Annual Working Hours",
    x = "Labor Productivity (log scale)",
    y = "Annual Working Hours")

figure_interactive <- ggplotly(figure, tooltip = "text") |>
  layout(
    legend = list(title = list(text = "Continent"))  # ← 
  )
print(figure_interactive)
