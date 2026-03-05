# =============================================================================
# 0. Package initialization and data download
# =============================================================================
packages <- c("dplyr", "ggplot2", "plotly", "countrycode", "ggrepel")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Download and make a clean dataframe from ourworldindata's productivity data 
url_productivity <- "https://ourworldindata.org/grapher/labor-productivity-per-hour-pennworldtable.csv?v=1&csvType=full&useColumnShortNames=true"
productivity <-  read.csv(url_productivity) |> dplyr::select(entity, year,productivity) |> dplyr::rename(country = entity)

# Same with working hours data
url_workhour <- "https://ourworldindata.org/grapher/annual-working-hours-per-worker.csv?v=1&csvType=full&useColumnShortNames=true"
workhour <-read.csv(url_workhour) |> dplyr::select(entity, year,working_hours_omm) |> dplyr::rename(country = entity, workhour = working_hours_omm)

# Same with population data
url_population <- "https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true"
population <-  read.csv(url_population) |> dplyr::select(entity, year,population_historical) |> dplyr::rename(country = entity, population = population_historical)
# Now we have three datasets

# =============================================================================
# 1. Data preparation
# =============================================================================

# We merge the productivity and working hours dataframes into one with inner_join(). Inner means empty (NA) observations/rows are discarded
productivity_workhour <- dplyr::inner_join(x=productivity, y=workhour, by=dplyr::join_by(country, year))
# Now we have a dataset with the following columns/variables: country, year, productivity, working_hours_omm

# We merge the (productivity+working hours) and population dataframes. Here also, inner_join() to remove observations with empty values.
# We also use the countrycode library to add a column with continent of each observation (to make the figure more beautiful)
df <- dplyr::inner_join(x=productivity_workhour, y=population, 
                        by=dplyr::join_by(country, year)) |>
     dplyr::mutate(
     continent = countrycode(country, origin = "country.name", destination = "continent")
     ) |>
     dplyr::filter(!is.na(continent)) |>           
     dplyr::group_by(year) |>
     dplyr::filter(n_distinct(continent) == 5) |>    # We only keep years where all 5 continents are represented
     dplyr::ungroup() |>
     dplyr::arrange(country, year) 

# =============================================================================
# 2. Creation of static plots with ggplot
# =============================================================================
figure_static <- df |>
  filter(year == 2023) |>                        
  arrange(desc(population)) |>
  ggplot(aes(x = productivity,
             y = workhour,
             color = continent,
             size = population,
             label = country)) +                
  geom_point(alpha = 0.6, stroke = 1.0) +
  geom_text_repel(aes(label = country),   
                  size = 3, max.overlaps = 20) +
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
  ) +
  labs(title = "Labor Productivity vs. Annual Working Hours (2023)",
       x = "Labor Productivity ( usd/hr, log scale)", 
       y = "Annual Working Hours( hrs/year)",
       caption = "Source : Our World in Data")

print(figure_static)

# =============================================================================
# 3. Creation of animated plots with plotly
# =============================================================================
# 
continent_colors <- c(
  "Africa"   = "#fc5173",
  "Americas" = "#fde803",
  "Asia"     = "#01d4e5",
  "Europe"   = "#7dea01",
  "Oceania"  = "#9B6BB5"
)
pop_min <- min(df$population)
pop_max <- max(df$population)

df_sorted <- df |>
  arrange(year, desc(population)) |>
  mutate(
    bubble_size = 2 + (population - pop_min) / (pop_max - pop_min) * 56)

figure <- plot_ly(
  data      = df_sorted,
  x         = ~productivity,
  y         = ~workhour,
  size      = ~bubble_size,      
  color     = ~continent,
  colors    = continent_colors,
  frame     = ~year,
  ids       = ~country,
  type      = "scatter",
  mode      = "markers",
  marker    = list(
    opacity  = 0.6,
    sizemode = "diameter",      
    sizeref  = 1                 
  ),
  text = ~paste0(
    "Country: ",          country,
    "<br>Productivity: ", round(productivity, 1),"usd/hr",
    "<br>Workhour: ",     round(workhour, 1),"hrs/year",
    "<br>Population: ",   round(population / 1e6, 1), "M" 
  ),
  hoverinfo = "text"
) |>
  layout(
    title  = list(text = "<b>Labor Productivity vs. Annual Working Hours</b>",
                  font = list(size = 15)),
    xaxis  = list(title = "Labor Productivity ( usd/hr, log scale)", type = "log"),
    yaxis  = list(title = "Annual Working Hours( hrs/year)"),
    legend = list(title = list(text = "Continent")),
    paper_bgcolor = "white",
    plot_bgcolor  = "#f9f9f9"
  ) |>
  animation_opts(frame = 800, easing = "linear", redraw = FALSE) |>
  animation_slider(currentvalue = list(prefix = "Year: ", font = list(size = 13))) |>
  animation_button(x = 0, xanchor = "right", y = 0, yanchor = "top")

print(figure)
