library(tidyverse)
library(leaflet)
library(scales)
library(tidyr)
library(maps)
library(mapproj)

getOption("url.method")
options(url.method = "libcurl")

## Load in incarceration trends data ----
file <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)



## Summary Information ----

### Total number of people in prison each year
total_prison_pop_by_year <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(total_prison_population = sum(total_prison_pop, na.rm = TRUE)) 
## View(total_prison_pop_by_year)

### Percentage of black people in prison each year 
percentage_black_jail_pop <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(total_black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>%
   inner_join(total_prison_pop_by_year, by = "year") %>%
   mutate(percentage = total_black_prison_pop / total_prison_population)

avg_black_percentage <- mean(percentage_black_jail_pop$percentage)

### Percentage of white people in prison each year 
percentage_white_jail_pop <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(total_white_prison_pop = sum(white_prison_pop, na.rm = TRUE)) %>%
   inner_join(total_prison_pop_by_year, by = "year") %>%
   mutate(percentage = total_white_prison_pop / total_prison_population)

avg_white_percentage <- mean(percentage_white_jail_pop$percentage)

### County where prison population of black people is highest
highest_black_prison_pop_county <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(county_name) %>%
   summarize(total_black_prison_pop = sum(black_prison_pop, na.rm = TRUE))
View(highest_black_prison_pop_county)

### County where prison population of white people is highest
highest_white_prison_pop_county <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(county_name) %>%
   summarize(total_white_prison_pop = sum(white_prison_pop, na.rm = TRUE))
View(highest_white_prison_pop_county)

   
## Trends of jail population over time (categorized by race) ----
total_jail_pop_by_race <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(Black = sum(black_prison_pop, na.rm = TRUE),
             White = sum(white_prison_pop, na.rm = TRUE),
             AAPI = sum(aapi_prison_pop, na.rm = TRUE),
             Native = sum(native_prison_pop, na.rm = TRUE),
             Latinx = sum(latinx_prison_pop, na.rm = TRUE))

total_jail_pop_by_race <- gather(
   total_jail_pop_by_race,
   key = Race,
   value = population,
   -year
)

## Trends over time chart
prison_pop_trend <- ggplot(total_jail_pop_by_race) +
   geom_point(mapping = aes(x = year, y = population, color = Race)) +
   scale_x_continuous(breaks = seq(1990, 2016, 5)) +
   scale_y_continuous(breaks = seq(0, 500000, 50000), labels = comma) +
   labs(x = "Year", y = "Population", title = "Population of Each Race in Prison by Year")
prison_pop_trend



## Proportion of jail population to total population 15 - 64 between Black and White people ----
black_white_prison_comparison <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(Black = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE),
             White = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE))

black_white_prison_comparison <- gather(
   black_white_prison_comparison,
   key = Race,
   value = percentage,
   -year
)

## Comparison chart
black_white_prison_percentage <- ggplot(black_white_prison_comparison) +
   geom_line(mapping = aes(x = year, y = percentage, color = Race)) +
   scale_x_continuous(breaks = seq(1990, 2016, 5)) +
   scale_y_continuous(labels = percent) +
   labs(x = "Year", y = "Percent", title = "Black vs. White Prison Population Percentage by Year")
black_white_prison_percentage



## Black prison population distribution map ----
incarceration_county_recent <- incarceration_trends %>%
   filter(year == 2016)

### Join county and fips data to add fips column
county_shapes <- map_data("county") %>%
   unite(polyname, region, subregion, sep = ",") %>%
   left_join(county.fips, by = "polyname")

### Merge incarceration with county data by fips
map_data <- county_shapes %>%
   left_join(incarceration_county_recent, by = "fips")

### Blank theme
blank_theme <- theme_bw() +
   theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
   )

black_prison_pop_map <- ggplot(map_data) +
   geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = black_prison_pop),
                color = "gray", size = 0.3) +
   coord_map() +
   scale_fill_continuous(name = "# of Black people in prison", 
                         limits = c(0, max(map_data$black_prison_pop)), na.value = "white", low = "green", high = "red") +
   blank_theme +
   ggtitle("Black Prison Population in 2016 Across Each County")
black_prison_pop_map   
   