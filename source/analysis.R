library(library)
library(ggplot2)
library(leaflet)
library(scales)

getOption("url.method")
options(url.method = "libcurl")

## Load in incarceration data
file <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
incarceration_trends <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
View(incarceration_trends)



## Get race demographic data for each county in the most recent year
race_demographic <- incarceration_trends %>%
   filter(year == max(year)) %>%
   group_by(county_name, state) %>%
   mutate(percent_aapi = round(aapi_pop_15to64 / total_pop_15to64 * 100, 2),
          percent_black = round(black_pop_15to64 / total_pop_15to64 * 100, 2),
          percent_latinx = round(latinx_pop_15to64 / total_pop_15to64 * 100, 2),
          percent_native = round(native_pop_15to64 / total_pop_15to64 * 100, 2),
          percent_white = round(white_pop_15to64 / total_pop_15to64 * 100, 2)) %>%
   select(state, county_name, total_pop_15to64, percent_aapi, percent_black, percent_latinx, percent_native, percent_white)
   
   
   
## Trends over time chart
black_prison_trend <- incarceration_trends %>%
   filter(year >= 1990, year <= 2016) %>%
   group_by(year) %>%
   summarize(black_prison_pop = sum(black_prison_pop, na.rm = TRUE))

black_prison_pop_chart <- ggplot(black_prison_trend) +
   geom_point(mapping = aes(x = year, y = black_prison_pop)) +
   geom_smooth(mapping = aes(x = year, y = black_prison_pop)) +
   scale_x_continuous(breaks = seq(1990, 2016, 5)) +
   scale_y_continuous(breaks = seq(100000, 550000, 50000), labels = comma) +
   labs(x = "Year", y = "Population", title = "Population of Black People in Prison by Year")
black_prison_pop_chart



## Comparison chart between Black and White people
black_white_prison_comparison <- incarceration_trends %>%
   filter(year >= 1990) %>%
   group_by(year) %>%
   mutate(black_prison_pop_rate = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE),
             white_prison_pop_rate = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE)) %>%
   select(year, black_prison_pop_rate, white_prison_pop_rate)

comparison_chart <- ggplot(black_white_prison_comparison) +
   geom_line(mapping = aes(x = year, y = black_prison_pop_rate, color = "blue")) +
   geom_line(mapping = aes(x = year, y = white_prison_pop_rate, color = "red")) +
   scale_y_continuous(labels = percent) +
   labs(x = "Year", y = "Prison Population", title = "Test")
comparison_chart



## Map
map <- leaflet(data = incarceration_trends) %>%
   addProviderTiles("CartoDB.Positron") %>%
   setView(lng = 37.0902, lat = 95.7129, zoom = 10)

   