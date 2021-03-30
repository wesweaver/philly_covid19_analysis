library(tidyverse)
library(ggrepel)
library(tidycensus)
library(sf)

# get covid test data from opendataphilly
tests <- read.csv(
  "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_date&filename=covid_cases_by_date&format=csv&skipfields=cartodb_id",
  stringsAsFactors = F
) %>%
  mutate(collection_date = as.Date(collection_date)) %>%
  arrange(collection_date)

# pull psotive cases and calculate 7 day lagged moving average
positive_cases <- tests %>%
  filter(test_result == "positive") %>%
  mutate(positive_7day_average = zoo::rollmean(
    count, k=7, fill = NA, align = "right"
  ))

# get highest number of positive tests as single row data frame
high_point <- positive_cases %>%
  filter(count == max(count)) %>%
  mutate(text_label = glue::glue("Single Day High\n{format(collection_date, '%b %d, %Y')}\nCount: {count}"))

# plot daily tests and moving average with highest point indicated
positive_cases %>%
  ggplot()+
  geom_col(aes(x = collection_date, y = count, fill = "Positive Tests"), color = "#349AE9")+
  geom_line(data = positive_cases %>%
              filter(collection_date <= max(collection_date - 3)), aes(x = collection_date, y = positive_7day_average, color = "7 Day Average"), size = 2)+#, color = "#1e1e1f")+
  geom_text_repel(data = high_point,
                  aes(x = collection_date,
                      y = count,
                      label = glue::glue("Single Day High\n{format(collection_date, '%b %d, %Y')}\nCount: {count}")
                  ),
                  fontface = "bold",
                  nudge_x = 25,
                  nudge_y = -5,
                  box.padding = 2.5,
                  size = 3.5,
                  segment.size = 1.1
  )+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_color_manual(name = "", values = c("#1e1e1f"))+
  scale_fill_manual(name = "", values = c("#349AE9"))+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Daily Positive COVID-19 Tests in Philadelphia",
    subtitle = paste("As of", format(max(positive_cases$collection_date), "%B %d, %Y")),
    x = "Collection Date",
    y = "Count",
    caption = "Source: OpenDataPhilly COVID Tests and Cases (www.opendataphilly.org/dataset/covid-cases)\nGraphic by @WesWeaver | wesmapping.com"
  )

ggsave(paste0("./output_images/daily_positive_covid_test_", max(positive_cases$collection_date), ".png"), plot = last_plot(), dpi = 600)


# pull zip codes and tests by zip codes from opendataphilly. rename covid status with actual words.
zip_codes <- st_read("http://data.phl.opendata.arcgis.com/datasets/b54ec5210cee41c3a884c9086f7af1be_0.geojson", quiet = T)
tests_by_zip <- read.csv('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+covid_cases_by_zip&filename=covid_cases_by_zip&format=csv&skipfields=cartodb_id',
                         stringsAsFactors = F) %>%
  mutate(
    covid_status = case_when(
      covid_status == "POS" ~ "Positive",
      covid_status == "NEG" ~ "Negative"
      ),
    zip_code = as.character(zip_code),
    date = as.Date(as.POSIXct(etl_timestamp))
  ) %>%
  dplyr::select(-the_geom, -the_geom_webmercator)

census_data <- get_acs(
  geography = "zcta",
  variables = c(medincome = "B19013_001", total_pop = "B01003_001", total_pov = "B17001_002"),
  year = 2018
) %>%
  mutate(code = substr(GEOID, 3, 7)) %>%
  filter(code %in% zip_codes$CODE)

census_data <- census_data %>%
  pivot_wider(id_cols = c(code), names_from = variable, values_from = estimate) %>%
  mutate(pov_rate= round(total_pov / total_pop, 2))

census_data %>%
  left_join(
    tests_by_zip %>%
      filter(covid_status == "Positive"),
    by = c("code" = "zip_code")
  ) %>%
  mutate(positive_per_thousand = round(count/total_pop * 1000, 2)) %>%
  ggplot(aes(x = pov_rate, y = positive_per_thousand, size = count, label = code))+
  geom_point(aes(x = pov_rate, y = positive_per_thousand), color = "#349AE9", alpha = .7)+
  geom_smooth(method="lm", se = FALSE, color = "#349AE9")+
  scale_size_continuous(breaks = c(500, 1000, 1500, 2000), name = "Total Positive Tests")+
  geom_text_repel(
    size = 3.5,
    point.padding = 0.1,
    min.segment.length = 0,
    box.padding = 0.4
  )+
  guides(size = guide_legend(override.aes = list(linetype = 0)))+
  scale_x_continuous(label = scales::percent)+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Philadelphia Positive COVID-19 Tests and Poverty by ZIP Code",
    subtitle = paste("As of", format(max(tests_by_zip$date), "%B %d, %Y")),
    x = "Poverty Rate",
    y = "Positive Tests Per Thousand Residents",
    caption = "Source: OpenDataPhilly COVID Tests and Cases (www.opendataphilly.org/dataset/covid-cases)\nand the U.S. Census ACS Estimates for 2018 (www.census.gov/programs-surveys/acs)\nGraphic by @WesWeaver | wesmapping.com"
  )

ggsave(paste0("./output_images/positive_tests_by_povrate.png"), plot = last_plot(), dpi = 600)

# get daily shooting data by zip code
shootings <- st_read('https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+shootings&filename=shootings&format=geojson&skipfields=cartodb_id', quiet=T)

# filter to only shootings that occurred the same day or after the first test results and spatial join zip codes
shootings_in_period <- shootings %>%
  filter(date_ >= min(positive_cases$collection_date),
         officer_involved == "N") %>%
  st_join(zip_codes %>% st_transform(crs = st_crs(shootings)), join = st_intersects)

census_data %>%
  left_join(
    shootings_in_period %>%
      st_drop_geometry() %>%
      group_by(CODE) %>%
      summarize(shootings = n()),
    by = c("code" = "CODE")
  ) %>%
  replace_na(list(shootings = 0)) %>%
  left_join(
    tests_by_zip %>%
      filter(covid_status == "Positive"),
    by = c("code" = "zip_code")
  ) %>%
  mutate(positive_per_thousand = round(count/total_pop * 1000, 2)) %>%
  mutate(shootings_per_thousand = round(shootings/total_pop * 1000, 2)) %>%
  ggplot(aes(x = shootings_per_thousand, y = positive_per_thousand, size = count, label = code))+
  geom_point(aes(x = shootings_per_thousand, y = positive_per_thousand), color = "#349AE9", alpha = .7)+
  geom_smooth(method="lm", se = FALSE, color = "#349AE9")+
  scale_size_continuous(breaks = c(500, 1000, 1500, 2000), name = "Total Positive Tests")+
  geom_text_repel(
    size = 3.5,
    point.padding = 0.1,
    min.segment.length = 0,
    box.padding = 0.4
  )+
  guides(size = guide_legend(override.aes = list(linetype = 0)))+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Philadelphia Positive COVID-19 Tests and Shootings by ZIP Code",
    subtitle = glue::glue("As of {format(max(tests_by_zip$date), '%B %d, %Y')}"),
    x = "Shootings Per Thousand Residents",
    y = "Positive Tests Per Thousand Residents",
    caption = "Source: OpenDataPhilly COVID Tests and Cases (www.opendataphilly.org/dataset/covid-cases)\nand Shooting Victims (www.opendataphilly.org/dataset/shooting-victims)\nGraphic by @WesWeaver | wesmapping.com"
  )

ggsave(paste0("./output_images/positive_cases_and_shootings.png"), plot = last_plot(), dpi = 600)

#variables <- load_variables(2018, "acs5")

# pull health insurance coverage and calculate uninsured rate
census_insurance_data <- get_acs(
  geography = "zcta",
  variables = c(
    total_pop = "B01003_001",
    insur_pop = "B27010_001",
    no_cov_u19 = "B27010_017",
    no_cov_u34 = "B27010_033",
    no_cov_u64 = "B27010_050",
    no_cov_o65 = "B27010_066"),
  year = 2018
) %>%
  mutate(code = substr(GEOID, 3, 7)) %>%
  filter(code %in% zip_codes$CODE) %>%
  pivot_wider(id_cols = c(code), names_from = variable, values_from = estimate) %>%
  mutate(
    no_coverage_rate = round((no_cov_u19 + no_cov_u34 + no_cov_u64 + no_cov_o65) / insur_pop, 4)
  )

census_insurance_data %>%
  left_join(
    tests_by_zip %>%
      filter(covid_status == "Positive"),
    by = c("code" = "zip_code")
  ) %>%
  mutate(positive_per_thousand = round(count/total_pop * 1000, 2)) %>%
  ggplot(aes(x = no_coverage_rate, y = positive_per_thousand, size = count, label = code))+
  geom_point(aes(x = no_coverage_rate, y = positive_per_thousand), color = "#349AE9", alpha = .7)+
  geom_smooth(method="lm", se = FALSE, color = "#349AE9")+
  scale_size_continuous(breaks = c(500, 1000, 1500, 2000), name = "Total Positive Tests")+
  geom_text_repel(
    size = 3.5,
    point.padding = 0.1,
    min.segment.length = 0,
    box.padding = 0.4
  )+
  scale_x_continuous(label = scales::percent)+
  guides(size = guide_legend(override.aes = list(linetype = 0)))+
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )+
  labs(
    title = "Philadelphia Positive COVID-19 Tests and Health Insurance by ZIP Code",
    subtitle = glue::glue("As of {format(max(tests_by_zip$date), '%B %d, %Y')}"),
    x = "Percent of Residents Without Health Coverage",
    y = "Positive Tests Per Thousand Residents",
    caption = "Source: OpenDataPhilly COVID Tests and Cases (www.opendataphilly.org/dataset/covid-cases)\nand the U.S. Census ACS Estimates for 2018 (www.census.gov/programs-surveys/acs)\nGraphic by @WesWeaver | wesmapping.com"
  )

ggsave(paste0("./output_images/positive_tests_and_insurance_coverage.png"), plot = last_plot(), dpi = 600)
