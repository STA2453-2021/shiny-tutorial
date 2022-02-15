

# CODE DIRECTLY FROM OPEN DATA TO -----------------------------------------

# load libraries ----------------------------------------------------------
library(opendatatoronto)
library(dplyr)


# get package
package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

data <- janitor::clean_names(data)

data <- data %>% 
  mutate(reported_date = lubridate::ymd(reported_date),
         episode_date = lubridate::ymd(episode_date))


# covert strings to dates -------------------------------------------------

data <- data %>% 
  mutate(reported_date = lubridate::ymd(reported_date),
         episode_date = lubridate::ymd(episode_date))


# save the data -----------------------------------------------------------


readr::write_csv(data, "basic-app/data/covid_data.csv")

