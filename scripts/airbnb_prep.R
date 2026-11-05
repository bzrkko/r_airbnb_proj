## File name: airbnb_prep.R
## Project: Analysis of Airbnb dataset (listing prices neighborhood characteristics in NYC)
## Course: Programming for Data Analytics (Winter Semester 2026), Uni zu Köln

## Tasks: clean the Airbnb dataset, merge with NYC neighborhood dataset


library(here)
library(tidyverse)
library(conflicted)
library(viridis)
library(knitr)
library(kableExtra)
conflict_prefer_all("dplyr") 

# load data
airbnb_df <- read_csv(here("learning/data/airbnb_data.csv"))
neighb_df <- read_csv(here("learning/data/neighb_dataset.csv"))

# inspect
summary(airbnb_df)
summary(neighb_df)
glimpse(airbnb_df)
glimpse(neighb_df)

######################## NEIGHBORHOOD DATASET ######################## 
# select and rename variables
neighb_clean <- neighb_df %>%
  filter(!is.na(`Total Population`)) %>%
  filter(Neighborhood != "NYC", !is.na(Borough)) %>%
  select(
    neighb_nta = Neighborhood,
    neighb_borough = Borough,
    total_population = `Total Population`,
    poverty_rate = Poverty,
    unemployment_rate = Unemployment,
    foreign_born = `Foreign Born Residents`,
    less_than_high_school = `Education Less than High School`,
    median_gross_rent = `Gross Rent (Median)`,
    rent_burden_30 = `Rent Burden Greater than 30 Percent`,
    crowded_housing = `Crowded Housing`,
    public_housing = `Residents Living in Public Housing (Percent)`,
    homeowner_vacancy = `Homeowner Vacancy`,
    violent_crime_per_1k = `Felony Crime Complaints: Violent Crime per 1000`,
    property_crime_per_1k = `Felony Crime Complaints: Property Crime per 1000`,
    jail_incarceration_per_1k = `Jail Incarceration per 1000`,
    total_crime_per_1k = `Felony Crime Complaints: All per 1000`,
    alcohol_retailers_per_10k = `Alcohol Retailers per 10,000`
  ) %>% 
  # remove whitespaces
  mutate(
    neighb_nta = str_trim(neighb_nta),
    neighb_borough = str_trim(neighb_borough)
  )

# create a dictionary to unify the neighb names (nta)
nta_df <- airbnb_clean %>%
  distinct(neighb_borough, neighb_detail) %>%
  inner_join(
    neighb_clean %>% select(neighb_borough, neighb_nta),
    by = "neighb_borough", 
    relationship = "many-to-many"
  ) %>%
  # match them if the Airbnb name is inside the census name
  filter(
    str_detect(str_to_lower(neighb_nta), fixed(str_to_lower(neighb_detail))) |
      str_detect(str_to_lower(neighb_detail), fixed(str_to_lower(neighb_nta)))
  ) %>%
  group_by(neighb_borough, neighb_detail) %>%
  slice(1) %>% # keep the first match
  ungroup()

## create a df with correct names of neighborhoods (used Gemini AI for matching)
manual_patches <- tribble(
  ~neighb_borough, ~neighb_detail, ~neighb_nta,
  # Manhattan major areas
  "Manhattan", "Hell's Kitchen", "Clinton",
  "Manhattan", "Financial District", "Battery Park City-Lower Manhattan",
  "Manhattan", "Flatiron District", "Hudson Yards-Chelsea-Flat Iron-Union Square",
  "Manhattan", "Greenwich Village", "West Village",
  "Manhattan", "Theater District", "Midtown-Midtown South",
  "Manhattan", "NoHo", "SoHo-TriBeCa-Civic Center-Little Italy",
  "Manhattan", "Nolita", "SoHo-TriBeCa-Civic Center-Little Italy",
  "Manhattan", "Two Bridges", "Lower East Side",
  "Brooklyn", "South Slope", "Park Slope-Gowanus",
  "Brooklyn", "Prospect-Lefferts Gardens", "Prospect Lefferts Gardens-Wingate",
  "Brooklyn", "Fort Hamilton", "Bay Ridge",
  "Brooklyn", "Sea Gate", "Seagate-Coney Island",
  "Brooklyn", "Navy Yard", "DUMBO-Vinegar Hill-Downtown Brooklyn-Boerum Hill",
  "Bronx", "Mount Eden", "Mount Hope",
  "Bronx", "Claremont Village", "Claremont-Bathgate",
  "Bronx", "East Morrisania", "Morrisania-Melrose",
  "Queens", "Rockaway Beach", "Hammels-Arverne-Edgemere",
  "Queens", "Neponsit", "Breezy Point-Belle Harbor-Rockaway Park-Broad Channel",
  "Staten Island", "Mariners Harbor", "Mariner's Harbor-Arlington-Port Ivory-Graniteville",
  "Staten Island", "Howland Hook", "Mariner's Harbor-Arlington-Port Ivory-Graniteville",
  "Staten Island", "Fort Wadsworth", "Grasmere-Arrochar-Ft. Wadsworth",
  "Staten Island", "Concord", "Grasmere-Arrochar-Ft. Wadsworth",
  "Staten Island", "West Brighton", "West New Brighton-New Brighton-St. George",
  "Staten Island", "Tompkinsville", "West New Brighton-New Brighton-St. George",
  "Staten Island", "Randall Manor", "West New Brighton-New Brighton-St. George",
  "Staten Island", "New Dorp Beach", "New Dorp-Midland Beach",
  "Staten Island", "Grant City", "Old Town-Dongan Hills-South Beach",
  "Staten Island", "Shore Acres", "Old Town-Dongan Hills-South Beach",
  "Staten Island", "Richmondtown", "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill",
  "Staten Island", "Willowbrook", "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill",
  "Staten Island", "Bull's Head", "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill",
  "Staten Island", "Castleton Corners", "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill",
  "Staten Island", "Bay Terrace, Staten Island","Great Kills"
)

# combine matches
nta_df <- bind_rows(nta_df, manual_patches) %>%
  distinct(neighb_borough, neighb_detail, .keep_all = TRUE)

######################## AIRBNB DATASET ######################## 

## only keep the variables for the analysis in Airbnb dataset + set the price > 0

# dropped: year_added, amenities_count, instant_bookable, room_size_category,
# availability, availability_365, calculated_host_listings_count,
# reviews_per_month, latitude, longitude, host_id
airbnb_clean <- airbnb_df %>%
  # only keep listings w valud prices and short-term rentals
  filter(price > 0 & minimum_nights < 31) %>%   
  mutate(name = replace_na(name, "empty")) %>% # replace missing names
  select(id, name, host_name,
         neighb_borough = neighbourhood_group,
         neighb_detail = neighbourhood,
         price, minimum_nights, number_of_reviews, host_experience_years,
         guest_satisfaction_score, property_type)

######################## MERGED DATASET ######################## 

airbnb_merged <- airbnb_clean %>%
 ## add matching census name (neighb_nta) to each listing
  left_join(nta_df, by = c("neighb_borough", "neighb_detail")) %>%
  ## add actual neighborhood data using neighb_nta
  left_join(
    neighb_clean, 
    by = c("neighb_borough" = "neighb_borough", "neighb_nta" = "neighb_nta")
  ) 

## check unmatched neighborhoods
unmatched_neighborhoods <- airbnb_clean %>%
  distinct(neighb_borough, neighb_detail) %>%
  anti_join(nta_df, by = c("neighb_borough", "neighb_detail"))

print(unmatched_neighborhoods)
rm(unmatched_neighborhoods)

write.csv(airbnb_merged, "learning/data_output/airbnb_merged.csv", row.names = FALSE)