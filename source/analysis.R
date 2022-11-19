# Load the data.
# Compute metrics of interest.
# Generate data visualizations to display.

# .R file- algorithmic work
# .Rmd- create visuals

# save key values & charts to variables so they can be referenced
# in the index.Rmd file!

# Load Libraries

library(dplyr)
library(rworldmap) # easy mapping
library(rworldxtra)
library(RColorBrewer) # color palette

# Load the data.
life_exp <- read.csv(
  "https://raw.githubusercontent.com/hyi14/CH18/main/data/API_SP.DYN.LE00.IN_DS2_en_csv_v2_4700946.csv",
  skip = 4,
  stringsAsFactors = FALSE
)
View(life_exp)


## Compute metrics of interest.

# Which country had the longest life expectancy in 2015?
longest_le <- life_exp %>% 
  filter(X2015 == max(X2015, na.rm = T)) %>% 
  select(Country.Name, X2015) %>% 
  mutate(expectancy = round(X2015, 1)) # rename and format column

# Which country had the shortest life expectancy in 2015?
shortest_le <- life_exp %>% 
  filter(X2015 == min(X2015, na.rm = T)) %>% 
  select(Country.Name, X2015) %>% 
  mutate(expectancy = round(X2015, 1)) # rename and format column

# Calculate range in life expectancies
le_difference <- longest_le$expectancy - shortest_le$expectancy

# What are the 10 countries that experienced the greatest gain in life expectancy?
# show table in R Markdown (create dataframe, render using kable())
top_10_gain <- life_exp %>%
  mutate(gain = X2015 - X1960) %>%
  top_n(10, wt = gain) %>% # a handy dplyr function!
  arrange(-gain) %>%
  mutate(gain_formatted = paste(format(round(gain, 1), nsmall = 1), "years")) %>%
  select(Country.Name, gain_formatted)

# Join this data frame to a shapefile that describes how to draw each country
# The `rworldmap` package provides a helpful function for doing this
mapped_data <- joinCountryData2Map(
  life_exp,
  joinCode = "ISO3",
  nameJoinColumn = "Country.Code",
  mapResolution = "high"
)