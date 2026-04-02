library(dplyr)

# Calculate 10-year rolling averages for each region and country combination
library(dplyr)
library(purrr)

# Function to compute rolling mean over a time window
rolling_mean_year <- function(years, values, window = 10) {
  map_dbl(seq_along(years), function(i) {
    in_window <- years >= (years[i] - window + 1) & years <= years[i]
    mean(values[in_window], na.rm = TRUE)
  })
}

herb_rolling <- herb_save |>
  group_by(Region) |>
  arrange(Year) |>
  mutate(
    Belgium_10yr = rolling_mean_year(Year, Belgium, window = 10),
    UK_10yr      = rolling_mean_year(Year, UK, window = 10),
    France_10yr  = rolling_mean_year(Year, France, window = 10),
    Spain_10yr   = rolling_mean_year(Year, Spain, window = 10)
  ) |>
  ungroup()

# Now extract min/max year values using the rolling averages
nmax <- herb_rolling |>
  filter(Region == "North", Year == max(Year[Region == "North"])) |>
  summarise(sum = Belgium_10yr + UK_10yr) |>
  pull(sum)

nmin <- herb_rolling |>
  filter(Region == "North", Year == min(Year[Region == "North"])) |>
  summarise(sum = Belgium_10yr + UK_10yr) |>
  pull(sum)

# Repeat pattern for middle and south regions
mmax <- herb_rolling |>
  filter(Region == "Middle", Year == max(Year[Region == "Middle"])) |>
  summarise(sum = France_10yr + Spain_10yr) |>
  pull(sum)

mmin <- herb_rolling |>
  filter(Region == "Middle", Year == min(Year[Region == "Middle"])) |>
  summarise(sum = France_10yr + Spain_10yr) |>
  pull(sum)

# Repeat pattern for middle and south regions
mUKmax <- herb_rolling |>
  filter(Region == "Middle", Year == max(Year[Region == "Middle"])) |>
  summarise(sum = UK_10yr + Belgium_10yr) |>
  pull(sum)

mUKmin <- herb_rolling |>
  filter(Region == "Middle", Year == min(Year[Region == "Middle"])) |>
  summarise(sum = UK_10yr + Belgium_10yr) |>
  pull(sum)

# Repeat pattern for South and south regions
smax <- herb_rolling |>
  filter(Region == "South", Year == max(Year[Region == "South"])) |>
  summarise(sum = France_10yr + Spain_10yr) |>
  pull(sum)

smin <- herb_rolling |>
  filter(Region == "South", Year == min(Year[Region == "South"])) |>
  summarise(sum = France_10yr + Spain_10yr) |>
  pull(sum)

# Repeat pattern for South and south regions
sUKmax <- herb_rolling |>
  filter(Region == "South", Year == max(Year[Region == "South"])) |>
  summarise(sum = UK_10yr + Belgium_10yr) |>
  pull(sum)

sUKmin <- herb_rolling |>
  filter(Region == "South", Year == min(Year[Region == "South"])) |>
  summarise(sum = UK_10yr + Belgium_10yr) |>
  pull(sum)

print(paste("UK and Belgium ancestry in the north changed from", nmin, "to", nmax))
print(paste("UK and Belgium ancestry in the middle changed from", mUKmin, "to", mUKmax))
print(paste("France and Spanish ancestry in the middle changed from", mmin, "to", mmax))
print(paste("UK and Belgium ancestry in the south changed from", sUKmin, "to", sUKmax))
print(paste("France and Spanish ancestry in the south changed from", smin, "to", smax))