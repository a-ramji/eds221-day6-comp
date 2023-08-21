#-------- Section 1: Filter --------#

library(tidyverse)
library(palmerpenguins)
library(lterdatasampler)

# Look for an exact match: ==

penguins_biscoe <- penguins |>  filter(island == "Biscoe")


penguins_2007 <- penguins |>  filter(year == 2007)


adelie_torgersen <- penguins |>  filter(species == "Adelie" & island == "Torgersen")

# Alternative: penguins |>  filter(species == "Adelie", island == "Torgersen")

# Create a subset from penguins that only contains Gentoo penguins observed in 2008

gentoo_2008 <- penguins |>  filter(species == "Gentoo", year == 2008)

# OR statements |

# Create a subset that contains Gentoos and (OR) Adelies

gentoo_adelie <- penguins |> filter(species == "Gentoo" | species == "Adelie")

# Create a subset that contains observations where the island is Dream OR the year is 2009

dream_or_2009 <- penguins |> filter(island == "Dream" | year == 2009)

# Create a GG plot with water temp vs size
ggplot(data = pie_crab, aes(x = water_temp, y = size)) +
  geom_point() +
  theme_minimal()
# Bergman's rule ? -- within the same species, in colder temperatures, that species will tend to be larger
# energy balances within organisms & why that can lead to larger organisms in colder temps -- in crabs, in colder temperatures, they molt fewer times. When crabs are molting, they're not growing -- if there is more time between molts, there is more time spent growing --- not fully proven though?


# Keep observations for sites NIB, ZI, DB, JC

#pie_crab |> filter(site == "NIB" | site == "ZI" | etc.)

# We can use the %in% operator to ask: does the value in our column match ANY of the values IN this vector


pie_sites <- pie_crab |>  filter(site %in% c("NIB", "ZI", "DB", "JC"))

# run a line of code in the Console to confirm that only the sites above remain in the new subset you created
# unique(pie_sites$site)


sites <- c("CC", "BB", "PIE")

pie_sites_2 <- pie_crab |> filter(site %in% sites)

# when you use the in operator, the order of things doesn't matter (which is what you want). If we used == instead, we're looking for vector matches in that order, recycled (almost NEVER what you want).

# Create a subset using the %in% operator that includes sites PIE, ZI, NIB, BB, and CC.

new_sites <- c("PIE", "ZI", "NIB", "BB", "CC")

pie_sites_3 <- pie_crab |> filter(site %in% new_sites)

# Excluding filter statements ----

# Include all sites EXCEPT ___
# != (asks is this NOT equal to that value)?

excludes_zi <- pie_crab |> filter(site != "ZI")

# What if I want to exclude sites BB, CC, and PIE?

exclude_bb_cc_pie <- pie_crab |> filter(!site %in% c("BB", "CC", "PIE"))


# Combinations of these

# Create a subset from pie_crab that only contains observations from sites NIB, CC, and ZI for crabs with carapace size exceeding 13


big_crabs_nib_cc_zi <- pie_crab |> filter(site %in% c("NIB", "CC", "ZI"),
                                          size > 13)

# ------- Selecting Columns -------#

# use dplyr::select()

# Select individual columns by name, separate by a comma

crabs_subset <- pie_crab |> select(latitude, size, water_temp)
# use names(crabs_subset)

# Select a range of columns using:
crabs_subset2 <- pie_crab |> select(site:air_temp)

# Select a range & an individual column
# include date:size and water temp

crabs_subset3 <- pie_crab |> select(date:water_temp, name)


# Exclude ranges, exclude individual columns

# Reorder things with select()
# also a reorder function in dplyr

#selects these columns IN THAT ORDER
pie_crab |> select(name, water_temp, size)

#---------------- Mutate ----------------#

# Use dplyr::mutate() to add or update a column, while keeping all exisiting columns

# add column for crab size in cm

crabs_cm <- pie_crab |>
  mutate(size_cm = size / 10)

# What happens if I use mutate() to add a new column containing the mean of the size column?

crabs_means <- pie_crab |>
  mutate(mean_size = mean(size, na.rm = TRUE))

crabs_awesome <- pie_crab |>
  mutate(name = "Teddy is awesome")


# Combine mutate() with group_by()

# Reminder of group_by() and summarize() in combination:

mean_size_by_site <- pie_crab |>
  group_by(site) |>
  summarize(mean_size = mean(size, na.rm = TRUE),
            sd_size = sd(size, na.rm = TRUE))

# Use group_by() and then mutate()

group_mutate <- pie_crab |>
  group_by(site) |>
  mutate(mean_size = mean(size, na.rm = TRUE))

# What if I want to create a new column in pie crab that contains "giant" if the size is greater than 35, or "not giant" if the size is less than or equal to 35?

# use dplyr::case_when() to write if-else statements more easily

crabs_bin <- pie_crab |>
  mutate(size_binned = case_when(
    size > 20 ~ "giant",
    size <= 20 ~ "not giant"
  ))

# one more example with case_when()
#

sites_bin <- pie_crab |>
  mutate(region = case_when(
    site %in% c("ZI", "CC", "PIE") ~ "Low",
    site %in% c("BB", "NIB") ~ "Middle",
    TRUE ~ "High"
#   .default = "High"
  ))


