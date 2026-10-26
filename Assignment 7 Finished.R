

# Assignment 7
# Religion yay
# I am not religious 

# tidy verse for tidying
library(tidyverse)


# 1. Import the data -----------------------------------------------

religion_raw <- read_csv("Utah_Religions_by_County.csv") %>%
  rename_with(~ str_replace_all(str_to_lower(.x), "[ -]", "_"))

# what the heck is going on here
head(religion_raw)
glimpse(religion_raw)
summary(religion_raw)


# 2. Check and clean the data --------------------------------------

# missing values and duplicates ?
colSums(is.na(religion_raw))
sum(duplicated(religion_raw$county))

# unique records
nrow(religion_raw)
n_distinct(religion_raw$county)

# make sure population values are positive
summary(religion_raw$pop_2010)

# the original file already reports religious groups as proportions
# I will keep the original proportions
# taking out total religious column bc its not an actual religion

religion_columns <- setdiff(
  names(religion_raw),
  c("county", "pop_2010", "religious", "non_religious")
)

# create a cleaned dataset with one row per county and religion
# I won't lie ai did teach me how to use these %>% things

religion_long <- religion_raw %>%
  select(county, pop_2010, non_religious, all_of(religion_columns)) %>%
  pivot_longer(
    cols = all_of(religion_columns),
    names_to = "religion",
    values_to = "proportion"
  ) %>%
  filter(
    !is.na(county),
    !is.na(pop_2010),
    pop_2010 > 0,
    !is.na(proportion)
  )

# tidy now?
glimpse(religion_long)
head(religion_long)

# I am also keeping a county level data set for comparisons with the non religious folks
religion_county <- religion_raw %>%
  filter(
    !is.na(county),
    !is.na(pop_2010),
    pop_2010 > 0
  )

# do all the proportions add up?
religion_county %>%
  summarise(
    min_total = min(religious + non_religious, na.rm = TRUE),
    max_total = max(religious + non_religious, na.rm = TRUE)
  )


# 3. Exploratory figures -------------------------------------------

# Figure 1: population by county
# I want to see how different the sizes of Utah counties are

religion_county %>%
  ggplot(aes(
    x = reorder(county, pop_2010),
    y = pop_2010
  )) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Population by Utah County (2010)",
    x = "County",
    y = "Population"
  ) +
  theme_minimal()


# Figure 2: Compare the proportion of religious and non religious people across counties
# this gives an overall picture before looking at specific groups

religion_county %>%
  select(county, religious, non_religious) %>%
  pivot_longer(
    cols = c(religious, non_religious),
    names_to = "category",
    values_to = "proportion"
  ) %>%
  ggplot(aes(
    x = reorder(county, proportion),
    y = proportion,
    fill = category
  )) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Religious and Non-Religious Proportions by County",
    x = "County",
    y = "Proportion",
    fill = "Category"
  ) +
  theme_minimal()

# wow check out utah county


# Figure 3: Average Religious Group proportion
# how much of each


# Figure 3: Compare religious groups across Utah
# finally, a graph that doesn't need 29 county labels

religion_long %>%
  group_by(religion) %>%
  summarise(
    average_proportion = mean(proportion, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(
    x = average_proportion,
    y = reorder(religion, average_proportion)
  )) +
  geom_col() +
  labs(
    title = "Average Religious Group Proportions Across Utah Counties",
    x = "Average Proportion of County Population",
    y = "Religious Group"
  ) +
  theme_minimal()



# Figure 4: Population compared with specific religious proportions
# scatter plot time
# The population axis is displayed on a log scale because county populations are widely varied

ggplot(
  religion_long,
  aes(x = pop_2010, y = proportion)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  facet_wrap(~ religion, scales = "free_y") +
  labs(
    title = "County Population and Religious Group Proportions",
    x = "County Population (log scale)",
    y = "Religious Group Proportion"
  ) +
  theme_minimal()


# 4. Question 1: Population and specific religious groups -----------

# I am calculating Pearson correlations to explore linear relationships between county population and each group's proportion

population_correlations <- religion_long %>%
  group_by(religion) %>%
  summarise(
    correlation = {
      valid <- complete.cases(pop_2010, proportion)
      if (
        sum(valid) >= 2 &&
        sd(pop_2010[valid]) > 0 &&
        sd(proportion[valid]) > 0
      ) {
        cor(pop_2010[valid], proportion[valid])
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  arrange(desc(abs(correlation)))

print(population_correlations)


# Figure 5: Make the correlation results easier to compare
# Positive correlations are on one side and negative correlations are on the other

ggplot(
  population_correlations,
  aes(
    x = reorder(religion, correlation),
    y = correlation
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Correlation Between County Population and Religion",
    x = "Religious Group",
    y = "Pearson Correlation"
  ) +
  theme_minimal()


# 5. Question 2: Religious groups and non religious proportions ------

# I am comparing each specific religious group's proportion with the proportion of non religious folks in the same county
# using the county level non religious proportion from the original dataset rather than calculating it from the group columns

nonreligious_correlations <- religion_long %>%
  group_by(religion) %>%
  summarise(
    correlation = {
      valid <- complete.cases(proportion, non_religious)
      if (
        sum(valid) >= 2 &&
        sd(proportion[valid]) > 0 &&
        sd(non_religious[valid]) > 0
      ) {
        cor(proportion[valid], non_religious[valid])
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  arrange(desc(abs(correlation)))

print(nonreligious_correlations)


# Figure 6: Do specific religious groups correlate with non religious folks?
# Let's see if there's actually a pattern here

ggplot(
  nonreligious_correlations,
  aes(
    x = reorder(religion, correlation),
    y = correlation
  )
) +
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Correlation Between Religious Groups and Non-Religious Proportions",
    x = "Religious Group",
    y = "Pearson Correlation"
  ) +
  theme_minimal()


# 6. Final checks ---------------------------------------------------

# I am checking the details of the cleaned data before finishing the assignment

dim(religion_long)
n_distinct(religion_long$county)
head(religion_long)

# I can use the printed correlation tables and figures to describe the patterns I found in the data