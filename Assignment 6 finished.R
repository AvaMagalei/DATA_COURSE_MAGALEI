# Assignment 6
#-------------------------------------------------
# Load animation packages
library(gganimate)
library(gifski)
# Load data 

# Load tidyverse
library(tidyverse)

# Import the data
dat <- read_csv("BioLog_Plate_Data.csv")

# Look at the data
View(dat)


# Tidy the data
tidy_dat <- tidyr::pivot_longer(
  dat,
  cols = c(Hr_24, Hr_48, Hr_144),
  names_to = "Time",
  values_to = "Absorbance"
)

# Look at the tidied data
View(tidy_dat)


# Clean up the time column
tidy_dat$Time[tidy_dat$Time == "Hr_24"] <- "24"
tidy_dat$Time[tidy_dat$Time == "Hr_48"] <- "48"
tidy_dat$Time[tidy_dat$Time == "Hr_144"] <- "144"

# Rename the time column
names(tidy_dat)[names(tidy_dat) == "Time"] <- "Time (hours)"

# Convert time to numbers
tidy_dat$`Time (hours)` <- as.numeric(
  tidy_dat$`Time (hours)`
)


# Create a column for the environment
tidy_dat$Environment <- "Water"

# Identify soil samples
tidy_dat$Environment[tidy_dat$`Sample ID` == "Soil_1"] <- "Soil"
tidy_dat$Environment[tidy_dat$`Sample ID` == "Soil_2"] <- "Soil"

# Check the results
table(tidy_dat$Environment)


# Select dilution 0.1
plot_dat <- tidy_dat[tidy_dat$Dilution == 0.1, ]

# Check the results
table(plot_dat$Environment)


# Calculate mean absorbance
average_dat <- dplyr::summarise(
  dplyr::group_by(
    plot_dat,
    Substrate,
    Environment,
    `Time (hours)`
  ),
  Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
  .groups = "drop"
)

# Look at the averages
View(average_dat)


# Plot carbon source utilization
p1 <- ggplot(
  average_dat,
  aes(
    x = `Time (hours)`,
    y = Mean_Absorbance,
    color = Environment,
    group = Environment
  )
) +
  geom_point() +
  geom_line() +
  facet_wrap(~ Substrate) +
  labs(
    title = "Carbon Source Utilization",
    x = "Time (hours)",
    y = "Mean Absorbance",
    color = "Environment"
  ) +
  theme_bw()

# Display the plot
print(p1)

# Select Itaconic Acid
itaconic_dat <- tidy_dat[
  tidy_dat$Substrate == "Itaconic Acid",
]

# Calculate mean absorbance for each sample,
# dilution, and time
itaconic_mean <- dplyr::summarise(
  dplyr::group_by(
    itaconic_dat,
    `Sample ID`,
    Dilution,
    `Time (hours)`
  ),
  Mean_Absorbance = mean(Absorbance, na.rm = TRUE),
  .groups = "drop"
)
# Create the animated plot
p2 <- ggplot(
  itaconic_mean,
  aes(
    x = Dilution,
    y = Mean_Absorbance,
    color = `Sample ID`,
    group = `Sample ID`
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    limits = c(0, 2.0),
    breaks = seq(0, 2.0, by = 0.5)
  ) +
  labs(
    title = "Itaconic Acid Utilization",
    subtitle = "Time: {current_frame} hours",
    x = "Dilution",
    y = "Mean Absorbance",
    color = "Sample"
  ) +
  theme_bw() +
  transition_manual(`Time (hours)`)

# Generate the animation
animated_plot <- animate(
  p2,
  fps = 2,
  width = 900,
  height = 600,
  renderer = gifski_renderer()
)

# Display the animation
animated_plot

# Save the animation
anim_save(
  "itaconic_acid_animation.gif",
  animation = animated_plot
)
