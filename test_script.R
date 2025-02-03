library(readxl)
library(dplyr)
library(ggplot2)


data_raw <- read_excel("01_data/test_data.xlsx", col_names = c("time", "power", "rpm", "distance", "unused_2"))

data_cleaned <- data_raw %>% 
  filter(!is.na(distance)) %>% 
  select(time, power, distance)


data_binned <- data_cleaned %>%
  mutate(distance_bin = cut(distance, breaks = seq(0, 5, by = 0.5), include.highest = FALSE, right = TRUE)) %>%
  group_by(distance_bin) %>%
  summarize(avg_power = mean(power), .groups = 'drop') %>%
  mutate(distance_bin_right = as.numeric(sub(".*,([0-9]+\\.?[0-9]*)\\]", "\\1", as.character(distance_bin)))) 


ggplot(data_binned, aes(x = distance_bin_right, y = avg_power)) +
  geom_point(
    shape = 21,
    size = 5,
    color = "black",
    fill = "white",
    stroke = 1
  ) +
  labs(
    x = "Distance (km)",
    y = "Power (W)"
  ) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, max(data_binned$avg_power + 10))) +
  theme(
    axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 15)),
    axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 15)),
    axis.text = element_text(size = 12, color = "black")
  )

