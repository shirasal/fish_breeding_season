library(lubridate)
library(tidyverse)

stern_table <- read_csv("SternTable.csv") %>% 
  mutate(indigeneous = case_when(species == "Scomber colias" ~ TRUE,
                                 TRUE ~ as.logical(indigeneous)))

stern_table %>% 
  filter(season_peak < season_end & season_peak > season_start) %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -season_start)) + 
  geom_segment(aes(xend = season_end, yend = species), lineend = "round", lwd = 3, alpha = .5, col = "cornsilk4") + 
  geom_point(aes(x = season_peak, col = indigeneous), size = 3, shape = 19) + 
  geom_text(aes(x = season_start, label = heb_species), size = 3, nudge_x = -0.05, hjust = 1) +
  scale_colour_manual(values = c("#C52626", "#29B3D5", "#555758")) + 
  scale_x_continuous(breaks = c(1:12)) + 
  theme_minimal() + theme(axis.text.y = element_text(face = "italic"), 
                          axis.title = element_blank(),
                          panel.grid.minor = element_blank())

# Are species from the same habitat have the same seasons?
stern_table %>% 
  filter(season_peak < season_end & season_peak > season_start) %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -season_start)) + 
  geom_segment(aes(xend = season_end, yend = species), lineend = "round", lwd = 3, alpha = .5, col = "cornsilk4") + 
  geom_point(aes(x = season_peak, col = indigeneous), size = 3, shape = 19) + 
  geom_text(aes(x = season_start, label = heb_species), size = 3, nudge_x = -0.05, hjust = 1) +
  scale_colour_manual(values = c("#C52626", "#29B3D5", "#555758")) + 
  scale_x_continuous(breaks = c(1:12)) + 
  facet_wrap(~habitat, scales = "free_y", ncol = 1) + 
  theme_bw() +
  theme(
    axis.text.y = element_text(face = "italic"), 
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )


# Species caught in the same methods have similar seasonality?
stern_table %>% 
  mutate(ID = row_number()) %>% 
  pivot_longer(cols = c(starts_with("fishing")), names_to = "var")  %>% 
  filter(value == TRUE) %>% 
  mutate(fishing = str_remove(var, "fishing_")) %>% 
  filter(season_peak < season_end & season_peak > season_start) %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -season_start)) + 
  geom_segment(aes(xend = season_end, yend = species), lineend = "round", lwd = 3, alpha = .5, col = "cornsilk4") + 
  geom_point(aes(x = season_peak, col = indigeneous), size = 3, shape = 19) + 
  scale_colour_manual(values = c("#C52626", "#29B3D5", "#555758")) + 
  geom_text(aes(x = season_start, label = heb_species), size = 3, nudge_x = -0.05, hjust = 1) +
  scale_x_continuous(breaks = c(1:12)) + 
  facet_wrap(~fishing, scales = "free_y") + 
  theme_bw() +
  theme(
    axis.text.y = element_text(face = "italic"), 
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )