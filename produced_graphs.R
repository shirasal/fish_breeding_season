library(lubridate)
library(tidyverse)

stern_table <- read_csv("SternTable.csv") %>% 
  mutate(indigeneous = case_when(species == "Scomber colias" ~ TRUE,
                                 TRUE ~ as.logical(indigeneous)))

stern_three <- read_rds("data_for_plotting.rds")

# Species ordered by season_start, colour of season span by habitat
stern_three %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -as.numeric(season_start)), group = year) + 
  geom_segment(aes(xend = season_end, yend = species, col = habitat), lineend = "round", lwd = 3, alpha = .8) + 
  scale_colour_manual(values = c("#C1C1BA", "#A7D1E6", "#E6DfA7")) +
  ggnewscale::new_scale_colour() + 
  geom_point(aes(x = season_peak), size = 3, shape = 18) + 
  geom_text(aes(x = season_start, label = heb_species, col = indigeneous), size = 3, nudge_x = -1, hjust = 1) +
  scale_colour_manual(values = c("#C52626", "#29B3D5")) + 
  scale_x_date(breaks = date_breaks('month'),
               labels = date_format("%b")) + 
  coord_cartesian(xlim = as.Date(c("2001-01-01", "2001-12-01"), origin = "1970-01-01")) +
  labs(title = "Eastern Mediterranean Species Breeding Season", 
       subtitle = "The points in the middle of the season span mark the season's peak.",
       caption = "נתונים מתוך דו\"ח \"עונתיות הרבייה של הדגה בחופי ישראל ובחינת פיזור הפאונה במרחב ובזמן\", שטרן ואחרים") + 
  ggthemes::theme_wsj() + theme(axis.text.y = element_text(face = "italic"), 
                                panel.grid.major.x = element_line(colour = "lightgray", linetype = "solid"),
                                panel.grid.major.y = element_line(colour = "lightgray", linetype = "dotted"),
                                legend.position = "bottom",
                                legend.title = element_text(size = 14),
                                legend.text = element_text(size = 12),
                                plot.caption = element_text(size = 9),
                                plot.title = element_text(size = 16),
                                plot.subtitle = element_text(size = 15))

# ggsave("Stern_Graph.png", units = "px", width = 4800, height = 3600)




