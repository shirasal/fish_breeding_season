stern_table_1 <- stern_table %>% 
  mutate(season_end = ifelse(season_peak < season_start, season_end + 12, season_end),
         season_peak = ifelse(season_peak < season_start, season_peak + 12, season_peak)
  ) %>% 
  mutate(season_end = ifelse(season_peak > season_end, season_end + 12, season_end))


stern_table_2 <- stern_table_1 %>% 
  mutate(across(c(season_start, season_peak, season_end), .fns = function(x) x + 12))


stern_table_3 <- stern_table_1 %>% 
  mutate(across(c(season_start, season_peak, season_end), .fns = function(x) x + 24))


bind_rows(stern_table_1, stern_table_2, stern_table_3, .id = "year") %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -season_start), groups = year) + 
  geom_segment(aes(xend = season_end, yend = species), lineend = "round", lwd = 3, alpha = .5, col = "cornsilk4") + 
  geom_point(aes(x = season_peak, col = indigeneous), size = 3, shape = 19) + 
  geom_text(aes(x = season_start, label = heb_species), size = 3, nudge_x = -0.05, hjust = 1) +
  scale_colour_manual(values = c("#C52626", "#29B3D5", "#555758")) + 
  theme_minimal() + theme(axis.text.y = element_text(face = "italic"), 
                          axis.title = element_blank(),
                          panel.grid.minor = element_blank())+
  coord_cartesian(xlim = c(13,24))
