library(lubridate)
library(tidyverse)
library(scales)

stern_table <- read_csv("SternTable.csv") %>% 
  mutate(indigeneous = case_when(species == "Scomber colias" ~ TRUE,
                                 TRUE ~ as.logical(indigeneous)))

# Species with seasons that go through January - we'll manipulate them to adjust for plotting
stern_table %>% 
  filter(season_peak > season_end)

stern_table %>% 
  filter(season_peak < season_start)

species_to_correct <- c(
  stern_table %>% 
    filter(season_peak > season_end) %>% 
    pull(species),
  stern_table %>% 
    filter(season_peak < season_start)  %>% 
    pull(species)
)

year_before <- stern_table %>% 
  mutate(across(contains("season"), .fns = function(x) str_glue("1/{x}/2000"))) %>% 
  mutate(across(contains("season"), .fns = lubridate::dmy)) %>% 
  mutate(time = season_end - season_start) %>% 
  mutate(across(contains("season"), .fns = as.Date)) %>% 
  mutate(season_end = ifelse(time >= 0, season_end, season_end + years(1)),
         season_peak = ifelse(time >= 0, season_peak, season_peak + years(1))) %>%
  mutate(season_end = as.Date(season_end, origin = "1970-01-01"),
         season_peak = as.Date(season_peak, origin = "1970-01-01")) %>% 
  mutate(year = 2000,
         heb_species = NA)


current_year <- stern_table %>% 
  mutate(across(contains("season"), .fns = function(x) str_glue("1/{x}/2001"))) %>% 
  mutate(across(contains("season"), .fns = lubridate::dmy)) %>% 
  mutate(time = season_end - season_start) %>% 
  mutate(across(contains("season"), .fns = as.Date)) %>% 
  mutate(season_end = ifelse(time >= 0, season_end, season_end + years(1)),
         season_peak = ifelse(time >= 0, season_peak, season_peak + years(1))) %>%
  mutate(season_end = as.Date(season_end, origin = "1970-01-01"),
         season_peak = as.Date(season_peak, origin = "1970-01-01")) %>% 
  mutate(year = 2001)

next_year <- stern_table %>% 
  mutate(across(contains("season"), .fns = function(x) str_glue("1/{x}/2002"))) %>% 
  mutate(across(contains("season"), .fns = lubridate::dmy)) %>% 
  mutate(time = season_end - season_start) %>% 
  mutate(across(contains("season"), .fns = as.Date)) %>% 
  mutate(season_end = ifelse(time >= 0, season_end, season_end + years(1)),
         season_peak = ifelse(time >= 0, season_peak, season_peak + years(1))) %>%
  mutate(season_end = as.Date(season_end, origin = "1970-01-01"),
         season_peak = as.Date(season_peak, origin = "1970-01-01")) %>% 
  mutate(year = 2002,
         heb_species = NA)


stern_three <- bind_rows(year_before, current_year, next_year) 

stern_three %>% 
  ggplot() +
  aes(x = season_start, y = reorder(species, -as.numeric(season_start)), group = year) + 
  geom_segment(aes(xend = season_end, yend = species), lineend = "round", lwd = 4, alpha = .5, col = "cornsilk4") + 
  geom_point(aes(x = season_peak, col = indigeneous), size = 3, shape = 19) + 
  geom_text(aes(x = season_start, label = heb_species), size = 3, nudge_x = -0.05, hjust = 1) +
  scale_colour_manual(values = c("#C52626", "#29B3D5", "#555758")) + 
  scale_x_date(breaks = date_breaks('month'),
               labels = date_format("%b")) + 
  theme_minimal() + theme(axis.text.y = element_text(face = "italic"), 
                          axis.title = element_blank(),
                          panel.grid.minor = element_blank())+
  coord_cartesian(xlim = as.Date(c("2001-01-01", "2001-12-01"), origin = "1970-01-01"))

# saveRDS(stern_three, "data_for_plotting.rds")
