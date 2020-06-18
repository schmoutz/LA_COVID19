library(tidyverse)
cases <- read_csv("C:/Users/cschm1/Downloads/cases.csv",
                  col_types = cols(date = col_date(format = "%m/%d/%Y")))
cases <- cases %>% 
  arrange(parish, date) %>%
  group_by(parish) %>%
  mutate(cases_cumul = replace_na(cases_cumul, 0)) %>%
  mutate(cases_daily = cases_cumul-lag(cases_cumul)) %>%
  ungroup()

cases %>% 
  filter(region != "NA") %>% 
ggplot(., aes(x = date, y = cases_cumul)) +
geom_path(aes(color = factor(parish))) +
facet_wrap(.~region, ncol = 5) +
scale_y_log10() +
theme_minimal() +
theme(legend.position = "null") +
  labs(title = "Near-horizontal lines represent less daily case burden",
       subtitle = "Color denotes each parish within the region", 
       y = "Cumulative cases (log scale)")
ggsave("region_log.png")

cases %>% 
  filter(region != "1") %>% 
ggplot(., aes(x = date, y = cases_daily)) +
  geom_smooth(aes(color = factor(region))) +
  theme_minimal() +
  labs(title = "Most regions have increased daily cases",
       color = "LDH region",
       y = "Daily confirmed cases") +
  annotate("text",
           x = as.Date("2020-06-01"),
           y = -9,
           size = 3,
           label = "source: LDH data (2020-06-17)\ncreator: cschmoutz@gmail.com")
ggsave("region_daily.png")

cases %>% 
  filter(parish %in% c("Bossier", "Caddo", "Webster", "Ouachita", "Rapides", "Bienville", "De Soto")) %>% 
  arrange(parish) %>% 
  ggplot(., aes(x = date, y = cases_daily, color = factor(parish))) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Nearby parishes have increased daily cases",
       color = "Parish",
       y = "Daily confirmed cases",
       x = "Date") +
  ylim(-10, 65) +
  annotate("text",
           x = as.Date("2020-05-25"),
           y = -9,
           size = 3,
           label = "source: LDH data (2020-06-17)\ncreator: cschmoutz@gmail.com")
ggsave("parish_daily.png")
