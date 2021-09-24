if (!require("pacman")) install.packages("pacman", repos = "https://cloud.r-project.org")
pacman::p_load(ggeffects, furrr, lme4, lubridate, tidyverse, patchwork, data.table)
pacman::p_load_gh("easystats/easystats")

# ----------------------
# Analyze age and gender from faces on TV
# Performs estimation and produces plots
# ----------------------

# Set up basics
plan(multisession)
theme_set(theme_bw() +
            theme(
              strip.background = element_blank(),
              axis.text = element_text(colour = "grey20"),
              legend.position = "bottom", legend.spacing = unit(0, "lines"),
              legend.title = element_blank()
            ))

# Load data, make sure public is a factor
d = read_csv("data/agm_program-aggregates.tsv.gz") %>%
  mutate(public=as_factor(public))

# ---------------------- GENDER MODELS
# Define models
models = list(
  m0 = female_prop ~ 1 + (1 | station),
  m1 = female_prop ~ year * public + (year | station),
  m2 = female_prop ~ year*public + genre*public + (year|station)
)

# Fit models
results =  models %>%
  future_map(~ glmer(.x, data=d, weights = n,
                     family = binomial(link = "logit"),
                     control = glmerControl(optimizer = "nloptwrap",
                                            optCtrl = list(algorithm = "NLOPT_LN_BOBYQA"),
                                            calc.derivs = FALSE)
  ),
  seed=TRUE)

# Null model
results$m0 %>% broom.mixed::tidy(conf.int = T) %>%
  slice(1) %>%
  select(estimate, conf.low, conf.high) %>%
  mutate_all(plogis) %>%
  mutate_all(round,2)

# Produce aggregates from raw data
daily = d %>%
  group_by(date, station) %>%
  summarise(fem = sum(female_count)/sum(n)) %>%
  mutate(year = lubridate::year(date)-2014) %>%
  mutate(genre = "Overall")

daily_genre = d %>%
  group_by(date, station, genre) %>%
  summarise(fem = sum(female_count)/sum(n)) %>%
  mutate(year = lubridate::year(date)-2014)

all_daily = bind_rows(daily, daily_genre)

# Get predictions from models
pred_overall =
  ggpredict(results$m1, c("year", "station", "public"),
          condition = c(n=1), type = "random", ci.lvl = .95) %>%
  as_tibble() %>%
  rename(year = x, fem = predicted, station = group, public = facet) %>%
  filter(public == T & station %in% c("ARD", "ZDF") | public == F & !station %in% c("ARD", "ZDF")) %>%
  mutate(genre = "Overall")

# Get predictions from models.
# Add in raw data aggregates - note the "data=" in geom_jitter
pred_by_genre = ggpredict(results$m2, c("year", "station", "public", "genre"), condition = c(n=1), type = "random", ci.lvl = .9) %>%
  as_tibble() %>%
  rename(year = x, fem = predicted, station = group, public = facet, genre = panel) %>%
  filter(public == T & station %in% c("ARD", "ZDF") | public == F & !station %in% c("ARD", "ZDF"))

# ----------------------
# FIGURE 1 - share of female faces per station per genre per year
# Visualize model estimates
bind_rows(pred_overall, pred_by_genre)%>%
  filter(!(genre=="Sports" & station %in% c("RTL", "SAT.1", "PRO7", "VOX"))) %>%
  mutate(station = factor(station, levels = c("ARD", "ZDF", "RTL", "SAT.1", "PRO7", "VOX")),
         genre = factor(genre, levels = c("Overall",  "News", "Editorial\nContent", "Fictional\nentertainment","Non-fictional\nentertainment","Sports", "Adverts"))) %>%
  ggplot(aes(x = year+2014, y = fem))+
  geom_hline(yintercept = .5, alpha = .5, linetype = 3)+
  geom_jitter(data = all_daily, aes(x = year+2014, y = fem), alpha = .2, size = .5) +
  geom_line()+
  facet_grid(as_factor(genre) ~ as_factor(station))+
  coord_cartesian()+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Proportion of females")

ggsave("figures/fig1.png", width = 10, height = 7)

# ----------------------
# FIGURE 2 - share of female faces per genre, split by public/private broadcaster
bind_rows(
  ggpredict(results$m2, c("genre", "public"),condition = c(n=1, year = 3)),
  .id = "estimator"
)%>%
  ggplot(aes(x = reorder(x, predicted), y = predicted, ymin = conf.low, ymax = conf.high, shape = group))+
  geom_hline(yintercept = .5, alpha = .5, linetype = 3)+
  geom_hline(yintercept = plogis(fixef(results$m0)), alpha = .5, linetype = 2)+
  geom_pointrange(position = position_dodge(.5))+
  coord_flip(ylim = c(0,.6)) +
  scale_shape_discrete(labels = c("Private broadcasters", "Public service broadcasters"))+
  labs(x = "", y = "Estimated proportion of female faces", color = "")+
  theme(legend.position = "top")

ggsave("figures/fig2_bw.png", width = 8, height = 4)

# ---------------------- AGE MODELS
# Linear models for age
# Null model
m0_fem = lmer(mean_age_female ~ 1 + (1|station), d)
m0_male = lmer(mean_age_male ~ 1 + (1|station), d)

avg_ages = list(m0_fem, m0_male) %>%
  map(fixef)

# Nested year in station
age_m2 = list("female" = "mean_age_female", "male" = "mean_age_male") %>%
  map(~ paste(.x, "~ year * public + (year | station)")) %>%
  map(~ lmer(.x, d,
             control = lmerControl(optimizer = "nloptwrap",
                                   optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD"))))
# With genre
age_m3 = list("female" = "mean_age_female", "male" = "mean_age_male") %>%
  map(~ paste(.x, "~ year*public + genre*public + (year|station)")) %>%
  map(~ lmer(.x, d,
      control = lmerControl(optimizer = "nloptwrap",
                            optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD"))))

# Aggregate raw data for age
daily_age = d %>%
  group_by(date, station) %>%
  summarise(female = mean(mean_age_female, na.rm=T), male = mean(mean_age_male, na.rm=T)) %>%
  mutate(year = lubridate::year(date)-2014) %>%
  mutate(genre = "Overall")

daily_genre_age = d %>%
  group_by(date, station, genre) %>%
  summarise(female = mean(mean_age_female, na.rm=T), male = mean(mean_age_male, na.rm=T)) %>%
  mutate(year = lubridate::year(date)-2014)

all_daily_age = bind_rows(daily_age, daily_genre_age) %>%
  gather(gender, avg, female, male)


# Get model predictions for age - without genre
pred_overall_age = age_m2 %>%
  map_df(~ ggpredict(.x, c("year", "station", "public"),
            condition = c(n=1), type = "random", ci.lvl = .95), .id = "gender") %>%
  as_tibble() %>%
  rename(year = x, age = predicted, station = group, public = facet) %>%
  filter(public == T & station %in% c("ARD", "ZDF") | public == F & !station %in% c("ARD", "ZDF")) %>%
  mutate(genre = "Overall")

# Get model predictions for age - with genre
pred_by_genre_age = age_m3 %>%
  map_df(~ ggpredict(.x, c("year", "station", "public", "genre"),
                     condition = c(n=1), type = "random", ci.lvl = .95), .id = "gender") %>%
  as_tibble() %>%
  rename(year = x, age = predicted, station = group, public = facet, genre = panel) %>%
  filter(public == T & station %in% c("ARD", "ZDF") | public == F & !station %in% c("ARD", "ZDF"))

# ----------------------
# FIGURE 3 - age by gender, station, year and genre
bind_rows(pred_overall_age, pred_by_genre_age)%>%
  filter(!(genre=="Sports" & station %in% c("RTL", "SAT.1", "PRO7", "VOX"))) %>%
  mutate(station = factor(station, levels = c("ARD", "ZDF", "RTL", "SAT.1", "PRO7", "VOX")),
         genre = factor(genre, levels = c("Overall",  "News", "Editorial\nContent", "Fictional\nentertainment","Non-fictional\nentertainment","Sports", "Adverts"))) %>%
  ggplot(aes(x = year+2014, y = age, color = gender))+
  #geom_hline(yintercept = plogis(fixef(m0)), alpha = .5, linetype = 2)+
  geom_hline(yintercept = .5, alpha = .5, linetype = 3)+
  geom_jitter(data = all_daily_age, aes(x = year+2014, y = avg), alpha = .2, size = .5) +
  geom_line()+
  # geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, color = 0)+
  facet_grid(as_factor(genre) ~ as_factor(station))+
  coord_cartesian(ylim = c(20,50))+
  theme(legend.position = "none")+
  labs(x = "Year", y = "Estimated average age")

ggsave("figures/fig3.png", width = 10, height = 7)

# ----------------------
# FIGURE 4 - age of faces by gender, genre and split by public/private broadcaster
age_m3 %>%
  map_df(~ ggpredict(.x, c("genre", "public"),condition = c(n=1, year = 3)), .id = "gender") %>%
  as_tibble() %>%
  ggplot(aes(x = reorder(x, predicted), y = predicted,
             ymin = conf.low, ymax = conf.high, shape = group, color = gender, group = group))+
  geom_pointrange(position = position_dodge(.5))+
  scale_shape_discrete(labels = c("Private broadcasters", "Public service broadcasters"))+
  scale_color_discrete(labels = c("Female", "Male"))+
  coord_flip() +
  labs(x = "", y = "Estimated average age", color = "")+
  theme(legend.position = "top")

ggsave("figures/fig4.png", width = 8, height = 4)


# Age gap analysis
lmer(I(mean_age_male - mean_age_female) ~  (1 | station), d,
     control = lmerControl(optimizer = "nloptwrap",
                           optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD"))) %>%
  summary


lmer(I(mean_age_male - mean_age_female) ~  year * public + (year | station), d,
     control = lmerControl(optimizer = "nloptwrap",
                           optCtrl = list(algorithm = "NLOPT_LN_NELDERMEAD"))) %>%
  summary

# ----------------
# FIGURE 5 - Simplified figure 3 without raw data
bind_rows(pred_overall, pred_by_genre)%>%
  filter(!(genre=="Sports" & station %in% c("RTL", "SAT.1", "PRO7", "VOX"))) %>%
  mutate(station = factor(station, levels = c("ARD", "ZDF", "RTL", "SAT.1", "PRO7", "VOX")),
         genre = factor(genre, levels = c("Overall",  "News", "Editorial\nContent", "Fictional\nentertainment","Non-fictional\nentertainment","Sports", "Adverts"))) %>%
  ggplot(aes(x = year+2014, y = fem, color=genre))+
  geom_hline(yintercept = .5, alpha = .5, linetype = 3)+
  geom_line()+
  facet_grid(~ as_factor(station))+
  coord_cartesian()+
  theme(legend.position = "bottom")+
  labs(x = "", y = "Proportion of females") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(colour = guide_legend(nrow = 1))

ggsave("figures/fig5-slopes.png", width = 10, height = 7)

# ----------------------
# FIGURE 6 - Heatmap
# Load hourly aggregates
b <- fread("data/hourly_age_gender.tsv.gz")

# FIGURE 6.1 - Heatmap of gender
heatmap1 <-  b %>%
  ggplot(aes(x=day_id, y=hour)) +
  geom_tile(aes(fill = female_prop*100), colour = "white") +
  scale_fill_distiller("percent female", palette = "PRGn", direction = -1, limits=c(0,100)) +
  facet_wrap(~station, ncol=1) +
  ylab("hour") +
  xlab("sample days") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text()) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = .5, title.vjust = 1))

ggsave("figures/fig6-1.png", width = 8, height = 4)

# FIGURE 6.2 - Heatmap of age
heatmap2 <- b %>%
  ggplot(aes(x=day_id, y=hour)) +
  geom_tile(aes(fill = age), colour = "white") +
  scale_fill_distiller("age",
                       direction = 1,
                       limits=c(30,50)) +
  facet_wrap(~station, ncol=1) +
  ylab("") +
  xlab("sample days") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_text()) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = .5, title.vjust = 1))

ggsave("figures/fig6-2.png", width = 8, height = 4)

heatmap_combined <- heatmap1 + heatmap2
ggsave("figures/fig6-combined.png", plot=heatmap_combined, width = 9, height = 8)

