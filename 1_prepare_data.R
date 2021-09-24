library(tidyverse)
library(haven)
library(lubridate)
library(data.table)

# -----------------------------------
# Read raw age and gender classification data
# match with ALM TV content analysis
# normalize, aggregate, the spiel
# Careful, needs ~64 GB of RAM
#
# NOTE: Due to its commercial source, we cannot
# share the raw ALM data. Code for handling the file
# is still included below for transparency.
# Replication of the analysis can be performed using
# the final aggregated data set and the `analysis.R` file.
# -----------------------------------

# -----------------------------------
# Load face classification data
ag <- fread("data/2021_age_gender_combined.tsv.gz",
            col.names = c("path", "classifier", "age", "female"))
# Pivot wide to go from 50 Mio+ to ~25 Mio
ag <- ag %>%
  distinct() %>%
  distinct(path, classifier, .keep_all = T) %>%
  pivot_wider(names_from = classifier,
              values_from=c(female, age))

# Load metadata on face size, bounding box and detector confidence
m <- fread("data/2021_meta_combined.tsv.gz",
           col.names = c("path", "width", "height", "top", "left", "bottom", "right", "confidence"))

# Join all classification data
agm <- ag %>%
  left_join(m, by="path") %>%
  select(-top, -bottom, -left, -right)

# Save checkpoint
agm %>% distinct() %>% fwrite("data/age_gender_meta.tsv.gz")
agm <- fread("data/age_gender_meta.tsv.gz")

# -----------------------------------
# Parse recording data from path names
# Attention: Takes a long time (~30-60 mins)
agm <- agm %>%
  mutate(path=str_remove(path, "NACH_")) %>%
  mutate(path=str_remove(path, ".jpg")) %>%
  separate(path,
           into=c("head", NA, "frame", NA, "second", "face"),
           sep = "_") %>%
  separate(head, into=c("station", "datestamp"), sep="-") %>%
  mutate(face=str_remove(face, "face")) %>%
  mutate(datestamp=str_remove(datestamp, ".ts")) %>%
  mutate(date=lubridate::ymd_hm(datestamp)) %>%
  mutate(date=date+lubridate::seconds(second))

# Save checkpoint
# Attention: At this point you will want to restart the session
# and load the datafile to free some RAM
agm %>% fwrite("data/agm_gender_meta_parsed.tsv.gz")
agm <- fread("data/agm_gender_meta_parsed.tsv.gz")

# -----------------------------------
# Load and merge in ALM content data

read_spss("data/ALM-2012-2018.sav") %>%
  mutate(sender=str_to_lower(as.character(as_factor(sender)))) %>%
  mutate(tab1=as.character(as_factor(tab1))) %>%
  mutate(tab6=as.character(as_factor(tab6))) %>%
  mutate(tab8=as.character(as_factor(tab8))) %>%
  mutate(tab10=as.character(as_factor(tab10))) %>%
  mutate(station=recode(sender, "prosieben"="pro7")) %>%
  mutate(start_date=make_datetime(
    year=jahr, month=monat, day=tag, hour=beghh, min=begmin, sec=begsek)
  ) %>%
  mutate(end_date=start_date+dauer) %>%
  mutate(range=lubridate::interval(start=start_date, end=end_date)) %>%
  fwrite("data/ALM-2012-2018.tsv.gz")

# This is preprocessed to include date ranges
alm <- fread("data/ALM-2012-2018.tsv.gz")

# This is a somewhat unusual time range join
# Join face timestamps to date ranges of shows
# The by far fastest way to do this is by using data.table
o <- alm[agm, on = .(station, start_date <= date, end_date >= date), nomatch = 0L]
fwrite(o, "data/agm_alm_gender.tsv.gz")
a <- as_tibble(o)

# -----------------------------------
# Prune data to remove overhanging hours between 0 and 3 am
spurious_days <- c(
  ymd("2016-04-20"),
  ymd("2016-05-08"),
  ymd("2017-02-22"),
  ymd("2017-06-12"),
  ymd("2018-02-19"),
  ymd("2018-03-16"),
  ymd("2018-04-08"),
  ymd("2018-04-25"),
  ymd("2018-05-15"),
  ymd("2018-06-07"),
  ymd("2018-06-30")
)

a <- a %>%
  mutate(date=date(start_date)) %>%
  filter(!date %in% spurious_days)

# -----------------------------------
# Calculate classifier means and gender counts
# Takes ~30-60 minutes
a <- a %>%
  rowwise() %>%
  mutate(female=mean(female_imdb, female_utk),
         age=mean(age_imdb, age_utk),
         female_count=sum(female >= .5),
         male_count=sum(female < .5)
  ) %>%
  ungroup()

# -----------------------------------
# Calculate crossed age and gender group counts
# To do so, we build ONE intermediary column from age decades with
# string representation for gender and age, i.e. "30f".
# That column is then pivoted. We repeat the same later for a set of broader
# age slices.
# Attention: Takes a while
a <- a %>%
  mutate(ar = floor(age/10)*10,
         ac=as.numeric(cut(age, c(0,16,30,50,100))),
         f=ifelse(female>=.5, "f", "m"),
         arf=paste0(ar,f),
         arc=paste0(ac, f),
         one=1) %>%
  pivot_wider(names_from = arc,
              values_from=one)

a <- a %>%
  mutate(one=1) %>%
  pivot_wider(names_from = arf,
              values_from=one)

# -----------------------------------
# Variable names with leading digits are really unhandy,
# so we prefix them
prefix1 <- function(x) str_c("gr1_", x)
prefix10 <- function(x) str_c("gr10_", x)

a <- a %>%
  rename_with(prefix10, matches("^[0-9]{2}f")) %>%
  rename_with(prefix10, matches("^[0-9]{2}m")) %>%
  rename_with(prefix1, matches("^[0-9]{1}f")) %>%
  rename_with(prefix1, matches("^[0-9]{1}m"))

# -----------------------------------
# Fill NAs introduced by pivoting
a <- a %>%
  mutate_at(vars(ends_with("m")),
            ~replace(., is.na(.), 0)) %>%
  mutate_at(vars(ends_with("f")),
            ~replace(., is.na(.), 0))

# Save checkpoint
a %>% fwrite("data/agm_alm_gender_counts.tsv.gz")
a <- fread("data/agm_alm_gender_counts.tsv.gz")

# -----------------------------------
# Aggregate by individual show fragments
# vars with "beg" are time points for show start, these together with station
# uniquely identify a single segment.
# i.e. begh = hour, begm = minute etc etc.
# Create first snapshot for inspection
a %>%
  group_by_at(vars(starts_with("beg"), station, jahr, titel)) %>%
  summarize(
    across(starts_with("gr"), ~sum(.)),
    across(ends_with("count"), ~sum(.)),
    across(starts_with("tab"), funs=~first(.)),
    across(c(dauer, date), funs=~first(.))
  ) %>%
  distinct() %>%
  fwrite("data/alm_age_gender_aggregate_shows.tsv.gz")

# -----------------------------------
# Prepare a filtered dataset with faces >= 64 pixels wide
# and confidence > .95
# Create second snapshot for comparison
a %>%
  filter(width>=64, confidence>=.95) %>%
  group_by_at(vars(starts_with("beg"), station, jahr, titel)) %>%
  summarize(
    across(starts_with("gr"), ~sum(.)),
    across(ends_with("count"), ~sum(.)),
    across(starts_with("tab"), funs=~first(.)),
    across(c(dauer, date), funs=~first(.)),
    mean_age = mean(age)
  ) %>%
  distinct() %>%
  fwrite("data/alm_age_gender_aggregate_shows_64width.tsv.gz")

# -----------------------------------
# Add age/gender means and SD for further processing,
# create an ID variable "ts"
a <- a %>%
  filter(width>=64, confidence>=.95) %>%
  unite(ts, jahr, station, titel, starts_with("beg"), remove=F) %>%
  mutate(gender=ifelse(female>.5, "female", "male")) %>%
  group_by(ts, gender) %>%
  summarize(
    across(starts_with("gr"), ~sum(.)),
    across(ends_with("count"), ~sum(.)),
    across(starts_with("tab"), funs=~first(.)),
    across(c(dauer, date, station, titel), funs=~first(.)),
    mean_age = mean(age),
    sd_age = sd(age)
  ) %>%
  distinct()

a <- a %>%
  pivot_wider(names_from=gender,
              values_from = c(mean_age, sd_age)) %>%
  replace_na(list(mean_age_male=0,
                  mean_age_female=0,
                  sd_age_male=0,
                  sd_age_female=0)) %>%
  summarise_all(max)

# Recode genres
a <- a %>%
  mutate(sparte = tab10,
         sparte = ifelse(tab10 %in% c("Sportnachrichten etc", "Sportübertragungen"), "Sport", sparte),
         sparte = ifelse(tab10 %in% c("Sonstige Magazine",
                                      "Reportagen Dokus",
                                      "Boulevardmagazine"),  "Magazine, Dokus", sparte),
         sparte = ifelse(tab10 == "FSP Reality", "Non-fiktion", sparte),
         sparte = ifelse(tab10 == "Interviews Talk", "Talk", sparte),
         sparte = ifelse(tab8 == "Non-fiktion", "Non-fiktion", sparte),
         sparte = ifelse(tab8 == "Fiktionale Unterhaltung", "Fiktion", sparte),
         sparte = ifelse(tab10 %in% c("FSF", "Übertragungen"), "Sonstige FSP", sparte),
         sparte = ifelse(tab8 %in% c("Nonfiktionale Kinder",
                                     "Religiöse Sendungen",
                                     "Religioese Sendungen",
                                     "Restliches Programm"), "Sonstige", sparte),
         # Werbung zuletzt wg. Dauerwerbesendungen == Non-fiktion/Restliches Programm
         sparte = ifelse(!tab1 %in% c(0,"Redaktionelle Sendungen"), "Werbung", sparte)
  )

# Coarser aggregation
a <- a %>%
  ungroup() %>%
  mutate(ASparte = recode(sparte,
                          "Magazine, Dokus"="FSP",
                          "Talk"="FSP",
                          "Sonstige FSP"="FSP",
  ))

a <- a %>%
  mutate(public = factor(station %in% c("ard", "zdf")),
       n = female_count + male_count,
       female_prop = female_count / n,
       station = toupper(station),
       jahr = lubridate::year(date),
       year =  jahr - 2014) %>%
  filter(jahr <= 2017) %>%
  rownames_to_column("id") %>%
  mutate(ASparte = ifelse(titel =="Werbung" | titel =="Sponsoring" | str_detect(titel, "SPOT"), "Werbung", ASparte)) %>%
  mutate(genre = recode(ASparte,
                        "Nachrichten"="News",
                        "Werbung"="Adverts",
                        "Fiktion"="Fictional\nentertainment",
                        "Sonstige"="Other",
                        "Sport"="Sports",
                        "Non-fiktion"="Non-fictional\nentertainment",
                        "FSP"="Editorial\nContent",
  )) %>%
  filter(genre!="Other")

# Save checkpoint for analysis
a %>%
  select(-sparte, -ASparte, -titel, -contains("tab")) %>%
  fwrite("data/tv_sparte.tsv.gz")

# Prepare hourly aggregates for heatmap
# We create an artificial year with days numbered sequantially
# i.e. sample day 1 is artificial January 1st, sample day 2 is the 2nd etc.
# So that we can use temporal plots easily
fread("data/agm_alm_gender_counts.tsv.gz") %>%
  as_tibble() %>%
  filter(date < lubridate::ymd("20180101")) %>%
  group_by(date) %>%
  mutate(day_id = group_indices() - 1) %>%
  ungroup() %>%
  mutate(date_time = (lubridate::ymd_hm(datestamp) + lubridate::seconds(second))) %>%
  mutate(hour=lubridate::hour(date_time), minute=lubridate::minute(date_time)) %>%
  mutate(fake_date=lubridate::ymd("2014-01-01") + day_id + lubridate::hours(hour)) %>%
  group_by(fake_date, station) %>%
  summarize(age=mean(age),
            female_count=sum(female_count),
            male_count=sum(male_count),
            day_id=day_id) %>%
  distinct() %>%
  ungroup() %>%
  mutate(female_prop=female_count/(female_count+male_count)) %>%
  mutate(hour=lubridate::hour(fake_date)) %>%
  fwrite("data/hourly_age_gender.tsv.gz")
