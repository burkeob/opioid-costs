library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(choroplethrMaps)
library(readxl)
### load data 

data(county.regions)
#data(state.regions)

state_geocodes <- read_excel("../data/state-geocodes-v2011.xls", skip = 6,
                             col_names = F)

names(state_geocodes) <- c("Region", "Division", "State Code", "State Name")

state_geocodes <- state_geocodes %>% mutate(`State Name` = str_trim(`State Name`)) %>%
  mutate(`Region` = `Region` %>% as.numeric,
         `Division` = `Division` %>% as.numeric) %>%
  select(-`State Name`)

state_props <- read_delim("../data/WONDER Queries/State_Age_16_18.txt", delim = "\t") %>%
  left_join(state_geocodes) %>%
  filter(is.na(Notes)) %>%
  select(Division, `State`, `State Code`, `Ten-Year Age Groups Code`, `Deaths`) %>%
  rename(state_age_deaths = `Deaths`) %>%
  mutate(state_age_deaths = ifelse(state_age_deaths == "Suppressed", NA, state_age_deaths) %>% as.numeric) %>%
  filter(!`Ten-Year Age Groups Code` %in% c("NS", "1", "1-4", "5-14", "75-84", "85+")) %>%
  mutate(age_groups = case_when(`Ten-Year Age Groups Code` %in% c("15-24","1", "1-4", "5-14") ~ "0-24",
                                `Ten-Year Age Groups Code` %in% c("65-74", "75-84", "85+") ~ "65+",
                                TRUE ~ `Ten-Year Age Groups Code`)) %>%
  group_by(Division, `State`, `State Code`, age_groups) %>%
  summarize(state_age_deaths = sum(state_age_deaths)) %>%
  group_by(Division, `State`, `State Code`) %>%
  mutate(state_age_prop = state_age_deaths/sum(state_age_deaths, na.rm = T)) %>%
  ungroup %>%
  group_by(age_groups, Division) %>%
  mutate(average_age_prop = mean(state_age_prop, na.rm = T)) %>%
  mutate(state_age_prop = ifelse(is.na(state_age_prop), average_age_prop, state_age_prop)) %>%
  ungroup %>%
  select(State, `State Code`, state_age_prop, age_groups) %>%
  group_by(State, `State Code`) %>%
  mutate(state_age_prop = state_age_prop/sum(state_age_prop)) %>%
  ungroup

write_excel_csv(state_props, "../data/state_weights.csv")
