library(dplyr)
library(readr)
library(choroplethr)
library(choroplethrMaps)
library(tidycensus)
library(readxl)
library(stringr)
library(haven)
census_api_key('69d9f9e5add214b53a97d01d26fbec5051720528')

### uploads opioid deaths

a_deaths1 <- read_delim("../data/Diffusion/all_codes.txt", delim = "\t")
a_deaths2 <- read_delim("../data/Diffusion/all_codes2.txt", delim = "\t")
a_deaths <- a_deaths2 %>% bind_rows(a_deaths1)
p_deaths1 <- read_delim("../data/Diffusion/methadone_other.txt", delim = "\t")
p_deaths2 <- read_delim("../data/Diffusion/methadone_other2.txt", delim = "\t")
p_deaths <- p_deaths2 %>% bind_rows(p_deaths1)
i_deaths1 <- read_delim("../data/Diffusion/opium_heroin_synth.txt", delim = "\t")
i_deaths2 <- read_delim("../data/Diffusion/opium_heroin_synth2.txt", delim = "\t")
i_deaths <- i_deaths2 %>% bind_rows(i_deaths1)

### upload geographic data
data(county.regions)
d_matrix <- read_csv("../data/Diffusion/sf12010countydistance100miles.csv") %>%
  mutate(county1 = county1 %>% as.numeric,
         county2 = county2 %>% as.numeric)

### all death diffusion

d1 <- a_deaths %>%
  select(-Notes) %>% 
  filter(!is.na(County)) %>% 
  mutate(region = `County Code` %>% as.numeric,
         a_deaths = Deaths,
         population = Population,
         a_rate = `Crude Rate`,
         year = Year) %>%
  select(region, year, a_deaths, a_rate, population) %>%
  filter(a_rate != "Unreliable") %>%
  mutate(a_rate = a_rate %>% as.numeric) %>%
  left_join(county.regions)

d2 <- p_deaths %>%
  select(-Notes) %>% 
  filter(!is.na(County)) %>% 
  mutate(region = `County Code` %>% as.numeric,
         p_deaths = Deaths,
         p_rate = `Crude Rate`,
         year = Year) %>%
  select(region, year, p_deaths, p_rate) %>%
  #filter(p_rate != "Unreliable") %>%
  mutate(p_rate = p_rate %>% as.numeric) %>%
  left_join(county.regions)

d3 <- i_deaths %>%
  select(-Notes) %>% 
  filter(!is.na(County)) %>% 
  mutate(region = `County Code` %>% as.numeric,
         i_deaths = Deaths,
         i_rate = `Crude Rate`,
         year = Year) %>%
  select(region, year, i_deaths, i_rate) %>%
  #filter(i_rate != "Unreliable") %>%
  mutate(i_rate = i_rate %>% as.numeric) %>%
  left_join(county.regions)

d <- d1 %>%
  full_join(d2) %>%
  full_join(d3)

### a diffusion

output <- tibble(region = numeric(),
                 year = numeric(),
                 a_rate_local = numeric(),
                 p_rate_local = numeric(),
                 i_rate_local = numeric(),
                 a_rate_rand = numeric(),
                 p_rate_rand = numeric(),
                 i_rate_rand = numeric())

for (i in 1:dim(d)[1]) {
  r <- d$region[i]
  y <- d$year[i]
  adj <- d_matrix %>% filter(county1 == r) %>%
    pull(county2)
  rand <- d_matrix %>% pull(county2) %>% sample(length(adj))
  
  if (length(adj) > 0) {
    d_adj <- d %>% filter(!is.na(a_deaths), region %in% adj, year == y)
    # d_rand <- d %>% filter(!is.na(a_deaths), region %in% rand, year == y)
    a_pop_local <- sum(d_adj %>% filter(!is.na(a_deaths)) %>% pull(population), na.rm = T)
    p_pop_local <- sum(d_adj %>% filter(!is.na(p_deaths)) %>% pull(population), na.rm = T)
    i_pop_local <- sum(d_adj %>% filter(!is.na(i_deaths)) %>% pull(population), na.rm = T)
    a_deaths_local <- sum(d_adj %>% filter(!is.na(a_deaths)) %>% pull(a_deaths), na.rm = T)
    p_deaths_local <- sum(d_adj %>% filter(!is.na(p_deaths)) %>% pull(p_deaths), na.rm = T)
    i_deaths_local <- sum(d_adj %>% filter(!is.na(i_deaths)) %>% pull(i_deaths), na.rm = T)
    a_rate_local <- ifelse(a_deaths_local > 0, a_deaths_local/a_pop_local * 100000, NA)
    p_rate_local <- ifelse(p_deaths_local > 0, p_deaths_local/p_pop_local * 100000, NA)
    i_rate_local <- ifelse(i_deaths_local > 0, i_deaths_local/i_pop_local * 100000, NA)
    # a_pop_rand <- sum(d_rand %>% filter(!is.na(a_deaths)) %>% pull(population), na.rm = T)
    # p_pop_rand <- sum(d_rand %>% filter(!is.na(p_deaths)) %>% pull(population), na.rm = T)
    # i_pop_rand <- sum(d_rand %>% filter(!is.na(i_deaths)) %>% pull(population), na.rm = T)
    # a_deaths_rand <- sum(d_rand %>% filter(!is.na(a_deaths)) %>% pull(a_deaths), na.rm = T)
    # p_deaths_rand <- sum(d_rand %>% filter(!is.na(p_deaths)) %>% pull(p_deaths), na.rm = T)
    # i_deaths_rand <- sum(d_rand %>% filter(!is.na(i_deaths)) %>% pull(i_deaths), na.rm = T)
    # a_rate_rand <- ifelse(a_deaths_rand > 0, a_deaths_rand/a_pop_rand * 100000, NA)
    # p_rate_rand <- ifelse(p_deaths_rand > 0, p_deaths_rand/p_pop_rand * 100000, NA)
    # i_rate_rand <- ifelse(i_deaths_rand > 0, i_deaths_rand/i_pop_rand * 100000, NA)
    output <- output %>% 
      bind_rows(tibble(region = r,
                       year = y,
                       a_rate_local = a_rate_local,
                       p_rate_local = p_rate_local,
                       i_rate_local = i_rate_local))
  }
}
    

d1 <- d %>% left_join(output) %>%
  select(region, state.abb, year, a_deaths, population, a_rate, i_rate, p_rate, a_rate_local, p_rate_local, i_rate_local)


# d1 <- d1 %>% arrange(region, year) %>%
#   mutate(f_year = lead(year),
#          f_a_rate = lead(a_rate),
#          f_i_rate = lead(i_rate),
#          f_p_rate = lead(p_rate),
#          t = year - 2007) %>%
#   filter(year == f_year - 1) 

summary(lm(log(f_a_rate) ~ log(a_rate) + factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) + factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) * factor(t)  , data = d1))

summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) * factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) * factor(t) + factor(state.abb), data = d1))

# summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) + log(a_rate_rand) + factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) + factor(t) + factor(state.abb), data = d1))

# summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) * factor(t), data = d1))
# summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) * factor(t) + factor(state.abb), data = d1))

summary(lm(log(f_a_rate) ~ log(a_rate) + log(p_rate_local) + factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(p_rate_local) + factor(t) + factor(state.abb), data = d1))

summary(lm(log(f_a_rate) ~ log(a_rate) + log(i_rate_local) + factor(t), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(i_rate_local) + factor(t) + factor(state.abb), data = d1))

summary(lm(log(f_a_rate) ~ log(a_rate) + log(i_rate_local) + factor(t) + factor(state.abb), data = d1))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(p_rate_local) + log(i_rate_local) + factor(t), data = d1))

### With covars

# merge county-level demographic data

# population <- get_acs(geography = "county",
#                       variables = c(population = "B01003_001"),
#                       survey = "acs5",
#                       year = 2009) %>%
#   mutate(region = GEOID %>% as.numeric,
#          total_population = estimate) %>%
#   select(region, total_population)
# 
# workers <- get_acs(geography = "county",
#                    variables = c(m2534 = "B15001_011", # get males and females aged 25 through 64
#                                  m3544 = "B15001_019", 
#                                  m4564 = "B15001_027",
#                                  f2534 = "B15001_052",
#                                  f3544 = "B15001_060",
#                                  f4564 = "B15001_068"),
#                    survey = "acs5",
#                    year = 2017) %>%
#   group_by(GEOID) %>%
#   summarise(working_population = sum(estimate, na.rm = T)) %>% #sum population by GEOID (county)
#   mutate(region = GEOID %>% as.numeric) %>% 
#   select(region, working_population) %>% ungroup
# 
# educational_attainment <- get_acs("county",
#                                   variables = c("B06009_001", "B06009_002", "B06009_003"),  year = 2017, survey = "acs5") %>%
#   group_by(GEOID) %>%
#   summarise(pct_no_hs = estimate[2]/estimate[1],
#             pct_hs = estimate[3]/estimate[1],
#             pct_college = 1 - pct_no_hs - pct_hs) %>%
#   rename(region = GEOID) %>%
#   mutate(region = region %>% as.numeric)

urban_rural <- read_excel("../data/percent_rural.xlsx") %>%
  mutate(region = GEOID %>% as.numeric,
         percent_rural = `2010 Census \r\nPercent Rural`) %>%
  select(region, percent_rural)

# race <- get_acs(geography = "county",
#                 variables = c(total_population = "B03002_001",
#                               black_alone_not_hispanic = "B03002_004",
#                               hispanic_all_races = "B03002_012"),
#                 survey = "acs5", year = 2017) %>%
#   group_by(GEOID) %>%
#   summarise(percent_black = estimate[2]/estimate[1] * 100,
#             percent_hispanic = estimate[3]/estimate[1] * 100) %>%
#   mutate(region = GEOID %>% as.numeric) %>%
#   select(region, percent_black, percent_hispanic)
# 
# income <- read_excel("../data/est17all.xls", skip = 3) %>% select(`State FIPS Code`, `County FIPS Code`, `Median Household Income`)


# med_income <- income %>% mutate(med_income = `Median Household Income` %>% as.numeric,
#                                 region = str_c(`State FIPS Code`, `County FIPS Code`) %>% as.numeric) %>%
#   right_join(county.regions) %>% select(region, med_income)

unemployment <- tibble(region = numeric(),
                       year = numeric(),
                       unrate = numeric())

yrs <- c("99", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18")

for (i in 1:length(yrs)) {
  u <- read_excel(paste0("../data/laucnty", yrs[i], ".xlsx"), range = "A7:J3225",
                  col_names = F)
  names(u) <- LETTERS[1:10]
  u <- u %>% filter(!is.na(B)) %>%
    mutate(region = str_c(B, C) %>% as.numeric,
           unrate = J %>% as.numeric,
           year = E %>% as.numeric) %>%
    select(region, year, unrate)
  unemployment <- unemployment %>% bind_rows(u)
}

demographics <- county.regions %>% 
  select(region) %>% 
  # left_join(population) %>%
  # left_join(workers) %>%
  # left_join(race) %>% 
  # left_join(med_income) %>%
  left_join(urban_rural) %>%
  # left_join(educational_attainment) %>%
  left_join(unemployment) %>% tbl_df

d2 <- d1 %>% left_join(demographics)
names(d2)[2] <- "state_abb"

write_dta(d2, "../data/Diffusion/diffusion.dta")
t <- read_dta("../data/Diffusion/diffusion.dta")
summary(lm(log(f_a_rate) ~ log(a_rate) + log(med_income) + percent_rural + unrate + factor(t) + factor(state.abb), data = d2))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(a_rate_local) + log(med_income) + percent_rural + unrate + factor(t) + factor(state.abb), data = d2))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(med_income) + percent_rural + unrate + log(a_rate_local) * factor(t) + factor(state.abb), data = d2))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(p_rate_local) + log(med_income) + percent_rural + unrate + factor(t) + factor(state.abb), data = d2))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(i_rate_local) + log(med_income) + percent_rural + unrate + factor(t) + factor(state.abb), data = d2))
summary(lm(log(f_a_rate) ~ log(a_rate) + log(i_rate_local) + log(p_rate_local) + log(med_income) + percent_rural + unrate + factor(t) + factor(state.abb), data = d2))


#### ---- SCRATCH WORK ---- ####
# 
# ### p diffusion
# 
# output <- tibble(region = numeric(),
#                  year = numeric(),
#                  pop50 = numeric(),
#                  a_deaths50 = numeric(),
#                  p_deaths50 = numeric(),
#                  i_deaths50 = numeric(),
#                  a_rate50 = numeric(),
#                  p_rate50 = numeric(),
#                  i_rate50 = numeric(),
#                  n_adj = numeric())
# 
# for (i in 1:dim(d)[1]) {
#   r <- d$region[i]
#   y <- d$year[i]
#   adj <- d_matrix %>% filter(county1 == r) %>%
#     pull(county2)
#   d_adj <- d %>% filter(!is.na(p_deaths), region %in% adj, year == y)
#   output <- output %>% 
#     bind_rows(tibble(region = r,
#                      year = y,
#                      pop50 = sum(d_adj %>% pull(population)),
#                      a_deaths50 = sum(d_adj %>% pull(a_deaths)),
#                      p_deaths50 = sum(d_adj %>% pull(p_deaths)),
#                      i_deaths50 = sum(d_adj %>% pull(i_deaths)),
#                      a_rate50 = mean(d_adj %>% pull(a_rate)),
#                      p_rate50 = mean(d_adj %>% pull(p_rate)),
#                      i_rate50 = mean(d_adj %>% pull(i_rate)),
#                      n_adj = dim(d_adj)[1]))
# }
# 
# 
# d2 <- d %>% left_join(output %>% 
#                         filter(n_adj > 0) %>% 
#                         mutate(a_wtrate50 = a_deaths50/pop50 * 100000,
#                                p_wtrate50 = p_deaths50/pop50 * 100000,
#                                i_wtrate50 = i_deaths50/pop50 * 100000) %>%
#                         select(region, year, a_rate50, a_wtrate50, p_rate50, p_wtrate50,
#                                i_rate50, i_wtrate50))
# 
# 
# d2 <- d2 %>% arrange(region, year) %>%
#   mutate(f_year = lead(year),
#          f_deaths = lead(p_deaths),
#          f_rate = lead(p_rate),
#          t = year - 2007) %>%
#   filter(year == f_year - 1) 
# 
# summary(lm(log(f_rate) ~ log(p_rate) + log(a_wtrate50) + log(p_wtrate50) + log(i_wtrate50) + factor(t), data = d1))
# summary(lm(log(f_rate) ~ log(p_rate) + log(p_wtrate50) + factor(t), data = d1))
# summary(lm(log(f_rate) ~ log(p_rate) + log(a_wtrate50) + log(p_wtrate50) + log(i_wtrate50) + factor(t), data = d1))
# summary(lm(log(f_rate) ~ log(p_rate) + log(a_wtrate50) + log(p_wtrate50) + log(i_wtrate50) + factor(t), data = d1))
# 
# ### i diffusion
# 
# output <- tibble(region = numeric(),
#                  year = numeric(),
#                  pop50 = numeric(),
#                  a_deaths50 = numeric(),
#                  p_deaths50 = numeric(),
#                  i_deaths50 = numeric(),
#                  a_rate50 = numeric(),
#                  p_rate50 = numeric(),
#                  i_rate50 = numeric(),
#                  n_adj = numeric())
# 
# for (i in 1:dim(d)[1]) {
#   r <- d$region[i]
#   y <- d$year[i]
#   adj <- d_matrix %>% filter(county1 == r) %>%
#     pull(county2)
#   d_adj <- d %>% filter(!is.na(i_deaths), region %in% adj, year == y)
#   output <- output %>% 
#     bind_rows(tibble(region = r,
#                      year = y,
#                      pop50 = sum(d_adj %>% pull(population)),
#                      a_deaths50 = sum(d_adj %>% pull(a_deaths)),
#                      p_deaths50 = sum(d_adj %>% pull(p_deaths)),
#                      i_deaths50 = sum(d_adj %>% pull(i_deaths)),
#                      a_rate50 = mean(d_adj %>% pull(a_rate)),
#                      p_rate50 = mean(d_adj %>% pull(p_rate)),
#                      i_rate50 = mean(d_adj %>% pull(i_rate)),
#                      n_adj = dim(d_adj)[1]))
# }
# 
# 
# d3 <- d %>% left_join(output %>% 
#                         filter(n_adj > 0) %>% 
#                         mutate(a_wtrate50 = a_deaths50/pop50 * 100000,
#                                p_wtrate50 = p_deaths50/pop50 * 100000,
#                                i_wtrate50 = i_deaths50/pop50 * 100000) %>%
#                         select(region, year, a_rate50, a_wtrate50, p_rate50, p_wtrate50,
#                                i_rate50, i_wtrate50))
# 
# 
# d3 <- d3 %>% arrange(region, year) %>%
#   mutate(f_year = lead(year),
#          f_deaths = lead(i_deaths),
#          f_rate = lead(i_rate),
#          t = year - 2007) %>%
#   filter(year == f_year - 1) 
# 
# summary(lm(log(f_rate) ~ log(i_rate) + log(a_wtrate50) + log(p_wtrate50) + log(i_wtrate50) + factor(t), data = d1))
