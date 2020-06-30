library(tidyverse)
library(readxl)

# ---- Read data ----

division <- read_tsv("../data/Census 9 x Urbanization/Division/Division_2018.txt")%>%
  filter(is.na(Notes)) %>%
  
  filter(!is.na(`Census Division`)) %>%
  select(`Census Division Code`, Year, Deaths, `Census Division`)%>%
  rename(total_deaths = Deaths)
division_age <- read_tsv("../data/Census 9 x Urbanization/Division/Division_Age_2018.txt")%>%
  filter(is.na(Notes)) %>%
  
  filter(!is.na(`Census Division`))%>%
  select(`Census Division Code`, Year, Deaths, `Ten-Year Age Groups`) %>%
  rename(age_deaths = Deaths)


region <- read_tsv("../data/Census 9 x Urbanization/Region/Region_2018.txt") %>%
  filter(is.na(Notes)) %>%
  
  filter(!is.na(`Census Region`)) %>%
  select(`Census Region Code`, Year, Deaths)%>%
  rename(total_deaths = Deaths)
region_age <- read_tsv("../data/Census 9 x Urbanization/Region/Region_Age_2018.txt")%>%
  filter(is.na(Notes)) %>%
  
  filter(!is.na(`Census Region`))%>%
  select(`Census Region Code`, Year, Deaths, `Ten-Year Age Groups`) %>%
  rename(age_deaths = Deaths)


national_age <- read_tsv("../data/WONDER Queries/Opioid_National_Age_Totals_18.txt") %>%
  select(`Ten-Year Age Groups`, Year, Deaths) %>%
  rename(national_age = Deaths) %>%
  filter(!is.na(`Ten-Year Age Groups`))

national_age <- national_age %>%
  group_by(Year) %>%
  mutate(total_n_deaths = sum(national_age)) %>%
  mutate(national_pct = national_age / total_n_deaths)


urbanization <- read_tsv("../data/Census 9 x Urbanization/urbanization_18.txt")  %>%
  filter(!is.na(`Census Division Code`)) %>%
  filter(is.na(Notes)) %>%
  
  select(`Census Division Code`, Year, Deaths, `2013 Urbanization Code`, Population)%>%
  rename(total_deaths = Deaths)

urbanization_age <- read_tsv("../data/Census 9 x Urbanization/urbanization_age_18.txt")  %>%
  filter(!is.na(`Census Division Code`)) %>%
  filter(is.na(Notes)) %>%
  select(`Census Division Code`, Year, Deaths, `2013 Urbanization Code`, `Ten-Year Age Groups`)%>%
  rename(u_age_deaths = Deaths)




# ---- Assign missing deaths based on region, then national percents ----


region_combined <- region %>%
  left_join(region_age) %>%
  group_by(Year, `Census Region Code`) %>%
  mutate(age_deaths = as.numeric(age_deaths)) %>%
  mutate(total_r_deaths = sum(age_deaths, na.rm = T)) %>%
  mutate(missing_region = total_deaths - total_r_deaths) 

missing_region <- region_combined %>%
  filter(is.na(age_deaths))

missing_region <- missing_region %>%
  left_join(national_age)

missing_region <- missing_region %>%
  group_by(`Census Region Code`, Year) %>%
  mutate(sum_missing_pct = sum(national_pct)) %>%
  mutate(r = national_pct / sum_missing_pct)%>%
  rowwise() %>%
  mutate(impute_r_deaths = r * missing_region)

new_region <- missing_region %>%
  select(`Census Region Code`, `Ten-Year Age Groups`, impute_r_deaths, Year) %>%
  full_join(region_combined) %>%
  mutate(r_deaths = case_when(
    !is.na(age_deaths) ~ age_deaths,
    T ~ impute_r_deaths
  )) 

new_region <- new_region %>%
  group_by(Year, `Census Region Code`) %>%
  mutate(r_sum = sum(r_deaths)) %>%
  rowwise() %>%
  mutate(r_pct = r_deaths / r_sum)

new_region <- new_region %>%
  select(`Census Region Code`, Year, `Ten-Year Age Groups`, r_pct)

# combine region and division

xwalk <- read_xlsx("../Data/Census 9 x Urbanization/crosswalk.xlsx")

division_combined <- division %>%
  left_join(division_age) %>%
  mutate(age_deaths = as.numeric(age_deaths))

new_division <- new_region %>%
  left_join(xwalk) %>%
  full_join(division_combined)

new_division <- new_division %>%
  group_by(`Census Division Code`, Year) %>%
  mutate(sum_d = sum(age_deaths, na.rm = T)) %>%
  rowwise() %>%
  mutate(missing_d = total_deaths - sum_d)

missing_division <- new_division %>%
  filter(is.na(age_deaths))

missing_division <- missing_division %>%
  group_by(`Census Division Code`, Year) %>%
  mutate(sum_pct = sum(r_pct)) %>%
  rowwise() %>%
  mutate(r = r_pct / sum_pct)

missing_division <- missing_division %>%
  mutate(impute_d_deaths = r * missing_d)

new_division <- new_division %>%
  left_join(missing_division) %>%
  mutate(new_d_deaths = case_when(
    !is.na(age_deaths) ~ age_deaths, 
    T ~ impute_d_deaths
  ))
# ---- Add Urbanization ----

urbanization_combined <- urbanization_age %>%
  left_join(urbanization)
urbanization_combined <- urbanization_combined %>%
  mutate(u_age_deaths = as.numeric(u_age_deaths)) %>%
  group_by(`Year`, `2013 Urbanization Code`, `Census Division Code`) %>%
  mutate(sum_u_age = sum(u_age_deaths, na.rm= T)) %>%
  mutate(missing_u_deaths = total_deaths - sum_u_age)%>%
  rename(total_u_deaths = total_deaths)


# Impute missing ages from division ages

missing_u <- urbanization_combined %>%
  filter(is.na(u_age_deaths)) %>%
  left_join(new_division)

missing_u <- missing_u %>%
  group_by(`Census Division Code`, `Year`, `2013 Urbanization Code`) %>%
  mutate(r = new_d_deaths / sum(new_d_deaths)) %>%
  rowwise() %>%
  mutate(impute_u_death = missing_u_deaths * r)

missing_u <- missing_u %>%
  select(impute_u_death, Year, `2013 Urbanization Code`, `Census Division Code`, `Ten-Year Age Groups`)

new_u <- urbanization_combined %>%
  full_join(missing_u)

new_u <- new_u %>%
  mutate(u_deaths = case_when(
    !is.na(u_age_deaths) ~ u_age_deaths,
    T ~ impute_u_death
  ))

# Use Ruhm inflation factors

inflation_factors <- read_csv("../data/CDC Microdata/inflation_factors.csv") %>%
  mutate(factor = 1 /(1-factor))

new_u <- new_u %>%
  mutate(b = case_when(
    `Ten-Year Age Groups` %in% c("< 1 year", "1-4 years","5-14 years","15-24 years") ~ "b1",
    `Ten-Year Age Groups` == "25-34 years" ~ "b2",
    `Ten-Year Age Groups` == "35-44 years" ~ "b3",
    `Ten-Year Age Groups` == "45-54 years" ~ "b4",
     T ~ "b5"))

new_u <- new_u %>%
  left_join(inflation_factors)

new_u <- new_u %>%
  mutate(inflated_d = factor * u_deaths) 

new_u <- new_u %>%
  mutate(urbanization = case_when(
    `2013 Urbanization Code` == 1  ~ 1,
    `2013 Urbanization Code` == 2 | `2013 Urbanization Code` == 3 | `2013 Urbanization Code` == 4 ~ 2,  
    T ~ 3
  ))



# Add age-dependent VSLs in millions

VSL <- read_csv("../data/VSL.csv")

VSL <- VSL %>%
  pivot_longer(cols = c(VSL_1,VSL_2,VSL_3,VSL_4,VSL_5))
VSL <- VSL %>%
  mutate(b = case_when(
    name == "VSL_1" ~ "b1",
    name == "VSL_2" ~ "b2",
    name == "VSL_3" ~ "b3",
    name == "VSL_4" ~ "b4",
    name == "VSL_5" ~ "b5",
  ))

new_u <- new_u %>%
  left_join(VSL)

new_u <- new_u %>%
  mutate(cost = value * inflated_d) %>%
  rowwise() %>%
  mutate(cost_pc = ((value * inflated_d) / Population)* 10^6)

new_u <- new_u %>%
  mutate(deaths_pc = (inflated_d / Population) * 1000)




costs <- new_u %>%
  group_by(`Census Division Code`, `Year`, `urbanization`) %>%
  summarize(cost = sum(cost),
            cost_pc = sum(cost_pc)) %>%
  mutate(urbanization = factor(urbanization, levels = c(1,2,3), labels = c("Large Central Metro", "Fringe/Small/Medium Metro", "Micropolitan/Rural")))

costs <- costs %>%
  left_join(division) %>% 
  mutate(`Census Division` = str_remove(`Census Division`, "(Division )([0-9])(: )")) %>%
  mutate(`Census Division` = factor(`Census Division`, levels = c("New England","Middle Atlantic", "East North Central",
                                                                  "West North Central","South Atlantic","East South Central",
                                                                  "West South Central", "Mountain","Pacific")))

deaths <- new_u %>%
  group_by(`Census Division Code`, Year, urbanization) %>%
  summarize(deaths = sum(inflated_d),
            deaths_pc = sum(deaths_pc))%>%
  mutate(urbanization = factor(urbanization, levels = c(1,2,3), labels = c("Large Central Metro", "Large Gringe/Small/Medium Metro", "Micropolitan/Rural")))%>%
  filter(!is.na(Year))


deaths <- deaths %>%
  left_join(division)%>% 
  mutate(`Census Division` = str_remove(`Census Division`, "(Division )([0-9])(: )")) %>%
  mutate(`Census Division` = factor(`Census Division`, levels = c("New England","Middle Atlantic", "East North Central",
                                                                  "West North Central","South Atlantic","East South Central",
                                                                  "West South Central", "Mountain","Pacific")))


write_excel_csv(costs, "../out/urbanicity_costs.csv")





