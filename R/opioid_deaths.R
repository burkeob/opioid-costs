library(tidyverse)
library(tidycensus)
library(lubridate)
library(readxl)

national_deaths_by_age <- read_tsv("../data/WONDER Queries/Opioid_National_Age_Totals_18.txt") %>%
  filter(is.na(Notes))

national_deaths_by_age <- national_deaths_by_age %>%
  mutate(b = case_when(
    `Ten-Year Age Groups Code` %in% c("1","1-4","5-14","15-24") ~ 1,
    `Ten-Year Age Groups Code` == "25-34" ~ 2,
    `Ten-Year Age Groups Code` == "35-44" ~ 3,
    `Ten-Year Age Groups Code` == "45-54" ~ 4,
    T ~ 5
  ))

national_deaths_by_age <- national_deaths_by_age %>%
  group_by(Year, b) %>%
  summarize(Deaths = sum(Deaths))

inflation_factors <- read_csv("../data/CDC Microdata/inflation_factors.csv") %>%
  mutate(b = str_extract(b, "[0-9]") %>% as.numeric()) %>%
  mutate(factor = 1/(1-factor))

national_deaths_by_age <- national_deaths_by_age %>%
  left_join(inflation_factors) %>%
  mutate(Deaths = Deaths * factor)

US_population <- read_excel("../data/Population.xls", skip = 10)
names(US_population) <- c("Year", "Pop")
US_population <- US_population %>%
  mutate(Year = year(Year))


# ---- Compute Cost ----
# This section updates the VSL estimates and applies their cost to the 
# different age bins. Costs are in 2017 dollars.

# Original VSL numbers from Aldy and Viscusi (2008) and are in millions of 2000 dollars
# https://www.transportation.gov/sites/dot.gov/files/docs/2016%20Revised%20Value%20of%20a%20Statistical%20Life%20Guidance.pdf

b1 <- 3.74  #18-24
b2 <- 9.43  #25-34
b3 <- 9.66  #35-44
b4 <- 8.07  #45-54
b5 <- 3.43  #55-62

Po <- 69.10664 #Dec 2000 price, indexed to Q42018 = 100 
Io <- 95.79832  #Q4 2000 income, indexed to Q42018 = 100 
Pt <- 100 #Q42018 prices indexed to 100

inflation <- read_csv("../data/CPIAUCSL.csv")%>%
  mutate(month = month(DATE)) %>%
  mutate(year = year(DATE)) %>%
  filter(month == 10 & year %in% c(1999:2018)) %>%
  select(year, CPIAUCSL_NBD20181201) %>%
  rename("CPIAUCSL" = CPIAUCSL_NBD20181201)

# Earnings changes come from BLS median real usual weekly earnings, not seasonally adjusted
# https://fred.stlouisfed.org/series/LEU0252881600Q

earnings <- read_csv("../data/earnings.csv", col_names = c("DATE", "Qtr4"), col_types = c("D", "d")) %>%
  mutate(Qtr4 = as.numeric(Qtr4)) %>%
  mutate(month = month(DATE), year = year(DATE)) %>%
  filter(month == 10 & year %in% c(1999:2018)) %>%
  select(year, Qtr4)


earnings <- earnings %>%
  left_join(inflation)

# This puts all VSL numbers in 2017 dollars
VSL <- earnings %>%
  filter(!is.na(CPIAUCSL)) %>%
  mutate(VSL_1 = b1 * (CPIAUCSL / Po) * (Qtr4 / Io) * (1+(Pt - CPIAUCSL) / Pt),
         VSL_2 = b2 * (CPIAUCSL / Po) * (Qtr4 / Io) * (1+(Pt - CPIAUCSL) / Pt),
         VSL_3 = b3 * (CPIAUCSL / Po) * (Qtr4 / Io) * (1+(Pt - CPIAUCSL) / Pt),
         VSL_4 = b4 * (CPIAUCSL / Po) * (Qtr4 / Io) * (1+(Pt - CPIAUCSL) / Pt),
         VSL_5 = b5 * (CPIAUCSL / Po) * (Qtr4 / Io) * (1+(Pt - CPIAUCSL) / Pt)) %>%
  rename(Year = "year")


write_excel_csv(VSL, "../data/VSL.csv")

national_deaths_by_age <- national_deaths_by_age %>%
  left_join(VSL) %>%
  mutate(cost = case_when(
    b ==1 ~ Deaths * VSL_1,
    b ==2 ~ Deaths * VSL_2,
    b ==3 ~ Deaths * VSL_3,
    b ==4 ~ Deaths * VSL_4,
    T ~ Deaths * VSL_5
  ))

national_deaths <- national_deaths_by_age %>%
  group_by(Year) %>%
  summarize(Deaths = sum(Deaths))
national_deaths_bins <- national_deaths_by_age %>%
  group_by(Year,b) %>%
  summarize(Deaths = sum(Deaths))


national_mortality_costs_by_year <- national_deaths_by_age %>%
  group_by(Year) %>%
  summarize(cost = sum(cost)) %>%
  left_join(US_population) %>%
  mutate(cost_pc = (cost / Pop)*1000000)

national_mortality_costs_by_year_age <- national_deaths_by_age %>%
  group_by(Year, b) %>%
  summarize(cost = sum(cost))%>%
  left_join(US_population) %>%
  mutate(cost_pc = (cost / Pop)*1000000)

write_excel_csv(national_mortality_costs_by_year, "../data/national_mortality_costs_by_year.csv")
