library(tidyverse)
library(lubridate)

inflators <- read_csv("../data/Non-Fatal/inflators.csv")


# ---- Pain Killer/Heroin Abuse or Dependence ----
# Data were pulled from the SAMHDA surveys

users <- tibble()

for(i in c(2007:2018)){
  using <- read_csv(paste0("../data/Non-Fatal/NSDUH/",i,".csv"))
  
  using <- using %>%
    rename("col1" = 1, "col2" = 2)
  
  using <- using %>%
    filter(col1 == "1 - Yes" | col2 == "1 - Yes")
  
  using <- using %>%
    filter(col1 != "Overall") %>%
    filter(col2 != "Overall")
  
  c <- sum(using %>% pull(`Weighted Count`))
  
  users <- users %>%
    bind_rows(tibble(Year = i, users = c))
}

# ---- Add inflation to inflators ----
inflation <- read_csv("../data/CPIAUCSL.csv") %>%
  mutate(Year = year(DATE)) %>%
  mutate(Month = month(DATE)) %>%
  filter(Month == 12) %>%
  filter(Year > 2006 & Year < 2019)

v2018 <- inflation %>%
  filter(Year == 2018) %>%
  pull(CPIAUCSL_NBD20181201)

inflation <- inflation %>%
  mutate(i  = CPIAUCSL_NBD20181201 / v2018) %>%
  select(Year, i)

inflators <- inflators %>%
  left_join(inflation)


# ---- Per-user costs ---- 
# These are taken directly from Florence et. al. (2016)

medical <- 28895 / 1.935
criminal_justice <- 7654 / 1.935
productivity <- 20441 / 1.935

# Put costs in millions of 2018 dollars
v2013 <- inflation %>%
  filter(Year == 2013) %>%
  pull(i)

medical <- medical / v2013
criminal_justice <- criminal_justice / v2013
productivity <- productivity / v2013

# Find the per-users costs in each year, in 2017 dollars
per_user_costs <- tibble(
  Year = inflators %>% pull(Year),
  medical = inflators %>% pull(Medical) * medical,
  criminal_justice = inflators %>% pull(Incarceration)* criminal_justice,
  productivity = inflators %>% pull(Productivity) * productivity
)


per_user_costs <- per_user_costs %>%
  mutate(total = medical + criminal_justice + productivity)




# Merge with number of users 

users <- users %>%
  left_join(per_user_costs)

users <- users %>%
  mutate(cost = total * users)

write_excel_csv(users, "../data/Non-Fatal/nonfatal.csv")

