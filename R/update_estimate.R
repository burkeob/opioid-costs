library(tidyverse)
# Guidance on updating VSL are found in this DOT report: 
# https://www.transportation.gov/sites/dot.gov/files/docs/2016%20Revised%20Value%20of%20a%20Statistical%20Life%20Guidance.pdf

# Because I don't trust how they updated VSL, or how they got the opioid deaths, I compute the cost using my data for 2015, then 
# again for 2017, and apply the percent change to the originally reported cost

# Original VSL Estimates are from Aldy and Viscusi (2008) and are in millions of 2000 dollars

b1. <- 3.74 #18-24
b2. <- 9.43 #25-34
b3. <- 9.66 #35-44
b4. <- 8.07 #45-54
b5. <- 3.43 #55-62

# Inflation data come from BLS CPI data series CUUR0000SA0, inflation from Jan 2000 - Dec 2017
Po <- 168.8 #Jan 2000
Pt. <- 236.525 # Dec 2015
Pt <- 246.524 # Dec 2017

# Earnings changes come from BLS median usual weekly earnings 
Io <- 585 #Q1 2000
It. <- 822 #Q4 2015
It <- 854 #Q4 2017

# This gets VSL in 2015 dollars 
# From 

b1_2015 <- b1. * (Pt. / Po) * (It. / Io)
b2_2015 <- b2. * (Pt. / Po) * (It. / Io)
b3_2015 <- b3. * (Pt. / Po) * (It. / Io)
b4_2015 <- b4. * (Pt. / Po) * (It. / Io)
b5_2015 <- b5. * (Pt. / Po) * (It. / Io)



# This gets the  VSL in 2017 dollars

b1_2017 <- b1. * (Pt / Po) * (It / Io)
b2_2017 <- b2. * (Pt / Po) * (It / Io)
b3_2017 <- b3. * (Pt / Po) * (It / Io)
b4_2017 <- b4. * (Pt / Po) * (It / Io)
b5_2017 <- b5. * (Pt / Po) * (It / Io)



# Import death data
deaths <- read_tsv("../data/Replicate CEA - MCD.txt")

d_2015 <- deaths %>%
  group_by(Year, `Ten-Year Age Groups`) %>%
  summarize(deaths = sum(Deaths)) %>%
  filter(Year == 2015) %>%
  filter(!is.na(`Ten-Year Age Groups`))

d_2015 <- d_2015 %>%
  mutate(deaths = deaths *1.24) %>% #CEA does this stupid multiplier
  mutate(cost = case_when(
    `Ten-Year Age Groups` %in% c("< 1 year", "1-4 years", "5-14 years", "15-24 years") ~ deaths * b1_2015,
    `Ten-Year Age Groups` == "25-34 years" ~ deaths * b2_2015,
    `Ten-Year Age Groups` == "35-44 years" ~ deaths * b3_2015,
    `Ten-Year Age Groups` == "45-54 years" ~ deaths * b4_2015,
    T ~ deaths * b5_2015 ))

cost_2015 <- d_2015 %>%
  pull(cost) %>%
  sum()


d_2017 <- deaths %>%
  group_by(Year, `Ten-Year Age Groups`) %>%
  summarize(deaths = sum(Deaths)) %>%
  filter(Year == 2017) %>%
  filter(!is.na(`Ten-Year Age Groups`))

d_2017 <- d_2017 %>%
  mutate(deaths = deaths *1.24) %>% #CEA does this stupid multiplier
  mutate(cost = case_when(
    `Ten-Year Age Groups` %in% c("< 1 year", "1-4 years", "5-14 years", "15-24 years") ~ deaths * b1_2017,
    `Ten-Year Age Groups` == "25-34 years" ~ deaths * b2_2017,
    `Ten-Year Age Groups` == "35-44 years" ~ deaths * b3_2017,
    `Ten-Year Age Groups` == "45-54 years" ~ deaths * b4_2017,
    T ~ deaths * b5_2015 ))

cost_2017 <- d_2017 %>%
  pull(cost) %>%
  sum()

# This figure is used in the excel sheet "CEA_Estimates" 
percent_change_c <- (cost_2017 - cost_2015) / cost_2015


