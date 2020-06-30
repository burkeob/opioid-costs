library(tidyverse)

mortality_costs <- read_csv("../data/national_mortality_costs_by_year.csv") %>%
  rename(mortality_cost = cost)
non_fatal_costs <- read_csv("../data/Non-Fatal/nonfatal.csv")

costs <- mortality_costs %>%
  left_join(non_fatal_costs)

costs <- costs %>%
  mutate(non_fatal_pc = cost / Pop) 

costs <- costs %>%
  mutate(mortality_cost = mortality_cost * 1000000) %>%
  mutate(total_cost = mortality_cost + cost) %>%
  mutate(total_cost_pc = total_cost / Pop)

costs <- costs %>%
  mutate(medical_cost = medical * users) %>% 
  mutate(criminal_cost = criminal_justice * users) %>%
  mutate(productivity_cost = productivity * users)


write_excel_csv(costs, "../data/total_costs.csv")