library(tidyverse)
library(readxl)

# ---- Health Cost inflator ----

# This section reads in the Medical Care Services CPI from the BLS
# And creates an index to inflate the per-user costs

medical_cpi <- read_csv("../data/Non-Fatal/Medical Care Services CPI 2018.csv") %>%
  filter(Period == "M12") %>%
  select(Year, Value)

# Original Costs are in 2018 dollars
v2018 <- medical_cpi %>%
  filter(Year == 2018) %>%
  pull(Value)

medical_cpi <- medical_cpi %>%
  mutate(Medical = Value / v2018) %>%
  select(Year, Medical)


# ---- Productivity Cost ---- 

p <- read_excel("../data/Non-Fatal/nonfarm_business-annual-series.xlsx", sheet = "NFBUS, All persons (Index)", skip = 6) %>%
  select(Year, "Labor productivity")

p <- p %>%
  filter(Year > 2006) %>%
  filter(Year < 2019)

v2018 <- p %>%
  filter(Year == 2018) %>%
  pull(`Labor productivity`)

p <- p %>%
  mutate(Productivity = `Labor productivity` / v2018) %>%
  select(Year, Productivity)

# ---- Average Cost of Incarceration ----

i <- read_excel("../data/Non-Fatal/average cost of incarceration.xlsx")

v2018 <- i %>%
  filter(FY == 2018) %>%
  pull(Cost)

i <- i %>%
  mutate(Incarceration = Cost / v2018) %>%
  mutate(Year = FY) %>%
  select(Incarceration, Year)

# Merge into single inflator set

inflators <- i %>%
  left_join(p) %>%
  left_join(medical_cpi)


# Write inflators to be used later in calculating total cost - 
# They are still in year t costs.

write_excel_csv(inflators, "../data/Non-Fatal/inflators.csv")
