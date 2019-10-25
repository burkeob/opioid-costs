library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# set working directory to script folder
# this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(this.dir)

files <- list.files("../data/Updated Hospitalization")

files <- files[str_detect(files, ".csv")]

# try one out
d <- tibble(location = character(),
                discharges = character(),
                discharge_rate = character(),
                mean_costs = character(),
                total_costs = character(),
                per_capita_costs = character())

for (i in 1:length(files)) {
  d. <- read_csv(str_c("../data/Updated Hospitalization/", files[i]))
  names(d.)[1] <- 'X1' # assign first column name X1
  if (ncol(d.) == 11) {
    opioid_data <- d. %>%
      filter(str_detect(X2, "Opioid") | str_detect(X1, "2016")) %>%
      fill(X1) %>%
      filter(!is.na(X2))
    
    out <- tibble(location = opioid_data %>% pull(X1),
                  discharges = opioid_data %>% pull(X3),
                  discharge_rate = opioid_data %>% pull(X4),
                  mean_costs = opioid_data %>% pull(X8),
                  total_costs = opioid_data %>% pull(X9),
                  per_capita_costs = opioid_data %>% pull(X10))
    
  }  else{
    print("here")
  } 
  d <- d %>% bind_rows(out)
    
}

d <- d %>% separate(location, c("region_name", "state_name"), ",") %>%
  mutate(region_name = str_replace(region_name, "2014 ", ""),
         discharges = str_replace(discharges, "\\*", "") %>% as.numeric,
         discharge_rate = str_replace(discharge_rate, "\\*", "") %>% as.numeric,
         mean_costs = str_replace(mean_costs, "\\*", "") %>% as.numeric,
         total_costs = str_replace(total_costs, "\\*", "") %>% as.numeric,
         per_capita_costs = str_replace(per_capita_costs, "\\*", "") %>% as.numeric)

write_rds(d, "../data/hospitalization_costs.rds")


