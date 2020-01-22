library(tidyverse)
library(choroplethrMaps)
library(ggplot2)
library(plotly)

county_2015 <- read_csv("../out/county_data_2015.csv")
state_2015 <- read_csv("../out/state_data_2015.csv")
county_2017 <- read_csv("../out/county_data.csv") %>%
  mutate(region = as.numeric(region))
state_2017 <- read_csv("../out/state_data.csv")

data(county.regions)


# x is 2015, y is 2017
comp <- county_2015 %>%

  left_join(county_2017, by = "region") %>%
  left_join(county.regions, by = "region") %>%

  mutate(death_cost_diff_pct = (death_cost.y - death_cost.x)  * 100 / death_cost.x) %>%
  mutate(death_cost_pc_diff_pct = (death_cost_pc.y - death_cost_pc.x)* 100 / death_cost_pc.x) %>%
  mutate(total_cost_diff_pct = (total_cost.y - total_cost.x)* 100 / total_cost.x) %>%
  mutate(total_cost_pc_diff_pct = (per_capita_total_cost.y - per_capita_total_cost.x)* 100 / per_capita_total_cost.x) %>%
  
  
  mutate(death_cost_diff_abs = (death_cost.y - death_cost.x) ) %>%
  mutate(death_cost_pc_diff_abs = (death_cost_pc.y - death_cost_pc.x) ) %>%
  mutate(total_cost_diff_abs = (total_cost.y - total_cost.x) ) %>%
  mutate(total_cost_pc_diff_abs = (per_capita_total_cost.y - per_capita_total_cost.x)) 


comp_s <- state_2015 %>%
  
  left_join(state_2017, by = "state.name") %>%

  mutate(death_cost_diff_pct = (death_cost.y - death_cost.x)  * 100 / death_cost.x) %>%
  mutate(death_cost_pc_diff_pct = (death_cost_pc.y - death_cost_pc.x)* 100 / death_cost_pc.x) %>%
  mutate(total_cost_diff_pct = (total_cost.y - total_cost.x)* 100 / total_cost.x) %>%
  mutate(total_cost_pc_diff_pct = (per_capita_total_cost.y - per_capita_total_cost.x)* 100 / per_capita_total_cost.x) %>%
  
  
  mutate(death_cost_diff_abs = (death_cost.y - death_cost.x) ) %>%
  mutate(death_cost_pc_diff_abs = (death_cost_pc.y - death_cost_pc.x) ) %>%
  mutate(total_cost_diff_abs = (total_cost.y - total_cost.x) ) %>%
  mutate(total_cost_pc_diff_abs = (per_capita_total_cost.y - per_capita_total_cost.x)) 

  
  
  

hist(comp$total_cost_pc_diff_pct)
summary(comp$total_cost_pc_diff_pct)


g_pc_total <- ggplot(comp) +
  geom_point(aes(x = per_capita_total_cost.x, y = per_capita_total_cost.y), alpha=.5) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~state.abb)+
  theme_classic()
g_pc_total


g_pc_dc <- ggplot(comp) +
  geom_point(aes(x = death_cost_pc.x, y = death_cost_pc.y), alpha=.5) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic()

g_pc_pc <- ggplot(comp) +
  geom_point(aes(x = productivity_cost_pc.x, y = productivity_cost_pc.y), alpha=.5) +
  geom_abline(slope = 1, intercept = 0) +
  facet_wrap(~state.abb)+
  theme_classic()
g_pc_pc


g_pc_cc <- ggplot(comp) +
  geom_point(aes(x = criminal_cost_pc.x, y = criminal_cost_pc.y, color = state.name.x), alpha=.5) +
  geom_abline(slope = 1, intercept = 0) +
  theme_classic()




line_cc <- seq.int(0, max(comp$criminal_cost_pc.x))
line_dc <- seq.int(0, max(comp$death_cost_pc.x))
line_pc <- seq.int(0, max(comp$productivity_cost_pc.x))
line_tc <- seq.int(0, max(comp$per_capita_total_cost.x))
line_hc <- seq.int(0, max(comp$health_cost_pc.x))

line_total_pct <- seq.int(0, max(comp$total_cost_pc_diff_pct, na.rm = T))

line_cc.s <- seq.int(0, max(comp_s$criminal_cost_pc.x))
line_pc.s <- seq.int(0, max(comp_s$productivity_cost_pc.x))
line_tc.s <- seq.int(0, max(comp_s$per_capita_total_cost.x))
line_hc.s <- seq.int(0, max(comp_s$health_cost_pc.x))

#county criminal cost
c_cc <- plot_ly() %>%
  add_trace(data= comp, x =~ criminal_cost_pc.x, y = ~ criminal_cost_pc.y, type = "scatter", color = ~state.name.x, text=~region) %>%
  add_trace(x =~ line_cc, y=~line_cc, type="scatter",mode="line")

#county death cost
c_dc <- plot_ly() %>%
  add_trace(data= comp, x =~ death_cost_pc.x, y = ~ death_cost_pc.y, type = "scatter", color = ~state.name.x, text=~region) %>%
  add_trace(x =~ line_dc, y=~line_dc, type="scatter",mode="line")

#county health cost
c_hc <- plot_ly() %>%
  add_trace(data= comp, x =~ health_cost_pc.x, y = ~ health_cost_pc.y, type = "scatter", color = ~state.name.x, text=~region) %>%
  add_trace(x =~ line_hc, y=~line_hc, type="scatter",mode="line")

#county productivity cost
c_pc <- plot_ly() %>%
  add_trace(data= comp, x =~ productivity_cost_pc.x, y = ~ productivity_cost_pc.y, type = "scatter", color = ~state.name.x, text=~region) %>%
  add_trace(x =~ line_pc, y=~line_pc, type="scatter",mode="line")

#county total cost
c_tc <- plot_ly() %>%
  add_trace(data= comp, x =~ per_capita_total_cost.x, y = ~ per_capita_total_cost.y, type = "scatter", color = ~state.name.x, text=~region) %>%
  add_trace(x =~ line_tc, y=~line_tc, type="scatter",mode="line")

c <- comp %>%
  filter(total_population.y > 5000)

c_tc.percent <- plot_ly() %>%
  add_trace(data= c, x =~ region, y = ~ total_cost_pc_diff_pct, type = "scatter", color = ~state.name.x, text=~region) 

c_tc.percent

s_cc <- plot_ly() %>%
  add_trace(data= comp_s, x =~ criminal_cost_pc.x, y = ~ criminal_cost_pc.y, type = "scatter", color = ~state.name, text=~state.name) %>%
  add_trace(x =~ line_cc.s, y=~line_cc.s, type="scatter",mode="line")

s_pc <- plot_ly() %>%
  add_trace(data= comp_s, x =~ productivity_cost_pc.x, y = ~ productivity_cost_pc.y, type = "scatter", color = ~state.name, text=~state.name) %>%
  add_trace(x =~ line_pc.s, y=~line_pc.s, type="scatter",mode="line")

s_tc <- plot_ly() %>%
  add_trace(data= comp_s, x =~ per_capita_total_cost.x, y = ~ per_capita_total_cost.y, type = "scatter", color = ~state.name, text=~state.name) %>%
  add_trace(x =~ line_tc.s, y=~line_tc.s, type="scatter",mode="line")

s_hc <- plot_ly() %>%
  add_trace(data= comp_s, x =~ health_cost_pc.x, y = ~ health_cost_pc.y, type = "scatter", color = ~state.name, text=~state.name) %>%
  add_trace(x =~ line_hc.s, y=~line_hc.s, type="scatter",mode="line")


s_hc

c_cc
c_dc
c_tc
c_pc
c_hc

# This imports counties made with the original code 



t. <- read_csv("C:/Users/burke.o'brien/Desktop/opioid_costs/out/subset2.csv")

t <- t %>%
  left_join(t., by = "region")

t <- t %>%
  select(order(colnames(t)))

