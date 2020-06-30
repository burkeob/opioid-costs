library(tidyverse)
library(ggthemes)
library(extrafont)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(mosaic)
library(extrafont)
library(gridExtra)
library(Cairo)

# ---- Figure 1: Costs over time ---- 
costs <- read_csv("../out/total_costs.csv")


g <- ggplot(data = costs) +

  theme_few() + 
  labs(y = "Cost Per Capita (2018 Dollars)", x = "Year", title = "Opioid-Related Per Capita Costs") + 
  
  scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016 ,2017, 2018, 2020),
                     limits = c(2009, 2020), expand = c(0,0),
                     labels = c("2009","2010", "2011", "2012","2013","2014","2015","2016","2017","2018", "")) +
  scale_y_continuous(breaks = c(0,500,1000,1500,2000), expand = c(0,0), labels = c("0","500","1000","1500","2000"), limits = c(0,2200)) + 
  theme(text = element_text(family = "Times New Roman"), legend.position = "none")+
  theme(plot.title = element_blank())+
  theme(axis.text.x = element_text(size = 11)) +
  theme(axis.text.y = element_text(size = 11)) +
  geom_line(aes(x = Year, y = total_cost_pc), color = "grey4") + 
  geom_line(aes(x = Year, y = cost_pc), color = "grey4") +
  geom_line(aes(x = Year, y = non_fatal_pc), color = "grey4") +
  annotate(geom = "text", x = 2018.45, y = 2030, label = "Total", family = "Times New Roman", size = 4) +
  annotate(geom = "text", x = 2018.68, y = 1835, label = "Mortality", size = 4, family = "Times New Roman") + 
  annotate(geom = "text", x = 2019, y = 215, label = "Non-Mortality", size = 4, family = "Times New Roman")


cairo_pdf("../Graphs/trend.pdf", height = 4, width = 6.6) 
g
dev.off()
 # ---- Figure 2: Urbanicity ----
formatter <- function(...){
  function(x) format(round(x), ...)
}

bfun <- function(limits){
  return(c(min(limits), ceiling((max(limits) - min(limits)) / 2),max(limits)))
}

costs <- read_csv("../out/urbanicity_costs.csv")
costs <- costs %>%
  rowwise() %>%
  mutate(urbanization = ifelse(urbanization == "Fringe/Small/Medium Metro", "Medium/Small/Fringe Metro", urbanization)) %>%
  ungroup()

costs$`Census Division` <- factor(costs$`Census Division`, levels = c("New England", "Middle Atlantic",
                                                                      "South Atlantic", "East North Central",
                                                                      "East South Central", "West North Central",
                                                                      "West South Central", "Mountain", "Pacific"))


costs$urbanization <- factor(costs$urbanization, levels = c("Large Central Metro", "Medium/Small/Fringe Metro", "Micropolitan/Rural"))

costs <- costs %>%
  mutate(cost_pc = cost_pc / 1000)

cost <- ggplot(data = costs) +
  theme_few() + 
  theme(text = element_text(family = "Times New Roman"))+
  theme(plot.title = element_blank())+
  
  geom_line(aes(x = Year, y = cost_pc, group = urbanization, linetype = urbanization)) +
  facet_wrap(facets = ~`Census Division`, scales = "fixed") +
  scale_x_continuous(breaks = c(2009, 2011, 2013, 2015, 2017), limits = c(2009, 2018), expand = c(0,0)) + 
  scale_linetype_manual(values = c("twodash", "solid", "dotted")) +
  labs(y = "Cost Per Capita (Thousands of 2018 Dollars)", title = "Cost by Census Division") +
  theme(axis.line=element_line())+ 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.justification = "left",
        axis.title.x = element_blank(),
        legend.margin = margin(0,0,0,0),
        legend.box.margin = margin(0,-10, 0,-10)) 


cairo_pdf("../Graphs/urbanicity.pdf", height = 5, width = 8)
cost
dev.off()

# ---- Maps -----
data(county.regions)
data(state.regions)

map2015 <- read_csv("../out/county_data_2015.csv")
map2016 <- read_csv("../out/county_data_2016.csv")
map2017 <- read_csv("../out/county_data_2017.csv")
map2018 <- read_csv("../out/county_data_2018.csv")


diff_cuts <- c(-Inf, -500, 500, Inf)

diff <- map2016 %>%
  select(region, per_capita_total_cost, state.name) %>%
  rename(c2016 = per_capita_total_cost,
         state = state.name) %>%
  left_join(map2018 %>% select(region, per_capita_total_cost))


diff <- diff %>%
  mutate(d = per_capita_total_cost - c2016) %>%
  mutate(value = cut(d, breaks = diff_cuts, labels = F))

cuts <- c(0, 1500, 3000, 4500, Inf)
map2015 <- map2015 %>%
  mutate(value = cut(per_capita_total_cost, breaks = cuts, labels = F)) %>%
  mutate(region = as.numeric(region)) %>%
  select(region, value)
map2016 <- map2016 %>%
  mutate(value = cut(per_capita_total_cost, breaks = cuts, labels = F)) %>%
  mutate(region = as.numeric(region)) %>%
  select(region, value)
map2017 <- map2017 %>%
  mutate(value = cut(per_capita_total_cost, breaks = cuts, labels = F)) %>%
  mutate(region = as.numeric(region)) %>%
  select(region, value)
map2018 <- map2018 %>%
  mutate(value = cut(per_capita_total_cost, breaks = cuts, labels = F)) %>%
  mutate(region = as.numeric(region)) %>%
  select(region, value)



diff <- diff %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")

diff <- diff %>%
  filter(!is.na(d))

# ---- Making CountyChoropleths ----
c_2015 <- CountyChoropleth$new(map2015)
c_2015$set_zoom(state.regions$region[-c(1, 12, 2, 5, 4, 6, 8, 16, 20, 29, 30, 31, 32, 40, 41, 46, 48, 49, 53, 56)])

c_2016 <- CountyChoropleth$new(map2016)
c_2016$set_zoom(state.regions$region[-c(1, 12, 2, 5, 4, 6, 8, 16, 20, 29, 30, 31, 32, 40, 41, 46, 48, 49, 53, 56)])

c_2017 <- CountyChoropleth$new(map2017)
c_2017$set_zoom(state.regions$region[-c(1, 12, 2, 5, 4, 6, 8, 16, 20, 29, 30, 31, 32, 40, 41, 46, 48, 49, 53, 56)])

c_2018 <- CountyChoropleth$new(map2018)
c_2018$set_zoom(state.regions$region[-c(1, 12, 2, 5, 4, 6, 8, 16, 20, 29, 30, 31, 32, 40, 41, 46, 48, 49, 53, 56)])


diff_map <- CountyChoropleth$new(diff)
diff_map$set_zoom(state.regions$region[-c(1, 12, 2, 5, 4, 6, 8, 16, 20, 29, 30, 31, 32, 40, 41, 46, 48, 49, 53, 56)])


# ---- Getting the DF ---- 
t_2015 <- c_2015$map.df
t_2015 <- t_2015 %>%
  left_join(map2015)

t_2015 <- t_2015 %>%
  mutate(value = ifelse(!is.na(value),value, -1))
t_2015$value = factor(t_2015$value)

t_2015 <- t_2015 %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")



t_2016 <- c_2016$map.df
t_2016 <- t_2016 %>%
  left_join(map2016)

t_2016 <- t_2016 %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")



t_2016 <- t_2016 %>%
  mutate(value = ifelse(!is.na(value),value, -1))
t_2016$value = factor(t_2016$value)

t_2017 <- c_2017$map.df
t_2017 <- t_2017 %>%
  left_join(map2017)

t_2017 <- t_2017 %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")



t_2017 <- t_2017 %>%
  mutate(value = ifelse(!is.na(value),value, -1))
t_2017$value = factor(t_2017$value)

t_2018 <- c_2018$map.df
t_2018 <- t_2018 %>%
  left_join(map2018)

t_2018 <- t_2018 %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")


t_2018 <- t_2018 %>%
  mutate(value = ifelse(!is.na(value),value, -1))
t_2018$value = factor(t_2018$value)

t_diff <- diff_map$map.df
t_diff <- t_diff %>%
  left_join(diff)

t_diff <- t_diff %>%
  filter(state != "alaska" & state != "hawaii" & state != "arizona" & state != "california" & state !=
           "colorado" & state != "idaho" & state != "kansas" & state != "montant" & state != "nebraska" & state !=
           "nevada" & state != "new mexico" & state != "north dakota" & state != "oklahoma" & state != "oregon" & state != "south dakota" & state !=
           "texas" & state != "utah" & state != "washington" & state != "wyoming" & state != "montana"& state != "minnesota"
         & state != "iowa" & state != "missouri" & state != "louisiana" & state != "arkansas")


t_diff <- t_diff %>%
  mutate(value = ifelse(!is.na(value),value, -1))
t_diff$value = factor(t_diff$value)


# Plotting


m2015 <- ggplot()+
  geom_polygon(data = t_2015, aes(fill = value, x = long, y = lat, group = group)) + 
  #scale_alpha_manual(values = c(.6, .05, .2, .4, 1))+
  theme_void() + 
  scale_fill_manual(values = c("Light Grey", "#F9F7F7", "#EDDDDC", "#D89595", "#9D1915"), name = "Cost Per Capita (in 2018 dollars)", 
                    labels = c("$0-$1500", "$1500-$3000", "$3000-$4500", ">$4500"), breaks = c(1,2,3,4)) +
  labs(title = "2015") + 
  theme(plot.title = element_text(family = "Times New Roman", size = 16,
                                  hjust = .5,  margin = margin(t = 10, r = 0, b =  0, l = 0)),
        legend.position = "bottom",
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman")) +
  coord_map()

cairo_pdf("../Graphs/map_2015.pdf")
m2015
dev.off()


m2016 <- ggplot()+
  geom_polygon(data = t_2016, aes(fill = value, x = long, y = lat, group = group)) + 
  theme_void() + 
  scale_fill_manual(values = c("Light Grey", "#F9F7F7", "#EDDDDC", "#D89595", "#9D1915"), name = "Cost PC", labels = c("NA", "$0-$1500", "$1500-$3000", "$3000-$4500", "$>4500")) +
  labs(title = "2016") + 
  theme(plot.title = element_text(family = "Times New Roman", size = 16,
                                  hjust = .5,  margin = margin(t = 10, r = 0, b =  0, l = 0)),
        legend.position = "none")+
  coord_map()

cairo_pdf("../Graphs/map_2016.pdf")
m2016
dev.off()


m2017 <- ggplot()+
  geom_polygon(data = t_2017, aes(fill = value, x = long, y = lat, group = group)) + 
  theme_void() + 
  scale_fill_manual(values = c("Light Grey", "#F9F7F7", "#EDDDDC", "#D89595", "#9D1915"), name = "Cost PC", labels = c("NA", "$0-$1500", "$1500-$3000", "$3000-$4500", "$>4500")) +
  labs(title = "2017") + 
  theme(plot.title = element_text(family = "Times New Roman", size = 16,
                                  hjust = .5,  margin = margin(t = 10, r = 0, b =  0, l = 0)),
        legend.position = "none")+
  coord_map()

cairo_pdf("../Graphs/map_2017.pdf")
m2017
dev.off()


m2018 <- ggplot()+
  geom_polygon(data = t_2018, aes(fill = value, x = long, y = lat, group = group)) + 
  theme_void() + 
  scale_fill_manual(values = c("Light Grey", "#F9F7F7", "#EDDDDC", "#D89595", "#9D1915"), name = "Cost PC", labels = c("NA", "$0-$1500", "$1500-$3000", "$3000-$4500", "$>4500")) +
  labs(title = "2018") + 
  theme(plot.title = element_text(family = "Times New Roman", size = 16,
                                  hjust = .5,  margin = margin(t = 10, r = 0, b =  0, l = 0)),
        legend.position = "none")+
  coord_map()

cairo_pdf("../Graphs/map_2018.pdf")
m2018
dev.off()



diff_plot <- ggplot()+
  geom_polygon(data = t_diff, aes(fill = value, x = long, y = lat, group = group)) + 
  theme_void() + 
  scale_fill_manual(values = c("Dark Grey", "Dark Blue", "Light Gray", "#D89595"), name = element_blank(), 
                    labels = c("Decrease of $500+", "Change of Less than $500", "Increase of $500+"), breaks = c(1,2,3)) +
  labs(title = "Change in Cost 2016-2018") + 
  theme(plot.title = element_text(family = "Times New Roman", size = 16,
                                  hjust = .5,  margin = margin(t = 10, r = 0, b =  0, l = 0)),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"),
        legend.position = "bottom")+
  coord_map()


cairo_pdf("../Graphs/diff.pdf", family = "Times")
diff_plot
dev.off()

