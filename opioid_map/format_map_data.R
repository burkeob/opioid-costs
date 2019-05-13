library(leaflet)
library(shiny)
library(tigris)
require(dplyr)
library(maps)
library(pryr)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load raw data objects required for map


# need this to run cb= TRUE for tigris
options(tigris_use_cache = FALSE)

# county raw data
sdf = read.csv(file = "../../out/state_data.csv")
cdf = read.csv(file = "../../out/county_data.csv")

# Load relevant state- and county-level per-capita cost estimates
costs_st = sdf %>% 
  select(region, per_capita_total_cost, death_cost_pc, 
         per_capita_nf_cost, health_cost_pc, 
         criminal_cost_pc, productivity_cost_pc) %>%
  mutate(GEOID = region)

costs_cty = cdf %>% 
  select(region, per_capita_total_cost, death_cost_pc,
         per_capita_nf_cost, health_cost_pc,
         criminal_cost_pc, productivity_cost_pc) %>%
  mutate(GEOID = region)

# get state names for filtering out non-continental states as per paper
statelist = state.name %>% 
  subset(!(state.name %in% c("Alaska", "Hawaii"))) %>%
  unique

# create state-level spatial object and join with data
st = states(cb = TRUE, resolution = "20m")
cont_state = filter_state(st, statelist) 
cont_state$lower_name = tolower(cont_state$NAME)
out_st = geo_join(cont_state, costs_st, by_sp = 'lower_name', by_df = 'region')

## create leaflet map
opioid_map = leaflet(out_st) %>% addTiles()


## objects to create polygons for opioid_map
# create lists to map descriptions and choropleth colors to variable names
desclist = list("Total Costs", "Fatal Costs", 
                "Non-Fatal Costs", "Health Care Costs", 
                "Productivity Costs", "Criminal Justice Costs")

colorlist = list("Greys","Reds", "Blues", "Greens", "Purples", "Oranges")

vlist = c("per_capita_total_cost", "death_cost_pc", 
          "per_capita_nf_cost", "health_cost_pc", 
          "productivity_cost_pc", "criminal_cost_pc")

names(desclist) = vlist

names(colorlist) = vlist

# layerid allows for identification of state during click event
layid1 = as.vector(out_st$STATEFP) %>% paste("1")
layid2 = as.vector(out_st$STATEFP) %>% paste("2")
layid3 = as.vector(out_st$STATEFP) %>% paste("3")
layid4 = as.vector(out_st$STATEFP) %>% paste("4")
layid5 = as.vector(out_st$STATEFP) %>% paste("5")
layid6 = as.vector(out_st$STATEFP) %>% paste("6")

laylist = list(layid1, layid2,
               layid3, layid4,
               layid5, layid6)

names(laylist) = vlist



## functions to create polygons for opioid_map
# state-level map
create_statemap <- function(obj, name, layid) {
  identifier = paste("allstates", name)
  print(identifier)
  
  # horrific way to allow for name extraction with string. 
  out_st$var = as.numeric(
    unlist(
      attr(out_st, "data")[name]))
  
  # create quintile cutoffs
  q_st <- out_st$var %>% 
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE) %>%
    unique()
  # color palette using bins 
  pal_st <- colorBin(colorlist[[name]], domain = out_st$var, bins = q_st)
  
  # Label options-- description for hovering cursor 
  labels <- sprintf(
    "<strong>%s<br/>%s</strong><br/>$%g per capita",
    out_st$NAME, desclist[[name]], round(out_st$var, 2)
  ) %>% lapply(htmltools::HTML)
  
  # create choropleth polygon and legend
  addPolygons(
    obj,
    data = out_st,
    layerId = layid,
    group = identifier,
    weight = 1, 
    fillColor = ~pal_st(var),
    opacity = 1,
    color = 'white',
    dashArray = '3',
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )  %>%
    # addLegend(
    #   group = identifier,
    #   pal = pal_st,
    #   values = ~var,
    #   opacity = 0.7,
    #   title = desclist[[name]],
    #   position = "bottomright",
    #   labFormat = labelFormat(prefix = "$", digits = 2)) %>%
    hideGroup(identifier)
}


# county-level map
create_countymap <- function(obj, state, name){
  identifier = paste(state, name)
  print(identifier)
  ctys = counties(cb = TRUE, resolution = "20m", state = state)
  sbset = costs_cty %>% 
    mutate(strid = sprintf("%05d",GEOID)) %>%
    filter(strid %>% substr(1, 2) == state)
  ctys_spdf = geo_join(ctys, sbset, by_sp = 'GEOID', by_df = 'strid')
  
  # function to allow for extracting variable with string
  ctys_spdf$var = as.numeric(
    unlist(
      attr(ctys_spdf, "data")[name]))
  
  # value quintiles
  q_ct <- ctys_spdf$var %>% 
    quantile(probs = seq(0, 1, 0.2), na.rm = TRUE) %>%
    unique()
  pal_ct <- colorBin(colorlist[[name]], domain = ctys_spdf$var, bins = q_ct)
  
  # Labels
  labels_cty <- sprintf(
    "<strong>%s<br/>%s</strong><br/>$%g per capita",
    ctys_spdf$NAME, desclist[[name]], round(ctys_spdf$var, 2)
  ) %>% lapply(htmltools::HTML)
  
  addPolygons(
    obj,
    group = identifier, 
    data = ctys_spdf,
    weight = 1,
    fillColor = ~pal_ct(var), #NEED TO CHANGE THIS
    opacity = 1,
    color = 'white',
    dashArray = '3',
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels_cty,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
    # addLegend(
    #   group = identifier,
    #   pal = pal_ct,
    #   values = ~var,
    #   opacity = 0.7,
    #   title = desclist[[name]],
    #   position = "bottomright",
    #   labFormat = labelFormat(prefix = "$", digits = 2)) %>%
  hideGroup(identifier)
}

stateabblist = state.abb %>% 
  subset(!(state.abb %in% c("AK", "HI"))) %>%
  unique


for (n in vlist){
  #add state layer
  opioid_map = opioid_map %>%
    create_statemap(n, laylist[[n]])
  
  for (i in stateabblist){
    stfips = sprintf("%02d", state.fips %>%
      filter(abb == i) %>%
      pull(fips))
    opioid_map = opioid_map %>%
      create_countymap(stfips, n)
  }
}

save(opioid_map, out_st, file = "map")
