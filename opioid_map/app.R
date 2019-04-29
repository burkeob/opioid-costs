#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This interactive map plots state- and county-level estimates of the costs of the opioid crisis. 
library(leaflet)
library(shiny)
library(tigris)
require(dplyr)
library(maps)
library(pryr)

# need this to run cb= TRUE for tigris
options(tigris_use_cache = FALSE)

# county raw data
sdf = read.csv(file = "../out/state_data.csv")
cdf = read.csv(file = "../out/county_data.csv")

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

# shiny UI object with buttons to change types of costs
ui = fluidPage(
  leafletOutput("map", height = 900),
  actionButton("total", "Total"),
  actionButton("fatal", "Fatal"),
  actionButton("nonfatal", "Non-Fatal"),
  actionButton("health", "Health Care"),
  actionButton("productivity", "Productivity"),
  actionButton("criminal", "Criminal Justice")
)

# create lists to map descriptions and choropleth colors to variable names
desclist = list("Total Costs", "Fatal Costs", 
                "Non-Fatal Costs", "Health Care Costs", 
                "Productivity Costs", "Criminal Justice Costs")

colorlist = list("Greys","Reds", "Blues", "Greens", "Purples", "Oranges")

names(desclist) = c("per_capita_total_cost", "death_cost_pc", 
                    "per_capita_nf_cost", "health_cost_pc", 
                    "productivity_cost_pc", "criminal_cost_pc")

names(colorlist) = c("per_capita_total_cost", "death_cost_pc", 
                     "per_capita_nf_cost", "health_cost_pc", 
                     "productivity_cost_pc", "criminal_cost_pc")

# function to switch back to state-level leaflet map
statemap <- function(obj, name) {
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
    group = "states",
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
  ) %>%
    addLegend(
      pal = pal_st, 
      values = ~var, 
      opacity = 0.7, 
      title = desclist[[name]],
      position = "bottomright", 
      labFormat = labelFormat(prefix = "$", digits = 2), 
      layerId = "st_leg")
}

# function to get a single state's county map
countymap <- function(obj, state, name){
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
    addLegend(
      pal = pal_ct, 
      values = ~var, 
      opacity = 0.7, 
      title = desclist[[name]],
      position = "bottomright", 
      labFormat = labelFormat(prefix = "$", digits = 2), 
      layerId = "cty_leg")
}

# function to reset map when switching between opioid cost layers (fatal to non-fatal, etc.)
resetmap <- function(counter, curr_st, curr_name){
  if ((counter %% 2) == 0)  {
    leafletProxy("map", data = out_st) %>%
      clearShapes() %>%
      countymap(state = curr_st, name = curr_name) %>%
      removeControl("st_leg")
  } else {
    leafletProxy("map", data = out_st) %>%
      clearShapes() %>%
      statemap(name = curr_name) %>%
      removeControl("cty_leg")
  }
}

# layerid allows for identification of state during click event
layid = as.vector(out_st$STATEFP)


## SERVER
server <- shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(out_st) %>% addTiles() %>% 
      
      # state layer
      statemap(name = curr_name)
  })
  curr_name = "per_capita_total_cost"
  curr_st = NA
  counter = 1
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click
    counter <<- counter + 1
    curr_st <<- p$id
    # print(mem_used())
    if ((counter %% 2) == 0)  {
      leafletProxy("map", data = out_st) %>%
        clearShapes() %>%
        setView(lng=p$lng,lat=p$lat,zoom = 6) %>%
        countymap(state = curr_st, name = curr_name) %>%
        # hideGroup("states") %>%
        removeControl("st_leg")
    } else {
      leafletProxy("map", data = out_st) %>%
        clearShapes() %>%
        setView(lat=39.8283,lng=-98.5795,zoom = 4) %>%
        statemap(name = curr_name) %>%
        # showGroup("states") %>%
        removeControl("cty_leg")
    }
  })
  observeEvent(input$total, {
    curr_name <<- "per_capita_total_cost"
    resetmap(counter, curr_st, curr_name)
  })
  observeEvent(input$nonfatal, {
    curr_name <<- "per_capita_nf_cost"
    resetmap(counter, curr_st, curr_name)
  })
  observeEvent(input$fatal, {
    curr_name <<- "death_cost_pc"
    resetmap(counter, curr_st, curr_name)
  })
  observeEvent(input$health, {
    curr_name <<- "health_cost_pc"
    resetmap(counter, curr_st, curr_name)
  })
  observeEvent(input$productivity, {
    curr_name <<- "productivity_cost_pc"
    resetmap(counter, curr_st, curr_name)
  })
  observeEvent(input$criminal, {
    curr_name <<- "criminal_cost_pc"
    resetmap(counter, curr_st, curr_name)
  })
  
})

shinyApp(ui, server)
