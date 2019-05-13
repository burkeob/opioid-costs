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

load("map")

# need this to run cb= TRUE for tigris
options(tigris_use_cache = FALSE)


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

showmap <- function(counter, curr_st, curr_name){
  map_name = paste(curr_st, curr_name)
  if ((counter %% 2) == 0)  {
    print(map_name)
    leafletProxy("map", data = out_st) %>%
      showGroup(map_name)
  } else {
    print(map_name)
    leafletProxy("map", data = out_st) %>%
      showGroup(map_name)
    
  }
}

hidelayers <- function(curr_st, curr_name){
  map_name = paste(curr_st, curr_name)
  leafletProxy("map", data = out_st) %>%
    hideGroup(map_name)
}

# layerid allows for identification of state during click event
layid = as.vector(out_st$STATEFP)


## SERVER
server <- shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
  opioid_map
  })
  curr_name = "per_capita_total_cost"
  curr_st = "allstates"
  counter = 1
  showmap(counter = 1, curr_st = curr_st, curr_name = "per_capita_total_cost")
  observeEvent(input$map_shape_click, {
    p <- input$map_shape_click
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    counter <<- counter + 1
    # print(mem_used())
    if ((counter %% 2) == 0)  {
      curr_st <<- p$id %>% substr(1, 2)
      showmap(counter = counter,
              curr_st = curr_st,
              curr_name = curr_name) %>%
      setView(lng=p$lng,lat=p$lat,zoom = 6)
    } else {
      curr_st <<- "allstates"
      showmap(counter = counter,
              curr_st = curr_st,
              curr_name = curr_name) %>%
        setView(lat=39.8283,lng=-98.5795,zoom = 4) 

    }
  })
  observeEvent(input$total, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "per_capita_total_cost"
    showmap(counter, curr_st, curr_name)
  })
  observeEvent(input$nonfatal, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "per_capita_nf_cost"
    showmap(counter, curr_st, curr_name)
  })
  observeEvent(input$fatal, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "death_cost_pc"
    showmap(counter, curr_st, curr_name)
  })
  observeEvent(input$health, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "health_cost_pc"
    showmap(counter, curr_st, curr_name)
  })
  observeEvent(input$productivity, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "productivity_cost_pc"
    showmap(counter, curr_st, curr_name)
  })
  observeEvent(input$criminal, {
    hidelayers(curr_st = curr_st, curr_name = curr_name)
    curr_name <<- "criminal_cost_pc"
    showmap(counter, curr_st, curr_name)
  })
  
})

shinyApp(ui, server)


