library(shiny); library(shinyjs); library(raster); library(rgdal); library(elevatr); library(sp); library(DT); library(leaflet)
  
load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

# Add global datasets and variables
map <- readRDS("shapes/gadm36_PER_1_sp.rds")
#map <- getData('GADM', country='PER', level=1)
#crs(map) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

wc <- brick("data/wc_average.tif")
names(wc) <- c("tmean","tmax","tmin","prec")

# Worldclim Rasters
#wc.bio <- getData("worldclim", var="bio", res=5)
#wc.tmin <- getData('worldclim', var='tmin', res=5)
#wc.tmax <- getData('worldclim', var='tmax', res=5)
#wc.prec <- getData('worldclim', var='prec', res=5)

# Create worldclim subsets for map
#wc.avg <- crop(wc.bio, map)
#wc.avg <- wc.avg[[c(1,5,6,12)]]
#names(raster) <- c("tmean","tmax", "tmin", "prec")

#setwd("C:/Shiny/climate_app_update_3519")
#writeRaster(wc.avg, filename = "wc_average.tif", format = "GTiff", overwrite = TRUE)

# These data layers are for the graph
#tmin <- crop(wc.tmin, map)
#tmax <- crop(wc.tmax, map)
#prec <- crop(wc.prec, map)

#raster <- stack(tmin, tmax, prec)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  div(
    id = "loading_page",
    h1("Loading...")
  ),
  hidden(
    div(
      id = "main_content",
      
      # Application title
      titlePanel("Rapid Inventory Climate Information"),
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput(inputId = "variableselected", 
                      label = "Choose a variable to display",
                      choices = c("Mean Temperature", "Maximum Temperature", "Minimum Temperature", "Precipitation"),
                      selected = "Mean Temperature"),
          
          #selectInput(inputId = "inventoryselected", label = "Select Inventory", choices = map$NAME_1),
          
          h6("This 'dashboard' displays Worldclim climate and SRTM elevation information from a user selected area and outputs a",
            strong("map "),
            "and monthly",
            strong("time series graph."),
          br(),
          br(),
           p("",
            strong("Temperature"),
              "is in *C"),
           p("",
            strong("Precipitation"),
            "is in mm")),
          br(),
          br(),
          
          p(img(src = "logo.png", width = "70px", height = "70px"))
          
        ),
        
        # Show a plots
        mainPanel(
          plotOutput(outputId = "map"),
          verbatimTextOutput('exp')
          #verbatimTextOutput('tmaxText'),
          #verbatimTextOutput('tminText')
        )
      )
    )
  )
)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Define server logic
server <- function(input, output) {
  
  load_data()

# Display the raster map
  output$map <- renderPlot(
    {
      var <- switch(input$variableselected, 
                    "Mean Temperature" = wc$tmean,
                    "Maximum Temperature" = wc$tmax,
                    "Minimum Temperature" = wc$tmin,
                    "Precipitation" = wc$prec)
      
      # Color bin
      pal <- palette(terrain.colors(10))
      
      plot(var, col=pal)
      plot(map, add=TRUE)
    }
  )
  
  output$exp <- renderText({
    
    var <- switch(input$variableselected, 
                  "Mean Temperature" = wc$tmean,
                  "Maximum Temperature" = wc$tmax,
                  "Minimum Temperature" = wc$tmin,
                  "Precipitation" = wc$prec)
    
    paste("From", as.character(maxValue(var)/10), "(maximum) to", as.character(minValue(var)/10), "(minimum)")
  })
  
# Display climate numbers 
  
  output$tmaxText <- renderText({
    var <- switch(input$variableselected, 
                  "Mean Temperature" = wc$tmean,
                  "Maximum Temperature" = wc$tmax,
                  "Minimum Temperature" = wc$tmin,
                  "Precipitation" = wc$prec)
    
    paste("Maximum:", as.character(maxValue(var)/10))
    
  })
  
  output$tminText <- renderText(
    {
    
      var <- switch(input$variableselected, 
                    "Mean Temperature" = wc$tmean,
                    "Maximum Temperature" = wc$tmax,
                    "Minimum Temperature" = wc$tmin,
                    "Precipitation" = wc$prec)
  
      paste("Minimum:", as.character(minValue(var)/10))
    }
  )
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run the application 
shinyApp(ui = ui, server = server)

