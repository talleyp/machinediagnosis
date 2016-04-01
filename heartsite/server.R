library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
allzips <- indicounty
zipdata <- indicounty
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$hddall),]

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -86, lat = 40, zoom = 8)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, indicounty$hddall, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$hddall,
      breaks = centileBreaks,
      main = "Heart Disease death in Indiana",
      xlab = "Death rate",
      xlim = range(allzips$hddall),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    print(xyplot(hddall ~ medianincome, data = zipsInBounds(), xlim = range(allzips$medianincome, na.rm=T), 
                 ylim = range(allzips$hddall, na.rm = T)))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
      
      # Selects color 
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
      # Selects size
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~county,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(county, lat, lng) {
    selectedZip <- allzips[allzips$latitude == lat & allzips$longitude== lng,]
    content <- as.character(tagList(
      tags$h4("Heart Disease Death:", as.integer(selectedZip$hddall)),
      tags$strong(HTML(sprintf("%s, %s",
        selectedZip$county, "IN"
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$medianincome * 1000)), tags$br(),
      sprintf("Adults without High School degree: %s%%", as.integer(selectedZip$nohsd25)), tags$br(),
      sprintf("Adult population: %s", selectedZip$totalpop)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = county)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Machine Diagnosis ###########################################

  observe({
      predFrame$age <- input$age
      predFrame$sex <- input$sex 
      predFrame$cp <- input$cp
      predFrame$testbps <- input$testbps 
      predFrame$ chol <- input$ chol
      predFrame$fbs <- input$fbs
      predFrame$restecg <- input$restecg
      predFrame$thalach <- input$thalach
      predFrame$exang <- input$exang
      predFrame$oldpeak <- input$oldpeak
      predFrame$slope <- input$slope
      predFrame$ca  <- input$ca 
      predFrame$thal <- input$thal
      
      numpredict <- predict(rfMod, predFrame)
      if(numpredict == 1){
        output$prediction <- renderText({
                  "There is a large reason to suspect the patient has heart disease"
        })
      }
      else{
        output$prediction <- renderText({
          "Based on the input provided, there is little reason to believe the patient has heart disease"
        })
      }
      
      
  })
  
})
