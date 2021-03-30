server <- function(input, output, session) {
  output$base_map <-renderLeaflet(
    leaflet(options = leafletOptions(attributionControl = FALSE,
                                     zoomControl = FALSE)) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    setView(lng = x_centroid, lat = y_centroid, zoom = 14) %>%
      addLayersControl(overlayGroups =   'biomass')
  )
  
  toListen <- reactive({
    list(input$date_select,input$summary_select)
  })
  
  observeEvent(toListen(),{
    focal_day <- long_series %>% filter(day == input$date_select) 
    raster::values(template) <- focal_day[,input$summary_select] %>% unlist()
    
    pal <- colorNumeric(palette = 'Spectral', raster::values(template), reverse = TRUE,
                        na.color = "transparent")
    
    leafletProxy('base_map') %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(template, 
                     group = 'biomass',
                     colors = pal,
                     opacity = 0.8) %>%
      addLegend(pal = pal,
                values = raster::values(template),
                title = paste('Biomass', tolower(input$summary_select)),
                position = 'bottomright'
                )
  })
  
  observeEvent(input$base_map_click,{
    click <- input$base_map_click
    if(is.null(click))
      return()
    click_xy <- SpatialPoints(coords = data.frame(click$lng, click$lat),
                              proj4string=CRS('+init=epsg:4326'))
    click_trans <- spTransform(click_xy, '+init=epsg:26911') 
    focal_cell <- cellFromXY(template, click_trans)
    biomass_series <- series_dat[focal_cell,] %>% select(-x, -y) %>% unlist()
    
    gg_df <- data.frame(date = lubridate::ymd(dates$ymd), biomass = biomass_series) %>%
      mutate(year = lubridate::year(date), 
             month = lubridate::month(date))
    
    plot <- ggplot(gg_df, aes(x = lubridate::yday(as.Date(date)), y = biomass, color = year)) +
      geom_jitter(height = 0, width = 2) +
      theme_classic() +
      scale_color_gradientn(colors = rev(RColorBrewer::brewer.pal(11, 'Spectral')), name = 'Year') +
      ylab('Biomass (pounds/acre)') +
      xlab('Day of year') +
      scale_x_continuous(breaks = c(seq(0,364, 30))) +
      ggtitle(paste(round(click$lng,4), round(click$lat,4)))
    
    output$modal_1 <- renderUI({
      showModal(modalDialog(renderPlot(plot), 
                            size = 'l', 
                            easyClose = T, 
                            footer = NULL))
      
    })
    
  })
  
  
  
}
   
  
