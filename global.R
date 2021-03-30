dates <- read.csv("www/biomass_dates.csv")
series_dat <- feather('www/biomass_series_tab')
long_series <- feather('www/biomass_long_series') %>% as.data.frame()

template <- raster('www/biomass_template.tif') 
x_centroid <- -114.00
y_centroid <- 46.69