ui <- fillPage(
  
  absolutePanel(top = '10px', left = '10px', id = "abs_panel",
    selectInput(
      width = '130px',
      label = NULL,
      inputId = 'date_select',
      choices= unique(long_series$day),
      selected = 177
        
   ),
   selectInput(
     width = '130px',
     label = NULL,
     inputId = 'summary_select',
     choices=c('Mean','Variance','Min', 'Max'),
     selected = 'Mean'
     
   )),
  uiOutput("modal_1", width = '100%', height = '100%'),
  leafletOutput('base_map', height = '100%'),
  absolutePanel(
    img(src = 'https://www.mpgranch.com/sites/all/themes/custom/mpgranch_theme/images/icons/png/mpg_logo_white.png', height = '80'),
    bottom = '10px',
    left = '10px'
  )
  
)