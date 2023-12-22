
ui <- fluidPage(
  
  
  
  
)





server <- function(input, output,session) {
  
  
  output$carte_nico <- renderLeaflet({
    
    
    leaflet() %>%
      setView(lng=12.766277, lat=55,zoom = 3,8) %>%
      # fitBounds(-20,65,20,40) %>%
      addTiles()
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
}