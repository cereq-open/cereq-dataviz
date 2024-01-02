ui <- fluidPage(
  
  
  fluidRow(column (width=6,
                   pickerInput(
                     inputId = "annee",
                     
                     label = h1("Choisir l'année :"), 
                     
                     choices = c("2010", "2015", "2020")
               
                   ))
  
  
  
),

fluidRow(
  column (width=12,

leafletOutput("carte_nico")
))


)