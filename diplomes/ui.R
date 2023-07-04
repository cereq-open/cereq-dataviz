library(shiny)
library(shinydashboard)
library(prompter)

dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Génération 2017",
    titleWidth = 200,
    tags$li(
      a(
        href = "https://www.cereq.fr/",
        img(src = "cereq_logo.png", height = "30px")
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    
        fluidRow(
          column(width = 6,
          box(status = "primary", width = NULL,
              tags$head(tags$style(".option:first-child, 
              .option optgroup:first-child option:first-child {
                         font-weight:bold;
                       }")),
            uiOutput("menu"),
            conditionalPanel(
              condition = "output.sousniveau == true",
              selectInput("degre3", label = "Texte", choices = NULL, selectize = FALSE, size = 2),
              actionButton("clear", "Déselectionner")
            ), 
            box(status = "primary", width = NULL, 
                use_prompt(),
                valueBoxOutput("ibox1"), 
                valueBoxOutput("ibox2"))
            
          )),
          column(width = 6, 
                 box(
            title = "Quels emplois ?", status = "primary", width = NULL, solidHeader = TRUE, 
            collapsible = TRUE,
            valueBoxOutput("ibox3"),
            valueBoxOutput("ibox4"),
            valueBoxOutput("ibox5")
            
          ))
          ),
        fluidRow(
          column(width = 6, box(status = "primary", width = NULL, 
                                plotOutput("plot"))),
          column(width = 3, box(title = "Répartition par profession", status = "primary", solidHeader = TRUE, collapsible = TRUE,  width = NULL,
                                plotOutput("pieprofession"))),
          column(width = 3, box(title = "Répartition par secteur", status = "primary", solidHeader = TRUE, collapsible = TRUE,  width = NULL,
                                plotOutput("piesecteur")))),
        fluidRow(
          column(width = 6),
          column(width = 6, 
          box(status = "primary", width = NULL,
              valueBoxOutput("ibox6", width = 6), 
              valueBoxOutput("ibox7", width = 6)
              )
          )   
        )
  )
)
