library(shiny)
library(shinydashboard)


dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "CEREQ",
    titleWidth = 380,
    tags$li(
      a(
        href = "https://www.cereq.fr/",
        img(src = "logo_cereq.png", heigh = "70px")
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    width = 380,
    sidebarMenu(
      id = "tabs",
      menuItem("Diplôme", tabName = "diplome", icon = icon("folder-open"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "diplome",
        fluidRow(
          box(
            title = "Titre", status = "primary", width = 6, solidHeader = TRUE, collapsible = TRUE,
            uiOutput("menu"),
            conditionalPanel(
              condition = "output.sousniveau == true",
              selectInput("degre3", label = "Texte", choices = NULL, selectize = FALSE, size = 2),
              actionButton("clear", "Clear selection")
            )
          ),
          box(
            title = "Trois ans après la sortie de formation", status = "primary", width = 6, solidHeader = TRUE, collapsible = TRUE,
            plotOutput("plot")
          )
        )
      )
    )
  )
)
