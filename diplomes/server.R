library(shiny)
library(shinydashboard)

# Define server for application
function(input, output, session) {
  output$menu <- renderUI({
    selectInput("niveau", "Choisis le niveau ou le sous niveau", transformed_list, selected = "Ensemble des sortants")
  })

  code <- reactive({
    req(input$niveau)
    as.numeric(filter(data, Libelle_Menu %in% input$niveau) %>% pull(Code))
  })

  # Do not display the second SeletInput when the first or the second level are selected (set condition as output from server).
  output$sousniveau <- reactive(str_sub(code(), -2, -1) != "00")
  outputOptions(output, "sousniveau", suspendWhenHidden = FALSE)

  observeEvent(code(), {
    if (!is.null(code()) & str_sub(code(), -2, -1) != "00") {
      from <- as.numeric(code() + 1)
      to <- as.numeric(code() + 9)
      sequence <- seq(from, to, by = 1)
      values <- dplyr::filter(data, Code %in% sequence) %>% pull(Libelle_Menu)
      updateSelectInput(
        inputId = "degre3",
        label = "Choisis le deuxième sous niveau",
        choices = values
      )
    }
  })


  level <- reactive({
    req(input$niveau)
  })

  niveau3 <- reactive({
    req(input$degre3)
  })


  # Function to generate the first plot when first and second levels are selected from the first SelectInput tool.
  generatePlot <- function(niveau) {
    DT <- data %>%
      select(Libelle_Menu, taux_emploi, taux_chomage) %>%
      mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
      filter(Libelle_Menu == level()) %>%
      pivot_longer(
        cols = c("taux_emploi", "taux_chomage", "autre_situations"),
        names_to = "emploi",
        values_to = "taux"
      )

    ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
      geom_bar(stat = "identity", width = 0.5) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      labs(caption = "Source: TEXTE")
  }

  output$plot <- renderPlot({
    generatePlot(level())
  })

  observeEvent(level(), {
    output$plot <- renderPlot({
      generatePlot(level())
    })
  })

  selectedValue <- reactiveVal(NULL) # Use the reactive value selectedValue to keep track of the selected value from the second list of third levels.

  observeEvent(niveau3(), {
    selectedValue(niveau3())
  })

  observeEvent(niveau3(), {
    output$plot <- renderPlot({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      DT <- data %>%
        select(Libelle_Menu, taux_emploi, taux_chomage) %>%
        mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
        filter(Libelle_Menu == niveau3()) %>%
        pivot_longer(
          cols = c("taux_emploi", "taux_chomage", "autre_situations"),
          names_to = "emploi",
          values_to = "taux"
        )

      ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
        geom_bar(stat = "identity", width = 0.5) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        ) +
        labs(caption = "Source: TEXT")
    })
  })

  observeEvent(input$clear, {
    updateSelectInput(session, "degre3", selected = NULL) # When the clear button is clicked, we update the selectInput to have a selected value of NULL.
    selectedValue(NULL) # We set selectedValue to NULL as well.
    output$plot <- renderPlot({
      generatePlot(level())
    })
  })
}
