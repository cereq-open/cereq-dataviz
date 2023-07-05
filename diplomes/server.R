library(shiny)
library(shinydashboard)
library(prompter)

# Define server for application
function(input, output, session) {
  output$menu <- renderUI({
    selectInput("niveau", "Choisir le plus haut diplôme atteint :", list_degre1_2, selected = "Ensemble des sortants")
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
        label = "Choisir la spécialité",
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

  code_niveau3 <- reactive({
    req(input$niveau)
    from <- as.numeric(code() + 1)
    to <- as.numeric(code() + 9)
    sequence <- seq(from, to, by = 1)
    as.numeric(filter(data, Code %in% sequence) %>% pull(Code))
  })

  # Function to generate the first plot when first and second levels are selected from the first SelectInput tool.
  generatePlot <- function(niveau) {
    DT <- data %>%
      select(Libelle_Menu, taux_emploi, taux_chomage) %>%
      mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
      filter(Libelle_Menu %in% c("Ensemble des sortants", niveau)) %>%
      pivot_longer(
        cols = c("taux_emploi", "taux_chomage", "autre_situations"),
        names_to = "emploi",
        values_to = "taux"
      )

    ggplot(DT, aes(Libelle_Menu, taux, fill = emploi)) +
      geom_bar(stat = "identity", width = 0.5) + coord_flip() +
      geom_text(aes(label = taux),
                position = position_stack(vjust = .5))
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
        filter(Code %in% c(code_niveau3(), 100) & Libelle_Menu %in% c(niveau3(), "Ensemble des sortants")) %>%
        select(Code, Libelle_Menu, Libelle_complet, taux_emploi, taux_chomage) %>%
        mutate(autre_situations = 100 - (taux_emploi + taux_chomage)) %>%
        pivot_longer(
          cols = c("taux_emploi", "taux_chomage", "autre_situations"),
          names_to = "emploi",
          values_to = "taux"
        )

      ggplot(DT, aes(Libelle_complet, taux, fill = emploi)) +
        geom_bar(stat = "identity", width = 0.5) + coord_flip() +
        geom_text(aes(label = taux),
                  position = position_stack(vjust = .5))
    })
  })
 
###################### Create valueBoxes ###################### 
  
  generatevalueBoxLevel <- function(titre, column) {
    value <- paste(filter(data, Libelle_Menu %in% level()) %>% pull(column)," %")
    valueBox(tags$div(style = "display: flex; align-items: center;",
                      tags$p(titre, style = "font-size: 50%; margin-right: 5px;"),
                       tags$span(icon(name = "info", style = "font-size: 30%;")) |>
                         add_prompt(message = "TEXTE", position = "right")
                       ), 
             value, 
             icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the icon size
  }
  
  generatevalueBoxNiveau3 <- function(titre, column) {
    value <- paste(filter(data, Code == code_niveau3() & Libelle_Menu %in% niveau3()) %>% pull(column), " %")
    valueBox(tags$div(style = "display: flex; align-items: center;",
                      tags$p(titre, style = "font-size: 50%; margin-right: 5px;"),
                      tags$span(icon(name = "info", style = "font-size: 30%;")) |>
                        add_prompt(message = "TEXTE", position = "right")
    ), 
    value, 
    icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the icon size
  }
  
  output$ibox1 <- renderValueBox({
    generatevalueBoxLevel("En Emploi", "taux_emploi")
  })
  
  output$ibox2 <- renderValueBox({
    generatevalueBoxLevel("Taux de chômage", "taux_chomage")
  })  
  
  output$ibox3 <- renderValueBox({
    generatevalueBoxLevel("en EDI", "taux_edi")
  })  
  
  output$ibox4 <- renderValueBox({
    generatevalueBoxLevel("à temps partiel", "part_tps_partiel")
  }) 
  
  output$ibox5 <- renderValueBox({
    value <- paste(filter(data, Libelle_Menu %in% level()) %>% pull("revenu_travail")," $")
    valueBox(tags$div(style = "display: flex; align-items: center;",
                      tags$p("de revenu médian", style = "font-size: 50%;margin-right: 5px;"),
                      tags$span(icon(name = "info", style = "font-size: 30%;")) |>
                        add_prompt(message = "TEXTE", position = "right")
    ), 
    value, 
    icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the icon size
  }) 
  
  output$ibox6 <- renderValueBox({
    generatevalueBoxLevel(tags$p("jugent leur emploi cohérent" , br(), "avec leur formation initiale"), "correspondance_ok")
  }) 
  
  output$ibox7 <- renderValueBox({
    generatevalueBoxLevel(tags$p("estiment être employés", br(), "sous leur niveau de compétence"), "competence_ok")
  }) 
  
  observeEvent(level(), {
    output$ibox1 <- renderValueBox({
      generatevalueBoxLevel("En Emploi", "taux_emploi")
    })
    
    output$ibox2 <- renderValueBox({
      generatevalueBoxLevel("Taux de chômage", "taux_chomage")
    })  
    
    output$ibox3 <- renderValueBox({
      generatevalueBoxLevel("en EDI", "taux_edi")
    }) 
    
    output$ibox4 <- renderValueBox({
      generatevalueBoxLevel("à temps partiel", "part_tps_partiel")
    })
    
    output$ibox5 <- renderValueBox({
      value <- paste(filter(data, Libelle_Menu %in% level()) %>% pull("revenu_travail")," $")
      valueBox(tags$div(style = "display: flex; align-items: center;",
                        tags$p("de revenu médian", style = "font-size: 50%;margin-right: 5px;"),
                        tags$span(icon(name = "info", style = "font-size: 30%;")) |>
                          add_prompt(message = "TEXTE", position = "right")
      ), 
      value, 
      icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the icon size
    }) 
    
    output$ibox6 <- renderValueBox({
      generatevalueBoxLevel(tags$p("jugent leur emploi cohérent" , br(), "avec leur formation initiale"), "correspondance_ok")
    })
    
    output$ibox7 <- renderValueBox({
      generatevalueBoxLevel(tags$p("estiment être employés", br(), "sous leur niveau de compétence"), "competence_ok")
    })
    
  })

  observeEvent(niveau3(), {
    output$ibox1 <- renderValueBox({
      generatevalueBoxNiveau3("En Emploi", "taux_emploi")
    })
    
    output$ibox2 <- renderValueBox({
      generatevalueBoxNiveau3("Taux de chômage", "taux_chomage")
    })  
    
    output$ibox3 <- renderValueBox({
      generatevalueBoxNiveau3("en EDI", "taux_edi")
    }) 
    
    output$ibox4 <- renderValueBox({
      generatevalueBoxNiveau3("à temps partiel", "part_tps_partiel")
    }) 
    
    output$ibox5 <- renderValueBox({
      value <- paste(filter(data, Code == code_niveau3() & Libelle_Menu %in% niveau3()) %>% pull("revenu_travail"), " $")
      valueBox(tags$span(tags$p("de revenu médian", style = "font-size: 50%;"), 
                                tags$span(icon(name = "info", style = "font-size: 30%;")) %>%
                                  add_prompt(message = "TEXTE", position = "top-right")),
              value, icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the size of the icon
      
    }) 
    
    output$ibox6 <- renderValueBox({
      generatevalueBoxLevel(tags$p("jugent leur emploi cohérent" , br(), "avec leur formation initiale"), "correspondance_ok")
    }) 
    
    output$ibox7 <- renderValueBox({
      generatevalueBoxLevel(tags$p("estiment être employés", br(), "sous leur niveau de compétence"), "competence_ok")
    }) 
    
  })
  
  ######### Create Pie charts ########################
  
  generatePieProfession <- function(niveau) {
    DT <- data %>%
      select(Libelle_Menu, pos_cadres,	pos_prof_int,	pos_emp_ouv_q,	pos_emp_ouv_nq,	pos_autres) %>%
      filter(Libelle_Menu == niveau) %>%
      pivot_longer(
        cols = c("pos_cadres",	"pos_prof_int",	"pos_emp_ouv_q",	"pos_emp_ouv_nq",	"pos_autres"),
        names_to = "profession",
        values_to = "taux"
      )
    
    ggplot(DT, aes(x = "", y = taux, fill = profession)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      coord_polar("y", start = 0)+
      geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black")
  }
  
  generatePieSecteur <- function(niveau) {
    DT <- data %>%
      select(Libelle_Menu, sec_industries_btp,	sec_commerce,	sec_administration,	sec_a_services,	sec_autres) %>%
      filter(Libelle_Menu == niveau) %>%
      pivot_longer(
        cols = c("sec_industries_btp",	"sec_commerce",	"sec_administration",	"sec_a_services",	"sec_autres"),
        names_to = "secteur",
        values_to = "taux"
      )
    
    ggplot(DT, aes(x = "", y = taux, fill = secteur)) +
      geom_bar(width = 1, stat = "identity", color = "black") +
      coord_polar("y", start = 0)+
      geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black")
  }
  
  output$piesecteur <- renderPlot({
    generatePieSecteur(level())
  })
  
  output$pieprofession <- renderPlot({
    generatePieProfession(level())
  })

  observeEvent(level(), {
    output$pieprofession <- renderPlot({
      generatePieProfession(level())
    })
  })
  
  observeEvent(level(), {
    output$piesecteur <- renderPlot({
      generatePieSecteur(level())
    })
  })
 
  observeEvent(niveau3(), {
    output$pieprofession <- renderPlot({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      DT <- data %>%
        select(Code, Libelle_Menu, pos_cadres,	pos_prof_int,	pos_emp_ouv_q,	pos_emp_ouv_nq,	pos_autres) %>%
        filter(Code == code_niveau3() & Libelle_Menu == niveau3()) %>%
        pivot_longer(
          cols = c("pos_cadres",	"pos_prof_int",	"pos_emp_ouv_q",	"pos_emp_ouv_nq",	"pos_autres"),
          names_to = "profession",
          values_to = "taux"
        )
      
      ggplot(DT, aes(x = "", y = taux, fill = profession)) +
        geom_bar(width = 1, stat = "identity", color = "black") +
        coord_polar("y", start = 0)+
        geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black")
    })
  })
  
  observeEvent(niveau3(), {
    output$piesecteur <- renderPlot({
      req(selectedValue()) # Make sure that the selected value is not NULL before rendering the plot.
      DT <- data %>%
        select(Code, Libelle_Menu, sec_industries_btp,	sec_commerce,	sec_administration,	sec_a_services,	sec_autres) %>%
        filter(Code == code_niveau3() & Libelle_Menu == niveau3()) %>%
        pivot_longer(
          cols = c("sec_industries_btp",	"sec_commerce",	"sec_administration",	"sec_a_services",	"sec_autres"),
          names_to = "secteur",
          values_to = "taux"
        )
      
      ggplot(DT, aes(x = "", y = taux, fill = secteur)) +
        geom_bar(width = 1, stat = "identity", color = "black") +
        coord_polar("y", start = 0)+
        geom_text(aes(label = taux), position = position_stack(vjust = .5), color = "black") 
    })
  })
  
  ######### Click on the Clear button ########################
  observeEvent(input$clear, {
    updateSelectInput(session, "degre3", selected = NULL) # When the clear button is clicked, we update the selectInput to have a selected value of NULL.
    selectedValue(NULL) # We set selectedValue to NULL as well.
    output$plot <- renderPlot({
      generatePlot(level())
    })
    output$ibox1 <- renderValueBox({
      generatevalueBoxLevel("En Emploi", "taux_emploi")
    })
    
    output$ibox2 <- renderValueBox({
      generatevalueBoxLevel("Taux de chômage", "taux_chomage")
    })  
    
    output$ibox3 <- renderValueBox({
      generatevalueBoxLevel("en EDI", "taux_chomage")
    }) 
    
    output$ibox4 <- renderValueBox({
      generatevalueBoxLevel("à temps partiel", "part_tps_partiel")
    }) 
    
    output$ibox5 <- renderValueBox({
      value <- paste(filter(data, Libelle_Menu %in% level()) %>% pull("revenu_travail")," $")
      valueBox(tags$div(style = "display: flex; align-items: center;",
                        tags$p("de revenu médian", style = "font-size: 50%;margin-right: 5px;"),
                        tags$span(icon(name = "info", style = "font-size: 30%;")) |>
                          add_prompt(message = "TEXTE", position = "right")
      ), 
      value, 
      icon = icon(name = "bar-chart", style = "font-size: 70%;")) # Decrease the icon size
    }) 
    
    output$ibox6 <- renderValueBox({
      generatevalueBoxLevel(tags$p("jugent leur emploi cohérent", br(), "avec leur formation initiale"), "correspondance_ok")
    }) 
    
    output$ibox7 <- renderValueBox({
      generatevalueBoxLevel(tags$p("estiment être employés", br(), "sous leur niveau de compétence"), "competence_ok")
    })
    
    output$pieprofession <- renderPlot({
      generatePieProfession(level())
    })
    
    output$piesecteur <- renderPlot({
      generatePieSecteur(level())
    })
    
  })
  
}
