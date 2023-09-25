fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
  app_head,
  gfontHtmlDependency(family = "Arimo"),
  header_div,
  fluidRow(
    column(
      width = 12,
      pickerInput(
        width = "fit",
        inline = TRUE,
        label = tags$h1("Choisir le plus haut diplôme atteint : "),
        inputId = "level_diploma",
        choices = setNames(as.character(menus_data$key), menus_data$Libelle_Menu),
        selected = "100",
        choicesOpt = list(
          style = menus_data$style
        )
      ),
      uiOutput("sub_type_diploma")
    )
  ),
  tags$h1("Situation trois ans après la sortie de formation initiale"),
  fluidRow(
    column(
      width = 12,
      tags$table(
        class = "stat-table",
        tags$tr(
          tags$td(uiOutput("tx_en_emploi")),
          tags$td(uiOutput("tx_chomage"))
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12, class = "p-2",
      girafeOutput("graph_situation_apres_3_ans", height = NULL)
    )
  ),
  tags$h1("Conditions d’emploi des jeunes trois ans après leur sortie de formation initiale"),
  fluidRow(
    column(
      width = 12, class = "p-2",
      tags$table(
        class = "stat-table",
        tags$tr(
          tags$td(uiOutput("tx_en_edi")),
          tags$td(uiOutput("tx_a_tps_partiel")),
          tags$td(uiOutput("revenu_median"))
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 12, class = "p-2",
      tags$table(
        class = "stat-table",
        tags$tr(
          tags$td(uiOutput("tx_jugent_coherent")),
          tags$td(uiOutput("tx_estiment_ss_employes"))
        )
      )
    )
  ),
  fluidRow(
    column(
      width = 6, class = "p-2",
      girafeOutput("plot_repartition_par_profession", height = NULL)
    ),
    column(
      width = 6, class = "p-2",
      girafeOutput("plot_repartition_par_secteur", height = NULL)
    )
  )
)
