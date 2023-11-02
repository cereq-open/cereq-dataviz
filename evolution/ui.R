# Define UI --------------------------------------------------------------------
fluidPage(
  theme = bs_theme(version = 5, primary = "#008b99"),
  app_head,
  gfontHtmlDependency(family = "Arimo"),
  header_div,
  fluidRow(
    div(
      class = "d-flex",
      column(
        width = 12,
        align = "left",
        class = "align-items-start",
        pickerInput(
          inputId = "type_diplome",
          label = h1("Choisir le plus haut diplôme atteint :"),
          choices = type_diplome,
          selected = type_diplome[1],
          width = "fit",
          inline = TRUE,
          options = list(
            size = 5
          )
        )
      )
    )
  ),
  h1("Situation trois ans après la sortie de formation initiale"),
  fluidRow(
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_tx_emploi")
    ),
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_part_chomage")
    ),
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_tx_chomage")
    ),
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_part_tps_partiel")
    )
  ),
  h1("Conditions d’emploi des jeunes trois ans après leur sortie de formation initiale"),
  fluidRow(
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_tx_edi")
    ),
    
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_revenu_travail")
    ),
    column(
      width = 4, class = "p-2 col-sm-12 col-lg-6",
      girafeOutput("plot_comptence_ok")
    )
  )
)
