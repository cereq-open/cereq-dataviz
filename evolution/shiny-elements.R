library(shiny)

DownloadButton <- function(outputId, label = label) {
  tags$a(
    id = outputId,
    class = "btn btn-default shiny-download-link",
    href = "",
    target = "_blank",
    download = NA, NULL, label
  )
}

header_div <- div(
  id = "logo",
  tags$img(
    src = "logo-cereq.svg"
  ),
  tags$p(
    style = "font-size:14px;",
    "Données : ",
    tags$img(
      src = "logo-generation.png"
    )
  ),
  tags$head(tags$style(".btn{background:#FFFFFF;} .btn{color: #008b99;}; @media print{@page {size: landscape}};")),
  DownloadButton("downloadData", ".xlsx"),
  actionButton("downloadPDF", ".pdf", onclick = "window.print();")
)

app_head <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # CSS personnalisé
  tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"), # Librairie font-awesome
  tags$script(src = "https://ajax.googleapis.com/ajax/libs/webfont/1.6.26/webfont.js"), # Web Font Loader
  tags$script(src = "utils.js")
)
