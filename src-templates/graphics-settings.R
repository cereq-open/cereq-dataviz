suppressPackageStartupMessages({
  library(ragg)
  library(systemfonts)
  library(gdtools)
  library(ggplot2)
  library(ggiraph)
  library(ggtext)
})

## use ragg
options(shiny.useragg = TRUE)

## load Arimo font
if (!font_family_exists("Arimo")) {
  systemfonts::register_font(
    name = "Arimo",
    plain = "www/arimo/fonts/arimo-v28-latin_latin-ext-regular.ttf",
    bold = "www/arimo/fonts/arimo-v28-latin_latin-ext-700.ttf",
    italic = "www/arimo/fonts/arimo-v28-latin_latin-ext-italic.ttf",
    bolditalic = "www/arimo/fonts/arimo-v28-latin_latin-ext-700italic.ttf"
  )
}

## taille de police ----
fs_default <- 10 * 4 / 3
fs_caption <- 8 * 4 / 3
fs_legend <- fs_default * .8
fs_title <- 12 * 4 / 3

## ggplot initial theme ----
theme_set(
  theme(
    line = element_line(colour = "black", linewidth = 0.1),
    text = element_text(size = fs_default, family = "Arimo"),
    panel.background = element_blank(), # element_rect(fill = "#EEEEEE", colour = "transparent"),
    plot.background = element_rect(
      fill = "transparent", colour = "#008b99",
      linewidth = 1.5
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      hjust = 0.5, size = fs_default, face = "bold",
      margin = margin(t = 6, r = 0, b = 3, l = 0)),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    plot.title.position = "plot",
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.caption = element_textbox_simple(
      hjust = 0,
      color = "#C0C0C2",
      size = fs_caption,
      margin = margin(t = 5, r = 2, b = 2, l = 2),
      lineheight = 1.3
    ),
    plot.title = element_textbox_simple(
      size = fs_title,
      margin = margin(t = 20, r = 5, b = 20, l = 5),
      face = "bold",
      lineheight = 1.3
    ),
    plot.caption.position = "plot",
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = "center",
    legend.box.spacing = unit(0, "cm"),
    legend.margin = margin(10, 10, 10, 10),
    legend.text = element_text(size = fs_legend),
    plot.margin = unit(c(t = 10, r = 10, l = 10, b = 10), "pt")
  )
)

## girafe_defaults ----
set_girafe_defaults(
  opts_hover_inv = opts_hover_inv(css = "stroke-width:2px; opacity:.5;"),
  opts_hover = opts_hover(css = ""),
  opts_selection = opts_selection(type = "none"),
  opts_toolbar = opts_toolbar(saveaspng = FALSE),
  opts_tooltip = opts_tooltip(
    css = sprintf("padding:5px;background:#EEEEEE;color:black;\
                  border-radius:2px 2px 2px 2px;text-align:left;\
                  line-height: 1.3;font-size:%.2fpx;", fs_default)
  ),
  opts_sizing(rescale = FALSE),
  fonts = list(sans = "Arimo")
)
