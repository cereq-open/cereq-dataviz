var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
  dimension[0] = window.innerWidth;
  dimension[1] = window.innerHeight;
  if (typeof Shiny != "undefined") {
    Shiny.onInputChange("dimension", dimension);
  }
});
WebFont.load({
  google: {
    families: ['Arimo']
  }
});