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

function showTooltips() {
  var all_labels = document.querySelectorAll(".stat-label[data-title]");
  for(var i = 0 ; i < all_labels.length ; i++) {
    if (all_labels[i].getAttribute("data-title") != '') {
      all_labels[i].addEventListener("mouseover", (event) => {
        var mousePos = new DOMPoint(event.pageX, event.pageY);
        var tooltipEl = document.getElementById("tooltip_ph");
        tooltipEl.style.left = mousePos.x + 20 + 'px';
        tooltipEl.style.top = mousePos.y + 20 + 'px';
        tooltipEl.style.opacity = '1';
        tooltipEl.textContent = event.target.getAttribute("data-title");
      });
      all_labels[i].addEventListener("mouseout", (event) => {
        var tooltipEl = document.getElementById("tooltip_ph");
        tooltipEl.style.opacity = '0';
      });
    }

  }
}

$(document).on("shiny:visualchange", function(e) {
  showTooltips();
});
$(document).on("shiny:value", function(e) {
  showTooltips();
});
