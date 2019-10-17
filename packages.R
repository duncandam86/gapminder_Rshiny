my_packages = c('gapminder','shiny','shinythemes','shinydashboard','colourpicker','plotly','DT','ggplot2','dplyr')


install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))