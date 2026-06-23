# app.R
library(shiny)

# Cargar fuentes externas
source("global.R")
source("ui.R")
source("server.R")

# Ejecutar aplicación
shinyApp(ui = ui, server = server)