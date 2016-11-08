
# R options
options(warn = -1)
options(scipen = 999)

# load packages
suppressMessages(library(shiny))

ui <- fluidPage()
server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)