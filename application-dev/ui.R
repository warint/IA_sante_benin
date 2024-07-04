library(shiny)
library(leaflet)

navbarPage("PharmaConnect Bénin", id="main",
           tabPanel("Map", leafletOutput("bbmap", height=1000)),
           tabPanel("Data", DT::dataTableOutput("data")),
           tabPanel("Read Me",includeMarkdown("readme.md")))

