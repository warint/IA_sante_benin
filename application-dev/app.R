# Load required libraries
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(markdown)
library(DBI)
library(dbplyr)

# Define UI
ui <- navbarPage("Localisation des Banques de Sang", id="main",
                 tabPanel("Carte", leafletOutput("bbmap", height=1000)),
                 tabPanel("Données", DT::dataTableOutput("data")),
                 tabPanel("Info", includeMarkdown("readme.md"))
)

# Define server logic
server <- function(input, output) {
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data/pharmacies.db")
  
  # Read the data from the SQLite database
  bb_data <- tbl(con, "pharmacies") %>% 
    collect() %>% 
    mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>% 
    filter(!is.na(Latitude))
  
  # New column for the popup label
  bb_data <- mutate(bb_data, cntnt = paste0('<strong>Name: </strong>', Blood.Bank.Name,
                                            '<br><strong>State:</strong> ', State,
                                            '<br><strong>Time:</strong> ', Service.Time,
                                            '<br><strong>Mobile:</strong> ', Mobile,
                                            '<br><strong>HelpLine:</strong> ', Helpline,
                                            '<br><strong>Contact1:</strong> ', Contact.No.1,
                                            '<br><strong>Contact2:</strong> ', Contact.No.2,
                                            '<br><strong>Contact3:</strong> ', Contact.No.3,
                                            '<br><strong>Contact4:</strong> ', Contact.No.4,
                                            '<br><strong>Contact5:</strong> ', Contact.No.5,
                                            '<br><strong>Contact6:</strong> ', Contact.No.6,
                                            '<br><strong>Contact7:</strong> ', Contact.No.7,
                                            '<br><strong>Email:</strong> ', Email,
                                            '<br><strong>Website:</strong> ', Website)) 
  
  # Create a color palette for category type in the data file
  pal <- colorFactor(pal = c("#1b9e77", "#d95f02", "#7570b3"), domain = c("Charity", "Government", "Private"))
  
  # Create the leaflet map  
  output$bbmap <- renderLeaflet({
    leaflet(bb_data) %>% 
      addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
      addTiles() %>%
      addCircleMarkers(data = bb_data, lat = ~Latitude, lng = ~Longitude, 
                       radius = 3, popup = ~as.character(cntnt), 
                       color = ~pal(Category),
                       stroke = FALSE, fillOpacity = 0.8) %>%
      addLegend(pal = pal, values = bb_data$Category, opacity = 1, na.label = "Not Available") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "ME",
        onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  # Create a data object to display data
  output$data <- DT::renderDataTable(datatable(
    bb_data[, -c(1, 23, 24, 25, 28:35)], filter = 'top',
    colnames = c("Blood Bank Name", "State", "District", "City", "Address", "Pincode", "Contact No.",
                 "Mobile", "HelpLine", "Fax", "Email", "Website", "Nodal Officer", "Contact of Nodal Officer",
                 "Mobile of Nodal Officer", "Email of Nodal Officer", "Qualification", "Category", "Blood Component Available",
                 "Apheresis", "Service Time", "Lat", "Long.")
  ))
  
  # Disconnect from the database when the server is stopped
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
