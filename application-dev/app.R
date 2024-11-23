# Load required libraries
library(shiny)
library(dplyr)
library(leaflet)
library(DT)
library(markdown)
library(DBI)
library(dbplyr)
library(RSQLite)


# Define UI
ui <- fluidPage(
  tags$head(
    # Favicon
    tags$link(rel = "icon", href = "img/favicon.ico"),
    
    # Google Fonts
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400;700&family=Roboto:wght@400;700&display=swap"),
    
    # Icon Font Stylesheets
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.0/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.4.1/font/bootstrap-icons.css"),
    
    # Libraries Stylesheets
    tags$link(rel = "stylesheet", href = "lib/owlcarousel/assets/owl.carousel.min.css"),
    tags$link(rel = "stylesheet", href = "lib/tempusdominus/css/tempusdominus-bootstrap-4.min.css"),
    
    # Customized Bootstrap and Template Stylesheets
    tags$link(rel = "stylesheet", href = "css/bootstrap.min.css"),
    tags$link(rel = "stylesheet", href = "css/style.css"),
    
    # jQuery
    tags$script(src = "https://code.jquery.com/jquery-3.4.1.min.js"),
    # Bootstrap
    tags$script(src = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.0/dist/js/bootstrap.bundle.min.js"),
    # Easing
    tags$script(src = "lib/easing/easing.min.js"),
    # Waypoints
    tags$script(src = "lib/waypoints/waypoints.min.js"),
    # Owl Carousel
    tags$script(src = "lib/owlcarousel/owl.carousel.min.js"),
    # Tempus Dominus (date-picker)
    tags$script(src = "lib/tempusdominus/js/moment.min.js"),
    tags$script(src = "lib/tempusdominus/js/moment-timezone.min.js"),
    tags$script(src = "lib/tempusdominus/js/tempusdominus-bootstrap-4.min.js"),
    # Template JavaScript principal
    tags$script(src = "js/main.js"),
    
    tags$style(HTML("
      .hero-header {
          width: 100%;
          height: 75vh !important;
          background: url('../img/hero4.jpg') top right no-repeat;
          background-size: cover;
      }
      
    ")),
  ),
  
  # Topbar Start
  tags$div(
    class = "container-fluid py-2 border-bottom d-none d-lg-block",
    tags$div(
      class = "container",
      tags$div(
        class = "row",
        # Colonne gauche
        tags$div(
          class = "col-md-6 text-center text-lg-start mb-2 mb-lg-0",
          tags$div(
            class = "d-inline-flex align-items-center",
            tags$a(class = "text-decoration-none text-body pe-3", href = "#",
                   tags$i(class = "bi bi-telephone me-2"), "+012 345 6789"),
            tags$span(class = "text-body", "|"),
            tags$a(class = "text-decoration-none text-body px-3", href = "#",
                   tags$i(class = "bi bi-envelope me-2"), "info@example.com")
          )
        ),
        # Colonne droite
        tags$div(
          class = "col-md-6 text-center text-lg-end",
          tags$div(
            class = "d-inline-flex align-items-center",
            tags$a(class = "text-body px-2", href = "#", tags$i(class = "fab fa-facebook-f")),
            tags$a(class = "text-body px-2", href = "#", tags$i(class = "fab fa-twitter")),
            tags$a(class = "text-body px-2", href = "#", tags$i(class = "fab fa-linkedin-in")),
            tags$a(class = "text-body px-2", href = "#", tags$i(class = "fab fa-instagram")),
            tags$a(class = "text-body ps-2", href = "#", tags$i(class = "fab fa-youtube"))
          )
        )
      )
    )
  ),
  # Topbar End
  
  # Navbar Start
  tags$div(
    class = "container-fluid sticky-top bg-white shadow-sm",
    tags$div(
      class = "container",
      tags$nav(
        class = "navbar navbar-expand-lg bg-white navbar-light py-3 py-lg-0",
        tags$a(
          href = "#", class = "navbar-brand",
          tags$h1(
            class = "m-0 text-uppercase text-primary",
            tags$i(class = "fa fa-clinic-medical me-2"), "Medinova"
          )
        ),
        tags$button(
          class = "navbar-toggler", type = "button",
          `data-bs-toggle` = "collapse", `data-bs-target` = "#navbarCollapse",
          tags$span(class = "navbar-toggler-icon")
        ),
        tags$div(
          class = "collapse navbar-collapse", id = "navbarCollapse",
          tags$div(
            class = "navbar-nav ms-auto py-0",
            # Liens dynamiques pour changer de page
            tags$a(href = "#", class = "nav-item nav-link active", onclick = "Shiny.setInputValue('page', 'home')", "Home"),
            tags$a(href = "#", class = "nav-item nav-link", onclick = "Shiny.setInputValue('page', 'about')", "About"),
            tags$a(href = "#", class = "nav-item nav-link", onclick = "Shiny.setInputValue('page', 'service')", "Service"),
            tags$a(href = "#", class = "nav-item nav-link", onclick = "Shiny.setInputValue('page', 'pricing')", "Pricing"),
            tags$div(
              class = "nav-item dropdown",
              tags$a(
                href = "#", class = "nav-link dropdown-toggle",
                `data-bs-toggle` = "dropdown", "Pages"
              ),
              tags$div(
                class = "dropdown-menu m-0",
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'blog')", "Blog Grid"),
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'detail')", "Blog Detail"),
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'team')", "The Team"),
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'testimonial')", "Testimonial"),
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'appointment')", "Appointment"),
                tags$a(href = "#", class = "dropdown-item", onclick = "Shiny.setInputValue('page', 'search')", "Search")
              )
            ),
            tags$a(href = "#", class = "nav-item nav-link", onclick = "Shiny.setInputValue('page', 'contact')", "Contact")
          )
        )
      )
    )
  ),
  # Navbar End
  
  # Contenu principal
  uiOutput("mainContent"),
  
  # Footer
  tags$footer(
    class = "container-fluid bg-dark text-light py-5",
    tags$div(
      class = "container",
      tags$div(
        class = "row gy-5",
        # Contact Information
        tags$div(
          class = "col-lg-4 col-md-6",
          tags$h4(class = "text-primary text-uppercase mb-4", "Get In Touch"),
          tags$p("No dolore ipsum accusam no lorem. Invidunt sed clita kasd clita et et dolor sed dolor"),
          tags$p(class = "mb-2", tags$i(class = "fa fa-map-marker-alt text-primary me-3"), "123 Street, New York, USA"),
          tags$p(class = "mb-2", tags$i(class = "fa fa-envelope text-primary me-3"), "info@example.com"),
          tags$p(class = "mb-0", tags$i(class = "fa fa-phone-alt text-primary me-3"), "+012 345 67890")
        ),
        # Quick Links
        tags$div(
          class = "col-lg-4 col-md-6",
          tags$h4(class = "text-primary text-uppercase mb-4", "Quick Links"),
          tags$div(
            class = "d-flex flex-column justify-content-start",
            tags$a(class = "text-light mb-2", href = "#", tags$i(class = "fa fa-angle-right me-2"), "Home"),
            tags$a(class = "text-light mb-2", href = "#", tags$i(class = "fa fa-angle-right me-2"), "About Us"),
            tags$a(class = "text-light mb-2", href = "#", tags$i(class = "fa fa-angle-right me-2"), "Our Services"),
            tags$a(class = "text-light mb-2", href = "#", tags$i(class = "fa fa-angle-right me-2"), "Meet The Team"),
            tags$a(class = "text-light mb-2", href = "#", tags$i(class = "fa fa-angle-right me-2"), "Latest Blog"),
            tags$a(class = "text-light", href = "#", tags$i(class = "fa fa-angle-right me-2"), "Contact Us")
          )
        ),
        # Newsletter
        tags$div(
          class = "col-lg-4 col-md-6",
          tags$h4(class = "text-primary text-uppercase mb-4", "Newsletter"),
          tags$form(
            tags$div(
              class = "input-group",
              tags$input(
                type = "text",
                class = "form-control p-3 border-0",
                placeholder = "Your Email Address"
              ),
              tags$button(class = "btn btn-primary", "Sign Up")
            )
          ),
          tags$h6(class = "text-primary text-uppercase mt-4 mb-3", "Follow Us"),
          tags$div(
            class = "d-flex",
            tags$a(class = "btn btn-lg btn-primary btn-lg-square rounded-circle me-2", href = "#", tags$i(class = "fab fa-twitter")),
            tags$a(class = "btn btn-lg btn-primary btn-lg-square rounded-circle me-2", href = "#", tags$i(class = "fab fa-facebook-f")),
            tags$a(class = "btn btn-lg btn-primary btn-lg-square rounded-circle me-2", href = "#", tags$i(class = "fab fa-linkedin-in")),
            tags$a(class = "btn btn-lg btn-primary btn-lg-square rounded-circle", href = "#", tags$i(class = "fab fa-instagram"))
          )
        )
      )
    ),
    tags$div(
      class = "container-fluid bg-dark text-light border-top border-secondary py-4",
      tags$div(
        class = "container",
        tags$div(
          class = "row g-5",
          tags$div(
            class = "col-md-6 text-center text-md-start",
            tags$p(class = "mb-md-0", HTML("&copy; "), tags$a(class = "text-primary", href = "#", "Your Site Name"), ". All Rights Reserved.")
          ),
          tags$div(
            class = "col-md-6 text-center text-md-end",
            tags$p(class = "mb-0", "Designed by ", tags$a(class = "text-primary", href = "https://htmlcodex.com", "HTML Codex"))
          )
        )
      )
    )
  ),
  
)

# Define server logic
server <- function(input, output, session) {
  
  antiepileptiques <- c(
    "Depakine (Chrono) 300 mg (comprimés à libération prolongée)",
    "Depakine (Chrono) 500 mg (comprimés à libération prolongée)",
    "Depakine (Chrono) 750 mg (comprimés à libération prolongée)",
    "Depakine (Chrono) 1000 mg (comprimés à libération prolongée)",
    "Tegretol 200 mg (comprimés)",
    "Tegretol 400 mg (comprimés)",
    "Frisium 5 mg (comprimés)",
    "Frisium 10 mg (comprimés)",
    "Rivotril 0,5 mg (comprimés)",
    "Rivotril 2 mg (comprimés)",
    "Zarontin 250 mg (capsules)",
    "Zarontin 250 mg/5 ml (sirop)",
    "Gabapentine (Neurontin) 100 mg (gélules)",
    "Gabapentine (Neurontin) 300 mg (gélules)",
    "Gabapentine (Neurontin) 400 mg (gélules)",
    "Gabapentine (Neurontin) 600 mg (comprimés)",
    "Gabapentine (Neurontin) 800 mg (comprimés)",
    "Lacosamide (Vimpat) 50 mg (comprimés)",
    "Lacosamide (Vimpat) 100 mg (comprimés)",
    "Lacosamide (Vimpat) 150 mg (comprimés)",
    "Lacosamide (Vimpat) 200 mg (comprimés)",
    "Levetiracetam (Keppra) 250 mg (comprimés)",
    "Levetiracetam (Keppra) 500 mg (comprimés)",
    "Levetiracetam (Keppra) 750 mg (comprimés)",
    "Levetiracetam (Keppra) 1000 mg (comprimés)",
    "Oxcarbazépine (Trileptal) 150 mg (comprimés)",
    "Oxcarbazépine (Trileptal) 300 mg (comprimés)",
    "Oxcarbazépine (Trileptal) 600 mg (comprimés)",
    "Oxcarbazépine (Trileptal) 900 mg (comprimés)",
    "Phénytoïne (Dilantin) 50 mg (comprimés)",
    "Phénytoïne (Dilantin) 100 mg (comprimés)",
    "Pregabaline (Lyrica) 25 mg (gélules)",
    "Pregabaline (Lyrica) 50 mg (gélules)",
    "Pregabaline (Lyrica) 75 mg (gélules)",
    "Pregabaline (Lyrica) 100 mg (gélules)",
    "Pregabaline (Lyrica) 150 mg (gélules)",
    "Pregabaline (Lyrica) 200 mg (gélules)",
    "Pregabaline (Lyrica) 225 mg (gélules)",
    "Pregabaline (Lyrica) 300 mg (gélules)",
    "Rufinamide (Inovelon) 100 mg (comprimés)",
    "Rufinamide (Inovelon) 200 mg (comprimés)",
    "Rufinamide (Inovelon) 400 mg (comprimés)",
    "Topiramate (Topamax) 25 mg (comprimés)",
    "Topiramate (Topamax) 50 mg (comprimés)",
    "Topiramate (Topamax) 100 mg (comprimés)",
    "Topiramate (Topamax) 200 mg (comprimés)",
    "Vigabatrine (Sabril) 500 mg (comprimés)",
    "Vigabatrine (Sabril) 1000 mg (comprimés)"
  )
  
  
  output$mainContent <- renderUI({
    page <- input$page
    if (is.null(page)) {
      page <- "home" # Définit une valeur par défaut si aucune page n'est sélectionnée
    }
    
    switch(page,
           "home" = div(
             
             # Hero Start
             tags$div(
               class = "container-fluid bg-primary py-5 mb-5 hero-header",
               tags$div(
                 class = "container py-5",
                 tags$div(
                   class = "row justify-content-start",
                   tags$div(
                     class = "col-lg-8 text-center text-lg-start",
                     tags$h5(
                       class = "d-inline-block text-primary text-uppercase border-bottom border-5",
                       style = "border-color: rgba(256, 256, 256, .3) !important;",
                       "Welcome To Medinova"
                     ),
                     tags$h1(class = "display-1 text-white mb-md-4", 
                             "Best Healthcare Solution In Your City"),
                     tags$div(
                       class = "pt-2",
                       tags$a(
                         href = "#", class = "btn btn-light rounded-pill py-md-3 px-md-5 mx-2", 
                         "Find Doctor"
                       ),
                       tags$a(
                         href = "#", class = "btn btn-outline-light rounded-pill py-md-3 px-md-5 mx-2", 
                         "Appointment"
                       )
                     )
                   )
                 )
               )
             ),
             # Hero End
             
             # Section About
             tags$div(
               class = "container-fluid py-5",
               tags$div(
                 class = "container",
                 tags$div(
                   class = "row gx-5",
                   # Image
                   tags$div(
                     class = "col-lg-5 mb-5 mb-lg-0",
                     style = "min-height: 500px;",
                     tags$div(
                       class = "position-relative h-100",
                       tags$img(
                         class = "position-absolute w-100 h-100 rounded",
                         src = "img/about4.jpg",
                         style = "object-fit: cover;"
                       )
                     )
                   ),
                   # Texte et icônes
                   tags$div(
                     class = "col-lg-7",
                     tags$div(
                       class = "mb-4",
                       tags$h1(
                         class = "d-inline-block text-primary text-uppercase border-bottom border-5",
                         "A propos de Nous"
                       ),
                       tags$h1(
                         class = "display-4",
                         "Améliorer l'Accès aux Médicaments AntiÉpileptiques"
                       )
                     ),
                     tags$p(style = "font-size: 2rem;", # Taille de texte augmentée
                            "L'inégalité d'accès aux médicaments pour l'épilepsie reste un problème mondial majeur, surtout dans les pays à revenu faible ou intermédiaire. Des millions de personnes n'ont pas accès aux traitements essentiels pour contrôler leurs crises, ce qui affecte leur santé, leur capacité à travailler, et leur qualité de vie. Notre mission est de lever ces barrières pour que chaque personne atteinte d'épilepsie ait accès aux médicaments dont elle a besoin."
                     ),
                     tags$div(
                       class = "row g-3 pt-3",
                       # Icones et descriptions
                       tags$div(
                         class = "col-sm-3 col-6",
                         tags$div(
                           class = "bg-light text-center rounded-circle py-4",
                           style = "width: 100px; height: 100px; margin: 0 auto; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                           tags$i(class = "fa fa-3x fa-pills text-primary mb-2"),
                           tags$h6(style = "font-size: 1.2rem; margin-top: 5px;", 
                                   "Inexistence", tags$small(class = "d-block text-primary", "dans les Pharmacies"))
                         )
                       ),
                       tags$div(
                         class = "col-sm-3 col-6",
                         tags$div(
                           class = "bg-light text-center rounded-circle py-4",
                           style = "width: 100px; height: 100px; margin: 0 auto; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                           tags$i(class = "fa fa-3x fa-pills text-primary mb-2"),
                           tags$h6(style = "font-size: 1.2rem; margin-top: 5px;",
                                   "Rupture", tags$small(class = "d-block text-primary", "de stock"))
                         )
                       ),
                       tags$div(
                         class = "col-sm-3 col-6",
                         tags$div(
                           class = "bg-light text-center rounded-circle py-4",
                           style = "width: 100px; height: 100px; margin: 0 auto; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                           tags$i(class = "fa fa-3x fa-road text-primary mb-2"),
                           tags$h6(style = "font-size: 1.2rem; margin-top: 5px;",
                                   "Distance", tags$small(class = "d-block text-primary", "Longue"))
                         )
                       ),
                       tags$div(
                         class = "col-sm-3 col-6",
                         tags$div(
                           class = "bg-light text-center rounded-circle py-4",
                           style = "width: 100px; height: 100px; margin: 0 auto; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                           tags$i(class = "fa fa-3x fa-money-bill-wave text-primary mb-2"),
                           tags$h6(style = "font-size: 1.2rem; margin-top: 5px;",
                                   "Prix", tags$small(class = "d-block text-primary", "Trop cher"))
                         )
                       )
                     )
                   )
                 )
               )
             ),
             
             # Section Services
             tags$div(
               class = "container-fluid py-5",
               tags$div(
                 class = "container",
                 # En-tête des services
                 tags$div(
                   class = "text-center mx-auto mb-5",
                   style = "max-width: 500px;",
                   tags$h1(
                     class = "d-inline-block text-primary text-uppercase border-bottom border-5",
                     "Services"
                   ),
                   tags$h2(class = "display-4", "Nous aidons les patients à retrouver les phamacies et nous aidons les pharmacies à résoudre le problème d'échec de marché pour les pharmacies")
                 ),
                 # Liste des services
                 tags$div(
                   class = "row g-5",
                   # Service 1
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fas fa-3x fa-user text-white")
                       ),
                       tags$h4(class = "mb-3", "Enregistrer les besoins des patients"),
                       tags$p(
                         class = "m-0",
                         "Enregistrer les besoins du patient avec les médicaments et sa localisation non pas précise mais juste la zone de résidence. Cela peut-être le quartier, la Ville ou le Département"
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   ),
                   # Service 2
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fa fa-3x fa-user-md text-white")
                       ),
                       tags$h4(class = "mb-3", "Enregistrer une Pharmacie"),
                       tags$p(
                         class = "m-0",
                         "Enregistrer les informations des Pharmacies et Mettre la liste à disposition des Patients"
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   ),
                   # Service 3
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fa fa-3x fa-map text-white")
                       ),
                       tags$h4(class = "mb-3", "Localisation des zones de patients"),
                       tags$p(
                         class = "m-0",
                         "Afficher sur une carte géographique les zones de résidences des patients"
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   ),
                   # Service 4
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fa fa-3x fa-map text-white")
                       ),
                       tags$h4(class = "mb-3", "Liste et localisation précise des pharmacies"),
                       tags$p(
                         class = "m-0",
                         "Kasd dolor no lorem nonumy sit labore tempor at justo rebum rebum stet, justo elitr dolor amet sit"
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   ),
                   # Service 5
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fa fa-3x fa-pills text-white")
                       ),
                       tags$h4(class = "mb-3", "Tableau de bord"),
                       tags$p(
                         class = "m-0",
                         "Afficher un tableau de bord relatif aux médicaments pour examiner les tendances actuelles et examiner les prédictions pour trouver une solution à ce problème d'échec de marché "
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   ),
                   # Service 6
                   tags$div(
                     class = "col-lg-4 col-md-6",
                     tags$div(
                       class = "service-item bg-light rounded d-flex flex-column align-items-center justify-content-center text-center",
                       tags$div(
                         class = "service-icon mb-4",
                         tags$i(class = "fa fa-3x fa-ribbon text-white")
                       ),
                       tags$h4(class = "mb-3", "Sensibilisation"),
                       tags$p(
                         class = "m-0",
                         "Sensibiliser sur les Epilepsies, lutter contre la stigmatisation et la désinformation pour apporter la bonne information, les bonnes pratiques face à une personne atteinte d'épilepsie"
                       ),
                       tags$a(
                         class = "btn btn-lg btn-primary rounded-pill",
                         href = "",
                         tags$i(class = "bi bi-arrow-right")
                       )
                     )
                   )
                 )
               )
             ),
             
             
             tags$div(
               class = "container-fluid bg-primary my-5 py-5",
               tags$div(
                 class = "container py-5",
                 tags$div(
                   class = "row gx-5",
                   # Left Column: Information
                   tags$div(
                     class = "col-lg-6 mb-5 mb-lg-0",
                     tags$div(
                       class = "mb-4",
                       tags$h5(
                         class = "d-inline-block text-white text-uppercase border-bottom border-5",
                         "Appointment"
                       ),
                       tags$h1(
                         class = "display-4 text-white",
                         "Make An Appointment For Your Family"
                       )
                     ),
                     tags$p(
                       class = "text-white mb-5",
                       "Eirmod sed tempor lorem ut dolores. Aliquyam sit sadipscing kasd ipsum. Dolor ea et dolore et at sea ea at dolor. Justo ipsum duo rebum sea invidunt voluptua. Eos vero eos vero ea et dolore eirmod et. Dolores diam duo invidunt lorem."
                     ),
                     tags$a(
                       class = "btn btn-dark rounded-pill py-3 px-5 me-3",
                       href = "#",
                       "Find Doctor"
                     ),
                     tags$a(
                       class = "btn btn-outline-dark rounded-pill py-3 px-5",
                       href = "#",
                       "Read More"
                     )
                   ),
                   # Right Column: Form
                   tags$div(
                     class = "col-lg-6",
                     tags$div(
                       class = "bg-white text-center rounded p-5",
                       tags$h1(
                         class = "mb-4",
                         "Book An Appointment"
                       ),
                       tags$form(
                         tags$div(
                           class = "row g-3",
                           # Localité de résidence
                           tags$div(
                             class = "col-12",
                             tags$label("Zone de résidence :", class = "form-label"),
                             tags$input(
                               type = "text",
                               class = "form-control bg-light border-0",
                               placeholder = "Où habitez-vous ? (Nous voulons juste votre quartier, commune ou département)",
                               style = "height: 55px;"
                             )
                           ),
                           # Médicaments antiépileptiques
                           tags$div(
                             class = "col-12",
                             tags$label("Médicaments :", class = "form-label"),
                             tags$select(
                               class = "form-select bg-light border-0",
                               style = "height: 55px;",
                               tags$option("Choisissez un médicament", selected = TRUE),
                               lapply(antiepileptiques, function(antiepileptique) {
                                 tags$option(antiepileptique, value = antiepileptique)
                               })
                             )
                           ),
                           # Champs pour les doses (matin, midi, soir) alignés sur une ligne
                           tags$div(
                             class = "row",  # Ligne pour aligner les champs
                             # Dose du matin
                             tags$div(
                               class = "col-4",  # Chaque champ occupe un tiers de la ligne
                               tags$label("Quelle est la dose du matin :", class = "form-label"),
                               tags$select(
                                 class = "form-select bg-light border-0",
                                 style = "height: 55px;",
                                 tags$option("Choisissez le nombre", selected = TRUE),
                                 lapply(seq(0.5, 3, by = 0.5), function(x) {
                                   tags$option(x, value = x)
                                 })
                               )
                             ),
                             # Dose de midi
                             tags$div(
                               class = "col-4",
                               tags$label("Quelle est la dose de midi :", class = "form-label"),
                               tags$select(
                                 class = "form-select bg-light border-0",
                                 style = "height: 55px;",
                                 tags$option("Choisissez le nombre", selected = TRUE),
                                 lapply(seq(0.5, 3, by = 0.5), function(x) {
                                   tags$option(x, value = x)
                                 })
                               )
                             ),
                             # Dose du soir
                             tags$div(
                               class = "col-4",
                               tags$label("Quelle est la dose du soir :", class = "form-label"),
                               tags$select(
                                 class = "form-select bg-light border-0",
                                 style = "height: 55px;",
                                 tags$option("Choisissez le nombre", selected = TRUE),
                                 lapply(seq(0.5, 3, by = 0.5), function(x) {
                                   tags$option(x, value = x)
                                 })
                               )
                             )
                           ),
                           # Champ pour le prix du médicament
                           tags$div(
                             class = "row mt-4",  # Nouvelle ligne avec marge en haut
                             tags$div(
                               class = "col-12",  # Largeur complète
                               tags$label("Prix approximatif du médicament :", class = "form-label"),
                               tags$input(
                                 type = "number", 
                                 class = "form-control bg-light border-0",
                                 placeholder = "Entrez le prix en CFA",
                                 style = "height: 55px;"
                               )
                             )
                           ),
                           tags$div(
                             class = "col-12 col-sm-6",
                             tags$input(
                               type = "text",
                               class = "form-control bg-light border-0",
                               placeholder = "Your Name",
                               style = "height: 55px;"
                             )
                           ),
                           tags$div(
                             class = "col-12 col-sm-6",
                             tags$input(
                               type = "email",
                               class = "form-control bg-light border-0",
                               placeholder = "Your Email",
                               style = "height: 55px;"
                             )
                           ),
                           tags$div(
                             class = "col-12 col-sm-6",
                             tags$input(
                               type = "text",
                               class = "form-control bg-light border-0",
                               placeholder = "Date",
                               style = "height: 55px;"
                             )
                           ),
                           tags$div(
                             class = "col-12 col-sm-6",
                             tags$input(
                               type = "text",
                               class = "form-control bg-light border-0",
                               placeholder = "Time",
                               style = "height: 55px;"
                             )
                           ),
                           tags$div(
                             class = "col-12",
                             tags$button(
                               type = "submit",
                               class = "btn btn-primary w-100 py-3",
                               "Make An Appointment"
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
             ),
             
           ),
           "about" = div(
             class = "container mt-5",
             h1("About Us"),
             p("Learn more about our team and mission.")
           ),
           "service" = div(
             class = "container mt-5",
             h1("Our Services"),
             p("Explore the services we provide.")
           ),
           "pricing" = div(
             class = "container mt-5",
             h1("Our Pricing"),
             p("Details about our pricing plans."),
             tags$table(
               class = "table table-striped",
               tags$thead(
                 tags$tr(
                   tags$th("Plan"), tags$th("Price"), tags$th("Details")
                 )
               ),
               tags$tbody(
                 tags$tr(tags$td("Basic"), tags$td("$10/month"), tags$td("Basic support")),
                 tags$tr(tags$td("Pro"), tags$td("$50/month"), tags$td("Premium features")),
                 tags$tr(tags$td("Enterprise"), tags$td("Contact us"), tags$td("Custom solutions"))
               )
             )
           ),
           "blog" = div(
             class = "container mt-5",
             h1("Blog Grid"),
             p("Latest blog articles.")
           ),
           "detail" = div(
             class = "container mt-5",
             h1("Blog Detail"),
             p("Detailed article content.")
           ),
           "team" = div(
             class = "container mt-5",
             h1("Meet the Team"),
             p("Information about our team members.")
           ),
           "testimonial" = div(
             class = "container mt-5",
             h1("Testimonials"),
             p("What our clients say about us.")
           ),
           "appointment" = div(
             class = "container mt-5",
             h1("Make an Appointment"),
             p("Schedule an appointment with us.")
           ),
           "search" = div(
             class = "container mt-5",
             h1("Search"),
             p("Search our website for content.")
           ),
           "contact" = div(
             class = "container mt-5",
             h1("Contact Us"),
             p("Get in touch with us.")
           ),
           # Valeur par défaut pour éviter l'erreur
           div(
             class = "container mt-5",
             h1("404 - Page Not Found"),
             p("The page you are looking for does not exist.")
           )
    )
  })
  
  
  observe({
    print(input$page)
  })
  
  
  
  # Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "data/pharmacies.db")
  
  # Function to fetch and process data from the database
  fetch_data <- function() {
    bb_data <- tbl(con, "pharmacies") %>%
      collect() %>%
      mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude)) %>%
      filter(!is.na(Latitude))
    
    # Add the popup label column
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
    
    return(bb_data)
  }
  
  # Initial data fetch
  bb_data <- fetch_data()
  
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
  
  # Handle form submission
  observeEvent(input$submit, {
    new_entry <- data.frame(
      Blood.Bank.Name = input$name,
      State = input$state,
      City = input$city,
      Address = input$address,
      Pincode = input$pincode,
      Contact.No.1 = input$contact,
      Mobile = input$mobile,
      Helpline = input$helpline,
      Email = input$email,
      Website = input$website,
      Latitude = input$latitude,
      Longitude = input$longitude,
      stringsAsFactors = FALSE
    )
    
    dbWriteTable(con, "pharmacies", new_entry, append = TRUE, row.names = FALSE)
    output$form_response <- renderText("Nouvelle entrée ajoutée avec succès!")
    
    # Refresh the data
    bb_data <<- fetch_data()
    
    # Update the leaflet map
    leafletProxy("bbmap", data = bb_data) %>%
      clearMarkers() %>%
      addCircleMarkers(data = bb_data, lat = ~Latitude, lng = ~Longitude,
                       radius = 3, popup = ~as.character(cntnt),
                       color = ~pal(Category),
                       stroke = FALSE, fillOpacity = 0.8)
    
    # Update the data table
    output$data <- DT::renderDataTable(datatable(
      bb_data[, -c(1, 23, 24, 25, 28:35)], filter = 'top',
      colnames = c("Blood Bank Name", "State", "District", "City", "Address", "Pincode", "Contact No.",
                   "Mobile", "HelpLine", "Fax", "Email", "Website", "Nodal Officer", "Contact of Nodal Officer",
                   "Mobile of Nodal Officer", "Email of Nodal Officer", "Qualification", "Category", "Blood Component Available",
                   "Apheresis", "Service Time", "Lat", "Long.")
    ))
  })
  
  # Disconnect from the database when the server is stopped
  onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
