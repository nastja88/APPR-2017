library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Razširjenost nekaterih bolezni"),
  
  sidebarLayout(
    sidebarPanel(("Izberi željene podatke:"), 
                 selectInput("bolezen", "Bolezen:", 
                             choices = c("AIDS", "kolera", "malarija", "sifilis", "tuberkuloza"),
                             multiple = FALSE),
                 selectInput("leto", "Leto:", choices = 2000:2015, multiple = FALSE),
                 actionButton("do", "Potrdi")
                 ),
    
    mainPanel(plotOutput("zemljevid")))
  
))
