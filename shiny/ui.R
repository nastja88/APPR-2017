library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Interaktivni prikaz"),
  
  
  tabsetPanel(
    tabPanel("bolezni",
             hr(),
             sidebarLayout(
               
               sidebarPanel(("Izberi željene podatke:"), 
                 selectInput("bolezen", "Bolezen:", 
                             choices = c("AIDS", "kolera", "malarija", "sifilis", "tuberkuloza"),
                             multiple = FALSE),
                 selectInput("leto", "Leto:", choices = 2000:2015, multiple = FALSE)
                 ),
    
    mainPanel(plotOutput("bolezni")))
  
  )),

  
  tabsetPanel(
    tabPanel("značilnosti",
             hr(),
             sidebarLayout(
               
               sidebarPanel(("Izberi željene podatke."),
                            selectInput("znacilnost", "Značilnost:",
                                        choices = c("dostopnost pitne vode", 
                                                    "zdravstvena potrošnja na prebivalca",
                                                    "stopnja podhranjenosti (5-)",
                                                    "stopnja debelosti (5-)",
                                                    "količina porabljenega alkohola na osebo (15+)"),
                                        multiple = FALSE),
                            selectInput("leto", "Leto:", choices = 2000:2015, multiple = FALSE)
                            ),
               
               mainPanel(plotOutput("znacilnosti")))
             
    )
  )

))