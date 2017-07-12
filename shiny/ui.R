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
    
    mainPanel(textOutput("opis"), plotOutput("bolezni")))
  
  )),

  
  tabsetPanel(
    tabPanel("značilnosti",
             hr(),
             sidebarLayout(
               
               sidebarPanel(("Izberi željene podatke."),
                            selectInput("znacilnost", "Značilnost:",
                                        choices = c("dostopnost pitne vode", 
                                                    "zdravstvena potrošnja na prebivalca",
                                                    "število plačanih prostih dni",
                                                    "stopnja podhranjenosti",
                                                    "stopnja debelosti",
                                                    "količina porabljenega alkohola na osebo",
                                                    "stopnja kadilcev",
                                                    "stopnja kadilk"),
                                        multiple = FALSE),
                            selectInput("leto1", "Leto:", choices = 2000:2015, multiple = FALSE)
                            ),
               
               mainPanel(plotOutput("znacilnosti")))
             
    )
  )

))