library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Interaktivni prikaz"),
  
  
  tabsetPanel(
    tabPanel("bolezni (opis)",
             hr(),
             sidebarLayout(
               
               sidebarPanel(("Izberi željene podatke:"), 
                 selectInput("bolezen", "Bolezen:", 
                             choices = c("AIDS", "kolera", "malarija", "prirojeni sifilis", "tuberkuloza"),
                             multiple = FALSE),
                 selectInput("leto", "Leto:", choices = 2000:2015, multiple = FALSE),
                 p("Za izbrano bolezen je prikazan kratek opis značilnosti. Sledi še zemljevid pojavnosti bolezni po državah v izbranem letu. (Za sivo obarvane države ni podatka.)")
                 ),
    
              mainPanel(uiOutput("opis"), uiOutput("povezava"), hr(), plotOutput("bolezni")))),
    
    
    tabPanel("bolezni (graf)",
             hr(),
             sidebarLayout(
               
               sidebarPanel(("Izberi željen podatek:"),
                            selectInput("bolezen1", "Bolezen:",
                                        choices = c("AIDS", "kolera", "malarija", "prirojeni sifilis", "tuberkuloza"),
                                        multiple = FALSE),
                            p("Za izbrano bolezen je prikazan graf, ki prikazuje gibanje razširjenosti bolezni v petih državah, kjer je ta bolezen najpogostejša.")
                            ),
               
               mainPanel(plotOutput("graf"), hr(), uiOutput("razlaga"))
             )),
  
    
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
                            selectInput("leto1", "Leto:", choices = 2000:2015, 
                                        multiple = FALSE),
                            p("Za izbrano značilnost je prikazan zemljevid z željenimi podatki. (Za sivo obarvane države ni podatka.)")
                            ),
               
               mainPanel(plotOutput("znacilnosti")))
             
    )
)
))