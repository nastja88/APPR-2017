library(shiny)

shinyServer(function(input, output){
  
  
  output$bolezni <- renderPlot({
    if (input$bolezen == "AIDS") {
      hi <- filter(bolezni, leto == input$leto, bolezen == "hiv") 
      hi$bolezen <- NULL
      hi$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost okuženih z virusom hiv") + 
        labs(caption = "Opomba: AIDS tu v resnici ne pomeni bolezni, pač pa označuje okužene z virusom hiv.")
    } else if (input$bolezen == "kolera") { 
      kol <- filter(bolezni, leto == input$leto, bolezen == "kolera")
      kol$bolezen <- NULL
      kol$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(kol, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost kolere")
    } else if (input$bolezen == "malarija") {
      mal <- filter(bolezni, leto == input$leto, bolezen == "malarija") 
      mal$bolezen <- NULL
      mal$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost malarije")
    } else if (input$bolezen == "sifilis") {
      sif <- filter(bolezni, leto == input$leto, bolezen == "sifilis")
      sif$bolezen <- NULL
      sif$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(sif, by = c("sovereignt" = "drzava")),
                                       aes(x = long, y = lat, group = group, color = "black", 
                                           fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost prirojenega sifilisa") + 
        labs(caption = "Opomba: Podatki o sifilisu so dostopni zgolj za leti 2012 in 2013.")
    } else {
      tub <- filter(bolezni, leto == input$leto, bolezen == "tuberkuloza")
      tub$bolezen <- NULL
      tub$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(tub, by = c("sovereignt" = "drzava")),
                                    aes(x = long, y = lat, group = group, color = "black", 
                                        fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost tuberkuloze")
    }
    z + theme_void() + theme(plot.title = element_text(hjust = 0.5)) + 
      guides(fill=guide_legend(title=NULL), color="none") + 
      scale_fill_gradientn(colours = c("blue", "green", "red"), 
                           values = rescale(c(0, 50, 200, 700)))
  })
  
  
  output$znacilnosti <- renderPlot({
    if (input$znacilnost == "dostopnost pitne vode") {
      vo <- filter(znacilnosti, leto == input$leto, znacilnost == "voda") 
      vo$znacilnost <- NULL
      vo$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(vo, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež populacije z dostopom do izboljšanega pitnega vodnega vira ") + 
        labs(caption = "Opomba: Izboljšan pitni vodni vir predstavlja zasebna in javna vodovodna napeljava, javni hidranti, zavarovani vodnjaki in izviri, zbrana deževnica.")
    } else if (input$znacilnost == "zdravstvena potrošnja na prebivalca") { 
      de <- filter(znacilnosti, leto == input$leto, znacilnost == "denar")
      de$znacilnost <- NULL
      de$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje zdravstveno potrošnjo na prebivalca (preračunano na mednarodni dolar, 2011)")
    } else if (input$znacilnost == "stopnja podhranjenosti (5-)") { 
      po <- filter(znacilnosti, leto == input$leto, znacilnost == "podhranjenost")
      po$znacilnost <- NULL
      po$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(po, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje prikazuje delež otrok, mlajših od pet let, s prenizko telesno težo")
    }  else if (input$znacilnost == "stopnja debelosti (5-)") { 
      de1 <- filter(znacilnosti, leto == input$leto, znacilnost == "debelost")
      de1$znacilnost <- NULL
      de1$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de1, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje prikazuje delež otrok, mlajših od pet let, s previsoko telesno težo")
    } else { 
      al <- filter(znacilnosti, leto == input$leto, znacilnost == "alkohol")
      al$znacilnost <- NULL
      al$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(al, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje prikazuje porabo alkohola med osebami, starejšimi od 15 let, v litrih čistega alkohola")
    } 
    z + theme_void() + theme(plot.title = element_text(hjust = 0.5)) + 
      guides(fill=guide_legend(title=NULL), color="none") + 
      scale_fill_gradientn(colours = c("blue", "green", "red"), 
                           values = rescale(c(0, 50, 200, 700)))
  })
  
  })