library(shiny)

shinyServer(function(input, output){
  
  
  output$bolezni <- renderPlot({
    if (input$bolezni == "AIDS") {
      hi <- filter(bolezni, leto == input$leto, bolezen == "hiv") 
      hi$bolezen <- NULL
      hi$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost okuženih z virusom hiv") + 
        labs(caption = "Opomba: AIDS tu v resnici ne pomeni bolezni, pač pa označuje okužene z virusom hiv.")
    } else if (input$bolezni == "kolera") { 
      kol <- filter(bolezni, leto == input$leto, bolezen == "kolera")
      kol$bolezen <- NULL
      kol$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(kol, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost kolere") + 
        labs(caption = "Opomba: Za leta 2004, 2006 in 2007 ni podatkov.")
    } else if (input$bolezni == "malarija") {
      mal <- filter(bolezni, leto == input$leto, bolezen == "malarija") 
      mal$bolezen <- NULL
      mal$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost malarije")
    } else if (input$bolezni == "sifilis") {
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
    if (input$znacilnosti == "dostopnost pitne vode") {
      vo <- filter(znacilnosti, leto == input$leto1, znacilnost == "voda") 
      vo$znacilnost <- NULL
      vo$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(vo, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež populacije z dostopom do izboljšanega pitnega vodnega vira ") + 
        labs(caption = "Opomba: Izboljšan pitni vodni vir predstavlja zasebna in javna vodovodna napeljava, javni hidranti, zavarovani vodnjaki in izviri, zbrana deževnica.")
    } else if (input$znacilnosti == "zdravstvena potrošnja na prebivalca") { 
      de <- filter(znacilnosti, leto == input$leto1, znacilnost == "denar")
      de$znacilnost <- NULL
      de$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje zdravstveno potrošnjo na prebivalca (preračunano na mednarodni dolar, 2011)") + 
        labs(caption = "Opomba: za leto 2015 ni podatkov.")
    } else if (input$znacilnosti == "število plačanih prostih dni") { 
      z <- ggplot() + geom_polygon(data = svet %>% left_join(tabela, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = dopust)) + 
        ggtitle("Zemljevid prikazuje minimalno število plačanih prostih dni (vključno s prazniki) pri petdnevnem delovniku kot ga določa zakon") + 
        labs(caption = "Opomba: Podatki prikazujejo današnje stanje.")
    } else if (input$znacilnosti == "stopnja podhranjenosti") { 
      po <- filter(znacilnosti, leto == input$leto1, znacilnost == "podhranjenost")
      po$znacilnost <- NULL
      po$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(po, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež otrok, mlajših od pet let, s prenizko telesno težo")
    }  else if (input$znacilnosti == "stopnja debelosti") { 
      de1 <- filter(znacilnosti, leto == input$leto1, znacilnost == "debelost")
      de1$znacilnost <- NULL
      de1$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de1, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje prikazuje delež otrok, mlajših od pet let, s previsoko telesno težo")
    } else if (input$znacilnosti == "količina porabljenega alkohola na osebo") { 
      al <- filter(znacilnosti, leto == input$leto1, znacilnost == "alkohol")
      al$znacilnost <- NULL
      al$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(al, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje porabo alkohola med osebami, starejšimi od 15 let, v litrih čistega alkohola")
    } else if (input$znacilnosti == "stopnja kadilcev") { 
      tm <- filter(tobak, leto == input$leto1, spol == "moski")
      tm$znacilnost <- NULL
      tm$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(tm, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež moških, starejših od 15 let, ki kadijo katerikoli tobačni izdelek") + 
        labs(caption = "Opomba: podatki so dostopni samo za leta 2000, 2005, 2010, 2012 in 2015.")
    } else { 
      tz <- filter(tobak, leto == input$leto1, spol == "zenske")
      tz$znacilnost <- NULL
      tz$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(tz, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež žensk, starejših od 15 let, ki kadijo katerikoli tobačni izdelek") + 
        labs(caption = "Opomba: podatki so dostopni samo za leta 2000, 2005, 2010, 2012 in 2015.")
    } 
    z + theme_void() + theme(plot.title = element_text(hjust = 0.5)) + 
      guides(fill=guide_legend(title=NULL), color="none") + 
      scale_fill_gradientn(colours = c("blue", "green", "red"), 
                           values = rescale(c(0, 50, 200, 700)))
  })
  
  })