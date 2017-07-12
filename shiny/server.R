library(shiny)

shinyServer(function(input, output){
  output$zemljevid <- renderPlot({
    if (input$bolezen == "AIDS") {
      hi <- filter(bolezni, leto == n, bolezen == "hiv") 
      hi$bolezen <- NULL
      hi$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost okuženih z virusom hiv") + 
        labs(caption = "Opomba: AIDS tu v resnici ne pomeni bolezni, pač pa označuje okužene z virusom hiv.")
    } else if (input$bolezen == "kolera") { 
      kol <- filter(bolezni, leto == n, bolezen == "kolera")
      kol$bolezen <- NULL
      kol$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(kol, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost kolere")
    } else if (input$bolezen == "malarija") {
      mal <- filter(bolezni, leto == n, bolezen == "malarija") 
      mal$bolezen <- NULL
      mal$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost malarije")
    } else if (input$bolezen == "sifilis") {
      sif <- filter(bolezni, leto == n, bolezen == "sifilis")
      sif$bolezen <- NULL
      sif$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(sif, by = c("sovereignt" = "drzava")),
                                       aes(x = long, y = lat, group = group, color = "black", 
                                           fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje razširjenost prirojenega sifilisa") 
    } else {
      tub <- filter(bolezni, leto == n, bolezen == "tuberkuloza")
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
  })})