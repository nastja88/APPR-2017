library(shiny)

shinyServer(function(input, output){
  
  output$opis <- renderText({
    if (input$bolezen == "AIDS") {
      besedilo <- "Aids ali sindrom pridobljene imunske pomanjkljivosti, je definiran kot skupek simptomov in okužb, ki so posledica izčrpanosti imunskega sistema zaradi okužbe z virusom HIV. Prenaša se z neposrednim stikom sluznice ali krvi s telesnimi tekočinami, ki vsebujejo HIV, npr. med spolnim odnosom, pri transfuzijah krvi, z uporabo okuženih podkožnih igel tako v zdravstvenih ustanovah kot med uživalci mamil; z matere na otroka, med nosečnostjo, rojstvom ali z dojenjem.\n Po doslej znanih podatkih se je aids najverjetneje pojavil v podsaharski Afriki v 20. stoletju, zdaj pa ima razsežnosti svetovne epidemije. Od 5. junija 1981 do leta 2009 je zaradi aidsa umrlo okoli 30 milijonov bolnikov.\n Čeprav lahko z zdravili upočasnimo in vsaj za določen čas zaustavimo potek bolezni, ne obstaja nobeno zdravilo, ki bi okužbo odpravilo, in ne cepivo, ki bi pred njo ščitilo. Protivirusna zdravila znižujejo umrljivost in obolevnost, a so draga in niso dosegljiva vsemu svetovnemu prebivalstvu."
    } else if (input$bolezen == "kolera") {
      besedilo <- "Kolera je akutna nalezljiva epidemična črevesna bolezen, ki jo povzroča bakterija Vibrio cholerae. Glavna simptoma sta driska in bruhanje. Pojavi se lahko odpoved obtočil in šok.\n Okužba se primarno prenaša z onesnaženo pitno vodo in hrano, ki je bila v stiku z blatom okužene osebe. Hudost driske in bruhanja lahko vodi do hitre izsušitve in elektrolitskega neravnovesja in v nekaterih primerih tudi do smrti.\n Kolera verjetno izhaja iz indijske podceline, od pradavnih časov je prisotna v delti Gangesa. Po kopenskih in morskih trgovskih poti se je razširila v Rusijo leta 1817, nato pa po vsej Evropi. Iz Evrope se je razširila v Severno Ameriko. Leta 2010 se je okužilo od 3 do 5 milijonov ljudi, umrlo jih od 100.000 do 130.000."
    } else if (input$bolezen == "malarija") {
      besedilo <- "Malarija je nalezljiva bolezen, ki jo povzročajo nekatere vrste zajedavskih praživali iz razreda trosovcev, plazmodiji. Letno se pojavi približno 350-500 milijonov okužb in od 1-3 milijonov smrti, pretežno v tropih in podsaharski Afriki. Prenašalec malarije je komar mrzličar.\n Bolezenski znaki malarije so mrzlica, drgetanje, bolečine v sklepih, bljuvanje in krči, včasih pa tudi ščemenje kože. Pri težjih oblikah se lahko pojavi koma in, če bolezni ne zdravimo, končno smrt, posebej pri majhnih otrocih. Lahko se pojavijo tudi pozni zapleti, ki privedejo do poškodb osrednjega živčevja ali do odpovedi ledvic pri črnosečni mrzlici.\n Na naslednjem grafu je prikazano število okuženih z malarijo v letih od 2000 do 2015, dodani so tudi kvartili porazdelitve. Opazen je velik trend naraščanja."
    } else if (input$bolezen == "sifilis") {
      besedilo <- "Sifilis je spolno prenosljiva okužba, ki jo povzroča bakterija spiroheta. Glavna pot prenosa je s spolnim stikom; lahko se tudi prenaša z matere na zarodek med nosečnostjo ali ob porodu, kar ima za posledico prirojeni sifilis.\n Znaki in simptomi so trda in neboleča kožna razjeda, ki ne srbi; razpršen izpuščaj, ki pogosto vključuje dlani in podplate; živčnimi in srčnimi simptomi. Sifilis je mogoče učinkovito zdraviti z antibiotiki. Za sifilisom se meni, da je v letu 1999 po vsem svetu zbolelo dodatno 12 milijonov ljudi, pri tem je več kot 90 % primerov bilo v državah v razvoju."
    } else {
      besedilo <- "Tuberkuloza ali jetika (zastarelo tudi sušica) je pogosta in mnogokrat smrtna nalezljiva bolezen, ki jo povzročajo različni mikobakterijski sevi. Tuberkuloza običajno okuži pljuča, lahko pa tudi druge telesne dele. Širi se po zraku, ko ljudje z aktivno tuberkulozno okužbo kašljajo, kihajo ali kako drugače prenašajo izdihano tekočino skozi zrak. Klasični znaki aktivne okužbe s tuberkulozo so kronični kašelj, krvav izmeček, vročica, nočno potenje in hujšanje.\n Ena tretjina svetovnega prebivalstva naj bi bila okuženas tuberkulozo, do novih okužb prihaja letno pri približno 1 % prebivalstva. Porazdelitev pa ni enakomerna, približno 80 % prebivalstva v številnih azijskih in afriških državah je pozitivnega na tuberkulinski test, v ZDA pa je pozitivnih samo 5-10 % prebivalcev. V razvijajočem se svetu se s tuberkulozo zaradi zmanjšane odpornosti okuži vse več ljudi, predvsem zaradi visoke ravni okužbe s HIV-om in z njo povezanim razvojem AIDS-a."
    } 
    besedilo
  })
  
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
        ggtitle("Zemljevid prikazuje razširjenost kolere") + 
        labs(caption = "Opomba: Za leta 2004, 2006 in 2007 ni podatkov.")
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
      vo <- filter(znacilnosti, leto == input$leto1, znacilnost == "voda") 
      vo$znacilnost <- NULL
      vo$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(vo, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež populacije z dostopom do izboljšanega pitnega vodnega vira ") + 
        labs(caption = "Opomba: Izboljšan pitni vodni vir predstavlja zasebna in javna vodovodna napeljava, javni hidranti, zavarovani vodnjaki in izviri, zbrana deževnica.")
    } else if (input$znacilnost == "zdravstvena potrošnja na prebivalca") { 
      de <- filter(znacilnosti, leto == input$leto1, znacilnost == "denar")
      de$znacilnost <- NULL
      de$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje zdravstveno potrošnjo na prebivalca (preračunano na mednarodni dolar, 2011)") + 
        labs(caption = "Opomba: za leto 2015 ni podatkov.")
    } else if (input$znacilnost == "število plačanih prostih dni") { 
      z <- ggplot() + geom_polygon(data = svet %>% left_join(tabela, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = dopust)) + 
        ggtitle("Zemljevid prikazuje minimalno število plačanih prostih dni (vključno s prazniki) pri petdnevnem delovniku kot ga določa zakon") + 
        labs(caption = "Opomba: Podatki prikazujejo današnje stanje.")
    } else if (input$znacilnost == "stopnja podhranjenosti") { 
      po <- filter(znacilnosti, leto == input$leto1, znacilnost == "podhranjenost")
      po$znacilnost <- NULL
      po$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(po, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje delež otrok, mlajših od pet let, s prenizko telesno težo")
    }  else if (input$znacilnost == "stopnja debelosti") { 
      de1 <- filter(znacilnosti, leto == input$leto1, znacilnost == "debelost")
      de1$znacilnost <- NULL
      de1$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(de1, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje prikazuje delež otrok, mlajših od pet let, s previsoko telesno težo")
    } else if (input$znacilnost == "količina porabljenega alkohola na osebo") { 
      al <- filter(znacilnosti, leto == input$leto1, znacilnost == "alkohol")
      al$znacilnost <- NULL
      al$leto <- NULL
      z <- ggplot() + geom_polygon(data = svet %>% left_join(al, by = c("sovereignt" = "drzava")),
                                   aes(x = long, y = lat, group = group, color = "black", 
                                       fill = pojavnost)) + 
        ggtitle("Zemljevid prikazuje porabo alkohola med osebami, starejšimi od 15 let, v litrih čistega alkohola")
    } else if (input$znacilnost == "stopnja kadilcev") { 
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