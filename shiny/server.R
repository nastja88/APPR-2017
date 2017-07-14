library(shiny)

shinyServer(function(input, output){
  
  output$opis <- renderUI({
    if (input$bolezen == "AIDS") {
      besedilo <- c("Aids ali sindrom pridobljene imunske pomanjkljivosti, je definiran kot skupek simptomov in okužb, ki so posledica izčrpanosti imunskega sistema zaradi okužbe z virusom HIV. Prenaša se z neposrednim stikom sluznice ali krvi s telesnimi tekočinami, ki vsebujejo HIV, npr. med spolnim odnosom, pri transfuzijah krvi, z uporabo okuženih podkožnih igel tako v zdravstvenih ustanovah kot med uživalci mamil; z matere na otroka med nosečnostjo, rojstvom ali z dojenjem.", 
                    "Po doslej znanih podatkih se je aids najverjetneje pojavil v podsaharski Afriki v 20. stoletju, zdaj pa ima razsežnosti svetovne epidemije. Od 5. junija 1981 do leta 2009 je zaradi aidsa umrlo okoli 30 milijonov bolnikov.", 
                    "Čeprav lahko z zdravili upočasnimo in vsaj za določen čas zaustavimo potek bolezni, ne obstaja nobeno zdravilo, ki bi okužbo odpravilo, in ne cepivo, ki bi pred njo ščitilo. Protivirusna zdravila znižujejo umrljivost in obolevnost, a so draga in niso dosegljiva vsemu svetovnemu prebivalstvu.")
    } else if (input$bolezen == "kolera") {
      besedilo <- c("Kolera je akutna nalezljiva epidemična črevesna bolezen, ki jo povzroča bakterija Vibrio cholerae. Glavna simptoma sta driska in bruhanje. Pojavi se lahko odpoved obtočil in šok.", 
                    "Okužba se primarno prenaša z onesnaženo pitno vodo in hrano, ki je bila v stiku z blatom okužene osebe. Hudost driske in bruhanja lahko vodi do hitre izsušitve in elektrolitskega neravnovesja in v nekaterih primerih tudi do smrti.", 
                    "Kolera verjetno izhaja iz indijske podceline, od pradavnih časov je prisotna v delti Gangesa. Po kopenskih in morskih trgovskih poti se je razširila v Rusijo leta 1817, nato pa po vsej Evropi. Iz Evrope se je razširila v Severno Ameriko. Leta 2010 se je z njo okužilo od 3 do 5 milijonov ljudi, umrlo jih od 100.000 do 130.000.")
    } else if (input$bolezen == "malarija") {
      besedilo <- c("Malarija je nalezljiva bolezen, ki jo povzročajo nekatere vrste zajedavskih praživali iz razreda trosovcev, plazmodiji. Letno se pojavi približno 350-500 milijonov okužb in 1-3 milijonov smrti, pretežno v tropih in podsaharski Afriki. Prenašalec malarije je komar mrzličar.", 
                    "Bolezenski znaki malarije so mrzlica, drgetanje, bolečine v sklepih, bljuvanje in krči, včasih pa tudi ščemenje kože. Pri težjih oblikah se lahko pojavi koma in, če bolezni ne zdravimo, končno smrt, posebej pri majhnih otrocih. Lahko se pojavijo tudi pozni zapleti, ki privedejo do poškodb osrednjega živčevja ali do odpovedi ledvic pri črnosečni mrzlici.")
    } else if (input$bolezen == "prirojeni sifilis") {
      besedilo <- c("Sifilis je spolno prenosljiva okužba, ki jo povzroča bakterija spiroheta. Glavna pot prenosa je s spolnim stikom; lahko pa se prenese tudi z matere na zarodek med nosečnostjo ali ob porodu, kar ima za posledico prirojeni sifilis.", 
                    "Znaki in simptomi so trda in neboleča kožna razjeda, ki ne srbi; razpršen izpuščaj, ki pogosto vključuje dlani in podplate; živčni in srčni simptomi. Sifilis je mogoče učinkovito zdraviti z antibiotiki. Za sifilisom je v letu 1999 po vsem svetu zbolelo dodatno 12 milijonov ljudi, pri tem je več kot 90 % primerov bilo v državah v razvoju.")
    } else {
      besedilo <- c("Tuberkuloza ali jetika (zastarelo tudi sušica) je pogosta in mnogokrat smrtna nalezljiva bolezen, ki jo povzročajo različni mikobakterijski sevi. Tuberkuloza običajno okuži pljuča, lahko pa tudi druge telesne dele. Širi se po zraku, ko ljudje z aktivno tuberkulozno okužbo kašljajo, kihajo ali kako drugače prenašajo izdihano tekočino skozi zrak. Klasični znaki aktivne okužbe s tuberkulozo so kronični kašelj, krvav izmeček, vročica, nočno potenje in hujšanje.", 
                    "Ena tretjina svetovnega prebivalstva naj bi bila okužena s tuberkulozo, do novih okužb prihaja letno pri približno 1 % prebivalstva. Porazdelitev pa ni enakomerna, približno 80 % prebivalstva v številnih azijskih in afriških državah je pozitivnega na tuberkulinski test, v ZDA pa je pozitivnih samo 5-10 % prebivalcev. V razvijajočem se svetu se s tuberkulozo zaradi zmanjšane odpornosti okuži vse več ljudi, predvsem zaradi visoke ravni okužbe s HIV-om in z njo povezanim razvojem AIDS-a.")
    } 
    besedilo %>% lapply(. %>% list(br())) %>% p()
  })
  
  
  output$povezava <- renderUI({
    if (input$bolezen == "AIDS") {
      link <- "https://sl.wikipedia.org/wiki/Aids"
    } else if (input$bolezen == "kolera") {
      link <- "https://sl.wikipedia.org/wiki/Kolera"
    } else if (input$bolezen == "malarija") {
      link <- "https://sl.wikipedia.org/wiki/Malarija"
    } else if (input$bolezen == "prirojeni sifilis") {
      link <- "https://sl.wikipedia.org/wiki/Sifilis"
    } else {
      link <- "https://sl.wikipedia.org/wiki/Tuberkuloza"
    } 
    p("Povzeto po:", a(href=link, link), ".")
  })
  
  
  output$bolezni <- renderPlot({
    if (input$bolezen == "AIDS") {
      hi <- filter(bolezni, leto == input$leto, bolezen == "hiv") 
      hi$bolezen <- NULL
      hi$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Razširjenost virusa HIV") + guides(fill = guide_colorbar(title = "Delež")) +
        labs(caption = "Opomba: AIDS tu v resnici ne pomeni bolezni, pač pa okuženost z virusom hiv.")
    } else if (input$bolezen == "kolera") { 
      kol <- filter(bolezni, leto == input$leto, bolezen == "kolera")
      kol$bolezen <- NULL
      kol$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(kol, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Razširjenost kolere") + guides(fill = guide_colorbar(title = "Pojavnost")) +
        labs(caption = "Opomba: Za leta 2004, 2006 in 2007 ni podatkov.")
    } else if (input$bolezen == "malarija") {
      mal <- filter(bolezni, leto == input$leto, bolezen == "malarija") 
      mal$bolezen <- NULL
      mal$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Razširjenost malarije") + guides(fill = guide_colorbar(title = "Pojavnost")) 
    } else if (input$bolezen == "prirojeni sifilis") {
      sif <- filter(bolezni, leto == input$leto, bolezen == "sifilis")
      sif$bolezen <- NULL
      sif$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(sif, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Razširjenost prirojenega sifilisa") + 
        guides(fill = guide_colorbar(title = "Delež")) +
        labs(caption = "Opomba: Podatki o prirojenem sifilisu so dostopni zgolj za leti 2012 in 2013.")
    } else {
      tub <- filter(bolezni, leto == input$leto, bolezen == "tuberkuloza")
      tub$bolezen <- NULL
      tub$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(tub, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Razširjenost tuberkuloze") + guides(fill = guide_colorbar(title = "Delež")) 
    }
    z + theme_void() + theme(plot.title = element_text(hjust = 0.5)) + 
      scale_fill_gradientn(colours = c("blue", "green", "red"), 
                           values = rescale(c(0, 50, 200, 700)))
  })
  
  
  output$graf <- renderPlot({
    slovar <- c("Afghanistan" = "Afganistan", "Botswana" = "Bocvana", "Brazil" = "Brazilija", 
                "Central African Republic" = "Srednjeafriška republika", "Colombia" = "Kolumbija", 
                "Federated States of Micronesia" = "Federativne države Mikronezije", 
                "Ghana" = "Gana", "Grenada" = "Grenada", "Haiti" = "Haiti", "India" = "Indija", 
                "Lesotho" = "Lesoto", "Mozambique" = "Mozambik", "Namibia" = "Namibija", 
                "Nauru" = "Nauru", "North Korea" = "Severna Koreja", "Somalia" = "Somalija", 
                "South Africa" = "Južna Afrika", "Swaziland" = "Svazi", "Uganda" = "Uganda", 
                "United Republic of Tanzania" = "Tanzanija", "Zimbabwe" = "Zimbabve")
    if (input$bolezen1 == "AIDS") {
      aids <- filter(bolezni, bolezen == "hiv") %>% 
        filter(drzava == "Botswana" | drzava == "Lesotho" | drzava == "South Africa" | 
                 drzava == "Swaziland" | drzava == "Zimbabwe")
      aids$bolezen <- NULL
      g <- ggplot(aids) + 
        aes(x = leto, y = pojavnost, group = drzava, color = slovar[parse_character(drzava)]) + 
        geom_line() + labs(y = "delež okuženih z virusom hiv")
    } else if (input$bolezen1 == "kolera") {
      kole <- filter(bolezni, bolezen == "kolera") %>% 
        filter(drzava == "Afghanistan" | drzava == "Haiti" | drzava == "South Africa" | 
                 drzava == "Somalia" | drzava == "Zimbabwe")
      kole$bolezen <- NULL
      g <- ggplot(kole)  +
        aes(x = leto, y = pojavnost / 1000000, group = drzava, 
            color = slovar[parse_character(drzava)]) + geom_line() + 
        labs(y = "število obolelih za kolero (v milijonih)")
    } else if (input$bolezen1 == "malarija") {
      mala <- filter(bolezni, bolezen == "malarija") %>% 
        filter(drzava == "Ghana" | drzava == "India" | drzava == "Mozambique" | drzava == "Uganda" | 
                 drzava == "United Republic of Tanzania")
      mala$bolezen <- NULL
      g <- ggplot(mala) + aes(x = leto, y = pojavnost / 1000000, group = drzava, 
                              color = slovar[parse_character(drzava)]) + geom_line() +
        labs(y = "število obolelih za malarijo (v milijonih)")
    } else if (input$bolezen1 == "prirojeni sifilis") {
      sifi <- filter(bolezni, bolezen == "sifilis") %>% 
        filter(drzava == "Brazil" | drzava == "Colombia" | drzava == "Grenada" |
                 drzava == "Federated States of Micronesia" |  drzava == "Nauru")
      sifi$bolezen <- NULL
      g <- ggplot(sifi) + aes(x = leto, y = pojavnost, 
                              color =  slovar[parse_character(drzava)]) + geom_point() + theme_bw() + 
        labs(y = "delež obolelih za prirojenim sifilisom") + 
        guides(color = guide_legend(title = "Država"))
    } else {
      tube <- filter(bolezni, bolezen == "tuberkuloza") %>% 
        filter(drzava == "Central African Republic" | drzava == "Namibia" |
                 drzava == "North Korea" | drzava == "South Africa" |  drzava == "Swaziland")
      tube$bolezen <- NULL
      g <- ggplot(tube) + aes(x = leto, y = pojavnost, group = drzava,
                              color =  slovar[parse_character(drzava)]) + geom_line() + 
        labs(y = "delež obolelih za tuberkulozo")
    } 
    g  + geom_point() + theme_bw() + guides(color = guide_legend(title = "Država"))
  })
  
  
  output$razlaga <- renderUI({
    if (input$bolezen1 == "AIDS") {
      besedilo <- p("Iz zgornjega grafa je razvidno skrb vzbujajoče dejstvo, da se delež okuženih z virusom HIV ponekod giblje kar okoli ene četrtine. Se pa na srečo v večini (afriških) držav zmanjšuje. Veliko izjemo predstavlja država Svazi, kjer se le ta vztrajno povečuje.")
    } else if (input$bolezen1 == "kolera") {
      besedilo <- p("V letih 2010-2013 je bilo število obolelih za kolero daleč največje na Haitiju. To pripisujem katastrofalnemu potresu 12. 1. 2010 z močjo 7. stopnje po Richterjevi lestvici. Zabeleženih je bilo tudi vsaj 33 popotresnih sunkov, od tega 14 z magnitudo med 5,0 in 5,9.", hr(), a(href="https://sl.wikipedia.org/wiki/Potres_na_Haitiju_(2010)", "https://sl.wikipedia.org/wiki/Potres_na_Haitiju_(2010)"))
    } else if (input$bolezen1 == "malarija") {
      besedilo <- p("Najhujše razmere so trenutno v Mozambiku in Ugandi, kjer se je med letoma 2013 in 2015 število obolelih za malarijo povečalo kar za 5 milijonov. Obolevnost narašča tudi v Gani, v Tanzaniji in Indiji (kjer je bilo leta 2000 najslabše stanje) pa je opazen trend padanja.")
    } else if (input$bolezen1 == "prirojeni sifilis") {
      besedilo <- p("Za prirojeni sifilis so dostopni podatki samo za leti 2012 in 2013. Prikazane so zgolj države, ki imajo najvišji delež obolelih. Najslabše stanje je bilo v Grenadi.")
    } else {
      besedilo <- p("Večino časa (med letoma 2002 in 2013) je bilo najslabše stanje v Svaziju, po najnovejših podatkih pa v Južni Afriki. Opazno je, da se bolezen seli iz južnega dela Afrike, npr. v Severno Korejo.")
    } 
    besedilo
  })
  
  
  output$znacilnosti <- renderPlot({
    if (input$znacilnost == "dostopnost pitne vode") {
      vo <- filter(znacilnosti, leto == input$leto1, znacilnost == "voda") 
      vo$znacilnost <- NULL
      vo$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(vo, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Dostop do izboljšanega pitnega vodnega vira*") + 
        guides(fill = guide_colorbar(title = "Delež populacije")) +
        labs(caption = "*vodovodna napeljava, javni hidranti, zavarovani vodnjaki in izviri, zbrana deževnica")
    } else if (input$znacilnost == "zdravstvena potrošnja na prebivalca") { 
      de <- filter(znacilnosti, leto == input$leto1, znacilnost == "denar")
      de$znacilnost <- NULL
      de$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(de, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Zdravstvena potrošnja na prebivalca (mednarodni dolar, 2011)") + 
        labs(caption = "Opomba: za leto 2015 ni podatkov.") + 
        guides(fill = guide_colorbar(title = "Vsota"))
    } else if (input$znacilnost == "število plačanih prostih dni") { 
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(tabela, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = dopust)) +
        guides(fill = guide_colorbar(title = "Število")) +
        ggtitle("Minimalno število plačanih prostih dni pri petdnevnem delovniku") + 
        labs(caption = "Opomba: Podatki prikazujejo današnje stanje. Korelacija med tem in razširjenostjo bolezni ni opazna.")
    } else if (input$znacilnost == "stopnja podhranjenosti") { 
      po <- filter(znacilnosti, leto == input$leto1, znacilnost == "podhranjenost")
      po$znacilnost <- NULL
      po$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(po, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        ggtitle("Delež otrok, mlajših od pet let, s prenizko telesno težo") + 
        guides(fill = guide_colorbar(title = "Delež"))
    }  else if (input$znacilnost == "stopnja debelosti") { 
      de1 <- filter(znacilnosti, leto == input$leto1, znacilnost == "debelost")
      de1$znacilnost <- NULL
      de1$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(de1, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) +  
        ggtitle("Delež otrok, mlajših od pet let, s previsoko telesno težo") + 
        guides(fill = guide_colorbar(title = "Delež"))
    } else if (input$znacilnost == "količina porabljenega alkohola na osebo") { 
      al <- filter(znacilnosti, leto == input$leto1, znacilnost == "alkohol")
      al$znacilnost <- NULL
      al$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(al, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        guides(fill = guide_colorbar(title = "Količina")) +
        ggtitle("Poraba alkohola med osebami, starejšimi od 15 let, v litrih čistega alkohola") + 
        labs(caption = "Opomba: Opazno je, da je v muslimanskih državah poraba alkohola izredno majhna.")
    } else if (input$znacilnost == "stopnja kadilcev") { 
      tm <- filter(tobak, leto == input$leto1, spol == "moski")
      tm$znacilnost <- NULL
      tm$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(tm, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) +  
        guides(fill = guide_colorbar(title = "Delež")) +
        ggtitle("Delež moških, starejših od 15 let, ki kadijo katerikoli tobačni izdelek") + 
        labs(caption = "Opomba: podatki so dostopni samo za leta 2000, 2005, 2010, 2012 in 2015.")
    } else { 
      tz <- filter(tobak, leto == input$leto1, spol == "zenske")
      tz$znacilnost <- NULL
      tz$leto <- NULL
      z <- ggplot() + 
        geom_polygon(data = svet %>% left_join(tz, by = c("sovereignt" = "drzava")), 
                     color = "black", aes(x = long, y = lat, group = group, fill = pojavnost)) + 
        guides(fill = guide_colorbar(title = "Delež")) +
        ggtitle("Delež žensk, starejših od 15 let, ki kadijo katerikoli tobačni izdelek") + 
        labs(caption = "Opomba: podatki so dostopni samo za leta 2000, 2005, 2010, 2012 in 2015.")
    } 
    z + theme_void() + theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_gradientn(colours = c("blue", "green", "red"), 
                           values = rescale(c(0, 50, 200, 700)))
  })
  
  })