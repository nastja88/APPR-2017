# 4. faza: Analiza podatkov

n <- 2010

# države po skupinah glede na značilnosti
alk <- filter(znacilnosti, leto == n, znacilnost == "alkohol") %>% rename(alkohol = pojavnost)
alk$znacilnost <- NULL
alk$leto <- NULL
den <- filter(znacilnosti, leto == n, znacilnost == "denar") %>% rename(denar = pojavnost)
den$znacilnost <- NULL
den$leto <- NULL
vod <- filter(znacilnosti, leto == n, znacilnost == "voda") %>% rename(voda = pojavnost)
vod$znacilnost <- NULL
vod$leto <- NULL

zna <- inner_join(alk, den) %>% inner_join(vod)
zna1<- zna
row.names(zna) <- zna$drzava
zna$drzava <- NULL

sk <- scale(zna) %>% kmeans(5, nstart = 1000)
skupine <- data.frame(drzava = zna1$drzava, skupina = factor(sk$cluster))
g_zna <- ggplot() + geom_polygon(data = svet %>% left_join(skupine, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, color = "black", fill = skupina)) + 
  theme_void() + ggtitle("Države po podobnih značilnostih") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none")

# države po skupinah glede na razširjenost uporabe tobačnih izdelkov
zenske <- filter(tobak, leto == n, spol == "zenske") %>% rename(zenske = pojavnost)
zenske$spol <- NULL
zenske$leto <- NULL
moski <- filter(tobak, leto == n, spol == "moski") %>% rename(moski = pojavnost)
moski$spol <- NULL
moski$leto <- NULL

to <- inner_join(zenske, moski)
to1 <- to
row.names(to) <- to$drzava
to$drzava <- NULL

sk0 <- scale(to) %>% kmeans(5, nstart = 1000)
skupine0 <- data.frame(drzava = to1$drzava, skupina0 = factor(sk0$cluster))
g_tob <- ggplot() + geom_polygon(data = svet %>% left_join(skupine0, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, color = "black", 
                            fill = skupina0)) + 
  theme_void() + ggtitle("Države po podobni razširjenosti uporabe tobačnih izdelkov") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none")


# države po skupinah glede na bolezni
hi <- filter(bolezni, leto == n, bolezen == "hiv") 
hi$bolezen <- NULL
hi$leto <- NULL
kol <- filter(bolezni, leto == n, bolezen == "kolera")
kol$bolezen <- NULL
kol$leto <- NULL
mal <- filter(bolezni, leto == n, bolezen == "malarija") 
mal$bolezen <- NULL
mal$leto <- NULL
sif <- filter(bolezni, leto == n, bolezen == "sifilis")
sif$bolezen <- NULL
sif$leto <- NULL
tub <- filter(bolezni, leto == n, bolezen == "tuberkuloza")
tub$bolezen <- NULL
tub$leto <- NULL



# zemljevid razširjenosti kolere
g_kol <- ggplot() + geom_polygon(data = svet %>% left_join(kol, by = c("sovereignt" = "drzava")),
                                 aes(x = long, y = lat, group = group, color = "black", 
                                     fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost kolere") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# zemljevid razširjenosti okuženih z virusom hiv
g_hiv <- ggplot() + geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, color = "black", 
                            fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost okuženih z virusom hiv") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# zemljevid razširjenosti malarije
g_mal <- ggplot() + geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, color = "black", 
                            fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost malarije") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# zemljevid razširjenosti sifilisa
g_sif <- ggplot() + geom_polygon(data = svet %>% left_join(sif, by = c("sovereignt" = "drzava")),
                                 aes(x = long, y = lat, group = group, color = "black", 
                                     fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost sifilisa") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# zemljevid razširjenosti tuberkuloze
g_tub <- ggplot() + geom_polygon(data = svet %>% left_join(tub, by = c("sovereignt" = "drzava")),
                                 aes(x = long, y = lat, group = group, color = "black", 
                                     fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost tuberkuloze") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# razširjenost malarije v določenih državah
slovar <- c("Ghana" = "Gana", "India" = "Indija", "Mozambique" = "Mozambik", 
            "Uganda" = "Uganda", "United Republic of Tanzania" = "Tanzanija")
mala <- filter(bolezni, bolezen == "malarija") %>% 
  filter(drzava == "Ghana" | drzava == "India" | drzava == "Mozambique" | drzava == "Uganda" | 
              drzava == "United Republic of Tanzania")
mala$bolezen <- NULL
g_mal2 <- ggplot(mala) + aes(x = leto, y = pojavnost / 1000000, group = drzava, 
                   color = slovar[parse_character(drzava)]) + geom_point() + 
  geom_line() + theme_bw() + labs(y = "število obolelih za malarijo (v milijonih)") + 
  guides(color = guide_legend(title = "Država"))


rm(alk, den, vod, hi, mal, tub, zenske, moski)



