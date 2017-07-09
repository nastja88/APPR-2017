# 4. faza: Analiza podatkov

n <- 2000

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

zna <- full_join(alk, den) %>% full_join(vod)
zna <- na.omit(zna)
zna1<- zna
row.names(zna) <- zna$drzava
zna$drzava <- NULL

sk <- scale(zna) %>% kmeans(5, nstart = 1000)
skupine <- data.frame(drzava = zna1$drzava, skupina = factor(sk$cluster))
ggplot() + geom_polygon(data = svet %>% left_join(skupine, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = skupina)) + 
  theme_void() + ggtitle("Države po podobnih značilnostih") + 
  theme(plot.title = element_text(hjust = 0.5))

# države po skupinah glede na bolezni
hi <- filter(bolezni, leto == n, bolezen == "hiv") 
hi$bolezen <- NULL
hi$leto <- NULL
mal <- filter(bolezni, leto == n, bolezen == "malarija") 
mal$bolezen <- NULL
mal$leto <- NULL
tub <- filter(bolezni, leto == n, bolezen == "tuberkuloza")
tub$bolezen <- NULL
tub$leto <- NULL

# zemljevid razširjenosti tuberkuloze
ggplot() + geom_polygon(data = svet %>% left_join(tub, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost tuberkuloze") + 
  theme(plot.title = element_text(hjust = 0.5))

# zemljevid razširjenosti okuženih z virusom hiv
ggplot() + geom_polygon(data = svet %>% left_join(hi, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost okuženih z virusom hiv") + 
  theme(plot.title = element_text(hjust = 0.5))

# zemljevid razširjenosti malarije
ggplot() + geom_polygon(data = svet %>% left_join(mal, by = c("sovereignt" = "drzava")),
                        aes(x = long, y = lat, group = group, fill = pojavnost)) + 
  theme_void() + ggtitle("Razširjenost malarije") + 
  theme(plot.title = element_text(hjust = 0.5))

rm(alk, den, vod, hi, mal, tub)