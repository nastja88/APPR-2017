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


kv <- lm(data = alko, povprecje ~ leto + I(leto^2)) %>% 
  predict(data.frame(leto=seq(2016, 2025, 1))) 
kv <- data_frame(kv) %>% rename(predikcija = kv)
kv$leto <- 2016:2025
kv <- kv[c(2,1)]


rm(alk, den, vod, zenske, moski)



