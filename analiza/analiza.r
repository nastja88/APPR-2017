# 4. faza: Analiza podatkov

n <- 2010

# države po skupinah glede na značilnosti v letu 2000
den <- filter(znacilnosti, leto == n, znacilnost == "denar") %>% rename(denar = pojavnost)
den$znacilnost <- NULL
den$leto <- NULL
vod <- filter(znacilnosti, leto == n, znacilnost == "voda") %>% rename(voda = pojavnost)
vod$znacilnost <- NULL
vod$leto <- NULL

zna <- inner_join(den, vod)
zna1 <- zna
row.names(zna) <- zna$drzava
zna$drzava <- NULL

sk <- scale(zna) %>% kmeans(5, nstart = 1000)
skupine <- data.frame(drzava = zna1$drzava, skupina = factor(sk$cluster))
g_zna <- ggplot() + geom_polygon(data = svet %>% left_join(skupine, by = c("sovereignt" = "drzava")),
                                 aes(x = long, y = lat, group = group, color = "black", fill = skupina)) + 
  theme_void() + ggtitle("Države po podobnih značilnostih") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL), color="none") +
  labs(caption="Opomba: Zemljevid prikazuje razporeditev glede na leto 2000.")


kv <- lm(data = alko, povprecje ~ leto + I(leto^2)) %>% 
  predict(data.frame(leto=seq(2016, 2025, 1))) 
kv <- data_frame(kv) %>% rename(predikcija = kv)
kv$leto <- 2016:2025
kv <- kv[c(2,1)]


rm(den, vod, zna, zna1, sk, skupine)
