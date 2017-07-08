# 4. faza: Analiza podatkov

alk <- filter(znacilnosti, leto == 2000, znacilnost == "alkohol") %>% rename(alkohol = pojavnost)
alk$znacilnost <- NULL
alk$leto <- NULL
deb <- filter(znacilnosti, leto == 2000, znacilnost == "debelost") %>% rename(debelost = pojavnost)
deb$znacilnost <- NULL
deb$leto <- NULL
den <- filter(znacilnosti, leto == 2000, znacilnost == "denar") %>% rename(denar = pojavnost)
den$znacilnost <- NULL
den$leto <- NULL
pod <- filter(znacilnosti, leto == 2000, znacilnost == "podhranjenost") %>% rename(podhranjenost = pojavnost)
pod$znacilnost <- NULL
pod$leto <- NULL
vod <- filter(znacilnosti, leto == 2000, znacilnost == "voda") %>% rename(voda = pojavnost)
vod$znacilnost <- NULL
vod$leto <- NULL
zna <- full_join(alk, deb) %>% full_join(den) %>% full_join(pod) %>% full_join(vod)
row.names(zna) <- zna$drzava
zna$drzava <- NULL
zna <- na.omit(zna)
skup <- hclust(dist(scale(zna))) %>% cutree(5)