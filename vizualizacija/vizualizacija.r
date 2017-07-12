# 3. faza: Vizualizacija podatkov

library(scales)
library(lazyeval)
library(ggplot2)
library(dplyr)

source("lib/uvozi.zemljevid.r", encoding = "UTF-8")

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                       "ne_110m_admin_0_countries") %>% pretvori.zemljevid()

# osnovni zemljevid sveta
# ggplot() + geom_polygon(data = svet, aes(x = long, y = lat, group = group)) + theme_void()

# g_alk <- ggplot() + geom_polygon(data = left_join(svet, filter(znacilnosti, 
#                                                       znacilnosti$znacilnost == "alkohol" & 
#                                                         znacilnosti$leto == 2010), 
#                                          by=c("sovereignt" = "drzava")), 
#                         aes(x = long, y = lat, group = group, color = "black", fill=pojavnost)) + 
#   theme_void() + ggtitle("Poraba alkohola (15+) v letu 2010 [liter čistega alkohola]") + 
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   guides(fill=guide_legend(title=NULL), color="none") + 
#   scale_fill_gradientn(colours = c("blue", "green", "red"), 
#                        values = rescale(c(0, 50, 200, 700)))

# g_tbm <- ggplot() + geom_polygon(data = left_join(svet, filter(tobak, tobak$leto == 2010 & 
#                                                         tobak$spol == "moski"), 
#                                          by=c("sovereignt" = "drzava")), 
#                         aes(x = long, y = lat, group = group, color = "black", fill=pojavnost)) + 
#   theme_void() + ggtitle("Pojavnost kajenja med moškimi (15+) v letu 2010 [%]") + 
#   theme(plot.title = element_text(hjust = 0.5)) + 
#   guides(fill=guide_legend(title=NULL), color="none") + 
#   scale_fill_gradientn(colours = c("blue", "green", "red"), 
#                        values = rescale(c(0, 50, 200, 700)))

#g_tbz <- ggplot() + geom_polygon(data = left_join(svet, filter(tobak, tobak$leto == 2010 & 
#                                                        tobak$spol == "zenske"), 
#                                         by=c("sovereignt" = "drzava")), 
#                        aes(x = long, y = lat, group = group, color = "black", fill=pojavnost)) + 
#  theme_void() + ggtitle("Pojavnost kajenja med ženskami (15+) v letu 2010 [%]") + 
#  theme(plot.title = element_text(hjust = 0.5)) + 
#  guides(fill=guide_legend(title=NULL), color="none") + 
#  scale_fill_gradientn(colours = c("blue", "green", "red"), 
#                       values = rescale(c(0, 50, 200, 700)))

# število prostih dni po državah
g_dop <- ggplot() + geom_polygon(data = left_join(svet, tabela, by=c("sovereignt" = "drzava")), 
                        aes(x = long, y = lat, group = group, color = "black", fill=dopust)) + 
  theme_void() + ggtitle("Število plačanih prostih dni (5 delovnih dni / teden)") + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.3)) + 
  guides(fill=guide_legend(title=NULL), color="none") + 
  scale_fill_gradientn(colours = c("blue", "green", "red"), 
                       values = rescale(c(0, 50, 200, 700)))

# delež kadilcev (15+)
g_tb <- ggplot(tobak) + aes(x=leto, y=pojavnost, color=spol) + geom_jitter() + theme_bw() + 
  labs(y="delež kadilcev (15+) [%]")
#tob <- tobak %>% group_by(spol) %>% summarise(mean(pojavnost))

# število okuženih z malarijo po državah
g_mal1 <- ggplot(filter(bolezni, bolezni$bolezen == "malarija" )) + 
  aes(x = leto, y = pojavnost/1000000, group = leto) + 
  geom_boxplot() + theme_bw() + labs(y="število okuženih z malarijo (v milijonih)")

# globalna poraba alkohola (15+)
alko <- filter(znacilnosti, znacilnosti$znacilnost == "alkohol") %>% group_by(leto) %>% 
  summarise(povprecje = mean(pojavnost)) 
g_alk1 <- ggplot(alko) + aes(x=leto, y=povprecje) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + theme_bw() + 
  labs(y="globalna poraba alkohola (15+) [liter čistega alkohola]")

#kv <- lm(data = alko, povprecje ~ leto + I(leto^2))
#predict(kv, data.frame(leto=seq(2015, 2025, 1)))

rm(alko)