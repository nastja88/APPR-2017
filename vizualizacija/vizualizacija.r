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


# število okuženih z malarijo po državah
g_mal1 <- ggplot(filter(bolezni, bolezni$bolezen == "malarija" )) + 
  aes(x = leto, y = pojavnost/1000000, group = leto) + 
  geom_boxplot() + theme_bw() + labs(y="število okuženih z malarijo (v milijonih)")

g_hiv <- ggplot(filter(bolezni, bolezni$bolezen == "hiv" )) + 
  aes(x = leto, y = pojavnost, group = leto) + 
  geom_boxplot() + theme_bw() + labs(y="delež okuženih z virusom hiv")

# globalna poraba alkohola (15+)
alko <- filter(znacilnosti, znacilnosti$znacilnost == "alkohol") %>% group_by(leto) %>% 
  summarise(povprecje = mean(pojavnost)) 
g_alk1 <- ggplot(alko) + aes(x=leto, y=povprecje) + geom_point() + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + theme_bw() + 
  labs(y="globalna poraba alkohola (15+) [liter čistega alkohola]")

# delež kadilcev (15+)
g_tb <- ggplot(tobak) + aes(x=leto, y=pojavnost, color=spol) + geom_jitter() + theme_bw() + 
  labs(y="delež kadilcev (15+) [%]")
#tob <- tobak %>% group_by(spol) %>% summarise(mean(pojavnost))


#kv <- lm(data = alko, povprecje ~ leto + I(leto^2))
#predict(kv, data.frame(leto=seq(2015, 2025, 1)))

rm(alko)