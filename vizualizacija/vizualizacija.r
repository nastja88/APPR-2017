# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(sp)
library(maptools)
library(digest)
gpclibPermit()

source(lib/uvozi.zemljevid.r)

uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                "ne_110m_admin_0_countries/ne_110m_admin_0_countries")

ggplot(tobak) + aes(x=leto, y=pojavnost, color=spol) + geom_point()
ggplot(sifilis) + aes(x=leto, y=pojavnost) + geom_point()
