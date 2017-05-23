# 3. faza: Vizualizacija podatkov

library(ggplot2)
library(dplyr)
library(sp)
library(maptools)
library(digest)
gpclibPermit()

uvozi.zemljevid <- function(url, pot.zemljevida, mapa = "../zemljevidi",
                            encoding = "UTF-8", force = FALSE) {
  ime.zemljevida <- digest(url, algo = "sha1")
  map <- paste0(mapa, "/", ime.zemljevida)
  pot <- paste0(map, "/", pot.zemljevida)
  shp <- paste0(pot, ".shp")
  zip <- paste0(map, "/", ime.zemljevida, ".zip")
  if (force || !file.exists(shp)) {
    if (!file.exists(map)) {
      dir.create(map, recursive = TRUE)
    }
    download.file(url, zip)
    unzip(zip, exdir = map)
  }
  re <- paste0("^", gsub("\\.", "\\.", pot.zemljevida), "\\.")
  files <- grep(paste0(re, "[a-z0-9.]*$"),
                grep(paste0(re, ".*$"), dir(map, recursive = TRUE), value = TRUE),
                value = TRUE, invert = TRUE)
  file.rename(paste0(map, "/", files),
              paste0(map, "/", sapply(strsplit(files, "\\."),
                                      function(x)
                                        paste(c(x[1], tolower(x[2:length(x)])),
                                              collapse = "."))))
  zemljevid <- readShapeSpatial(shp)
  
  for (col in names(zemljevid)) {
    if (is.factor(zemljevid[[col]])) {
      zemljevid[[col]] <- factor(iconv(zemljevid[[col]], encoding))
    }
  }
  
  return(zemljevid)
}

pretvori.zemljevid <- function(zemljevid) {
  fo <- fortify(zemljevid)
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

svet <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                       "ne_110m_admin_0_countries") %>% pretvori.zemljevid()
# osnovni zemljevid sveta
# ggplot() + geom_polygon(data = svet, aes(x = long, y = lat, group = group)) + theme_void()

# Kako dodam drzave za katere ni podatka?
ggplot() + geom_polygon(data = inner_join(svet, filter(alkohol, alkohol$leto == 2010), by=c("sovereignt" = "drzava")), 
                        aes(x = long, y = lat, group = group, fill=poraba)) + theme_void() + ggtitle("Poraba alkohola(15+) v letu 2010 [liter čistega alkohola]") + theme(plot.title = element_text(hjust = 0.5))
ggplot() + geom_polygon(data = inner_join(svet, filter(tobak, tobak$leto == 2010 & tobak$spol == "moski"), by=c("sovereignt" = "drzava")), 
                        aes(x = long, y = lat, group = group, fill=pojavnost)) + theme_void() + ggtitle("Pojavnost kajenja med moškimi v letu 2010 [%]") + theme(plot.title = element_text(hjust = 0.5))
ggplot() + geom_polygon(data = inner_join(svet, filter(tobak, tobak$leto == 2010 & tobak$spol == "zenske"), by=c("sovereignt" = "drzava")), 
                        aes(x = long, y = lat, group = group, fill=pojavnost)) + theme_void() + ggtitle("Pojavnost kajenja med ženskami v letu 2010 [%]") + theme(plot.title = element_text(hjust = 0.5))

ggplot(tobak) + aes(x=leto, y=pojavnost, color=spol) + geom_jitter() + theme_bw() + labs(y="pojavnost kajenja [%]")
ggplot(filter(bolezni, bolezni$bolezen == "malarija" )) + aes(x=leto, y=pojavnost) + geom_boxplot() + theme_bw() + labs(y="število obolelih")

# Zakaj mi graf izrise padajoce po letih?
alko <- alkohol %>% group_by(leto) %>% summarise(povprecje = mean(poraba)) 
alko <- alko[order(alko$leto), ]
ggplot(alko) + aes(x=leto, y=povprecje) + geom_point() + theme_bw() + labs(y="globalna poraba alkohola(15+) [liter čistega alkohola]")
