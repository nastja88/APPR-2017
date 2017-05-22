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

ggplot() + geom_polygon(data = svet, aes(x = long, y = lat, group = group))

#??
ggplot() + geom_polygon(data = inner_join(svet, alkohol, by=c("sovereignt" = "drzava")), 
                        aes(x = long, y = lat, group = group, fill=poraba)) + facet_grid(leto~.)

ggplot(tobak) + aes(x=leto, y=pojavnost, color=spol) + geom_jitter() + theme_bw() + labs(y="pojavnost [%]")
ggplot(filter(bolezni, bolezni$bolezen == "malarija" )) + aes(x=leto, y=pojavnost) + geom_boxplot() + theme_bw() + labs(y="število")

# alko <- alkohol %>% group_by(leto) %>% summarise(mean(poraba))
# ggplot(alko) + aes(x=leto, y=mean(poraba)) + geom_line()

ggplot(sifilis) + aes(x=leto, y=pojavnost) + geom_point()
