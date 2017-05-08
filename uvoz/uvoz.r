# 2. faza: Uvoz podatkov

library(readr)
library(dplyr)
library(gsubfn)
library(rvest)
library(reshape2)
library(tidyr)

worldbank <- read_csv("podatki/worldbank.csv", skip=1,
                      col_names=c("serija", "kodaserije", "drzava", "kodadrzave", 
                                  "2000", "2001", "2002", "2003", "2004", "2005",
                                  "2006", "2007", "2008", "2009", "2010", "2011",
                                  "2012", "2013", "2014", "2015"),
                      na="..", locale=locale(encoding = "UTF-8"))
worldbank$kodadrzave <- NULL
worldbank$kodaserije <- NULL
worldbank <- melt(worldbank, id.vars=c("serija", "drzava"), variable.name="leto", value.name="pojavnost")
worldbank <- filter(worldbank,  !is.na(pojavnost))

# for(drzava in worldbank$drzava) {
#   if (drzava=="Bahamas, The") {
#     drzava <- "Bahamas"
#   }
# }

# Pojavnost tuberkuloze

tuberkuloza <- read_csv("podatki/who-tuberculosis.csv", skip=1,
                        col_names = c("drzava", "leto", "pojavnost"),
                        locale = locale(encoding = "UTF-8"))
tuberkuloza$pojavnost <- tuberkuloza$pojavnost %>% strapplyc("^[0-9 ]+") %>% 
  unlist() %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))

# Pojavnost kolere
kolera <- read_csv("podatki/who-cholera.csv", skip=1,
                   col_names=c("drzava", "leto", "pojavnost"), 
                   locale=locale(encoding = "UTF-8"))

# Pojavnost prirojenega sifilisa
sifilis <- read_csv("podatki/who-sifilis.csv", skip=1, 
                    col_names = c("drzava","vir", "leto", "pojavnost"), 
                    locale=locale(encoding = "UTF-8"))
sifilis$vir <- NULL

# Poraba alkohola (v litrih čistega alkohola) na osebo (15+)
alkohol <- read_csv("podatki/who-alcohol.csv", skip=2, 
                    col_names=c("drzava", "vir", "tippijace", "2015", "2014", "2013",
                                "2012", "2011", "2010", "2009", "2008", "2007", "2006",
                                "2005", "2004", "2003", "2002", "2001", "2000"), 
                    na=c("", "No data"), locale=locale(encoding = "UTF-8"))
alkohol$vir <- NULL
alkohol$tippijace <- NULL
alkohol <- melt(alkohol, id.vars=c("drzava"), variable.name="leto", value.name="poraba")
alkohol <- filter(alkohol,  !is.na(poraba))

# Razširjenost kajenja tobačnih izdelkov 15+
tobak <- read_csv("podatki/who-tabacco.csv", skip=2, 
                  col_names=c("drzava", "leto", "moski", "zenske"), na="",
                  locale=locale(encoding = "UTF-8"))
tobak <- filter(tobak,  moski!="")
tobak$moski <- tobak$moski %>% strapplyc("^[0-9. ]+") %>% 
  unlist() %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))
tobak$zenske <- tobak$zenske %>% strapplyc("^[0-9. ]+") %>% 
  unlist() %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))
tobak <- melt(tobak, id.vars=c("drzava", "leto"), measure.vars=c("moski", "zenske"), 
              variable.name="spol", value.name="pojavnost")


# Število plačanih prostih dni
link <- "https://en.wikipedia.org/wiki/List_of_minimum_annual_leave_by_country"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% 
  .[[1]] %>% html_table(dec = ".") %>% select(drzava = 1, dopust = 5) %>% drop_na(dopust)
