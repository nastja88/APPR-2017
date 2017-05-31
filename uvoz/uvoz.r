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
worldbank$serija <- as.factor(worldbank$serija)
levels(worldbank$serija) <- list(voda="Improved water source (% of population with access)", 
                                 denar="Health expenditure per capita, PPP",
                                 hiv="Prevalence of HIV, total (% of population ages 15-49)",
                                 malarija="Malaria cases reported",
                                 podhranjenost="Malnutrition prevalence, weight for age (% of children under 5)",
                                 debelost="Prevalence of overweight (% of children under 5)")

worldbank$drzava[worldbank$drzava == "Bahamas, The"] <- "The Bahamas"
worldbank$drzava[worldbank$drzava == "Brunei Darussalam"] <- "Brunei"
worldbank$drzava[worldbank$drzava == "Congo, Rep."] <- "Republic of Congo"
worldbank$drzava[worldbank$drzava == "Congo, Dem. Rep."] <- "Democratic Republic of the Congo"
worldbank$drzava[worldbank$drzava == "Cote d'Ivoire"] <- "Ivory Coast"
worldbank$drzava[worldbank$drzava == "Egypt, Arab Rep."] <- "Egypt"
worldbank$drzava[worldbank$drzava == "Gambia, The"] <- "Gambia"
worldbank$drzava[worldbank$drzava == "Guinea-Bissau"] <- "Guinea Bissau"
worldbank$drzava[worldbank$drzava == "Iran, Islamic Rep."] <- "Iran"
worldbank$drzava[worldbank$drzava == "Korea, Dem. People’s Rep."] <- "North Korea"
worldbank$drzava[worldbank$drzava == "Korea, Rep."] <- "South Korea"
worldbank$drzava[worldbank$drzava == "Kyrgyz Republic"] <- "Kyrgyzstan"
worldbank$drzava[worldbank$drzava == "Lao PDR"] <- "Laos"
worldbank$drzava[worldbank$drzava == "Macedonia, FYR"] <- "Macedonia"
worldbank$drzava[worldbank$drzava == "Micronesia, Fed. Sts."] <- "Federated States of Micronesia"
worldbank$drzava[worldbank$drzava == "Russian Federation"] <- "Russia"
worldbank$drzava[worldbank$drzava == "Serbia"] <- "Republic of Serbia"
worldbank$drzava[worldbank$drzava == "Slovak Republic"] <- "Slovakia"
worldbank$drzava[worldbank$drzava == "Syrian Arab Republic"] <- "Syria"
worldbank$drzava[worldbank$drzava == "Tanzania"] <- "United Republic of Tanzania"
worldbank$drzava[worldbank$drzava == "Timor-Leste"] <- "East Timor"
worldbank$drzava[worldbank$drzava == "United States"] <- "United States of America"
worldbank$drzava[worldbank$drzava == "Venezuela, RB"] <- "Venezuela"
worldbank$drzava[worldbank$drzava == "Yemen, Rep."] <- "Yemen"

# samo bolezni(iz worldbank-a)
bol <- filter(worldbank, worldbank$serija == "hiv" | worldbank$serija == "malarija") %>% 
  rename(bolezen = serija)

# samo znacilnosti(iz worldbank-a)
znac <- filter(worldbank, worldbank$serija != "hiv" & worldbank$serija != "malarija") %>% 
  rename(znacilnost = serija)

# Pojavnost tuberkuloze
tuberkuloza <- read_csv("podatki/who-tuberculosis.csv", skip=1,
                        col_names = c("drzava", "leto", "pojavnost"),
                        locale = locale(encoding = "UTF-8"))
tuberkuloza$pojavnost <- tuberkuloza$pojavnost %>% strapplyc("^[0-9 ]+") %>% 
  unlist() %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))
tuberkuloza$pojavnost <- tuberkuloza$pojavnost / 1000   #hočemo procente
tuberkuloza$bolezen <- "tuberkuloza"

# Pojavnost prirojenega sifilisa
sifilis <- read_csv("podatki/who-syphilis.csv", skip=1, 
                    col_names = c("drzava","vir", "leto", "pojavnost"), 
                    locale=locale(encoding = "UTF-8"))
sifilis <- filter(sifilis,  !is.na(leto))
sifilis$vir <- NULL
sifilis$pojavnost <- sifilis$pojavnost / 1000   #procenti
sifilis$bolezen <- "sifilis"

# Pojavnost kolere
kolera <- read_csv("podatki/who-cholera.csv", skip=1,
                   col_names=c("drzava", "leto", "pojavnost"), 
                   locale=locale(encoding = "UTF-8"))
kolera$bolezen <- "kolera"

# Poraba alkohola (v litrih čistega alkohola) na osebo (15+)
alkohol <- read_csv("podatki/who-alcohol.csv", skip=2, 
                    col_names=c("drzava", "vir", "tippijace", "2015", "2014", "2013",
                                "2012", "2011", "2010", "2009", "2008", "2007", "2006",
                                "2005", "2004", "2003", "2002", "2001", "2000"), 
                    na=c("", "No data"), locale=locale(encoding = "UTF-8"))
alkohol$vir <- NULL
alkohol$tippijace <- NULL
alkohol <- melt(alkohol, id.vars=c("drzava"), variable.name="leto", value.name="pojavnost")
alkohol <- filter(alkohol,  !is.na(pojavnost))
alkohol$pojavnost %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))
alkohol$bolezen <- "alkohol"

# povezane tabele iz who-ja
who <- rbind(tuberkuloza, sifilis, kolera, alkohol)

who$drzava[who$drzava == "Bahamas"] <- "The Bahamas"
who$drzava[who$drzava == "Bolivia (Plurinational State of)"] <- "Bolivia"
who$drzava[who$drzava == "Brunei Darussalam"] <- "Brunei"
who$drzava[who$drzava == "Congo"] <- "Republic of Congo"
who$drzava[who$drzava == "Côte d'Ivoire"] <- "Ivory Coast"
who$drzava[who$drzava == "Czechia"] <- "Czech Republic"
who$drzava[who$drzava == "Democratic People's Republic of Korea"] <- "North Korea"
who$drzava[who$drzava == "Guinea-Bissau"] <- "Guinea Bissau"
who$drzava[who$drzava == "Iran (Islamic Republic of)"] <- "Iran"
who$drzava[who$drzava == "Lao People's Democratic Republic"] <- "Laos"
who$drzava[who$drzava == "Micronesia (Federated States of)"] <- "Federated States of Micronesia"
who$drzava[who$drzava == "Republic of Korea"] <- "South Korea"
who$drzava[who$drzava == "Republic of Moldova"] <- "Moldova"
who$drzava[who$drzava == "Russian Federation"] <- "Russia"
who$drzava[who$drzava == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
who$drzava[who$drzava == "Saint Lucia"] <- "St. Lucia"
who$drzava[who$drzava == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
who$drzava[who$drzava == "Serbia"] <- "Republic of Serbia"
who$drzava[who$drzava == "Syrian Arab Republic"] <- "Syria"
who$drzava[who$drzava == "Timor-Leste"] <- "East Timor"
who$drzava[who$drzava == "The former Yugoslav republic of Macedonia"] <- "Macedonia"
who$drzava[who$drzava == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
who$drzava[who$drzava == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
who$drzava[who$drzava == "Viet Nam"] <- "Vietnam"

# samo bolezni(iz who-ja)
bole <- filter(who, who$bolezen != "alkohol")

# alkohol(znacilnost) posebej
alkohol <- filter(who, who$bolezen == "alkohol") %>% rename(znacilnost = bolezen)

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

tobak$drzava[tobak$drzava == "Bolivia (Plurinational State of)"] <- "Bolivia"
tobak$drzava[tobak$drzava == "Brunei Darussalam"] <- "Brunei"
tobak$drzava[tobak$drzava == "Congo"] <- "Republic of Congo"
tobak$drzava[tobak$drzava == "Czechia"] <- "Czech Republic"
tobak$drzava[tobak$drzava == "Iran (Islamic Republic of)"] <- "Iran"
tobak$drzava[tobak$drzava == "Lao People's Democratic Republic"] <- "Laos"
tobak$drzava[tobak$drzava == "Republic of Korea"] <- "South Korea"
tobak$drzava[tobak$drzava == "Republic of Moldova"] <- "Moldova"
tobak$drzava[tobak$drzava == "Russian Federation"] <- "Russia"
tobak$drzava[tobak$drzava == "Serbia"] <- "Republic of Serbia"
tobak$drzava[tobak$drzava == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
tobak$drzava[tobak$drzava == "Viet Nam"] <- "Vietnam"

tobak$drzava <- as.factor(tobak$drzava)

# Število plačanih prostih dni
link <- "https://en.wikipedia.org/wiki/List_of_minimum_annual_leave_by_country"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% 
  .[[1]] %>% html_table(dec = ".") %>% select(drzava = 1, dopust = 5) %>% drop_na(dopust)

tabela$drzava[tabela$drzava == "Brunei Darussalam"] <- "Brunei"
tabela$drzava[tabela$drzava == "Cape Verde"] <- "Cabo Verde"
tabela$drzava[tabela$drzava == "Serbia"] <- "Republic of Serbia"
tabela$drzava[tabela$drzava == "Tanzania"] <- "United Republic of Tanzania"
tabela$drzava[tabela$drzava == "United States"] <- "United States of America"

tabela$drzava <- as.factor(tabela$drzava)

# Vse bolezni skupaj
bolezni <- rbind(bol, bole)
bolezni$drzava <- as.factor(bolezni$drzava)
bolezni$bolezen <- factor(bolezni$bolezen)

# Znacilnosti skupaj
znacilnosti <- rbind(znac, alkohol)
znacilnosti$drzava <- as.factor(znacilnosti$drzava)
znacilnosti$znacilnost <- factor(znacilnosti$znacilnost)

rm(bol, bole, kolera, sifilis, tuberkuloza, alkohol, worldbank, znac, who)
