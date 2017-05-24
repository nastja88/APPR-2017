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

bolezni <- filter(worldbank, worldbank$serija == "hiv" | worldbank$serija == "malarija") %>% 
  rename(bolezen = serija)
znacilnosti <- filter(worldbank, worldbank$serija != "hiv" & worldbank$serija != "malarija") %>% 
  rename(znacilnost = serija)

# worldbank$drzava <- as.factor(worldbank$drzava)

# Pojavnost tuberkuloze

tuberkuloza <- read_csv("podatki/who-tuberculosis.csv", skip=1,
                        col_names = c("drzava", "leto", "pojavnost"),
                        locale = locale(encoding = "UTF-8"))
tuberkuloza$pojavnost <- tuberkuloza$pojavnost %>% strapplyc("^[0-9 ]+") %>% 
  unlist() %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))
tuberkuloza$pojavnost <- tuberkuloza$pojavnost / 1000   #hočemo procente

tuberkuloza$drzava[tuberkuloza$drzava == "Bahamas"] <- "The Bahamas"
tuberkuloza$drzava[tuberkuloza$drzava == "Bolivia (Plurinational State of)"] <- "Bolivia"
tuberkuloza$drzava[tuberkuloza$drzava == "Brunei Darussalam"] <- "Brunei"
tuberkuloza$drzava[tuberkuloza$drzava == "Congo"] <- "Republic of Congo"
tuberkuloza$drzava[tuberkuloza$drzava == "Côte d'Ivoire"] <- "Ivory Coast"
tuberkuloza$drzava[tuberkuloza$drzava == "Czechia"] <- "Czech Republic"
tuberkuloza$drzava[tuberkuloza$drzava == "Democratic People's Republic of Korea"] <- "North Korea"
tuberkuloza$drzava[tuberkuloza$drzava == "Guinea-Bissau"] <- "Guinea Bissau"
tuberkuloza$drzava[tuberkuloza$drzava == "Iran (Islamic Republic of)"] <- "Iran"
tuberkuloza$drzava[tuberkuloza$drzava == "Lao People's Democratic Republic"] <- "Laos"
tuberkuloza$drzava[tuberkuloza$drzava == "Micronesia (Federated States of)"] <- "Federated States of Micronesia"
tuberkuloza$drzava[tuberkuloza$drzava == "Republic of Korea"] <- "South Korea"
tuberkuloza$drzava[tuberkuloza$drzava == "Russian Federation"] <- "Russia"
tuberkuloza$drzava[tuberkuloza$drzava == "Saint Kitts and Nevis"] <- "St. Kitts and Nevis"
tuberkuloza$drzava[tuberkuloza$drzava == "Saint Lucia"] <- "St. Lucia"
tuberkuloza$drzava[tuberkuloza$drzava == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
tuberkuloza$drzava[tuberkuloza$drzava == "Serbia"] <- "Republic of Serbia"
tuberkuloza$drzava[tuberkuloza$drzava == "Syrian Arab Republic"] <- "Syria"
tuberkuloza$drzava[tuberkuloza$drzava == "Timor-Leste"] <- "East Timor"
tuberkuloza$drzava[tuberkuloza$drzava == "The former Yugoslav republic of Macedonia"] <- "Macedonia"
tuberkuloza$drzava[tuberkuloza$drzava == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
tuberkuloza$drzava[tuberkuloza$drzava == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
tuberkuloza$drzava[tuberkuloza$drzava == "Viet Nam"] <- "Vietnam"

# Pojavnost prirojenega sifilisa
sifilis <- read_csv("podatki/who-syphilis.csv", skip=1, 
                    col_names = c("drzava","vir", "leto", "pojavnost"), 
                    locale=locale(encoding = "UTF-8"))
sifilis$vir <- NULL
sifilis$pojavnost <- sifilis$pojavnost / 1000   #procenti

# Pojavnost kolere
kolera <- read_csv("podatki/who-cholera.csv", skip=1,
                   col_names=c("drzava", "leto", "pojavnost"), 
                   locale=locale(encoding = "UTF-8"))

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
alkohol$poraba %>% parse_number(locale=locale(decimal_mark=".", grouping_mark=" "))

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

# world <- unique(worldbank$drzava) %>% sort()
# tuber <- unique(tuberkuloza$drzava) %>% sort()
# razlicni <- world != tuber

# Število plačanih prostih dni
link <- "https://en.wikipedia.org/wiki/List_of_minimum_annual_leave_by_country"
stran <- html_session(link) %>% read_html()
tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>% 
  .[[1]] %>% html_table(dec = ".") %>% select(drzava = 1, dopust = 5) %>% drop_na(dopust)
