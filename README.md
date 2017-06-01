# Analiza podatkov s programom R, 2016/17

Repozitorij z gradivi pri predmetu APPR v študijskem letu 2016/17

## Tematika

Analizirala bom nekaj najpogostejših bolezni(AIDS, tuberkuloza, malarija, kolera, sifilis) po državah in letih (2000-2015) ter skušala ugotoviti razlog zanje (npr. podhranjenost, prekomerna teža, dostopnost pitne vode, poraba alkohola in tobačnih izdelkov, število plačanih prostih dni, zdravstvena potrošnja). Moj cilj je ugotoviti, kako poskrbeti za to, da bi imeli kar najbolj zdravo populacijo.

### Viri:

* http://databank.worldbank.org/data/reports.aspx?source=health-nutrition-and-population-statistics# (CSV)
* http://apps.who.int/gho/data/node.home (CSV)
* https://en.wikipedia.org/wiki/List_of_minimum_annual_leave_by_country (spletna stran)

bolezen | država | leto | pojavnost
-------|------|---------|----------
malarija | Afganistan | 2008 | 81574

značilnost | država | leto | pojavnost
-------|------|------------|---------
podhranjenost | Afganistan | 2004 | 32,9

država | dopust
-------|-------
Afganistan | 35

tobak:

država | leto | spol | pojavnost
-------|------|------|----------
Albanija | 2015 | moški | 51,2

## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`. Ko ga prevedemo,
se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`. Podatkovni
viri so v mapi `podatki/`. Zemljevidi v obliki SHP, ki jih program pobere, se
shranijo v mapo `../zemljevidi/` (torej izven mape projekta).

## Potrebni paketi za R

Za zagon tega vzorca je potrebno namestiti sledeče pakete za R:

* `knitr` - za izdelovanje poročila
* `rmarkdown` - za prevajanje poročila v obliki RMarkdown
* `shiny` - za prikaz spletnega vmesnika
* `DT` - za prikaz interaktivne tabele
* `maptools` - za uvoz zemljevidov
* `sp` - za delo z zemljevidi
* `digest` - za zgoščevalne funkcije (uporabljajo se za shranjevanje zemljevidov)
* `readr` - za branje podatkov
* `rvest` - za pobiranje spletnih strani
* `reshape2` - za preoblikovanje podatkov v obliko *tidy data*
* `dplyr` - za delo s podatki
* `gsubfn` - za delo z nizi (čiščenje podatkov)
* `ggplot2` - za izrisovanje grafov
* `extrafont` - za pravilen prikaz šumnikov (neobvezno)
