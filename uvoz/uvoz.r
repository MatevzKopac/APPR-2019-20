# 2. faza: Uvoz podatkov
source("lib/libraries.r", encoding="UTF-8")

moji_podatki <- read_delim("podatki/goli_podatki.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="windows-1250"), 
                           na=c("#VALUE!","/", "0",""))

#OBDELAVA PODATKOV
moji_podatki$NAZIV <- gsub("\\?", "č", moji_podatki$NAZIV,ignore.case = FALSE)
moji_podatki$NASLOV <- gsub("\\?", "č", moji_podatki$NASLOV,ignore.case = FALSE)
moji_podatki$KRAJ <- gsub("\\?", "č", moji_podatki$KRAJ,ignore.case = FALSE)
moji_podatki$DEJAVNOST <- gsub("\\?", "č", moji_podatki$DEJAVNOST,ignore.case = FALSE)
moji_podatki$VELIKOST_SUBJEKTA <- gsub("\\?", "č", moji_podatki$VELIKOST_SUBJEKTA,ignore.case = FALSE)

osnovni_podatki <- moji_podatki[c(1:632),c(4,8,10,11,13,14)]
stevilski_podatki <- moji_podatki[c(1:632),c(4,8,11,22:29,32,34,36,38,40,41)]

colnames(osnovni_podatki)[2:6] <- c("REGIJA","DEJAVNOST","DEJAVNOST ŠTEVILKA","VELIKOST SUBJEKTA","ŠTEVILO ZAPOSLENIH")
colnames(stevilski_podatki)[6:7] <- c("Profit 2018","Profit 2017")
colnames(stevilski_podatki)[14:15] <- c("Profit 2016","Profit 2015")

#LEGENDA REGIJ
legenda_regij <- matrix(c("Ljubljana","Maribor","Celje","Kranj","Nova Gorica", "Koper", "Novo Mesto", "Murska Sobota",
                          1,2,3,4,5,6,7,8),ncol=2) 
colnames(legenda_regij) <- c('KRAJ', 'ŠTEVILKA')
legenda_regij <- tbl_df(legenda_regij)

#LEGENDA DEJAVNOSTI
legenda_dejavnosti <- osnovni_podatki[c("DEJAVNOST", "DEJAVNOST ŠTEVILKA")] 
legenda_dejavnosti <- distinct(legenda_dejavnosti)
#legenda_dejavnosti <- legenda_dejavnosti[arrange(legenda_dejavnosti$`DEJAVNOST ŠTEVILKA`)]


prihodki_regija <- stevilski_podatki[c(1:632),c(2,4,5,12,13)]
NOVA <-prihodki_regija %>%
  group_by(regija) %>%
  summarize(prihodek2018 = sum(`Prihodki 2018`, na.rm = TRUE),
            prihdek2017 = sum(`Prihodki 2017`, na.rm = TRUE))
            
#noče mi sešrtevati po več stolpcih hkrati            
            


