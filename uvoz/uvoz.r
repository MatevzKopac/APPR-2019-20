# 2. faza: Uvoz podatkov
source("lib/libraries.r", encoding="UTF-8")
moji_podatki <- read_delim("podatki/goli_podatki.csv",
                           ";", escape_double=FALSE, trim_ws=TRUE,
                           locale=locale(encoding="windows-1250",
                                         decimal_mark=",", grouping_mark="."),
                           col_types=cols(DATUM_UVOZA=col_date(format="%d.%m.%Y"),
                                          `Osnovna sredstva 2018`=col_number()),
                           na=c("#VALUE!", "/", "0", "", "-", "ni podatka"), # ali je 0 res manjkajoč podatek?
                           n_max=632)

#moji_podatki <- read_delim("podatki/goli_podatki.csv", 
#                           ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="windows-1250"), 
#                           na=c("#VALUE!","/", "0",""))

#OBDELAVA PODATKOV
moji_podatki$NAZIV <- gsub("\\?", "č", moji_podatki$NAZIV,ignore.case = FALSE)
moji_podatki$NASLOV <- gsub("\\?", "č", moji_podatki$NASLOV,ignore.case = FALSE)
moji_podatki$KRAJ <- gsub("\\?", "č", moji_podatki$KRAJ,ignore.case = FALSE)
moji_podatki$DEJAVNOST <- gsub("\\?", "č", moji_podatki$DEJAVNOST,ignore.case = FALSE)
moji_podatki$VELIKOST_SUBJEKTA <- gsub("\\?", "č", moji_podatki$VELIKOST_SUBJEKTA,ignore.case = FALSE)

osnovni_podatki <- moji_podatki[c(1:632),c(4,8,10,11,13,14)]
stevilski_podatki <- moji_podatki[c(1:632),c(4,8,11,22:29,32,34,36,38,40,41)]

colnames(osnovni_podatki)[2:6] <- c("REGIJA","DEJAVNOST","DEJAVNOST ŠTEVILKA","VELIKOST SUBJEKTA","ŠTEVILO ZAPOSLENIH")
colnames(stevilski_podatki)[6:7] <- c("Profit_2018","Profit_2017")
colnames(stevilski_podatki)[14:15] <- c("Profit_2016","Profit_2015")

#LEGENDA REGIJ
legenda_regij <- matrix(c("Ljubljana","Maribor","Celje","Kranj","Nova Gorica", "Koper", "Novo mesto", "Murska Sobota",
                          1,2,3,4,5,6,8,9),ncol=2) 
colnames(legenda_regij) <- c('KRAJ', 'REGIJA')

legenda_regij <- tibble::as_tibble(legenda_regij)
legenda_regij$`REGIJA` <- parse_integer(legenda_regij$`REGIJA`)

#LEGENDA DEJAVNOSTI
legenda_dejavnosti <- osnovni_podatki[c("DEJAVNOST", "DEJAVNOST ŠTEVILKA")] 
legenda_dejavnosti <- distinct(legenda_dejavnosti)
#legenda_dejavnosti <- legenda_dejavnosti[arrange(legenda_dejavnosti$`DEJAVNOST ŠTEVILKA`)]


prihodki_regija <- stevilski_podatki[c(1:632),c(2,4,5,12,13)]
#NOVA <-prihodki_regija %>%
  #group_by(regija) %>%
  #summarise(prihodek2018 = sum(`Prihodki 2018`, na.rm = TRUE),
            #prihdek2017 = sum(`Prihodki 2017`, na.rm = TRUE))
            
          

#noče mi seštevati po več stolpcih hkrati.            
#prihodki_regija$`Prihodki_2017` <- as.numeric(prihodki_regija$`Prihodki_2017`)

prihodki_regija <- gather(prihodki_regija, "leto", "Dohodek", 2:5)
prihodki_regija$Dohodek <- gsub(",.*", "", prihodki_regija$Dohodek)
prihodki_regija$Dohodek <- gsub("\\.", "", prihodki_regija$Dohodek)
prihodki_regija$`Dohodek` <- parse_integer(prihodki_regija$`Dohodek`)
prihodki_regija$leto <- gsub("Prihodki", "", prihodki_regija$leto)
prihodki_regija[is.na(prihodki_regija)] <- 0

# graf prihodki glede na regijo je vredu 

dobicek_regija <- stevilski_podatki %>% select(2, 6, 7, 14, 15)
#dobicek_regija$`Profit_2018` <- as.numeric(dobicek_regija$`Profit_2018`)
#dobicek_regija$`Profit_2017` <- as.numeric(dobicek_regija$`Profit_2017`)
dobicek_regija <- gather(dobicek_regija, "leto", "Profit", 2:5)
dobicek_regija$Profit <- gsub(",.*", "", dobicek_regija$Profit)
dobicek_regija$Profit <- gsub("\\.", "", dobicek_regija$Profit)
dobicek_regija$`Profit` <- parse_integer(dobicek_regija$`Profit`)
dobicek_regija$leto <- gsub("Profit_", "", dobicek_regija$leto)
dobicek_regija[is.na(dobicek_regija)] <- 0

# graf dobicek glede na regijo je vredu 


zapolseni_regija_dejavnost <- osnovni_podatki %>% select(2, 3, 6) 

#tabela prihodkov dobičkov in zaposlenih
prihodki_zaposleni <- inner_join(osnovni_podatki, stevilski_podatki, by = "NAZIV") %>% select(3, 9:10, 17:18, 6)
prihodki_zaposleni <- gather(prihodki_zaposleni, "leto", "Dohodek", 2:5)
prihodki_zaposleni$Dohodek <- gsub(",.*", "", prihodki_zaposleni$Dohodek)
prihodki_zaposleni$Dohodek <- gsub("\\.", "", prihodki_zaposleni$Dohodek)
prihodki_zaposleni$`Dohodek` <- parse_integer(prihodki_zaposleni$`Dohodek`)
prihodki_zaposleni$leto <- gsub("Prihodki", "", prihodki_zaposleni$leto)
prihodki_zaposleni[is.na(prihodki_zaposleni)] <- 0

do10 <- prihodki_zaposleni %>% filter(prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` < 10)

od10do30 <- prihodki_zaposleni %>% filter(prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 30, prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` >= 10)

od30do100 <- prihodki_zaposleni %>% filter(prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 100, prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` > 30)

nad100 <- prihodki_zaposleni %>% filter(prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` > 100)



dobicek_zaposleni <- inner_join(osnovni_podatki, stevilski_podatki, by = "NAZIV") %>% select(3, 11:12, 19:20, 6)
dobicek_zaposleni <- gather(dobicek_zaposleni, "Leto", "Profit", 2:5)
dobicek_zaposleni$Profit <- gsub(",.*", "", dobicek_zaposleni$Profit)
dobicek_zaposleni$Profit <- gsub("\\.", "", dobicek_zaposleni$Profit)
dobicek_zaposleni$`Profit` <- parse_integer(dobicek_zaposleni$`Profit`)
dobicek_zaposleni$Leto <- gsub("Profit_", "", dobicek_zaposleni$Leto)
dobicek_zaposleni[is.na(dobicek_zaposleni)] <- 0


Profit_do10 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` < 10)

Profit_od10do30 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 30, dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` >= 10)

Profit_od30do100 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 100, dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` > 30)

Profit_nad100 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` > 100)

#






















