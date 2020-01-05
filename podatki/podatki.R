library(readr)
library(tidyverse)
moji_podatki <- read_delim("podatki/goli_podatki.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="windows-1250"), 
                           na=c("#VALUE!","/", "0",""))
osnovni_podatki <- moji_podatki[c(1:632),c(4,8,10,11,13,14)]

stevilski_podatki <- moji_podatki[c(1:632),c(4,22:29,32,34,36,40,41)]

legenda_regij <- matrix(c("Ljubljana","Maribor","Celje","Kranj","Nova Gorica", "Koper", "Novo Mesto", "Murska Sobota",
                          1,2,3,4,5,6,7,8),ncol=2) 
colnames(legenda_regij) <- c('KRAJ', 'ŠTEVILKA')
legenda_regij.m <- melt(legenda_regij)

legenda_dejavnosti <- osnovni_podatki[c("regija", "DEJAVNOST")]

#legendo regij rabim spravit iz matrike v tabelo in legendo dejavnosti moram "zmetat skupi" po številki dejavnosti,, da bo izgledala kot tabela regij. dodaj še tabel, vprašaj kam pisati vse to 