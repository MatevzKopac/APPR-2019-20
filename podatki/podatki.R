library(readr)
moji_podatki <- read_delim("podatki/goli_podatki.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="windows-1250"), 
                           na=c("#VALUE!","/", "0",""))
osnovni_podatki <- moji_podatki[c(1:632),c(4,8,10,11,13,14)]

stevilski_podatki <- moji_podatki[c(1:632),c(4,22:29,32,34,36,40,41)]


