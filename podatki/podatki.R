library(readr)
moji_podatki <- read_delim("podatki/goli_podatki.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, locale=locale(encoding="windows-1250"), 
                           na=c("#VALUE!","/"))
View(moji_podatki)

                         