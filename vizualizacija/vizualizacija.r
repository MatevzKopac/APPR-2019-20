# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
  #summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))

# tabele in grafi za zaposlene glede na regijo in dejavnost

tabela1 <- zapolseni_regija_dejavnost %>% rename("Zaposleni" = "ŠTEVILO ZAPOSLENIH") %>% mutate(Zaposleni = as.numeric(Zaposleni)) %>%
  group_by(REGIJA) %>% summarise(Zaposleni = sum(Zaposleni, na.rm = TRUE)) 
tabela1 <- inner_join(tabela1, legenda_regij, by = "REGIJA") %>% select(2,3)


tabela2 <- zapolseni_regija_dejavnost %>% rename("Zaposleni" = "ŠTEVILO ZAPOSLENIH") %>% mutate(Zaposleni = as.numeric(Zaposleni)) %>%
  group_by(DEJAVNOST) %>% summarise(Zaposleni = sum(Zaposleni, na.rm = TRUE)) 

graf1 <- ggplot(tabela1, aes(x=KRAJ, y=Zaposleni, group = 1)) +
  geom_bar(stat = "identity", aes(x=KRAJ, y=Zaposleni, group = 1), width = 0.5) + theme_bw() + coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 15))  +
  ggtitle("Zaposleni glede na regionalno enoto") 
#geombar za stil, kako tortica
graf1

graf2 <- ggplot(tabela2, aes(x=DEJAVNOST, y=Zaposleni, group = 1)) +
  geom_bar(stat = "identity", aes(x=DEJAVNOST, y=Zaposleni, group = 1), width = 0.5) + theme_bw() + coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 25))  + 
  ggtitle("Zaposleni glede na regionalno enoto")
graf2


colnames(dobicek_regija)[1] <- c('REGIJA')
dobicek_regija <- inner_join(dobicek_regija, legenda_regij, by = "REGIJA")
graf3 <- ggplot(dobicek_regija, aes(x = KRAJ)) + geom_bar(stat = "identity", aes(y = Profit, fill = leto), position = position_dodge()) +
  theme_bw() + coord_flip() 
graf3
#čudno pri 2016, gre v negativno in pozitivno 


colnames(prihodki_regija)[1] <- c('REGIJA')
prihodki_regija <- inner_join(prihodki_regija, legenda_regij, by = "REGIJA")
graf4 <- ggplot(prihodki_regija, aes(x = KRAJ)) + geom_bar(stat = "identity", aes(y = Dohodek, fill = leto), position = position_dodge()) +
  theme_bw() + coord_flip()
graf4



