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


tabela2 <- zapolseni_regija_dejavnost %>% rename("Zaposleni" = "ŠTEVILO ZAPOSLENIH") %>% mutate(Zaposleni = as.numeric(Zaposleni)) %>%
  group_by(DEJAVNOST) %>% summarise(Zaposleni = sum(Zaposleni, na.rm = TRUE)) 


graf1 <- ggplot(data = tabela1, aes(x=REGIJA, y=Zaposleni) + 
  geom_bar(stat = "identity", width = 0.5)) 
