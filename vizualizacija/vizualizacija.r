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
#geom_bar za stil, kako tortica
graf1

graf2 <- ggplot(tabela2, aes(x=DEJAVNOST, y=Zaposleni, group = 1)) +
  geom_bar(stat = "identity", aes(x=DEJAVNOST, y=Zaposleni, group = 1), width = 0.5) + theme_bw() + coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x,width = 25))  + 
  ggtitle("Zaposleni glede na dejavnost")
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

#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih manj kot 10 ljudi 
graf5 <- ggplot(do10, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top")
graf5  
#problem z grafom, imena na x koordinati se prekrivajo.   
  
#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih med 10 in 30 ljudi 
graf6 <- ggplot(od10do30, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top")
graf6  

#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih med 30 in 100 ljudi 
graf7 <- ggplot(od30do100, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top")
graf7  

#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih več kot 100 ljudi 
graf8 <- ggplot(nad100, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top")
graf8  
 

do10 <- prihodki_zaposleni %>% filter(prihodki_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 10) 
do10 <- do10 %>% group_by(leto, DEJAVNOST) %>% summarise(Dohodek = sum(`Dohodek`, na.rm = TRUE))
  
graf9 <- ggplot(do10, aes(x = leto, y = Dohodek, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE))
graf9    
#legenda pobegne iz robov  

od10do30 <- od10do30 %>% group_by(leto, DEJAVNOST) %>% summarise(Dohodek = sum(`Dohodek`, na.rm = TRUE))
graf10 <- ggplot(do10, aes(x = leto, y = Dohodek, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE))
graf10


od30do100 <- od30do100 %>% group_by(leto, DEJAVNOST) %>% summarise(Dohodek = sum(`Dohodek`, na.rm = TRUE))
graf11 <- ggplot(do10, aes(x = leto, y = Dohodek, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE))
graf11


nad100 <- nad100 %>% group_by(leto, DEJAVNOST) %>% summarise(Dohodek = sum(`Dohodek`, na.rm = TRUE))
graf12 <- ggplot(do10, aes(x = leto, y = Dohodek, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE))
graf12

Slovenija <- uvozi.zemljevid("http://biogeo.ucdavis.edu/data/gadm2.8/shp/SVN_adm_shp.zip", "SVN_adm1", encoding="windows-1250") %>% fortify() 
colnames(Slovenija)[12] <- 'REGIJA'
Slovenija$REGIJA <- gsub('Gorenjska', 'Kranj', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('GoriĹˇka', 'Nova Gorica', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Spodnjeposavska', 'Novo mesto', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Zasavska', 'Ljubljana', Slovenija$REGIJA) 
Slovenija$REGIJA <- gsub('Jugovzhodna Slovenija', 'Novo mesto', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('KoroĹˇka', 'Maribor', Slovenija$REGIJA) 
Slovenija$REGIJA <- gsub('Notranjsko-kraĹˇka', 'Koper', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Obalno-kraĹˇka', 'Koper', Slovenija$REGIJA) 
Slovenija$REGIJA <- gsub('Osrednjeslovenska', 'Ljubljana', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Podravska', 'Maribor', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Pomurska', 'Murska Sobota', Slovenija$REGIJA)
Slovenija$REGIJA <- gsub('Savinjska', 'Celje', Slovenija$REGIJA) 


colnames(tabela1)[2] <- 'REGIJA'
Slovenija1 <- right_join(tabela1, Slovenija, by = "REGIJA")

Zemljevid <- ggplot() +
  geom_polygon(data = Slovenija1, aes(x = long, y = lat, group = group, fill = Zaposleni))+
  geom_path(data = Slovenija1, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Zaposleni v regiji') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_viridis(option = "viridis", direction = -1) + 
  coord_fixed()
Zemljevid
























