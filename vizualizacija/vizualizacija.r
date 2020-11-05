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


graf1 <- ggplot(tabela1, aes(x="", y=Zaposleni, fill=KRAJ)) +
  geom_col(width=1) + coord_polar(theta="y") +
  theme_bw() + xlab("") +
  scale_fill_discrete(labels=function(x) str_wrap(x, width=15)) +
  ggtitle("Zaposleni glede na regijo")
graf1


graf2 <- ggplot(tabela2, aes(x="", y=Zaposleni, fill=DEJAVNOST)) +
  geom_col(width=1) + coord_polar(theta="y") +
  theme_bw() + xlab("") +
  scale_fill_discrete(labels=function(x) str_wrap(x, width=15)) +
  ggtitle("Zaposleni glede na dejavnost")
graf2


colnames(dobicek_regija)[1] <- c('REGIJA')
profit_regija <- inner_join(dobicek_regija, legenda_regij, by = "REGIJA")
graf3 <- ggplot(profit_regija %>% group_by(leto, KRAJ) %>% summarise(Profit=sum(Profit)),
                aes(x=parse_number(leto), y=Profit / 1e6, color=KRAJ)) + geom_line() + xlab("Leto") +
  ggtitle("Dobiček v posameznih letih") + ylab("Dobiček v milijonih €") 
theme_bw() 
graf3


colnames(prihodki_regija)[1] <- c('REGIJA')
prihodki_regija <- inner_join(prihodki_regija, legenda_regij, by = "REGIJA")
graf4 <- ggplot(prihodki_regija, aes(x = KRAJ)) + geom_bar(stat = "identity", aes(y = Dohodek / 1e6, fill = leto), position = position_dodge()) +
  theme_bw() + coord_flip() +
  ggtitle("Prihodki v regijah milijonih €") + ylab("Prihodki")
graf4

#Prihodki glede na zdravstveno dejavnost, kjer je zaposlenih manj kot 10 ljudi 
graf5 <- ggplot(do10, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek / 1e6, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top") +
  ggtitle("Prihodki na dejavnost v podjetjih z manj kot 10 zaposlenimi v milijonih €")  + ylab("Prihodki v milijonih €") 
graf5  

#Prihodki glede na zdravstveno dejavnost, kjer je zaposlenih med 10 in 30 ljudi 
graf6 <- ggplot(od10do30, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek / 1e6, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top")  +
  ggtitle("Prihodki na dejavnost v podjetjih z med 10 in 30 zaposlenimi v milijonih €") + ylab("Prihodki v milijonih €")
graf6  

#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih med 30 in 100 ljudi 
graf7 <- ggplot(od30do100, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek / 1e6, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top") +
  ggtitle("Prihodki na dejavnost v podjetjih z med 30 in 100 zaposlenimi v milijonih €") + ylab("Prihodki v milijonih €")
graf7  

#Prihodki glede na zdravstvena dejavnost, kjer je zaposlenih več kot 100 ljudi 
graf8 <- ggplot(nad100, aes(x = DEJAVNOST)) + geom_bar(stat = "identity", aes(y = Dohodek / 1e6, fill = leto), position = position_dodge()) +
  theme_bw() + scale_x_discrete(labels = function(y) str_wrap(y,width = 1)) + theme(legend.position = "top") +
  ggtitle("Prihodki na dejavnost v podjetjih z več kot 100 zaposlenimi v milijonih €") + ylab("Prihodki v milijonih €")
graf8  

## Profit glede na leto in regijo 

profit_do10 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 10) 
profit_do10 <- profit_do10 %>% group_by(Leto, DEJAVNOST) %>% summarise(Profit = sum(`Profit`, na.rm = TRUE))

graf9 <- ggplot(profit_do10, aes(x = Leto, y = Profit / 1e6, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE)) +
  ggtitle("Dobiček po letih 2015-2018 v podjetjih z manj kot 10 zaposlenimi v milijonih €") + ylab("Dobiček v milijonih €")
graf9 



profit_od10do30 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` > 10, dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 30) 
profit_od10do30 <- profit_od10do30 %>% group_by(Leto, DEJAVNOST) %>% summarise(Profit = sum(`Profit`, na.rm = TRUE))
graf10 <- ggplot(profit_od10do30, aes(x = Leto, y = Profit / 1e6, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE)) +
  ggtitle("Dobiček v letih 2015-2018 v podjetjih z med 10 in 30 zaposlenimi v milijonih €") + ylab("Dobiček v milijonih €")
graf10


profit_od30do100 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` > 30, dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` <= 100) 
profit_od30do100 <- profit_od30do100 %>% group_by(Leto, DEJAVNOST) %>% summarise(Profit = sum(`Profit`, na.rm = TRUE))
graf11 <- ggplot(profit_od30do100, aes(x = Leto, y = Profit / 1e6, group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE)) +
  ggtitle("Dobiček v letih 2015-2018 v podjetjih z med 30 in 100 zaposlenimi v milijonih €") + ylab("Dobiček v milijonih €")
graf11

profit_nad100 <- dobicek_zaposleni %>% filter(dobicek_zaposleni$`ŠTEVILO ZAPOSLENIH` > 100) 
profit_nad100 <- profit_nad100 %>% group_by(Leto, DEJAVNOST) %>% summarise(Profit = sum(`Profit`, na.rm = TRUE))
graf12 <- ggplot(profit_nad100, aes(x = Leto, y = Profit / 1e6 , group = DEJAVNOST)) + geom_line(aes(color = DEJAVNOST)) + geom_point() + 
  theme(legend.position = "bottom") + guides(fill=guide_legend(nrow=6, byrow=TRUE)) +
  ggtitle("Dobiček v letih 2015-2018 v podjetjih z več kot 100 zaposlenimi v milijonih v milijonih €") + ylab("Dobiček v milijonih €")
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

Zemljevid1 <- ggplot() +
  geom_polygon(data = Slovenija1, aes(x = long, y = lat, group = group, fill = Zaposleni))+
  geom_path(data = Slovenija1, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Zaposleni v regiji') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_viridis(option = "viridis", direction = -1) + 
  coord_fixed()
Zemljevid1

#

dobicek_regija1 <- dobicek_regija %>% group_by(REGIJA) %>% summarise(Profit=sum(Profit)/ 1e6)
dobicek_regija1$REGIJA  <- legenda_regij$KRAJ
Slovenija2 <- right_join(dobicek_regija1, Slovenija, by = "REGIJA")

Zemljevid2 <- ggplot() +
  geom_polygon(data = Slovenija2, aes(x = long, y = lat, group = group, fill = Profit))+
  geom_path(data = Slovenija2, aes(x = long,y = lat, group = group),color = "white", size = 0.1) +
  xlab("") + ylab("") + ggtitle('Profit v milijonih evrov po regijah') + 
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank()) +
  scale_fill_viridis(option = "viridis", direction = -1) + 
  coord_fixed()
Zemljevid2





















