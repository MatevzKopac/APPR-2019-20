# 4. faza: Analiza podatkov

#dobiček v miljonih evrov


dobicek_regija
dobicek2 <- dobicek_regija %>% group_by(leto) %>% summarise(Profit=sum(Profit)/ 1e6)
dobicek2$leto <- parse_integer(dobicek2$leto)

graf_regresija1 <- ggplot(dobicek2, aes(x=leto, y=Profit)) + geom_point() + 
  geom_smooth(method='lm', fullrange=TRUE, color='green', formula = y ~ x) +
  scale_x_continuous('leto', limits = c(2015,2025), breaks=seq(2015, 2025, 2)) +
  ylab('Dobički v milijonih evrov')+
  ggtitle('Napoved dobičkov v zdravstvu do leta 2025')
graf_regresija1

