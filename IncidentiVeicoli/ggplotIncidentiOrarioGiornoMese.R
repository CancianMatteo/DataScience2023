library(ggplot2)
library(dplyr)
library(tibble)

# crea grafico con gli incidenti nelle varie fasce orarie (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerOrarioItalia %>% select(Territorio, ORA, Value) %>% group_by(ORA), 
       mapping = aes(x = ORA, y = Value)) +
  geom_col()

# Crea grafico con gli incidenti nelle varie fascie orarie filtrati per i tre anni simbolici (2001, 2011, 2021)
ggplot(data = IncidentiPerOrarioItalia %>% filter(TIME %in% c(2001, 2011, 2021)), 
       mapping = aes(x = ORA, y = Value, color = TIME)) +
  geom_point()


# Riordina i giorni della settimana nel modo desiderato
IncidentiPerGiornoItalia$Giorno.della.settimana = factor(IncidentiPerGiornoItalia$Giorno.della.settimana, levels = c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato", "domenica"))

# Crea grafico con gli incidenti nei vari giorni della settimana (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerGiornoItalia %>% select(Territorio, Giorno.della.settimana, Value) %>% group_by(Giorno.della.settimana), 
       mapping = aes(x = Giorno.della.settimana, y = Value)) +
  geom_col() +
  xlab("Giorno della settimana") +
  ylab("Numero incidenti")
