library(ggplot2)
library(dplyr)

# crea grafico con gli incidenti nelle varie fasce orarie (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerOrarioItalia %>% select(ORA, Value) %>% group_by(ORA), 
       mapping = aes(x = ORA, y = Value)) +
  geom_col() +
  xlab("Fascia oraria") +
  ylab("Numero incidenti")
# Crea grafico con gli incidenti nelle varie fascie orarie filtrati per i tre anni simbolici (2001, 2011, 2021)
ggplot(data = IncidentiPerOrarioItalia %>% filter(Anno %in% c(2001, 2011, 2021)), 
       mapping = aes(x = ORA, y = Value, color = Anno)) +
  geom_point() +
  xlab("Fascia oraria") +
  ylab("Numero incidenti")



# Riordina i giorni della settimana nel modo desiderato
IncidentiPerGiornoItalia$Giorno.della.settimana = factor(IncidentiPerGiornoItalia$Giorno.della.settimana, levels = c("lunedì", "martedì", "mercoledì", "giovedì", "venerdì", "sabato", "domenica"))

# Crea grafico con gli incidenti nei vari giorni della settimana (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerGiornoItalia %>% select(Giorno.della.settimana, Value) %>% group_by(Giorno.della.settimana),
       mapping = aes(x = Giorno.della.settimana, y = Value)) +
  geom_col(fill = "orange") +
  xlab("Giorno della settimana") +
  ylab("Numero incidenti")



# Riordina i mesi nel modo desiderato
IncidentiPerMeseItalia$Mese = factor(IncidentiPerMeseItalia$Mese, levels = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre"))

# crea grafico con gli incidenti nei vari mesi (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerMeseItalia, mapping = aes(x = Mese, y = Value)) +
  geom_col(fill = "green") +
  xlab("Mese") +
  ylab("Numero incidenti")


# Crea grafico con gli incidenti nei vari mesi filtrati per i tre anni simbolici (2001, 2011, 2021)
ggplot(data = IncidentiPerMeseItalia %>% filter(Anno %in% c(2001, 2011, 2021)), 
       mapping = aes(x = Mese, y = Value, color = Anno)) +
  geom_point() +
  xlab("Mese") +
  ylab("Numero incidenti")
