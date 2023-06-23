library(ggplot2)


# Riordina la rappresentazione visiva delle fette di torta in modo crescente
# crea grafico a torta con gli incidenti nei vari tipi di strada (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerTipoStradaItalia, aes(x = "", y = ValoriPercentuali, fill = Localizzazione.dell.incidente)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  labs(fill = "Tipo di strada dell'incidente") +
  geom_text(aes(label = ValoriPercentuali), position = position_stack(vjust = 0.5))



# crea grafico con gli incidenti nelle varie intersezioni (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerIntersezioneItalia, aes(x = "", y = ValoriPercentuali, fill = Intersezione)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = ValoriPercentuali), position = position_stack(vjust = 0.5))



# crea grafico con gli incidenti nelle varie intersezioni (somma di tutti gli anni dal 2001 al 2021)
ggplot(data = IncidentiPerNaturaItalia, aes(x = "", y = ValoriPercentuali, fill = Natura.dell.incidente)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = ValoriPercentuali), position = position_stack(vjust = 0.5))
