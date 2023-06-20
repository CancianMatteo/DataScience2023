library(maps)
library(ggplot2)

# INNER_JOIN tra i dataset ParcoVeicolareProvince e IncidentiConVeicoli
RapportoIncidentiVeicoliProvince = inner_join(ParcoVeicolareProvince, IncidentiConVeicoliProvince, join_by(Territorio==Territorio, TIME==TIME))
RapportoIncidentiVeicoliProvince = rename(RapportoIncidentiVeicoliProvince, nVeicoli = Value.x, nIncidenti = Value.y)
# Aggiunge il rapporto tra nIncidenti e nVeicoli
RapportoIncidentiVeicoliProvince = RapportoIncidentiVeicoliProvince %>%
  mutate(IncidentiVeicoli = nIncidenti / nVeicoli)

# Carica la mappa dell'italia (confini geospaziali per provincia)
mappa_italia = map_data("italy")
mappa_italia = rename(mappa_italia, province=region)

# Creazione della funzione per generare i colori
arrayColori = colorRampPalette(c("green", "red"))
# Generazione dell'array di tonalità di colori
tonalitaColori = arrayColori(95)[as.numeric(cut(RapportoIncidentiVeicoliProvince$IncidentiVeicoli, length(RapportoIncidentiVeicoliProvince$IncidentiVeicoli)))]

# Crea un dataframe con province e colori
ColoriProvince = data.frame(
  province = RapportoIncidentiVeicoliProvince,
  colori = tonalitaColori
) %>% arrange(province.Territorio)

# Disegna la mappa con i colori
ggplot() +
  geom_polygon(data = mappa_italia, aes(x = long, y = lat, group = group, fill = province), color = "black") +
  scale_fill_manual(values = ColoriProvince$colori) +
  theme_void() +     # per togliere assi, ecc dal grafico
  theme(legend.position = "right", ) # Aggiunge la legenda a destra