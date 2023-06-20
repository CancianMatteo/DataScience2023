library(maps)
library(ggplot2)
library(dplyr)
library(tibble)

# INNER_JOIN tra i dataset PopolazioneResidente e IncidentiConVeicoli
RapportoIncidentiPopolazioneProvince = inner_join(PopolazioneResidenteProvince, IncidentiConVeicoliProvince, join_by(Territorio==Territorio, TIME==TIME))
RapportoIncidentiPopolazioneProvince = rename(RapportoIncidentiPopolazioneProvince, nPopolazione = Value.x, nIncidenti = Value.y)
# Aggiunge il rapporto tra nIncidenti e nPopolazione (incidenti per mille abitanti)
RapportoIncidentiPopolazioneProvince = RapportoIncidentiPopolazioneProvince %>%
  mutate(IncidentiPerMlnAbitanti = nIncidenti*1000000 / nPopolazione) %>% 
  arrange(IncidentiPerMlnAbitanti)

# Carica la mappa dell'italia (confini geospaziali per provincia)
mappa_italia = map_data("italy")
mappa_italia = rename(mappa_italia, province=region)

# Creazione della funzione per generare i colori
arrayColori = colorRampPalette(c("green", "red"))
# Generazione dell'array di tonalità di colori
tonalitaColori = arrayColori(95)[as.numeric(cut(RapportoIncidentiPopolazioneProvince$IncidentiPerMlnAbitanti, length(RapportoIncidentiPopolazioneProvince$IncidentiPerMlnAbitanti)))]

# Crea un dataframe con province e colori
ColoriProvince = data.frame(
      province = RapportoIncidentiPopolazioneProvince,
      colori = tonalitaColori
  ) %>% arrange(province.Territorio)

# Disegna la mappa con i colori
 ggplot() +
   geom_polygon(data = mappa_italia, aes(x = long, y = lat, group = group, fill = province), color = "black") +
   scale_fill_manual(values = ColoriProvince$colori) +
   theme_void() +     # per togliere assi, ecc dal grafico
   theme(legend.position = "right", ) # Aggiunge la legenda a destra
 