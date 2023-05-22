library("ggplot2")
library(dplyr)
library(tibble)

# INNER_JOIN between the datasets PopolazioneResidente and IncidentiConVeicoli
RapportoIncidentiPopolazione = inner_join(PopolazioneResidente, IncidentiConVeicoli, join_by(Territorio==Territorio, Anno==TIME))
RapportoIncidentiPopolazione = rename(RapportoIncidentiPopolazione, nIncidenti = Value, nPopolazione = Valore)
# Add the relationship between nIncidenti and nPopolazione (incidenti per mille abitanti)
RapportoIncidentiPopolazione = RapportoIncidentiPopolazione %>%
  mutate(IncPerPop = nIncidenti*1000 / nPopolazione)

ggplot(data = RapportoIncidentiPopolazione) +
  geom_point(mapping = aes(x = Territorio, y = IncPerPop))