library("ggplot2")
library(dplyr)
library(tibble)

# INNER_JOIN between the datasets PopolazioneResidente and IncidentiConVeicoli
RapportoIncidentiPopolazione = inner_join(PopolazioneResidente, IncidentiConVeicoli, join_by(Territorio==Territorio, Anno==TIME))
RapportoIncidentiPopolazione = rename(RapportoIncidentiPopolazione, nIncidenti = Value, nPopolazione = Valore)
# Add the relationship between nIncidenti and nPopolazione (incidenti per mille abitanti)
RapportoIncidentiPopolazione = RapportoIncidentiPopolazione %>%
  mutate(IncPerMilleAbitanti = nIncidenti*1000 / nPopolazione)

ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = Territorio, y = IncPerMilleAbitanti, color = Anno)) +
  geom_point() + 
  geom_smooth(se = FALSE) +
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=0.5))


