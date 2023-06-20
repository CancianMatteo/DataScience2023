library(ggplot2)
library(dplyr)
library(tibble)

# INNER_JOIN tra i dataset PopolazioneResidente e IncidentiConVeicoli
RapportoIncidentiPopolazione = inner_join(PopolazioneResidente, IncidentiConVeicoliPerRegioni, join_by(Territorio==Territorio, Anno==TIME))
RapportoIncidentiPopolazione = rename(RapportoIncidentiPopolazione, nIncidenti = Value, nPopolazione = Valore)
# Aggiunge il rapporto tra nIncidenti e nPopolazione (incidenti per mille abitanti)
RapportoIncidentiPopolazione = RapportoIncidentiPopolazione %>%
  mutate(IncidentiPerMlnAbitanti = nIncidenti*1000000 / nPopolazione)

ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = Territorio, y = IncidentiPerMlnAbitanti, color = Anno)) +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=0.5, margin = margin(t = -50, r = 0, b = 0, l = 0, unit = "pt")))

ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = nPopolazione, y = nIncidenti, color = Anno)) +
  geom_point() + 
  geom_smooth(se = FALSE)

#ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = nPopolazione, y = nIncidenti, color = Anno)) +
#  geom_histogram() + 
#  geom_smooth(se = FALSE)