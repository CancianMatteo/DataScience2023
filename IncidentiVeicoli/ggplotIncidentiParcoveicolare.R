library(ggplot2)
library(dplyr)
library(tibble)

# INNER_JOIN tra i dataset ParcoVeicolareRegioni e IncidentiConVeicoli
RapportoIncidentiVeicoli = inner_join(ParcoVeicolareRegioni, IncidentiConVeicoliPerRegioni, join_by(Territorio==Territorio, TIME==TIME))
RapportoIncidentiVeicoli = rename(RapportoIncidentiVeicoli, nIncidenti = Value.x, nVeicoli = Value.y)
# Aggiunge il rapporto tra nIncidenti e nVeicoli
RapportoIncidentiVeicoli = RapportoIncidentiVeicoli %>%
  mutate(IncidentiVeicoli = nIncidenti*1000 / nVeicoli)

ggplot(data = RapportoIncidentiVeicoli, mapping = aes(x = Territorio, y = IncidentiVeicoli, color = TIME)) +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=0.5, margin = margin(t = -50, r = 0, b = 0, l = 0, unit = "pt")))

ggplot(data = RapportoIncidentiVeicoli, mapping = aes(x = nVeicoli, y = nIncidenti, color = TIME)) +
  geom_point() + 
  geom_smooth(se = FALSE)