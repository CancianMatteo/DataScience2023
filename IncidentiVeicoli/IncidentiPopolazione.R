source("ImportTidyTransform.R")

library("ggplot2")
library(dplyr)
library(tibble)

# INNER_JOIN between the datasets PopolazioneResidente and IncidentiConVeicoli
RapportoIncidentiPopolazione = inner_join(PopolazioneResidente, IncidentiConVeicoliPerRegioni, join_by(Territorio==Territorio, Anno==TIME))
RapportoIncidentiPopolazione = rename(RapportoIncidentiPopolazione, nIncidenti = Value, nPopolazione = Valore)
# Add the relationship between nIncidenti and nPopolazione (incidenti per mille abitanti)
RapportoIncidentiPopolazione = RapportoIncidentiPopolazione %>%
  mutate(IncidentiPerMilleAbitanti = nIncidenti*1000 / nPopolazione)

ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = Territorio, y = IncidentiPerMilleAbitanti, color = Anno)) +
  geom_point() + 
  theme(axis.text.x=element_text(angle=60, hjust=1, vjust=0.5, margin = margin(t = -50, r = 0, b = 0, l = 0, unit = "pt")))

ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = nPopolazione, y = nIncidenti, color = Anno)) +
  geom_point() + 
  geom_smooth(se = FALSE)

#ggplot(data = RapportoIncidentiPopolazione, mapping = aes(x = nPopolazione, y = nIncidenti, color = Anno)) +
#  geom_histogram() + 
#  geom_smooth(se = FALSE)