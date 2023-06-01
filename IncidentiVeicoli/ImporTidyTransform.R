library(tibble)
library(tidyr)
library("dplyr")

#dataset in "_" notation solo importati, dataset in CamelCase sistemati

# Sistema il parco veicolare regione per regione
parco_veicolare_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/parco_veicolare_2021.csv")
ParcoVeicolare = parco_veicolare_2021 %>% 
  filter(nchar(ITTER107) == 4,                        # Filtra le righe per regione (le regioni hanno lunghezza del codice 4)
         Territorio != "Non indicato",
         Tipo.dato == "parco veicolare",              # Filtra i dati di tipo "parco veicolare"
         Tipo.veicolo == "totale") %>%                # Filtra solo il totale dei veicoli (tutti indipendentemente dal tipo)
  select(Territorio, TIME, Value)
ParcoVeicolare %>% filter(Territorio!="Provincia Autonoma Trento") %>% 
  filter(Territorio!="Provincia Autonoma Bolzano / Bozen")


# Sistema i veicoli/incidenti regione per regione
veicoli_incidenti_2001 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2001.csv")
veicoli_incidenti_2011 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2011.csv")
veicoli_incidenti_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2021.csv")
IncidentiConVeicoli = rbind(veicoli_incidenti_2001, veicoli_incidenti_2011, veicoli_incidenti_2021)
IncidentiConVeicoli = IncidentiConVeicoli %>% 
  filter(nchar(ITTER107) == 4,                        # Filtra le righe per regione (le regioni hanno lunghezza del codice 4)
         Territorio != "Non indicato",
         Localizzazione.dell.incidente.. == "totale", # Filtra solo il totale (tutti indipendentemente dal tipo di strada)
         Intersezione == "totale",
         Natura.dell.incidente == "totale",           # Filtra solo i dati di tutti gli incidenti senza distinzione di tipo
         Categoria.dei.veicoli == "totale",
         Mese == "totale") %>%                        # Filtra solo i dati annuali
  select(Territorio, TIME, Value)
# rimuovo le due province
IncidentiConVeicoli = IncidentiConVeicoli %>% filter(Territorio!="Provincia Autonoma Trento") %>% 
  filter(Territorio!="Provincia Autonoma Bolzano / Bozen")


# Sistema la popolazione residente
# funzione unica per i 3 anni
fix_popolazione_residente = function(datasetPopolazione){
  return(datasetPopolazione %>%
           filter(Sesso == "Totale",
                  Classe.di.età == "Totale") %>% 
           mutate(Valore = strtoi(paste(gsub('\'', '', Osservazione), gsub('\'', '', Stato.dell.osservazione), gsub('\'', '', Nota...Territorio), sep=""))) %>%   
           select(Territorio, Anno, Valore)
         )
}
popolazione_residente_2001 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2001.csv")
popolazione_residente_2001 = fix_popolazione_residente(popolazione_residente_2001)

popolazione_residente_2011 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2011.csv")
popolazione_residente_2011 = fix_popolazione_residente(popolazione_residente_2011)

popolazione_residente_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2021.csv")
popolazione_residente_2021 = fix_popolazione_residente(popolazione_residente_2021)

# unisco Trento e Bolzano e cancello le due province, poi tolgo le '' al nome della Valle d'Aosta
sistemaMontanari = function (dataset, anno){
  if( nrow(dataset%>%filter(Territorio=="Trentino Alto Adige / Südtirol")) == 0 ){ # controllo che non ci sia già il Trentino
    trento = dataset %>%
      filter(Territorio == "Provincia Autonoma Trento")
    bolzano = dataset %>%
      filter(Territorio == "Provincia Autonoma Bolzano / Bozen")
    valoreTrentino = trento[["Valore"]]+bolzano[["Valore"]];
    dataset = rbind(dataset, list("Trentino Alto Adige / Südtirol", anno, valoreTrentino))
  }
  
  dataset = dataset %>%
    mutate(Territorio = stringr::str_replace(Territorio, "'Valle d'Aosta / Vallée d'Aoste'", "Valle d'Aosta / Vallée d'Aoste"))
  
  return(dataset %>% filter(Territorio!="Provincia Autonoma Trento") %>% 
                     filter(Territorio!="Provincia Autonoma Bolzano / Bozen")
         )
}

PopolazioneResidente = rbind(
  sistemaMontanari(popolazione_residente_2001, 2001), 
  sistemaMontanari(popolazione_residente_2011, 2011),
  sistemaMontanari(popolazione_residente_2021, 2021))