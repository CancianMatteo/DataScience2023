library(tibble)
library(tidyr)
library("dplyr")

# REGOLA GENERALE: dataset in "_"notation solo importati, dataset in CamelCase sistemati

# Importo il parco veicolare per il 2021
parco_veicolare_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/parco_veicolare_2021.csv")
# Sistema il parco veicolare regione per regione
ParcoVeicolareRegioni = parco_veicolare_2021 %>% 
  filter(nchar(ITTER107) == 4,                        # Filtra le righe per regione (le regioni hanno lunghezza del codice 4)
         Territorio != "Non indicato",
         Tipo.dato == "parco veicolare",              # Filtra i dati di tipo "parco veicolare"
         Tipo.veicolo == "totale") %>%                # Filtra solo il totale dei veicoli (tutti indipendentemente dal tipo)
  select(Territorio, TIME, Value)
ParcoVeicolareRegioni %>% filter(Territorio!="Provincia Autonoma Trento") %>% 
  filter(Territorio!="Provincia Autonoma Bolzano / Bozen")


adattaProvinceMap = function(dataset){
  # rinomina la provincia di Aosta
  dataset = dataset %>%
    mutate(Territorio = stringr::str_replace(Territorio, "Valle d'Aosta / Vallée d'Aoste", "Aosta"))
  #1 mette la provincia di Barletta-Andria-Trani dentro a Bari
  dataset[8, 3] = dataset[8, 3] + dataset[9, 3]
  dataset = dataset[-9, ]
  #2 mette la provincia di Biella dentro a Vercelli
  dataset[93, 3] = dataset[93, 3] + dataset[12, 3]
  dataset = dataset[-12, ]
  # rinomina la provincia di Bolzano
  dataset = dataset %>%
    mutate(Territorio = stringr::str_replace(Territorio, "Bolzano / Bozen", "Bolzano-Bozen"))
  # rinomina la provincia di Forli'
  dataset = dataset %>%
    mutate(Territorio = stringr::str_replace(Territorio, "Forlì-Cesena", "Forli'"))
  #3 mette la provincia di Crotone dentro a Catanzaro
  dataset[22, 3] = dataset[22, 3] + dataset[26, 3]
  dataset = dataset[-26, ]
  #4 mette la provincia di Fermo dentro a Ascoli Piceno
  dataset[5, 3] = dataset[5, 3] + dataset[28, 3]
  dataset = dataset[-28, ]
  #5 mette la provincia di Lecco dentro a Como
  dataset[24, 3] = dataset[24, 3] + dataset[42, 3]
  dataset = dataset[-42, ]
  #6 mette la provincia di Lodi dentro a Milano
  dataset[51, 3] = dataset[51, 3] + dataset[43, 3]
  dataset = dataset[-43, ]
  #7 mette la provincia di Monza Brianza dentro a Milano
  dataset[50, 3] = dataset[50, 3] + dataset[51, 3]
  dataset = dataset[-51, ]
  # rinomina Reggio Calabria e Reggio Emilia
  dataset = dataset %>%
    mutate(Territorio = stringr::str_replace(Territorio, "Reggio di Calabria", "Reggio Calabria")) %>%
    mutate(Territorio = stringr::str_replace(Territorio, "Reggio nell'Emilia", "Reggio Emilia"))
  #8 mette la provincia di Prato dentro a Firenze
  dataset[30, 3] = dataset[30, 3] + dataset[67, 3]
  dataset = dataset[-67, ]
  #9 mette la provincia di Rimini dentro a Forli
  dataset[32, 3] = dataset[32, 3] + dataset[72, 3]
  dataset = dataset[-72, ]
  #10 mette la provincia di Sud Sardegna dentro a Cagliari
  dataset[16, 3] = dataset[16, 3] + dataset[80, 3]
  dataset = dataset[-80, ]
  #11 mette la provincia di Verbano-Cussio-Ossola dentro a Novara
  dataset[52, 3] = dataset[52, 3] + dataset[92, 3]
  dataset = dataset[-92, ]
  #12 mette la provincia di Vibo dentro a Catanzaro
  dataset[21, 3] = dataset[21, 3] + dataset[94, 3]
  dataset = dataset[-94, ]
  return(dataset)
}
# Sistema il parco veicolare provincia per provincia per il 2021
ParcoVeicolareProvince = parco_veicolare_2021 %>% 
  filter(nchar(ITTER107) == 5,                        # Filtra le righe per provincia (le provincia hanno lunghezza del codice 5)
         Territorio != "Non indicato",
         Territorio != "Totale",
         Tipo.dato == "parco veicolare",              # Filtra i dati di tipo "parco veicolare"
         Tipo.veicolo == "totale") %>%                # Filtra solo il totale dei veicoli (tutti indipendentemente dal tipo)
  select(Territorio, TIME, Value) %>% 
  arrange(Territorio)
ParcoVeicolareProvince = adattaProvinceMap(ParcoVeicolareProvince)



# importo i dataset veicoli incidenti
veicoli_incidenti_2001 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2001.csv")
veicoli_incidenti_2011 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2011.csv")
veicoli_incidenti_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/veicoli_incidenti_2021.csv")

IncidentiConVeicoliPerRegioni = rbind(veicoli_incidenti_2001, veicoli_incidenti_2011, veicoli_incidenti_2021)
# TIDY: Rimuovo le due province
IncidentiConVeicoliPerRegioni = IncidentiConVeicoliPerRegioni %>% filter(Territorio!="Provincia Autonoma Trento") %>% 
  filter(Territorio!="Provincia Autonoma Bolzano / Bozen")
# TRANSFORM: Sistema i veicoli/incidenti regione per regione
IncidentiConVeicoliPerRegioni = IncidentiConVeicoliPerRegioni %>% 
  filter(nchar(ITTER107) == 4,                        # Filtra le righe per regione (le regioni hanno lunghezza del codice 4)
         Territorio != "Non indicato",
         Localizzazione.dell.incidente.. == "totale", # Filtra solo il totale (tutti indipendentemente dal tipo di strada)
         Intersezione == "totale",
         Natura.dell.incidente == "totale",           # Filtra solo i dati di tutti gli incidenti senza distinzione di tipo
         Categoria.dei.veicoli == "totale",
         Mese == "totale") %>%                        # Filtra solo i dati annuali
  select(Territorio, TIME, Value)


IncidentiConVeicoliProvince = veicoli_incidenti_2021 %>%
  filter(nchar(ITTER107) == 5,                       # Filtra le righe per provincia (le provincia hanno lunghezza del codice 5)
        Territorio != "Non indicato",
        Localizzazione.dell.incidente.. == "totale", # Filtra solo il totale (tutti indipendentemente dal tipo di strada)
        Intersezione == "totale",
        Natura.dell.incidente == "totale",           # Filtra solo i dati di tutti gli incidenti senza distinzione di tipo
        Categoria.dei.veicoli == "totale",
        Mese == "totale") %>%                        # Filtra solo i dati annuali
  select(Territorio, TIME, Value) %>%
  arrange(Territorio)
IncidentiConVeicoliProvince = adattaProvinceMap(IncidentiConVeicoliProvince)



# importo i dataset degli incidenti in italia per l'analisi dei tempi (fasce orarie e giorni)
incidenti_stradali = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/incidenti_stradali_tempi_2001-2021.csv")

IncidentiPerOrarioItalia = incidenti_stradali %>%
  filter(Territorio == "Italia",
         Localizzazione.dell.incidente == "totale",   # Filtra solo il totale (tutti indipendentemente dal tipo di strada)
         Intersezione == "totale",
         Natura.dell.incidente == "totale",           # Filtra solo i dati di tutti gli incidenti senza distinzione di tipo
         Incidente.mortale == "totale",
         ORA != 99,
         ORA != 25,
         Giorno.della.settimana == "totale",
         Mese == "totale") %>%                        # Filtra solo i dati annuali
  select(Territorio, ORA, TIME, Value)

IncidentiPerGiornoItalia  = incidenti_stradali %>%
  filter(Territorio == "Italia",
         Localizzazione.dell.incidente == "totale",   # Filtra solo il totale (tutti indipendentemente dal tipo di strada)
         Intersezione == "totale",
         Natura.dell.incidente == "totale",           # Filtra solo i dati di tutti gli incidenti senza distinzione di tipo
         Incidente.mortale == "totale",
         ORA == 99,
         Giorno.della.settimana != "totale",
         Mese == "totale") %>%                        # Filtra solo i dati annuali
  select(Territorio, Giorno.della.settimana, TIME, Value)


# Importa la popolazione residente
popolazione_residente_2001 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2001.csv")
popolazione_residente_2011 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2011.csv")
popolazione_residente_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/Popolazione_residente_regioni_2021.csv")

# TIDY: Unisco Trento e Bolzano e cancello le due province, poi tolgo le '' al nome della Valle d'Aosta
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
sistema_popolazione_residente = function(datasetPopolazione){
  return(datasetPopolazione %>%
           filter(Sesso == "Totale",
                  Classe.di.età == "Totale") %>%
           # le virgole che vengono usate come separatore di migliaia si incasinano con il resto del csv quindi bisogna unire le false colonne
           mutate(Valore = strtoi(paste(gsub('\'', '', Osservazione), gsub('\'', '', Stato.dell.osservazione), gsub('\'', '', Nota...Territorio), sep=""))) %>%   
           select(Territorio, Anno, Valore)
         )
}

PopolazioneResidente = rbind(
  sistemaMontanari(sistema_popolazione_residente(popolazione_residente_2001), 2001), 
  sistemaMontanari(sistema_popolazione_residente(popolazione_residente_2011), 2011), 
  sistemaMontanari(sistema_popolazione_residente(popolazione_residente_2021), 2021))


popolazione_residente_province_2021 = read.csv("/Users/matteocancian/Documents/UNIUD/1° ANNO/DATA SCIENCE/Incidenti d'auto/ISTAT_CSV/popolazione_residente_province_2021.csv")
PopolazioneResidenteProvince = popolazione_residente_province_2021 %>%
  select(Territorio, TIME, Value) %>%
  arrange(Territorio)
PopolazioneResidenteProvince = adattaProvinceMap(PopolazioneResidenteProvince)
