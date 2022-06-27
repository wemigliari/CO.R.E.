library(readr)
library(dplyr)
library(plyr)
library(tidyr)
library(xlsx)
library(ggplot2)


### 2011-2020
### All the contracts published on TED's portal
spain_emergency_2020 <- read.xlsx("/Users/wemigliari/Documents/R/tabelas/uoc_contracts/tabla-contratos-emergencia21062020.xlsx",
                                  sheetName = "Emergencias COVID")
spain_emergency_2020 <- data.frame(spain_emergency_2020)
spain_emergency_2020$year <- 2020

test <- count(spain_emergency_2020$PLATAFORMA.)

spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA CATALUÑA"] <- "Plataforma Cataluña"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA EUSKADI"] <- "Plataforma Euskadi"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA LA RIOJA"] <- "Plataforma La Rioja"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA MADRID"] <- "Plataforma Madrid"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA ANDALUCIA"] <- "Plataforma Andalucia"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA GALICIA"] <- "Plataforma Galicia"
spain_emergency_2020$PLATAFORMA.[spain_emergency_2020$PLATAFORMA. == "PLATAFORMA NAVARRA"] <- "Plataforma Navarra"


platform <- data.frame(count(spain_emergency_2020$PLATAFORMA.))
platform <- platform %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
platform$x <- factor(platform$x, levels = platform$x[order(platform$freq)])


ggplot(data=platform, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=freq), vjust=1.6, color="gray", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.")
 
########


competition <-data.frame(count(sort(spain_emergency_2020$ÓRGANO.CONTRATACIÓN.)))
competition <- competition %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))


########

levels <- data.frame(count(spain_emergency_2020$NATURALEZA.OC))
levels <- levels %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
levels$x <- factor(levels$x, levels = levels$x[order(levels$freq)])


ggplot(data=levels, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#011f4b")+
  geom_text(aes(label=freq), vjust=1.6, color="gray", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Public Administration Level",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.")


spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "3"] <- "March"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "4"] <- "April"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "5"] <- "May"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "6"] <- "June"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "12"] <- "December"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "febrero"] <- "February"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "marzo"] <- "March"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "abril"] <- "April"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "mayo"] <- "May"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "junio"] <- "June"
spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES[spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES == "m"] <- "March"

month <- data.frame(count(spain_emergency_2020$ANUNCIO.ADJUDICACIÓN.MES))
month <- month %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
month$x <- factor(month$x, levels = month$x[order(month$freq)])

ggplot(data=month, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#b3cde0")+
  geom_text(aes(label=freq), vjust=1.6, color="black", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Month",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.")



#########

type_contract <- data.frame(count(spain_emergency_2020$TIPO.CONTRATO))
type_contract <- type_contract %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
type_contract$x <- factor(type_contract$x, levels = type_contract$x[order(type_contract$freq)])

ggplot(data=type_contract, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#6497b1")+
  geom_text(aes(label=freq), vjust=1.6, color="black", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Type of Contract",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.")


#########

spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "NO INFO"] <- "No Info"
spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "No info"] <- "No Info"
spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "otros"] <- "Otros"
spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "OTROS"] <- "Otros"
spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "abierto"] <- "Abierto"
spain_emergency_2020$PROCEDIMIENTO[spain_emergency_2020$PROCEDIMIENTO == "Derivado de acuerdo marco"] <- "Acuerdo Marco"

procedure <- data.frame(count(spain_emergency_2020$PROCEDIMIENTO))
procedure <- procedure %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
procedure$x <- factor(procedure$x, levels = procedure$x[order(procedure$freq)])

ggplot(data=procedure, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#5D918B")+
  geom_text(aes(label=freq), vjust=1.6, color="black", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Type of Procedure",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##########

spain_emergency_2020$ADJUDICATARIO.NIF[spain_emergency_2020$ADJUDICATARIO.NIF == "NO INFO"] <- "No Info"
spain_emergency_2020$ADJUDICATARIO.NIF[spain_emergency_2020$ADJUDICATARIO.NIF == "No info"] <- "No Info"



contract_spain_selection <- cbind(spain_emergency_2020$ADJUDICATARIO.NOMBRE,
                             spain_emergency_2020$ADJUDICATARIO.NIF)
contract_spain_selection <- data.frame(contract_spain_selection)

names(contract_spain_selection)[1] <- "ADJUDICATARIO.NOMBRE"
names(contract_spain_selection)[2] <- "ADJUDICATARIO.NIF"


company <- ddply(contract_spain_selection, .(contract_spain_selection$ADJUDICATARIO.NOMBRE,
                                             contract_spain_selection$ADJUDICATARIO.NIF), nrow)
names(company) <- c("x", "y", "freq")

##### Company Name

company <- company %>%                 # Order table with dplyr
  as.data.frame() %>% 
  arrange(desc(freq))
company$y <- factor(company$freq)

company <- company[c(1:20),]

ggplot(data=company, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#CEF1ED")+
  geom_text(aes(label=freq), vjust=1.6, color="black", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Company Name",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


####### NIF

library(dplyr)
company2 <- data.frame(contract_spain_selection$ADJUDICATARIO.NOMBRE, contract_spain_selection$ADJUDICATARIO.NIF)
company2 <- company2 %>% count(contract_spain_selection.ADJUDICATARIO.NIF, sort=TRUE)
names(company2)[1:2] <- c("x", "freq")

company2 <- company2[c(1:20),]

ggplot(data=company2, aes(x=x, y=freq)) +
  geom_bar(stat="identity", fill="#CEF1ED")+
  geom_text(aes(label=freq), vjust=1.6, color="black", size=3.5)+
  theme_minimal() +
  labs(title="Public Procurement under Emergency 2020, SARS-CoV-2, Spain, Type of NIF",
       x ="", y = "Total Number of Contracts") +
  theme(panel.background = element_blank()) +
  theme(legend.position="right") +
  labs(caption="Source: Ministerio de Hacienda y Función Pública. Elaborated by W. Migliari. RStudio.") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

sum(company2$freq)





