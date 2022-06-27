library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(sf)



### 2011-2020
### All the contracts published on TED's portal
ted_2020 <- read_csv("/Users/wemigliari/Downloads/ted_time_series_2011_2020.csv")
ted_2020 <- data.frame(ted_2020)

##### Maps

ted_2020_2 <- ted_2020
names(ted_2020_2)[15] <- "NUTS_NAME"


europe_nuts3 <- read_sf("/Users/wemigliari/Downloads/NUTS_RG_20M_2021_3035.shp")

europe_contracts_municipalities <- data <- merge(ted_2020_2, europe_nuts3, by = "NUTS_NAME")


######

shp_joined_2021 <- st_as_sf(europe_contracts_municipalities) 

# Create a color palette with handmade bins.
mybins <- c(1000000, 2000000, 3000000, 4000000, 5000000, 6000000, 7000000, 8000000, 9000000, 10000000, 50000000)

# Definition of a set of blue colors
blues <- brewer.pal(9, "Blues") # from the RColorBrewer package

# 1 - Make a color range using colorRampPalette() and the set of blues
blue_range <- colorRampPalette(blues)

mypalette <- colorBin( palette=mycolors, domain=europe_contracts_municipalities$VALUE_EURO, na.color="transparent", bins=mybins)

mytext <- paste(
  "City: ", europe_contracts_municipalities$NUTS_NAME, "<br/>",
  "Total: ", europe_contracts_municipalities$VALUE_EURO, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


#### Map

leaflet(shp_joined_2021) %>% 
  addPolygons( 
    fillColor = ~mypalette(VALUE_EURO), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addLegend(pal=blue_range(11), values=~europe_contracts_municipalities$VALUE_EURO, opacity=1, title = "Geographical Coverage and Values of Contracts", position = "bottomright" )%>%
  setView(-3.703790, 40.416775, zoom = 4.2)


################################################SWEDEN
### All the contracts published by Sweden
ted_sweden <- filter(ted_2020, ISO_COUNTRY_CODE=="SE")

### All the Swedish contracts published filtered by framework agreement
ted_sweden_contract_agreement <- data.frame((count(ted_sweden, "B_FRA_AGREEMENT")))

ted_sweden_contract_agreement<-arrange(ted_sweden_contract_agreement,desc(freq))

################################################SPAIN

### All the contracts published by Spain
ted_spain <- filter(ted_2020, ISO_COUNTRY_CODE=="ES")



### All the contracts published by Spain
ted_spain_dispatch <- as.Date(ted_spain$DT_DISPATCH)

library(lubridate)

ted_spain_dispatch <- dmy(ted_spain$DT_DISPATCH)
ted_spain_dispatch <- data.frame(ted_spain_dispatch)

ted_spain_application <- dmy(ted_spain$DT_APPLICATIONS)
ted_spain_application <- data.frame(ted_spain_application)


ted_spain_selection <- cbind(ted_spain$YEAR,
                             ted_spain$CAE_NAME,
                             ted_spain$CAE_TOWN,
                             ted_spain$CAE_POSTAL_CODE,
                             ted_spain$B_RECURRENT_PROCUREMENT,
                             ted_spain$MAIN_ACTIVITY,
                             ted_spain$DURATION,
                             ted_spain$VALUE_EURO,
                             ted_spain_dispatch, 
                             ted_spain_application)

ted_spain_selection_2020 <- filter(ted_spain_selection, ted_spain$YEAR=="2020")
ted_spain_selection_2019 <- filter(ted_spain_selection, ted_spain$YEAR=="2019")
ted_spain_selection_2018 <- filter(ted_spain_selection, ted_spain$YEAR=="2018")


test <-data.frame(count(ted_spain_selection_2020$ted_spain_dispatch))
test$year <- 2020
test_19 <-data.frame(count(ted_spain_selection_2019$ted_spain_dispatch))
test_19$year <- 2019
test_18 <-data.frame(count(ted_spain_selection_2018$ted_spain_dispatch))
test_18$year <- 2018

test_18_20 <- rbind(test_18, test_19, test)

library(xlsx)

write.xlsx(test_18_20, file = "/Users/wemigliari/Documents/R/tabelas/uoc_contracts/ted_spain_2018_2020.xlsx",
           sheetName = "TED-2011-2020", append = FALSE)

plot(test_18_20$x, test_18_20$freq, ylim=c(0, 1500), pch = c(17))


library(ggplot2)


ggplot(test_18_20, aes(x=x, y=freq, color=year)) + 
  ylim(0,1200) +
  geom_point() +
  theme(legend.position="none") +
  labs(title="Tenders Electronic Daily Data Series 2018-2020, Spain",
                                      x ="", y = "Number of Contracts per Day") +
  theme(panel.background = element_blank()) +
  geom_vline(xintercept = rev(test_18_20[trunc(test_18_20$x) == "2020-03-15", "x"])[1]) +
  geom_text(data=test_18_20, aes( x=rev(test_18_20[trunc(test_18_20$x) == "2020-03-02", "x"])[1], y=500, label="Real Decreto 463/2020"),                 
            color="darkgray", 
            size=4 , angle=90, family= "Helvetica") 

#####
count(test_18_20$year)


### All the Spanish contracts published filtered by main activity
ted_spain_main_activity <- data.frame((count(ted_spain, "MAIN_ACTIVITY")))

ted_spain_main_activity<-arrange(ted_spain_main_activity,desc(freq))

### All the Spanish contracts published filtered by framework agreement
ted_spain_contract_agreement <- data.frame((count(ted_spain, "B_FRA_AGREEMENT")))

ted_spain_contract_agreement<-arrange(ted_spain_contract_agreement,desc(freq))

### All the Spanish contracts published filtered by main activity
ted_spain_health <- filter(ted_spain, MAIN_ACTIVITY=="Health")
ted_spain_elect_auction  <- data.frame((count(ted_spain_health, "B_ELECTRONIC_AUCTION")))
ted_spain_contract_correction  <- data.frame((count(ted_spain_health, "CORRECTIONS")))

###############




