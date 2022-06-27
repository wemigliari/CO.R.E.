library(readxl)
library(psych)
library(stats)


### See https://rpubs.com/pjmurphy/758265
### Load the data

irt <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx",
                   sheet = "Variables-Test" )

irt <- data.frame(irt)

irt$Country <- NULL
#names(irt)[c(1:4)] <- c("General","Emergency","CO.R.E","TI")


####### Item Response Theory
####### See https://bookdown.org/bean_jerry/using_r_for_social_work_research/item-response-theory.html


library(mirt)
library(knitr)      
library(dplyr)

### Removing those variables with just one response

irt$Public.Contracting.Web. <- NULL
irt$Info.about.Web.e.procurment. <- NULL
irt$Contact.Channels <- NULL
irt$Information.Structure <- NULL
irt$Precise.Certified.Data <- NULL
irt$Different.Government.Levels <- NULL
irt$API <- NULL
irt$Metadata <- NULL
irt$Open.Data.Webpage <- NULL
irt$Administrative.Capacity <- NULL


mod1 <- (mirt(irt, 1, verbose = FALSE, itemtype = 'graded', SE = TRUE, technical = list(NCYCLES = 10000)))

M2(mod1, type = "C2", calcNULL = FALSE)  ### Originally, calcNULL = FALSE.

itemfit(mod1)

IRT_parms <- coef(mod1, IRTpars = TRUE, simplify = TRUE)
IRT_parms$items

summary(mod1)

plot(mod1, type='trace', which.item = c(1:50), facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=2, space = 'top', cex = .8), 
     theta_lim = c(-2, 2), 
     main = "")

plot(mod1, type='infotrace', which.item = c(1:50), facet_items=T, 
     as.table = TRUE, auto.key=list(points=F, lines=T, columns=1, space = 'right', cex = .8), 
     theta_lim = c(-2, 2), 
     main="")

library(latticeExtra)

plot(mod1, type = 'infoSE', theta_lim = c(-2, 2), 
     main="")

plot(mod1, type = 'rxx', theta_lim = c(-2, 2), 
     main="" )

marginal_rxx(mod1)

plot(mod1, type = 'score', theta_lim = c(-2, 2), main = "")

