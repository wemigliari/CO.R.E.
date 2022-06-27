library(readxl)
library(psych)
library(stats)

### See https://rpubs.com/pjmurphy/758265

index <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx",
                         sheet = "Indexes" )

index <- data.frame(index)


index$Country <- NULL
names(index)[c(1:4)] <- c("General","Emergency","CO.R.E","TI")
correlation <- corr.test(index)
print(correlation, short=FALSE)

index$CO.R.E <- NULL

colSums(is.na(index))
missings <- colSums(is.na(index)) # Count # missing in each column

summary(missings) # Evaluate the breakdown of missings
KMO(index)
#library(mice)
#mice <- mice(index, m=1, maxit=500, method='cart', seed=500)

library(psych)
fit <- principal(index, nfactors=2, rotate="varimax")
fit # print results


mydat <- index[, KMO(index)$MSAi>0.50] # Get rid of all variables with MSA < 0.50
mydata <- mydat

round( KMO(index)$MSA, 2 )

cortest.bartlett(index)
ev <- eigen(cor(index)) # get eigenvalues
ev$values

scree(index, pc=FALSE)

fa.parallel(index, fa="fa")

library(psych)

loads <- fit$loadings

fa.diagram(loads)


load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(index),cex=.7)



















