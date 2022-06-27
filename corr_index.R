library(readxl)
library(psych)
library(writexl)
library(tidyr)

index_corr <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx")
index_corr <- data.frame(index_corr)

#########

index_corr2 <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_eqi_data_long21.xlsx")
index_corr2 <- subset(index_corr2, year!="2010" & year!="2013" & year!="2017")

#########

eqi_index <- tapply(index_corr2$EQI, index_corr2$cname, max)
class(eqi_index)

eqi_index=as.data.frame(eqi_index)
eqi_index<- tibble::rownames_to_column(eqi_index, "Country") 
eqi_index$EQI_Normaliz <- as_tibble(scale(eqi_index$eqi))
class(eqi_index)

eqi_Normaliz <- eqi_index$EQI_Normaliz
eqi_Normaliz <- data.frame(eqi_Normaliz)
colnames(eqi_Normaliz)[1] <- "eqi_Normaliz"
class(eqi_Normaliz)

write_xlsx(eqi_index, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_eqi_index.xlsx")


########
#ibp <-  read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_ibp_data_2021.xlsx")
#ibp = as.data.frame(ibp)
#ibp$ibp_Normaliz <- as_tibble(scale(ibp$OPEN_BUDGET_INDEX))
#ibp <- merge(index_corr, ibp, by = "Country" )
#ibp <-ibp$ibp_Normaliz
#ibp <- data.frame(ibp)
#colnames(ibp)[1] <- "ibp_Normaliz"

########

internet_subscription <-read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_internet_subscription_data_europa.xlsx")
internet_subscription <- internet_subscription[-c(1:911),]
internet_subscription <- internet_subscription[-c(11),]
internet_subscription <- internet_subscription[,-c(1, 3:5)]
internet_subscription <- as.numeric(internet_subscription$value)
internet_subscription <- data.frame(internet_subscription)
write_xlsx(internet_subscription, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_internet_subs_index.xlsx")

internet_subscription <- as.data.frame(scale(internet_subscription))
names(internet_subscription)[1] <- "Internet_Subscript_Normaliz"


#########

#internet_subscription <- internet_subscription[,-c(2:11)]
#internet_subscription <- merge(internet_subscription, test, by ="Country")
#internet_subscription <- as.data.frame(scale(internet_subscription$`2020`))
#internet_subscription <-data.frame(internet_subscription)
#names(internet_subscription)[1] <- "Internet_Subscript_Normaliz"

test <- cbind(index_corr, internet_subscription, eqi_Normaliz)

test$EQI <- NULL
test$Fixed.broadband.subscriptions..per.100.inhabitants.._Normaliz <- NULL


library(xlsx)

write_xlsx(test, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/index_normaliz.xlsx")

#######

test_no_outliers <- test[-c(1,2),]


#Mean Squared Error https://www.statology.org/how-to-calculate-mse-in-r/

#sqrt(mean((index_corr$CO.R.E - index_corr$TI)^2))

#fit regression model
model <- lm(test_no_outliers$CO.R.E._Normaliz~#test_no_outliers$TI_2021_Normaliz + 
              #test_no_outliers$ODMaturityAssessment_Normaliz +
              #test_no_outliers$Worldwide.Governance.Indicators.ControlOfCorruption_Normaliz +
              #test_no_outliers$Administrative_Burden_Normaliz +
              #test_no_outliers$BudgetTransparency_Normaliz +
              #test_no_outliers$Individuals.using.the.Internet..per.100.inhabitants..+
              #test_no_outliers$Global.Competitiveness.Data.Set..2019.Score..7...best. + 
              test_no_outliers$Internet_Subscript_Normaliz +
              test_no_outliers$eqi_Normaliz, test_no_outliers)

#get model summary
model_summ <-summary(model)
model_summ
mean(model_summ$residuals^2)


#create data frame with a column of actual values and a column of predicted values

data <- data.frame(pred = predict(model), actual = test_no_outliers$CO.R.E._Normaliz)
mean((data$actual - data$pred)^2)

plot(data$pred,data$actual)
cor(data$pred,data$actual)

###### Test with the indexes (NOT Z-scores)

indexes <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx",
                     sheet = "Indexes")
indexes <- data.frame(indexes)


#Mean Squared Error https://www.statology.org/how-to-calculate-mse-in-r/

#sqrt(mean((index_corr$CO.R.E - index_corr$TI)^2))

#fit regression model
model <- lm(indexes$CO.R.E.~#indexes$TI_2021 + 
              indexes$ODMaturityAssessment +
              indexes$Worldwide.Governance.Indicators.ControlOfCorruption +
              indexes$eqi +
              indexes$Fixed.broadband.subscriptions..per.100.inhabitants.. +
              indexes$Administrative_Burden+
              indexes$BudgetTransparency + 
              indexes$Individuals.using.the.Internet..per.100.inhabitants..) #+
              #indexes$Global.Competitiveness.Data.Set..2019.Score..7...best., test)

#get model summary
model_summ <-summary(model)
model_summ
mean(model_summ$residuals^2)



## stepAIC will eliminate inappropriate terms
library(MASS)
model_summ <- summary(stepAIC(model))

plot(model$fitted.values, model$residuals, ylab='Standardized Residuals', xlab='Fitted Values') 
abline(0,0)

library(car)
avPlots(model)

##################################

library("reshape2")
library("ggplot2")
library(forcats)

test_data_long <- melt(index_corr, id="Country")  # convert to long format
newdata <- test_data_long[order(test_data_long$value),]
legend_title <- "Indexes"

ggplot(newdata, aes(fct_inorder(Country), color = variable)) +       # Create ggplot2 plot
  geom_point(aes(y = value)) +
  labs(x = "", y = "") +
  scale_color_manual(name = "", values = c("CO.R.E" = "purple",
                                "TI"="orange")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


##### Factor Analysis

library(stats)

names(index_corr2)[c(1,2)] <- c("CO.R.E","TI")

fit <- princomp(index_corr2, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit)


library(psych)
fit_2 <- principal(index_corr2, nfactors=2, rotate="varimax")
fit_2 # print results

KMO(index_corr2)

round( KMO(index_corr2)$MSA, 2 )

cortest.bartlett(index_corr2)
ev <- eigen(cor(index_corr2)) # get eigenvalues
ev$values

scree(index_corr2, pc=FALSE)

fa.parallel(index_corr2, fa="fa")

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation


index_corr3 <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx",
                         sheet = "Indexes" )

index_corr3 <- data.frame(index_corr3)

index_corr3$Country <- NULL
#names(index_corr3)[c(1:4)] <- c("General","Emergency","CO.R.E","TI")

library(psych)
fit_3 <- principal(index_corr3, nfactors=4, rotate="varimax")
fit_3 # print results

KMO(index_corr3)

round( KMO(index_corr3)$MSA, 2 )

cortest.bartlett(index_corr3)
ev <- eigen(cor(index_corr3)) # get eigenvalues
ev$values

scree(index_corr3, pc=FALSE)

fa.parallel(index_corr3, fa="fa")


######## Correlation
########

alpha_test <- alpha(index_corr3, check.keys = TRUE)

corr_index <- cor(index_corr3)


library("ggpubr")
ggscatter(test, x = "CO.R.E._Normaliz", y = "Internet_Subscript_Normaliz", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CO.R.E. Index", ylab = "Transparency International Index")



#####

index_corr4 <- test
index_corr4 <- subset(test, select = c(CO.R.E._Normaliz, eqi_Normaliz))

names(index_corr4)[1:2] <- c("PA", "EQI")

alpha_test2 <- psych::alpha(index_corr3, check.keys = TRUE)


corr_index <- cor(index_corr4)


ggscatter(index_corr4, x = "PA", y = "TI", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CO.R.E. Index", ylab = "Transparency International Index")


fitted.model <- lm(index_corr4$PA ~ index_corr4$EQI, data=index_corr4)





