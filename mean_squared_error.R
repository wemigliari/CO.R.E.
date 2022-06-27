library(readxl)
library(psych)
library(writexl)
library(tidyr)

index_corr <- read_xlsx("/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/correlation_indexes.xlsx")
index_corr <- data.frame(index_corr)

#### Normalizing

test=as.data.frame(index_corr)
test$Country <- NULL
test <- as_tibble(scale(test))
class(test)
test <- data.frame(test)


library(xlsx)
write_xlsx(test, "/Users/wemigliari/Documents/Pós-Doutorado & Doutorado/Pós-Doc/UOC/Project/Tables/all_index_normaliz.xlsx")


#Mean Squared Error https://www.statology.org/how-to-calculate-mse-in-r/

#sqrt(mean((index_corr$CO.R.E - index_corr$TI)^2))

#fit regression model
model <- lm(test$CO.R.E.~test$TI_Var_20_21 + 
              #test$ODMaturityAssessment +
              #test$Worldwide.Governance.Indicators.ControlOfCorruption +
              #test$eqi + #With and without the index
              #test$Fixed.broadband.subscriptions..per.100.inhabitants.. +
              test$FU_19 + 
              test$Admin_Burden_19 +
              #test$BudgetTransparency +
              test$Individuals.using.the.Internet..per.100.inhabitants.. +
              test$Global.Competitiveness.Data.Set..2019.Score..7...best.)

#get model summary
model_summ <-summary(model)
model_summ
mean(model_summ$residuals^2)


#create data frame with a column of actual values and a column of predicted values

data <- data.frame(pred = predict(model), actual = test$CO.R.E.)
mean((data$actual - data$pred)^2)

outliers <- cbind(index_corr$Country, test)
plot(data$pred,data$actual)
cor(data$pred,data$actual)


outliers <- cbind(index_corr$Country, data)

library(ggplot2)
library(ggrepel)
library(ggthemes)

ggplot(outliers, aes(outliers$pred, outliers$actual)) +
  geom_point() +
  geom_label_repel(aes(label = `index_corr$Country`), size = 3) +
  scale_x_continuous(name="Predicted Values for the CO.R.E.") +
  scale_y_continuous(name="Actual Values for the CO.R.E.") +
  #annotate("text", x=-1, y=1, label = c("Pearson Correlation Coefficient = 0.70")) + 
  stat_cor(method = "pearson", label.x = -1.5, label.y = 1) +
  geom_rangeframe() + 
  theme_tufte() +
  theme_set(theme_gray(base_size = 12, base_family = 'Helvetica'))

cor(data$pred,data$actual)
