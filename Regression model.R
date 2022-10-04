#Apertura e lettura data set in esame

setwd("/Users/adnan/Desktop/R/Materiale Big Data")
Data_set_eu <- read.csv("EU_Econ_Data_2021.csv", sep=";")

#Normalizzazione variabili 

data_set_eu_norm <- sapply(Data_set_eu[2:5], scale)
head(data_set_eu_norm)
dati_norm <- as.data.frame(data_set_eu_norm)

#Analisi esplorativa dei dati tramite plot

hist(dati_norm$Per_capita_GDP)

plot(dati_norm$Per_capita_GDP, dati_norm$Capital_perc_GDP,
     main = "Relazione tra: Capitale Procapite e Capitale Percepito",
     xlab = "Capitale Procapite", ylab = "Capitale Percepito")
 
plot(dati_norm$Per_capita_GDP, dati_norm$Inflation_rate,
     main = "Relazione tra: Capitale Procapite e Inflazione",
     xlab = "Capitale Procapite", ylab = "Inflazione")

plot(dati_norm$Per_capita_GDP, dati_norm$Unemployment_rate,
     main = "Relazione tra: Capitale Procapite e Disoccupazione",
     xlab = "Capitale Procapite", ylab = "Disoccupazione")

#Esiste correlazione tra dati?
library("corrplot")
CPC_CP <- cbind(dati_norm$Per_capita_GDP, dati_norm$Capital_perc_GDP)
corr_CPC_CP <-cor(CPC_CP)
corrplot(cor(CPC_CP),
         method = "shade", 
         type = "full",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",    
         title = "",       
         col = NULL)       

CPC_U <- cbind(dati_norm$Per_capita_GDP, dati_norm$Unemployment_rate)
corr_CPC_U <- cor(CPC_U)
corrplot(cor(CPC_U),
         method = "shade", 
         type = "full",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "",       
         col = NULL)

CPC_I <- cbind(dati_norm$Per_capita_GDP, dati_norm$Inflation_rate)
corr_CPC_I <- cor(CPC_I)
corrplot(cor(CPC_I),
         method = "shade", 
         type = "full",    
         diag = TRUE,      
         tl.col = "black", 
         bg = "white",     
         title = "",       
         col = NULL)

#Ricerca miglior modello

intercept_only_model <- lm(Per_capita_GDP ~ 1, data = dati_norm)
full_model <- lm(Per_capita_GDP ~ ., data = dati_norm)
stepwise_forward <- step(intercept_only_model, direction = 'forward', scope = formula(full_model), trace = 1)
stepwise_backward <- step(full_model, direction = 'backward', scope = formula(full_model), trace = 1)
stepwise_both <- step(intercept_only_model, direction = 'both', scope = formula(full_model), trace = 1)

#Riepilogo modelli
summary(intercept_only_model)
summary(full_model)
summary(stepwise_forward)
summary(stepwise_backward)
summary(stepwise_both)

miglior_modello <- lm(Per_capita_GDP ~ Capital_perc_GDP, data = dati_norm)
summary(miglior_modello)
#Commenti su parametri trovati 
"""
La variabile CpC viene influenzata da CP piu che dalle altre. Il suo p-valore
ci suggerisce di rifiutare l'ipotesi nulla  (beta = 0) mentre un aumento di cp
diminuisce il valore di cpc anche se di poco. R^2 basso mentre test F = 5,96

"""

#Cross-Validation

install.packages("caret")
library(caret)

LooCV <- trainControl(method = "LOOCV")
modello <- train(Per_capita_GDP ~ Capital_perc_GDP , data = dati_norm, method = "lm", trControl = LooCV)
print(modello)

qqnorm(stepwise_forward$residuals)
qqline(stepwise_forward$residuals)

par(mfrow=c(2,2))
plot(miglior_modello)
par(mfrow=c(1,1))

#Plot dei risultati trovati

Per_capita_GDP.graph<-ggplot(dati_norm, aes(x=dati_norm$Per_capita_GDP,
                             y=dati_norm$Capital_perc_GDP))+ geom_point()

Per_capita_GDP.graph <- Per_capita_GDP.graph + 
                        geom_smooth(method = "lm", col = "blue")

Per_capita_GDP.graph




