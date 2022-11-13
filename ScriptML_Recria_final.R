#############################################################################################
#
#                        Machine Learning por Arvore de Regressão
#                                        ~ Recria ~  
#
###########################################################     Prof. Mérik Rocha, CCA-UESPI
setwd("C:/Users/Rocha_65_999054578/OneDrive/..Tese_JESUS_/Capitulo_2_MorfometriaMassaCorporal/MachineLearning")
DadosRecria <- read_excel("Dados.xlsx", sheet="Recria",  col_types=c("text","text","numeric", "numeric", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
library(pacman)
pacman::p_load(readxl, tidyverse, caret, rpart, scatterplot3d, fBasics, Metrics, caTools, miscTools)

# Gráfico de dispersão
ggplot(DadosRecria, aes(x=PT, y=Peso)) +
  labs(x = "Perimetro Toracico", y = "Peso") +
  geom_point(size = 1.8) + geom_abline()
  theme_classic()

graph <- scatterplot3d(DadosRecria$Peso ~ DadosRecria$PT + DadosRecria$AC,
                         pch = 16, angle = 30, color = "steelblue", box = FALSE,
                         xlab="Perímetro Torácico", ylab="Altura de CeRecriaelha", zlab="Peso") + 
set.seed(789)

#Dividindo os DadosRecria
amostra_treinoRecria <- DadosRecria$Peso %>%   
  createDataPartition(p = 0.7, list = F)
#Recriando subconjunto com 70% dos DadosRecria e outro com 30%
DadosRecria_treino <- DadosRecria[amostra_treinoRecria,]  #treino <- [amostra_treinoRecria,]
DadosRecria_teste <- DadosRecria[-amostra_treinoRecria,]  #teste <- [-amostra_treinoRecria,]

#Recriando o ModeloRecria
ModeloRecria <- rpart(Peso ~ . ,data = DadosRecria, control=rpart.control(cp=0)) #method = "anova"
summary(ModeloRecria)

#Aplicando Modelo
DadosRecria$Previsao <- predict(ModeloRecria,DadosRecria) #Recria coluna com resultados preditos

#Aplicando ModeloRecria
DadosRecria$Previsao <- predict(ModeloRecria,DadosRecria) #Recria coluna com resultados preditos

# Avaliação do ModeloRecria -----------------------------------------------------
cor.test(DadosRecria$Peso, DadosRecria$Previsao) #0,90
#Mean Error
DadosRecria$Erro <- round(DadosRecria$Peso-DadosRecria$Previsao, 4)
ME <- mean(DadosRecria$Erro,4)

#Mean Absolute Error
DadosRecria$ErroModulo <-abs(DadosRecria$Erro)
MAE <- mean(DadosRecria$ErroModulo)
MAE
#MSE : Mean Squared Error
DadosRecria$Erro2 <- (DadosRecria$Erro^2)
MSE <- mean(DadosRecria$Erro2)
mse(DadosRecria$Peso,DadosRecria$Previsao) #410,13

#RMSE
RMSE <- sqrt(MSE)
RMSE

printcp(ModeloRecria) # display the results
plotcp(ModeloRecria) # visualize cross-validation results
summary(ModeloRecria)$adj.r.squared
Previsionamento <-predict(ModeloRecria, newdata = DadosRecria_treino)
rmse(DadosRecria$Peso, predict(ModeloRecria, DadosRecria)) #
mse(DadosRecria$Peso, DadosRecria$Previsao) #2
accuracy(DadosRecria$Peso, DadosRecria$Previsao) #0

R2 <- rSquared(DadosRecria_treino[['Peso']], resid = DadosRecria_treino[['Peso']]-Previsionamento)
R2

R2 <- rSquared(DadosRecria_teste[['Peso']], resid = DadosRecria_teste[['Peso']]-Previsionamento)
R2

R2 <- rSquared(DadosRecria[['Peso']], resid = DadosRecria[['Peso']]-Previsionamento)
R2

# Plot Arvore -------------------------------------------------------------
plot(ModeloRecria, uniform=TRUE,
     main="Arvore de Classificação dos Pesos dos CPD")
text(ModeloRecria, use.n=TRUE, all=TRUE, cex=.5) #https://www.statmethods.net/advstats/cart.html

plot(DadosRecria$Peso)
points(DadosRecria$Previsao, col="red", pch=20 )
