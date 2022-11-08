#Importa as bibliotecas necessárias
#-> não esqueça de instalá-las se for necessário!

#library(devtools)
library(ggplot2)
library(qqplotr)
library(ellipse)
library(RColorBrewer)
library(GGally)
library(olsrr)
library(car)   #Para usar a função influencePlot()

#Importa os dados
dados_vida = read.csv("Lifexpec.csv")
dados_vida_lite = read.csv("Lifexpec.csv")   #removeremos algumas colunas posteriormente
#-> Extrai o índice para uso futuro
indice <- as.numeric(rownames(dados_vida_lite))

#Sumário dos dados:
summary(dados_vida)

#Remove as colunas country e status para fazer as correlações
dados_vida_lite$Country <- NULL
dados_vida_lite$Status <- NULL


### Análise descritiva: correlogramas  ###
#Constroi os correlogramas
#-> Paleta:
my_colors <- brewer.pal(5, "Spectral")
my_colors <- colorRampPalette(my_colors)(100)

#->Correlograma basico
ggpairs(dados_vida_lite,
        lower  = list(continuous = "points"),
        upper  = list(continuous = "blank"),
  )

#->Plot colorido e com elipses
data_corr <- cor(dados_vida_lite)
write.csv(data_corr, 'corr-matriz-lifexpec.csv')  #salva a matriz de correlação

ord <- order(data_corr[1, ])
data_ord <- data_corr[ord, ord]
plotcorr(data_ord , col=my_colors[data_ord*50+50] , mar=c(1,1,1,1)  )


###--- Seleção por step-wise ---###
# Cria o modelo de regressão com lm() e aplica o stepwise
model <- lm(data = dados_vida_lite)
model_stepfoward = ols_step_forward_p(model, details=TRUE)

#Faz o gráfico com todos os plots gerados automaticamente e salva como pdf
pdf('step_wise-plots.pdf')
plot(k)
dev.off()

#Histogramas para ambos modelos
hist(model$residuals)
hist(model_stepfoward$model$residuals)

#Agora, devemos testar a homocedasticidade deste modelo 
#-> Primeiro, calcula algumas caracteristicas do modelo stepfoward
preditores <- model_stepfoward$predictors   
residuos <- model_stepfoward$model$residuals     
fitted_values <- model_stepfoward$model$fitted.values

infl_stepfoward <- influence(model_stepfoward$model)
residuos_student <- rstudent(model_stepfoward$model)
dffits_stepfoward <- dffits(model_stepfoward$model)
cook_stepfoward <- cooks.distance(model_stepfoward$model)
hat_diagonal <- hatvalues(model_stepfoward$model, type = c("diagonal"))

#-> Gráfico dos resíduos studentizados vs previsão, além de linha em zero
plot(fitted_values, residuos_student); qqline(y=0, col = 2,lwd=2,lty=2)
#--> Valores < -3 ou > 3:
Filter(function(x) abs(x) > 3, residuos_student)

#-> dffits vs índice, com linha em zero
plot(indice, dffits_stepfoward); qqline(y=0, col = 2,lwd=2,lty=2)
#--> Valores < -1 ou > 1:
Filter(function(x) abs(x) > 1, dffits_stepfoward)

#-> Distância de cook vs índice
plot(indice, cook_stepfoward)
#--> Valores < 50% de F ou > 50% de F:
valor_F <- pf(0.5, 5, 131)
Filter(function(x) abs(x) > valor_F, cook_stepfoward)

#-> Hat vs índice
plot(indice, hat_diagonal)
#--> Valores h_ii>2p/n (neste caso, .1)
Filter(function(x) abs(x) > .1, hat_diagonal)

#-> Residuos vs hat
influencePlot(model_stepfoward$model)

#-> Plot Q-Q
qqnorm(residuos); qqline(residuos, col = 2,lwd=2,lty=2)