
# REGRESSÃO LOGÍSTICA MULTINOMIAL

################ INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS ##################

pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","readxl")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


######################## CARREGAMENTO DA BASE DE DADOS #######################

dados <- read_excel("cruzamentos_triagem-atendimentos.xlsx")
dados_selecionados <- c("Evolucao", "Tratativa", "Status", "Classificacao", "Qtde_Compareceu")
dados_multinomial <- dados[dados_selecionados]

######################### OBSERVAÇÃO DA BASE DE DADOS #########################

#Visualizando a base de dados
dados_multinomial %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 13)

#Estatísticas descritivas univariadas da base de dados
summary(dados_multinomial)

# Calcular o desvio padrão
desvio_padrao <- sd(dados_multinomial$Qtde_Compareceu)
print(desvio_padrao)

#Tabela de frequências absolutas das variáveis qualitativas
table(dados_multinomial$Evolucao)
table(dados_multinomial$Tratativa)
table(dados_multinomial$Status)
table(dados_multinomial$Classificacao)

dados_multinomial$Evolucao <- as.factor(dados_multinomial$Evolucao)
dados_multinomial$Tratativa <- factor(dados_multinomial$Tratativa)
dados_multinomial$Status <- factor(dados_multinomial$Status)
dados_multinomial$Classificacao <- factor(dados_multinomial$Classificacao)

glimpse(dados_multinomial)

############################ PROCEDIMENTO N-1 DUMMIES #########################

multinomial_dummy <- dummy_columns(.data = dados_multinomial,
                                    select_columns = c("Tratativa",
                                                       "Status",
                                                       "Classificacao"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = T)

######################### DIVISÃO DA BASE EM TREINO E TESTE ###########################

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(multinomial_dummy), replace=TRUE, prob=c(0.8,0.2))
multinomial_dummy_treino <- multinomial_dummy[sample, ]
multinomial_dummy_teste <- multinomial_dummy[!sample, ]

#################### ESTIMAÇÃO DE UM MODELO LOGÍSTICO MULTINOMIAL ###########################

#Apontando a categoria de referência usando a base com as variáveis dummizadas
multinomial_dummy_treino$Evolucao <- relevel(multinomial_dummy_treino$Evolucao, 
                                  ref = "Melhora")

#Estimação do modelo - função multinom do pacote nnet
modelo_multinomial <- multinom(formula = Evolucao ~ .,
                               data = multinomial_dummy_treino)

#Parâmetros do modelo_atrasado
summary(modelo_multinomial)

#Outra maneira de apresentar os outputs do modelo
#função stargazer do pacote stargazer -> mais adequado que a função export_summs
stargazer(modelo_multinomial, nobs=T, type="text")

#LL do modelo_atrasado
logLik(modelo_multinomial)

#########################################################################

#Definição uma função Qui2 para se extrair a estatística geral do modelo:
Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#Estatística geral do modelo_multinomial
Qui2(modelo_multinomial)

###################################


#Calcular das estatísticas z de Wald 
zWald_modelo_multinomial <- (summary(modelo_multinomial)$coefficients / 
                            summary(modelo_multinomial)$standard.errors)
zWald_modelo_multinomial


#Cálculo dos respectivos p-values
round((pnorm(abs(zWald_modelo_multinomial), lower.tail = F) * 2), 4)

######################## EFETIVIDADE GERAL DO MODELO ##########################

#Adicionando as prováveis ocorrências de evento apontadas pela modelagem à base de dados
multinomial_dummy_teste$Predicao <- predict(modelo_multinomial, 
                                     newdata = multinomial_dummy_teste, 
                                     type = "class")


#Visualizando a nova base de dados com a variável 'predicao'
multinomial_dummy_teste %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 13)

attach(multinomial_dummy_teste)

#Criando uma tabela para comparar as ocorrências reais com as predições

ordem_desejada <- c("Melhora", "Inalterado", "Piora")

# Conventendo as categorias Predicao e Evolucao em fatores com a ordem desejada
Predicao <- factor(Predicao, levels = ordem_desejada)
Evolucao <- factor(Evolucao, levels = ordem_desejada)
EGM <- as.data.frame.matrix(table(Predicao, Evolucao))

#Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Eficiência global do modelo
acuracia <- (round((sum(diag(table(Evolucao, Predicao))) / 
                      sum(table(Evolucao, Predicao))), 2))

acuracia
