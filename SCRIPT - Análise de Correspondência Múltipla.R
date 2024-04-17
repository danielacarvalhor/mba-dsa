
## Análise de Correspondência Múltipla

# Instalação e carregamento dos pacotes utilizados
pacotes <- c("plotly", #plataforma gráfica
             "tidyverse", #carregar outros pacotes do R
             "ggrepel", #geoms de texto e rótulo para 'ggplot2' que ajudam a evitar sobreposição de textos
             "knitr", "kableExtra", #formatação de tabelas
             "sjPlot", #elaboração de tabelas de contingência
             "FactoMineR", #função 'CA' para elaboração direta da Anacor
             "amap", #funções 'matlogic' e 'burt' para matrizes binária e de Burt
             "ade4",
             "readxl") #função 'dudi.acm' para elaboração da ACM

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

############## Análise de Correspondência Múltipla (ACM) ###################

# Carregamento da base de dados
dados <- read_excel("cruzamentos_triagem-atendimentos.xlsx")
dados_selecionados <- c("ID", "Genero", "Estado_civil", "Escolaridade", "Atendimento", "Tratativa", "Classificacao", "Status", "Evolucao")
dados_acm <- dados[dados_selecionados]

dados_acm$ID <- as.character(dados_acm$ID)
dados_acm$Genero <- as.factor(dados_acm$Genero)
dados_acm$Estado_civil <- as.factor(dados_acm$Estado_civil)
dados_acm$Escolaridade <- as.factor(dados_acm$Escolaridade)
dados_acm$Evolucao <- as.factor(dados_acm$Evolucao)
dados_acm$Atendimento <- as.factor(dados_acm$Atendimento)
dados_acm$Tratativa <- as.factor(dados_acm$Tratativa)
dados_acm$Status <- as.factor(dados_acm$Status)
dados_acm$Classificacao <- as.factor(dados_acm$Classificacao)

glimpse(dados_acm)


# Visualização da base de dados
dados_acm %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = FALSE,
                font_size = 13)

# Tabelas de frequência das variáveis qualitativas
summary(dados_acm)

# Tabelas de contingência
# Evolução x Genero
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Genero,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Estado Civil
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Estado_civil,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Escolaridade
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Escolaridade,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Atendimento
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Atendimento,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Status
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Status,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Classificação Inicial
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Classificacao,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)

# Evolução x Tratativa
sjt.xtab(var.row = dados_acm$Evolucao,
         var.col = dados_acm$Tratativa,
         show.exp = TRUE,
         show.row.prc = TRUE,
         show.col.prc = TRUE)


# Matriz Binária OU Matriz de Burt: Tabelas de Contingências - escolher qual usar

# Matriz binária: processo que transforma as vaiáveis qualitativas em variáveis binárias, ou seja, valores de 0 ou 1
# Com base na matriz binária pode ser obtida a inércia principal total na ACM
matriz_binaria <- matlogic(dados_acm[,5:9])
matriz_binaria

# OU
# Para a matriz de Burt
matriz_burt <- burt(dados_acm[,5:9])
matriz_burt

# Exatamente igual a matriz de Burt 
verifica_burt <- t(matriz_binaria) %*% matriz_binaria


# Elaboração da análise de correspondência múltipla (ACM) - Gerando o Mapa Perceptual
# Geração das coordenadas
ACM <- dudi.acm(dados_acm[,5:9], scannf = FALSE)


# Visualização das coordenadas principais das categorias das variáveis
# Método da matriz de Burt B (componente 'co' do objeto 'ACM')
round(ACM$co, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 13)
# OU 
# Visualização das coordenadas-padrão das categorias das variáveis
# Método da matriz binária (componente 'c1' do objeto 'ACM')
round(ACM$c1, 3) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 13)

# Massas das linhas e colunas (componente 'cw' do objeto 'ACM'): Proporções
ACM$cw

# Inércias principais (componente 'eig' do objeto 'ACM')
ACM$eig

# Percentual de variância explicada por dimensão
perc_variancia <- (ACM$eig / sum(ACM$eig)) * 100
perc_variancia

# Visualização do percentual de variância explicada por dimensão
data.frame(Dimensão = paste("Dimensão", 1:length(perc_variancia)),
           Variância = perc_variancia) %>%
  ggplot(aes(x = Dimensão,
             y = Variância,
             label = paste0(round(Variância, 2),"%"))) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(vjust = 2.5, size = 5) +
  theme_bw()

# Mapa perceptual na ACM

# Definição da quantidade de categorias de cada variável qualitativa
quant_categorias <- apply(dados_acm[,5:9],
                          MARGIN =  2,
                          FUN = function(x) nlevels(as.factor(x)))

# Consolidando as coordenadas-padrão obtidas por meio da matriz binária ('c1')
df_ACM <- data.frame(ACM$c1, Variável = rep(names(quant_categorias),
                                            quant_categorias))

# Visualizando as coordenadas
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("Evolucao.","", Categoria),
         Categoria = gsub("Status.","", Categoria),
         Categoria = gsub("Classificacao.","", Categoria),
         Categoria = gsub("Tratativa.","", Categoria),
         Categoria = gsub("Atendimento.","", Categoria,)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = FALSE,
                font_size = 13)

# Plotando o mapa perceptual
df_ACM %>%
  rownames_to_column() %>%
  rename(Categoria = 1) %>%
  mutate(Categoria = gsub("Evolucao.","", Categoria),
         Categoria = gsub("Status.","", Categoria),
         Categoria = gsub("Classificacao.","", Categoria),
         Categoria = gsub("Tratativa.","", Categoria),
         Categoria = gsub("Atendimento.","", Categoria,)) %>%
  ggplot(aes(x = CS1, y = CS2, label = Categoria, color = Variável)) +
  geom_point() +
  geom_label_repel() +
  geom_vline(aes(xintercept = 0), linetype = "longdash", color = "grey48") +
  geom_hline(aes(yintercept = 0), linetype = "longdash", color = "grey48") +
  labs(x = paste("Dimensão 1:", paste0(round(perc_variancia[1], 2), "%")),
       y = paste("Dimensão 2:", paste0(round(perc_variancia[2], 2), "%"))) +
  scale_color_manual("Variável",
                     values = c("turquoise3", "springgreen4", "blue", "orange", "red")) +
  theme_bw()
