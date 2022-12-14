
# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("plotly", # plotar
             "tidyverse", # manipula??o de dados
             "knitr", # apresenta??o de tabelas
             "kableExtra", # apresenta??o de tabelas
             "car", #
             "rgl", #
             "gridExtra", #
             "PerformanceAnalytics", # estudar correlações
             "reshape2", #
             "rayshader", #
             "psych", # estimar os modelos de analises fatoriais
             "pracma", # calculo matematico
             "polynom", # calculo matematico
             "rqPen", # calculo matematico
             "ggrepel", # visualização grafica
             "factoextra", # complementa analises fatoriais
             "sp", # 
             "tmap", # 
             "magick") #

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}
