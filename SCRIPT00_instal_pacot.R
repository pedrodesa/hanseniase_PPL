########################
# Instalação de pacotes
pacotes <- c('data.table', 
             'dplyr',
             'formattable',
             'tidyr',
             'tidyverse',
             'ggplot2',
             'descr',
             'dplyr',
             'lubridate',
             'markdown',
             'formattable',
             'knitr',
             'scales',
             'ggpubr',
             'foreign',
             "CGPfunctions",
             "treemapify",
             "RColorBrewer",
             "colorspace"
             )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


theme_set(theme_pubr())


# Base de dados
load("bd.RData")
load('cnesat.RData')
load('cnesnot.RData')
