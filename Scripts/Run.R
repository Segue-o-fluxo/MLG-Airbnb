#Pacotes =============
require(tidyverse)
require(lubridate)

# diretorios ==============
diretorio <- "C:\\Users\\alvar\\Dropbox\\6Semestre\\Modelos_Lineares_Generalizados\\Airbnb"
setwd(diretorio)
dir.create('Data')

dir.create('cache')


# arrumando o banco para análise ==========
source('Scripts/dt_treat.R')
