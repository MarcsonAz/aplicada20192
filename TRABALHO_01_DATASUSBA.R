# bebe sem = 2, loc nasc = 1
# DIRETORIO
dir = "/home/216054078/Área de Trabalho/aplicada/"

# salvar base
saveRDS(base,paste0(dir,"baseTrabalho.rds"))

# PACOTES
library(read.dbc)
library(dplyr)
library(ggplot2)
library(magrittr)
library(sjPlot)
        
# LEITURA DA BASE
# Foi selecionado para este estudo a base do sus do Estado da Bahia.
# tamanho inicial: 204096 obs. de 64 variaveis
basededados <- read.dbc(paste0(dir,"DNBA2017.dbc"))
basededados <- as_tibble(basededados)
base <- basededados
# TRATAMENTO
# Registros de nascimentos de mães com idade entre 18 a 40 anos 
# + bebê sem anomalia 
# + nascimentos em hospital
base$IDADEMAE <- as.numeric(base$IDADEMAE)

base %<>% filter(IDADEMAE>=18 & IDADEMAE<=40 &LOCNASC=="1" &IDANOMAL=="2") %<>% 
           select(NUMERODN,IDADEMAE,ESTCIVMAE,ESCMAE2010,CODOCUPMAE,RACACORMAE,
                 TPROBSON,PARIDADE,SEXO,PESO,RACACOR,GRAVIDEZ,SEMAGESTAC,
                 KOTELCHUCK,TPAPRESENT,PARTO)

# salvar base de Trabalho
saveRDS(base,paste0(dir,"baseTrabalho.rds"))

# limpar base antiga da memória
rm(basededados)

# funcao para buscar NA
a <- data.frame(id = names(base),x = 1:16)
b = 0 
for(i in 1:16){
temp <- sum(is.na(base[i]))
b[i] <- temp
}
a$x <- b
a$y = round(100*(a$x/length(base$NUMERODN)),2)
arrange(a,desc(y))

# identificar NA na base

# analise descritiva dos dados
summary.data.frame(base)

# tabela de frequência
View(base %>%
  count(IDADEMAE) %>%         # now required with changes to dplyr::count()
  mutate(`prop %` = (prop.table(n))) %>%
  select(IDADEMAE, `prop %` )
)

# 

