library(PNADcIBGE)
library(tidyverse)
library(srvyr) 
library(arrow)

lista_dados <- list()

for (year in 2021:2023) {
  
  for (quarter in 1:4) {

    dados <- get_pnadc(year = year, quarter = quarter)

    dados_srvyr <- as_survey(dados)
    
    microdados <- dados_srvyr[["variables"]]
    
    nome_df <- paste("dados", year, "T", quarter, sep = "_")
    lista_dados[[nome_df]] <- microdados
  }
}


dados_completos <- bind_rows(lista_dados)

arrow::write_parquet(dados_completos,"pnadc_21_23.parquet")
write.csv(dados_completos, "pnadc21_23.csv")
