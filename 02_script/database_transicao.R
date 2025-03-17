
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)


# Definindo função --------------------------------------------------------

base_pnad <- function(ano, trimestre){
  pnadc_04_2017 <- get_pnadc(ano, trimestre)
  pnadc_04_2017srv <- as_survey(pnadc_04_2017)
  data_04_2017 <- as.data.frame(pnadc_04_2017srv) |> 
    select(Ano, Trimestre, UF, Capital, RM_RIDE, 
           UPA, Estrato, V1008, V1014, V1016, V1022,
           V1023, V1027, V1028, V2001, V2003, V2007,
           V2008, V2009, V20081, V20082, V2009, V2010, VD4001,
           VD4002, VD4003, VD4004, VD4004A, V4009, V4010, 
           V4012, V4015, V40151, V4016, V4017, V40171, 
           V4018, V4019, V4020, V4021, V4022, V4032, 
           V4033, V4039, V4039C, V4040, V4041, V4043, 
           V40431, V4044, V4046, V4049, V4050, V4056, 
           V4056C, V4057, V4058, V4062, V4062C, VD3004, 
           VD3005, VD4010, VD4011, V4058, V4062, V4062C, 
           VD3004, VD3005, VD4010, VD4011, VD4019)
}


# Baixando dados PNAD -----------------------------------------------------

#Baixando dado do último trimestre de 2017
pnad_04_2017 <- base_pnad(2017, 4)


#Criando loop para rodar função
ano <- 2018
trimestre <- 1:4
resultado <- list()
for (ano in ano) {
  for (trimestre in trimestre) {
    resultado[[length(resultado) + 1]] <-
      base_pnad(ano = ano, trimestre = trimestre)
  }
}
resultado_2018 <- do.call(rbind, resultado)

#Juntando a base de 2017 e 2018
resultado_final <- rbind(data_04_2017, resultado_2018)

#Salvando para fins de backup
#write.csv(resultado_final,"C:/Users/alefs/Downloads/backup_pnad_042017_2018.csv")


# Tratando os dados -------------------------------------------------------

#Filtrando apenas os que responderam a variável V2008 e criando variável ID
pnadc <- 
  resultado_final |> 
  filter(V2008 != "99") |> 
  mutate(id = paste(UPA, V1008, V1014,
                    V2008, V20081, V20082,
                    V2007, Estrato, sep = "_"), .before = Ano)


#Filtrando apenas os que foram TCP ou desocupada ou fora da força de trabalho
pnadc_tcp_pft <- 
  pnadc |> 
  filter(V4012 == "Conta própria" | 
           (VD4001 == "Pessoas na força de trabalho" & VD4002 == "Pessoas desocupadas") | 
           VD4001 == "Pessoas fora da força de trabalho")


#Identificando os que cumpriram com a condição anterior
vetor_tcp_pft <- unique(pnadc_tcp_pft$id)


#Filtrando a base apenas com os que cumpriram a condição
pnadc_tcp_pft_todos <- 
  pnadc |> 
  filter(id %in% vetor_tcp_pft)


# Pegando apenas pessoas que tiveram exatamente 5 entrevistas 
id_pnadc_trimestres <- pnadc_tcp_pft_todos |> 
                        group_by(id) |> 
                        count() |> 
                        filter(n == 5)

#Identificando os ID
vetor_tcp_5trimestres <- unique(id_pnadc_trimestres$id)

#Filtrando os ID
tcp_pft_5trim <- 
  pnadc_tcp_pft_todos |> 
  filter(id %in% vetor_tcp_5trimestres)

#Salvando
#write.csv(tcp_pft_5trim,"tcp_pft_5trimestres.csv")