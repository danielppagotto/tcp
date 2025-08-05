
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)


# Definindo função --------------------------------------------------------

base_pnad <- function(ano, trimestre){
  pnadc <- get_pnadc(ano, trimestre)
  pnadc <- transform(pnadc, VD4016_ajust = VD4016 * Habitual)
  pnadc_srv <- as_survey(pnadc)
  data <- as.data.frame(pnadc_srv) |> 
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
           VD3004, VD3005, VD4010, VD4011, VD4019, VD4015,
           VD4016, VD4017, VD4019, VD4020, VD2002, V2005,
           V4076, V40761, V40762, V40763, V1028, VD4016_ajust
)
}


# Baixando dados PNAD -----------------------------------------------------

#Baixando dado do último trimestre de 2017
pnad_04_2023 <- base_pnad(2023, 4)


#Criando loop para rodar função
anos <- 2023:2024
trimestres <- 1:4
resultado <- list()

for (ano in anos) {
  for (trimestre in trimestres) {
    cat("Ano:", ano, "- Trimestre:", trimestre, "\n")
    resultado[[length(resultado) + 1]] <-
      base_pnad(ano = ano, trimestre = trimestre)
  }
}


resultado_final <- do.call(rbind, resultado)

#Juntando a base de 2017 e 2018
resultado_final <- rbind(resultado_1,resultado_2)

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
filtro_ocupacao <- 
  pnadc |> 
  filter(V4012 == "Conta própria")

vetor <- unique(filtro_ocupacao$id)


#Filtrando a base apenas com os que cumpriram a condição
pnadc_2 <- 
  pnadc |> 
  filter(id %in% vetor)


# Pegando apenas pessoas que tiveram exatamente 5 entrevistas 
filtro_trimestres <- pnadc_2 |> 
                        group_by(id) |> 
                        count() |> 
                        filter(n == 5)

#Identificando os ID
vetor_trimestres <- unique(filtro_trimestres$id)

#Filtrando os ID
pnadc_final <- 
  pnadc_2 |> 
  filter(id %in% vetor_trimestres) |> 
  mutate(cod_dom = paste(UPA, V1008, V1014, sep = "-"))

codigos_domicilio <- unique(pnadc_final$cod_dom)


# Adicionando variáveis ---------------------------------------------------

qtd_pessoas <- pnadc |> 
  mutate(cod_dom = paste(UPA, V1008, V1014, sep = "-")) |> 
  filter(cod_dom %in% codigos_domicilio) |> 
  group_by(cod_dom, V1016) |> 
  count() |> 
  ungroup()

data_1 <- pnadc_final |> 
  left_join(qtd_pessoas, by = c("cod_dom" = "cod_dom", "V1016" = "V1016"))

renda_media <- pnadc |> 
  mutate(renda = if_else(is.na(VD4016_ajust),0, VD4016_ajust)) |> 
  mutate(cod_dom = paste(UPA, V1008, V1014, sep = "-")) |> 
  filter(cod_dom %in% codigos_domicilio) |> 
  group_by(cod_dom, V1016) |> 
  summarise(renda_familiar_media = mean(renda, na.rm = TRUE))

data_2 <- data_1 |> 
  left_join(renda_media, by = c("cod_dom", "V1016"))

qtd_filhos <- pnadc |> 
  mutate(cod_dom = paste(UPA, V1008, V1014, sep = "-")) |> 
  filter(cod_dom %in% codigos_domicilio) |> 
  filter(V2009 < 14) |> 
  select(cod_dom, V1016, V2005) |> 
  filter(V2005 %in% c("Filho(a) somente do responsável", 
                      "Filho(a) do responsável e do cônjuge")) |> 
  group_by(cod_dom, V1016, V2005) |> 
  count() |> 
  mutate(V2005 = if_else(V2005 == "Filho(a) somente do responsável", "filho_responsavel",
                         "filho_responsavel_conjuge")) |> 
  pivot_wider(names_from = V2005, values_from = n) |> 
  mutate(filho_responsavel = if_else(is.na(filho_responsavel), 0, filho_responsavel),
         filho_responsavel_conjuge = if_else(is.na(filho_responsavel_conjuge), 0, filho_responsavel_conjuge))

data_3 <- data_2 |> 
  left_join(qtd_filhos, by = c("cod_dom", "V1016")) |> 
  mutate(filho_responsavel = if_else(is.na(filho_responsavel), 0, filho_responsavel),
         filho_responsavel_conjuge = if_else(is.na(filho_responsavel_conjuge), 0, filho_responsavel_conjuge))

#Salvando
write.csv(data_3,"as_2023_2024_v3.csv")
