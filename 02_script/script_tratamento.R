
library(tidyverse)

load("C:/Users/Lapei_Cigets/Desktop/PNADC/pnadc_2014_2016.RData")
load("C:/Users/Lapei_Cigets/Desktop/PNADC/pnadc_2021_2023.RData")
load("C:/Users/Lapei_Cigets/Desktop/PNADC/pnadc17_18.RData")
load("C:/Users/Lapei_Cigets/Desktop/PNADC/pnadc19-20.RData")


dados21_23 <- 
  dados_completos |> 
  select(Ano, Trimestre, UF, Capital, RM_RIDE, 
         UPA, Estrato, V1008, V1014, V1016, V1022,
         V1023, V1027, V1028, V2001, V2003, V2007,
         V2008, V20081, V20082, V2009, V2010, VD4001,
         VD4002, VD4003, VD4004, VD4004A, V4009, V4010, 
         V4012, V4015, V40151, V4016, V4017, V40171, 
         V4018, V4019, V4020, V4021, V4022, V4032, 
         V4033, V4039, V4039C, V4040, V4041, V4043, 
         V40431, V4044, V4046, V4049, V4050, V4056, 
         V4056C, V4057, V4058, V4062, V4062C, VD3004, 
         VD3005, VD4010, VD4011, V4058, V4062, V4062C, 
         VD3004, VD3005, VD4010, VD4011)


dados19_20 <- 
  dados_completos |> 
  select(Ano, Trimestre, UF, Capital, RM_RIDE, 
         UPA, Estrato, V1008, V1014, V1016, V1022,
         V1023, V1027, V1028, V2001, V2003, V2007,
         V2008, V20081, V20082, V2009, V2010, VD4001,
         VD4002, VD4003, VD4004, VD4004A, V4009, V4010, 
         V4012, V4015, V40151, V4016, V4017, V40171, 
         V4018, V4019, V4020, V4021, V4022, V4032, 
         V4033, V4039, V4039C, V4040, V4041, V4043, 
         V40431, V4044, V4046, V4049, V4050, V4056, 
         V4056C, V4057, V4058, V4062, V4062C, VD3004, 
         VD3005, VD4010, VD4011, V4058, V4062, V4062C, 
         VD3004, VD3005, VD4010, VD4011)

dados17_18 <- 
  dados_completos |> 
  select(Ano, Trimestre, UF, Capital, RM_RIDE, 
         UPA, Estrato, V1008, V1014, V1016, V1022,
         V1023, V1027, V1028, V2001, V2003, V2007,
         V2008, V20081, V20082, V2009, V2010, VD4001,
         VD4002, VD4003, VD4004, VD4004A, V4009, V4010, 
         V4012, V4015, V40151, V4016, V4017, V40171, 
         V4018, V4019, V4020, V4021, V4022, V4032, 
         V4033, V4039, V4039C, V4040, V4041, V4043, 
         V40431, V4044, V4046, V4049, V4050, V4056, 
         V4056C, V4057, V4058, V4062, V4062C, VD3004, 
         VD3005, VD4010, VD4011, V4058, V4062, V4062C, 
         VD3004, VD3005, VD4010, VD4011)



dados14_16 <- 
  dados_completos |> 
  select(Ano, Trimestre, UF, Capital, RM_RIDE, 
         UPA, Estrato, V1008, V1014, V1016, V1022,
         V1023, V1027, V1028, V2001, V2003, V2007,
         V2008, V20081, V20082, V2009, V2010, VD4001,
         VD4002, VD4003, VD4004, VD4004A, V4009, V4010, 
         V4012, V4015, V40151, V4016, V4017, V40171, 
         V4018, V4019, V4020, V4021, V4022, V4032, 
         V4033, V4039, V4039C, V4040, V4041, V4043, 
         V40431, V4044, V4046, V4049, V4050, V4056, 
         V4056C, V4057, V4058, V4062, V4062C, VD3004, 
         VD3005, VD4010, VD4011, V4058, V4062, V4062C, 
         VD3004, VD3005, VD4010, VD4011)


todos_anos <- rbind(dados14_16,
                    dados17_18,
                    dados19_20,
                    dados21_23)

# write.csv(todos_anos, "p")


# identificando pessoas ---------------------------------------------------

# concatenamos algumas variaveis para buscar identificar 
# individuos: usamos as variaveis UPA, estrato, domicilio, 
# dia de nascimento, mes, ano, genero. 

# Alem disso, retiramos qualquer observacao cujo dia de 
# nascimento foi 99

# Isso saiu de 20.515.740 observacoes para 18.612.985

pnadc <- 
  todos_anos |> 
    filter(V2008 != "99") |> 
    mutate(genero = if_else(V2007 == "Homem", "01", "00")) |> 
    mutate(id = paste0(UPA, Estrato, V2008, V1008,
                      V20081, V20082, genero,
                      sep = ""))

## Pegando pessoas que atuaram pelo menos uma vez como TCP
## ou como trabalho principal ou secundário
## Isso resultou em 2.292.446 observacoes
## decorrentes de 964234 ids unicos


pnadc_tcp <- 
  pnadc |> 
    filter(V4012 == "Conta própria" |
           V4043 == "Conta própria")

vetor_tcp <- unique(pnadc_tcp$id)

## O vetor anterior traz as pessoas que foram tcp pelo menos
## um momento. Filtrando da base original, percebemos que 
## este procedimento resulta em 3.677.263 observacoes
## Isso mostra que houve um acrescimo de 1.384.817 observacoes
## Logo, isso leva a hipotese de que cada tcp possui mais de 
## uma ocupacao.

pnadc_tcp_todos <- 
  pnadc |> 
    filter(id %in% vetor_tcp)

## vamos guardar essa base para fins de backup
# write.csv(pnadc_tcp_todos, "pnadc_tcp.csv")

# Pegando apenas pessoas que tiveram exatamente 5 entrevistas 
# na pnadc, resultando em um vetor de 478.441 observacoes

id_pnadc_trimestres <- 
                pnadc_tcp_todos |> 
                  group_by(id) |> 
                  count() |> 
                  filter(n == 5)

vetor_tcp5 <- unique(id_pnadc_trimestres$id)


## Vamos manter apenas os tcp que possuem exatamente 
## cinco entrevistas. Portanto, foram mantidos tcp_5trim. 

tcp_5trim <- 
  pnadc_tcp_todos |> 
  filter(id %in% vetor_tcp5)
