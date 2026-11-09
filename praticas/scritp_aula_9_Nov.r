library(tidyverse)

# # Preparando dados
# 
# # Baixando de dados.gov.br https://dados.gov.br/dados/conjuntos-dados/srag-2021-e-2022
# # Especificamente dados brutos de 2022
# 
# dados22 <- read_csv2("~/Data/Sivep/Crude/INFLUD22-23-03-2026.csv")
# 
# # Selecionando apenas os casos de COVID residentes do estado do RJ
# 
# dadosRJ2022 <- dados22 |> 
#   filter(
#     # Casos hospitalizados e classificados como SRAG-COVID
#     CLASSI_FIN == 5, HOSPITAL == 1, 
#     # Do estado do Rio de Janeiro
#     CO_MUN_RES > 330000, CO_MUN_RES < 340000,
#     # Maiores de 18 anos         
#     COD_IDADE >= 3018)
# 
# 
# dadosRJ2022.clean <- dadosRJ2022 |> 
#   transmute(
#     # Evolucao 1 - cura, 2 - obito, 3 - obitos por outras causas
#     Obito = case_when(
#       EVOLUCAO == 2 ~ 1, 
#       EVOLUCAO == 1 ~ 0,
#       EVOLUCAO == 3 ~ 0,
#       TRUE ~ NA),
#     # Data da evolucao 
#     DT_EVOLUCA,
#     # Data da internacao
#     DT_INTERNA,
#     # Data de primeiros sintomas
#     DT_SIN_PRI,
#     # Fatores de risco
#     FATOR_RISC = case_when(
#       FATOR_RISC == 1 ~ 1,
#       TRUE ~ 0
#     ),
#     # Tempo ate o obito
#     tempo = as.numeric(DT_EVOLUCA - DT_INTERNA)/(60*60*24),
#     # Idade categorica (18-34; 35-64; 65 ou mais)
#     Idade_cat = case_when(
#       COD_IDADE < 3035 ~ "18 - 34 anos",
#       COD_IDADE >= 3035 & COD_IDADE < 3065 ~ "35 - 64 anos",
#       COD_IDADE >= 3065 ~ "65 anos ou mais"
#     ),
#     # Sexo
#     SexoMasc = case_when(
#       CS_SEXO == "M" ~  1,
#       CS_SEXO == "F" ~ 0,
#       TRUE ~ NA
#     ),
#     # Tomou a dose de reforco?
#     VacinaRef = if_else(is.na(DOSE_REF),0,1), 
#     # Passou pela Unidade de Tratamento Intensivo?
#     UTI = case_when(
#       UTI == 1 ~ 1,
#       TRUE ~ 0
#     ),
#     # Codigo de municipio de residencia
#     CO_MUN_RES
#   )
# 
# 
# dadosRJ2022.clean |> write_csv2("praticas/dados/covidRJ2022.csv")

dadosRJ2022 <- read_csv2(file = "https://raw.githubusercontent.com/lsbastos/md32/refs/heads/main/praticas/dados/covidRJ2022.csv")


# Dados binários ----------------------------------------------------------

mean(dadosRJ2022$Obito, na.rm = T)

## Olhando para as proporcoes


dadosRJ2022 |> 
  group_by(VacinaRef) |> 
  summarise(
    Prop = mean(Obito, na.rm = T)
  )


dadosRJ2022 |> 
  group_by(Idade_cat) |> 
  summarise(
    Prop = mean(Obito, na.rm = T)
  )


dadosRJ2022 |> 
  group_by(VacinaRef, Idade_cat) |> 
  summarise(Prop = mean(Obito, na.rm = T))

dadosRJ2022 |> 
  group_by(VacinaRef, SexoMasc) |> 
  summarise(Prop = mean(Obito, na.rm = T))

dadosRJ2022 |> 
  group_by(FATOR_RISC) |> 
  summarise(
    Prop = mean(Obito, na.rm = T)
  )


dadosRJ2022 |> 
  group_by(UTI) |> 
  summarise(
    Prop = mean(Obito, na.rm = T)
  )

m0 <- glm(Obito ~ VacinaRef,
                    family = binomial, 
                    data = dadosRJ2022) 

summary(m0)

m.full <- glm(Obito ~ VacinaRef + Idade_cat + SexoMasc + FATOR_RISC +  UTI, 
    family = binomial, 
    data = dadosRJ2022) 

summary(m.full)





# Dados: tempo até o óbito ------------------------------------------------


mean(dadosRJ2022$tempo, na.rm = T)

median(dadosRJ2022$tempo, na.rm = T)

# Levando em conta a censura
Surv(time = dadosRJ2022$tempo, event = dadosRJ2022$Obito) |> median(na.rm=T)



library(ggplot2.utils) # Necessario para o Kaplan-Meier no ggplot

dadosRJ2022 |> 
  filter(tempo < 365) |> 
  ggplot( aes(time = tempo, status = Obito) )  + 
  geom_km() + 
  theme_bw()


dadosRJ2022 |> 
  filter(tempo < 365) |> 
  ggplot( aes(time = tempo, status = Obito, color = factor(VacinaRef)) )  + 
  geom_km() + 
  theme_bw()


dadosRJ2022 |> 
  filter(tempo < 365) |> 
  ggplot( aes(time = tempo, status = Obito, color = factor(UTI)) )  + 
  geom_km() + 
  theme_bw()


m0.cox <- coxph(Surv(tempo, Obito) ~ VacinaRef,
                data = dadosRJ2022)

m.full.cox <- coxph(Surv(tempo, Obito) ~ VacinaRef + Idade_cat + FATOR_RISC + SexoMasc + UTI,
      data = dadosRJ2022)


summary(m0.cox)
summary(m.full.cox)


