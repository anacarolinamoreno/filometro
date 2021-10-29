

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)


# Script pra analisar em algum dado momento do dia em quantos postos tem e em quantos falta Astrazeneca

# Raspar os dados do site De Olho na Fila, da Prefeitura de SP

deolhonafila <- function(){

  url <- "https://deolhonafila.prefeitura.sp.gov.br/processadores/dados.php"

  httr::POST(url, body = list(dados = "dados"), encode = "form") |>
    httr::content("text") |>
    jsonlite::fromJSON()

}

# Criar funcao para ajustar datas e horarios usando o pacote lubridate

sf <- stamp("01/20")

sf(ymd_hms("2020-12-05"))

# Criar uma tabela com os dados e adicionar colunas
# Código escrito por José de Jesus Filho (https://github.com/jjesusfilho)

df <- deolhonafila() %>%
  rename(
    regiao_da_cidade = crs
  ) %>%
  mutate(
    atualizacao_mais_recente = max(data_hora),
    data_e_hora_atualizacao = str_sub(atualizacao_mais_recente, start = 1, end = 16),
    contagem = case_when(
      !is.na(equipamento) ~ 1,
      T ~ 1
    ),
    falta_az = case_when(
      astrazeneca == 0 ~ "postos_SEM_astrazeneca",
      astrazeneca == 1 ~ "postos_COM_astrazeneca"),
    falta_cv = case_when(
      coronavac == 0 ~ "postos_SEM_coronavac",
      coronavac == 1 ~ "postos_COM_coronavac"),
    falta_pf = case_when(
      pfizer == 0 ~ "postos_SEM_pfizer",
      pfizer == 1 ~ "postos_COM_pfizer"
    )
  )

# ASTRAZENECA - Calcular porcentagem de postos em cada regiao onde existe falta de Astrazeneca naquele momento

analise_az <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, regiao_da_cidade, falta_az) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_az, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_astrazeneca + postos_COM_astrazeneca,
    pct_postos_SEM_astrazeneca = round((( postos_SEM_astrazeneca * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# ASTRAZENECA - Calcular a mesma porcentagem para a cidade inteira

total_az <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, falta_az) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_az, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_astrazeneca + postos_COM_astrazeneca,
    pct_postos_SEM_astrazeneca = round((( postos_SEM_astrazeneca * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# ASTRAZENECA - Juntar as duas tabelas

resumo_az <- bind_rows(
  analise_az, total_az) %>%
  replace(is.na(.), "TOTAL DA CIDADE")


# CORONAVAC - Calcular porcentagem de postos em cada regiao onde existe falta de Astrazeneca naquele momento

analise_cv <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, regiao_da_cidade, falta_cv) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_cv, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_coronavac + postos_COM_coronavac,
    pct_postos_SEM_coronavac = round((( postos_SEM_coronavac * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# CORONAVAC - Calcular a mesma porcentagem para a cidade inteira

total_cv <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, falta_cv) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_cv, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_coronavac + postos_COM_coronavac,
    pct_postos_SEM_coronavac = round((( postos_SEM_coronavac * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# CORONAVAC - Juntar as duas tabelas

resumo_cv <- bind_rows(
  analise_cv, total_cv) %>%
  replace(is.na(.), "TOTAL DA CIDADE")


# PFIZER - Calcular porcentagem de postos em cada regiao onde existe falta de Astrazeneca naquele momento

analise_pf <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, regiao_da_cidade, falta_pf) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_pf, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_pfizer + postos_COM_pfizer,
    pct_postos_SEM_pfizer = round((( postos_SEM_pfizer * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# PFIZER - Calcular a mesma porcentagem para a cidade inteira

total_pf <- df %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, falta_pf) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
    pivot_wider(names_from = falta_pf, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = postos_SEM_pfizer + postos_COM_pfizer,
    pct_postos_SEM_pfizer = round((( postos_SEM_pfizer * 100 ) / total_postos_FUNCIONANDO), 1)
  )

# PFIZER - Juntar as duas tabelas

resumo_pf <- bind_rows(
  analise_pf, total_pf) %>%
  replace(is.na(.), "TOTAL DA CIDADE")



# TODAS AS VACINAS - Juntar o resumo das três vacinas

resumo_pct_az <- resumo_az %>%
  select(data_e_hora_atualizacao, regiao_da_cidade, pct_postos_SEM_astrazeneca)

resumo_pct_cv <- resumo_cv %>%
  select(regiao_da_cidade, pct_postos_SEM_coronavac)

resumo_pct_pf <- resumo_pf %>%
  select(regiao_da_cidade, pct_postos_SEM_pfizer)

resumo_mais_atual_az_cv <- left_join(
  x = resumo_pct_az,
  y = resumo_pct_cv,
  by = "regiao_da_cidade"
) %>%
  rename(
    data_e_hora_atualizacao = data_e_hora_atualizacao.x
  ) %>%
  select(data_e_hora_atualizacao,
         regiao_da_cidade,
         pct_postos_SEM_astrazeneca,
         pct_postos_SEM_coronavac)


resumo_mais_atual <- left_join(
  x = resumo_mais_atual_az_cv,
  y = resumo_pct_pf,
  by = "regiao_da_cidade"
) %>%
  rename(
    data_e_hora_atualizacao = data_e_hora_atualizacao.x
  ) %>%
  select(data_e_hora_atualizacao,
         regiao_da_cidade,
         pct_postos_SEM_astrazeneca,
         pct_postos_SEM_coronavac,
         pct_postos_SEM_pfizer)


# Criar e salvar arquivo CSV com o resumo
# O horario e a data representam a hora mais recente de atualizacao de um dos postos
# no momento da coleta dos dados

write.csv(resumo_mais_atual, "dados/resumo_mais_atual.csv", row.names = F)
