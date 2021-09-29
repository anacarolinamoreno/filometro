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
      astrazeneca == 1 ~ "postos_COM_astrazeneca"
    )
  )

# Calcular porcentagem de postos em cada regiao onde existe falta de Astrazeneca naquele momento

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

# Calcular a mesma porcentagem para a cidade inteira

total <- df %>%
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
  ) %>%
  select(data_e_hora_atualizacao, postos_SEM_astrazeneca, postos_COM_astrazeneca, total_postos_FUNCIONANDO, pct_postos_SEM_astrazeneca)

# Juntar as duas tabelas

resumo_mais_atual <- bind_rows(
  analise_az, total
) %>%
  replace(is.na(.), "TOTAL DA CIDADE")


# Criar e salvar arquivo CSV com o resumo
# O horario e a data representam a hora mais recente de atualizacao de um dos postos
# no momento da coleta dos dados

write.csv(resumo_mais_atual, "dados/resumo_mais_atual.csv", row.names = F)
