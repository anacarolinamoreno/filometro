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

# Criar uma tabela com os dados e adicionar colunas
# Código escrito por José de Jesus Filho (https://github.com/jjesusfilho)

df <- deolhonafila() %>%
  mutate(
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
  group_by(crs, falta_az) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = falta_az, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_regiao = postos_SEM_astrazeneca + postos_COM_astrazeneca,
    pct_postos_SEM_astrazeneca = round((( postos_SEM_astrazeneca * 100 ) / total_postos_regiao), 1)
  )
