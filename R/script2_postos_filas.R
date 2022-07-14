
library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)


# Script pra analisar em algum dado momento do dia quantos postos estavam com fila para a vacina

# Raspar os dados do site De Olho na Fila, da Prefeitura de SP
# (Código escrito por José de Jesus Filho: https://github.com/jjesusfilho)

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


df_total <- deolhonafila()


filometro <- deolhonafila() %>%
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
    vacina_influenza = case_when(
      influenza == 0 ~ "postos_SEM_vacina_influenza",
      influenza == 1 ~ "postos_COM_vacina_influenza"
      ),
    status_fila = case_when(
      status_fila == "SEM FILA" ~ "sem_fila",
      status_fila == "FILA PEQUENA" ~ "fila_pequena",
      status_fila == "FILA MÉDIA" ~ "fila_media",
      status_fila == "FILA GRANDE" ~ "fila_grande",
      status_fila == "NÃO FUNCIONANDO" ~ "nao_funcionando",
    )
  )


# Análise por região da cidade

analise_fila <- filometro %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, regiao_da_cidade, status_fila) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = status_fila, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = fila_media + fila_grande + fila_pequena + sem_fila,
    postos_com_fila_media_ou_grande = fila_grande + fila_media,
    pct_postos_com_fila = round((( postos_com_fila_media_ou_grande * 100 ) / total_postos_FUNCIONANDO), 1)
  ) %>%
  select(data_e_hora_atualizacao,
         regiao_da_cidade,
         total_postos_FUNCIONANDO,
         postos_com_fila_media_ou_grande,
         pct_postos_com_fila)

analise_fila_todos <- filometro %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, status_fila) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = status_fila, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = fila_media + fila_grande + fila_pequena + sem_fila,
    postos_com_fila_media_ou_grande = fila_grande + fila_media,
    pct_postos_com_fila = round((( postos_com_fila_media_ou_grande * 100 ) / total_postos_FUNCIONANDO), 1)
  ) %>%
  select(data_e_hora_atualizacao,
         total_postos_FUNCIONANDO,
         postos_com_fila_media_ou_grande,
         pct_postos_com_fila)

analise_fila_regiao <- bind_rows(
  analise_fila, analise_fila_todos) %>%
  replace(is.na(.), "TOTAL DA CIDADE")



# Análise por distrito da cidade

analise_fila_distrito <- filometro %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, distrito, status_fila) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = status_fila, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = fila_media + fila_grande + fila_pequena + sem_fila,
    postos_com_fila_media_ou_grande = fila_grande + fila_media,
    pct_postos_com_fila = round((( postos_com_fila_media_ou_grande * 100 ) / total_postos_FUNCIONANDO), 1)
  ) %>%
  select(data_e_hora_atualizacao,
         distrito,
         total_postos_FUNCIONANDO,
         postos_com_fila_media_ou_grande,
         pct_postos_com_fila)

analise_fila_todos <- filometro %>%
  filter(indice_fila != 5) %>%
  group_by(data_e_hora_atualizacao, status_fila) %>%
  summarise(
    contagem = sum(contagem)
  ) %>%
  pivot_wider(names_from = status_fila, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  mutate(
    total_postos_FUNCIONANDO = fila_media + fila_grande + fila_pequena + sem_fila,
    postos_com_fila_media_ou_grande = fila_grande + fila_media,
    pct_postos_com_fila = round((( postos_com_fila_media_ou_grande * 100 ) / total_postos_FUNCIONANDO), 1)
  ) %>%
  select(data_e_hora_atualizacao,
         total_postos_FUNCIONANDO,
         postos_com_fila_media_ou_grande,
         pct_postos_com_fila)

analise_fila_distrito_total <- bind_rows(
  analise_fila_distrito, analise_fila_todos) %>%
  replace(is.na(.), "TOTAL DA CIDADE")


fila_grande <- filometro %>%
  filter(indice_fila == 4)


# Criar e salvar arquivo CSV com o resumo
# O horario e a data representam a hora mais recente de atualizacao de um dos postos
# no momento da coleta dos dados

write.csv(analise_fila_regiao, "dados/resumo_fila_por_regiao.csv", row.names = F)
write.csv(analise_fila_distrito_total, "dados/resumo_por_distrito.csv", row.names = F)
