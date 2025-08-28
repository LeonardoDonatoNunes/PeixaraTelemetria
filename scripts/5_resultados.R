source('scripts/utils_settings.R')
options(scipen = 999)

dados <- get_sql('
     select radio_id,
            base_id,
            data_hora_ini,
            data_hora_fim,
            distancia,
            row_number() over (partition by radio_id order by data_hora_ini) as ordem_registro
     from telemetria.dados_consolidados
')

resultados <-
dados %>%
  dplyr::arrange(radio_id, ordem_registro) %>%
  dplyr::group_by(radio_id) %>%
  dplyr::mutate(
    data_hora_dest = lead(data_hora_ini),
    distancia_dest = lead(distancia),
    base_id_dest = lead(base_id),
    deslocamento = distancia_dest - distancia,
    sentido = dplyr::case_when(
      deslocamento == 0 ~ "Jusante",
      is.na(deslocamento) ~ "Jusante",
      deslocamento > 0 ~ "Jusante",
      deslocamento < 0 ~ "Montante"
      ),
    deslocamento = ifelse(is.na(deslocamento), 0, deslocamento),
    tempo = as.numeric(difftime(data_hora_dest, data_hora_ini, units = 'days')),
    taxa_movimento = abs(deslocamento)/tempo,
    dist_da_soltura = first(distancia) - distancia,
    sentido_rel_soltura = dplyr::case_when(
      dist_da_soltura == 0 ~ "Jusante",
      is.na(dist_da_soltura) ~ "Jusante",
      dist_da_soltura > 0 ~ "Jusante",
      dist_da_soltura < 0 ~ "Montante"
    )
  ) %>%
  dplyr::group_by(radio_id) %>%
  dplyr::summarise(
    jusante = sum(deslocamento[sentido == "Jusante"], na.rm = TRUE) * -1,
    montante = sum(deslocamento[sentido == "Montante"], na.rm = TRUE) * -1,
    total = abs(jusante) + abs(montante),
    jusante_max = max(abs(deslocamento[sentido == "Jusante"]), na.rm = TRUE),
    montante_max = ifelse(
      sum(deslocamento[sentido == "Montante"]) < 0,
      max(deslocamento[sentido == "Montante"], na.rm = TRUE) * -1, 0
    ),
    # atividade_dias = as.numeric(difftime(max(data_hora_fim), min(data_hora_ini), units = 'days')),
    atividade_dias = if (all(is.na(data_hora_ini[deslocamento != 0]))) {0
    } else {as.numeric(difftime(
        max(data_hora_fim[deslocamento != 0], na.rm = TRUE),
        min(data_hora_ini[deslocamento != 0], na.rm = TRUE),
        units = "days"))},
    tempo_pri_mov = first(tempo),
    tempo_pri_mov_mont = first(tempo[sentido == "Montante"]),
    tempo_pri_mov_jus = first(tempo[sentido == "Jusante"]),
    dir_primeiro_mov = ifelse(total == 0, 'Não detectado', first(sentido)),
    taxa_desloc_media_kmrd = mean(taxa_movimento, na.rm = TRUE),
    taxa_desloc_mont_media_kmrd = mean(taxa_movimento[sentido == "Montante"], na.rm = TRUE),
    taxa_desloc_jus_media_kmrd = mean(taxa_movimento[sentido == "Jusante"], na.rm = TRUE),
    jusante_max = ifelse(is.infinite(jusante_max), 0, jusante_max),
    montante_max = ifelse(is.infinite(montante_max), 0, montante_max),
    dist_da_soltura_max = max(abs(dist_da_soltura)),
    jusante_max_rel_soltura = max(dist_da_soltura[sentido_rel_soltura == "Jusante"], na.rm = TRUE),
    montante_max_rel_soltra = ifelse(
      sum(dist_da_soltura[sentido_rel_soltura == "Montante"]) < 0,
      min(dist_da_soltura[sentido_rel_soltura == "Montante"], na.rm = TRUE) * -1, 0)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    dplyr::across(dplyr::everything(), ~ ifelse(is.nan(.), NA, .)),
    num = ifelse(total == 0, 0, 1)
  ) %>%
  dplyr::arrange(tempo_pri_mov) %>% dplyr::mutate(numero_ind_prim_mov = cumsum(num)) %>%
  dplyr::arrange(tempo_pri_mov_mont) %>% dplyr::mutate(numero_ind_prim_mov_mont = cumsum(num)) %>%
  dplyr::arrange(tempo_pri_mov_jus) %>% dplyr::mutate(numero_ind_prim_mov_jus = cumsum(num)) %>%
  dplyr::select(-num)

inserir_dados(resultados, schema = 'telemetria', table = 'resultados', append = FALSE)

##  Inserir comentários na tabela do banco
comentarios <- list(
  radio_id = 'Código do transmissor do indivíduo (canalcodigo)',
  jusante = 'Distância total percorrida (km de rio) para jusante',
  montante = 'Distância total percorrida (km de rio) para montante.',
  total = 'Distância total percorrida (km de rio)',
  jusante_max = 'Deslocamento com a maior distância percorrida (km de rio) para jusante',
  montante_max = 'Deslocamento com a maior distância percorrida (km de rio) para montante',
  atividade_dias = 'Número de dias com atividade registrada (diferença entre data da primeira e a última)',
  tempo_pri_mov = 'Tempo (em dias) até o primeiro movimento identificado.',
  tempo_pri_mov_mont = 'Tempo até o primeiro movimento para montante.',
  tempo_pri_mov_jus = 'Tempo até o primeiro movimento para jusante.',
  dir_primeiro_mov = 'Direção do primeiro movimento detectado (Montante ou Jusante).',
  taxa_desloc_media_kmrd = 'Taxa média de deslocamento (km de rio por dia).',
  taxa_desloc_mont_media_kmrd = 'Taxa média de deslocamento no sentido montante (km de rio por dia).',
  taxa_desloc_jus_media_kmrd = 'Taxa média de deslocamento no sentido jusante (km de rio por dia).',
  numero_ind_prim_mov = 'Número de indivíduso foi detectado a primeira vez realizando movimentos',
  numero_ind_prim_mov_mont = 'Número de indivíduso foi detectado a primeira vez realizando movimentos para montante',
  numero_ind_prim_mov_jus = 'Número de indivíduso foi detectado a primeira vez realizando movimentos para jusante'
)

comentario_tabela <- "Tabela com resultados de análise de deslocamento das unidades por radio_id, incluindo métricas de tempo, direção e taxas de deslocamento."

# Comenta a tabela
stmt_tabela <- glue::glue(
  "COMMENT ON TABLE telemetria.resultados IS '{comentario_tabela}';"
)
executar_sql(stmt_tabela)

# Loop para comentar cada coluna
for (coluna in names(comentarios)) {
  comentario <- comentarios[[coluna]]
  stmt <- glue::glue(
    "COMMENT ON COLUMN telemetria.resultados.{coluna} IS '{comentario}';"
  )
  executar_sql(stmt)
}

