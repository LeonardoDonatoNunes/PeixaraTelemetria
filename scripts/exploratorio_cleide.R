source('scripts/utils_settings.R')
options(scipen = 999)

# rodar script 4
#------------------------------------------------------------------------------------
# Criar painel com os peixes mais ativos
peixes_desejados <- c(10012, 10022) #rodar um por vez
peixes_desejados <- c(78028, 78029, 78035)

for (peixe in peixes_desejados) {
  dados_i <- dados_final %>%
    filter(radio_id == peixe)

  dados_i$data_hora <- as.POSIXct(dados_i$data_hora)

  dt_min <- min(dados_i$data_hora)
  dt_max <- max(dados_i$data_hora)


  grafico <- ggplot(dados_i, aes(x = data_hora, y = distancia)) +
    geom_line() +
    geom_hline(data = bases, aes(yintercept = dist), linetype = 2, alpha = 0.3, col='red') +
    geom_text(data = bases, aes(x = dt_max, y = dist, label = base_id),
              hjust = -1, color='red', alpha=0.5) +
    geom_point(data = dados_i %>% filter(!base_id %in% c('EST', 'MAR', 'MOV')),
               aes(x = data_hora, y = distancia), shape = 16, size = 3) +
    geom_point(data = dados_i %>% filter(base_id == 'MOV'),
               aes(x = data_hora, y = distancia), shape = 17, size = 3, col = 'blue') +
    geom_image(data = dados_i %>% filter(base_id == 'MAR'),
               aes(x = data_hora, y = distancia, image = 'img/piraiba.png'), size = 0.1) +
    coord_cartesian(clip = 'off') +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months") +
    theme_classic() +
    scale_y_reverse() +
    theme(plot.margin = margin(4, 40, 4, 4),
          axis.line = element_blank()) +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14))+
    labs(
      x = NULL,
      y = 'Distância a partir do km 0 de rio (Km)',
      title = paste0('Movimentos do indivíduo: ', peixe),
    )

  ggsave(filename = paste0('img/graficos/', peixe, '.png'), grafico, width = 8, height = 4)
}


library(cowplot)
library(png)
library(grid)

arquivos <- paste0('img/graficos/', peixes_desejados, '.png')

graficos <- lapply(arquivos, function(arquivo) {
  rasterGrob(readPNG(arquivo), interpolate = TRUE)
})

painel <- plot_grid(plotlist = graficos, ncol = 1)

ggsave('img/graficos/painel_selecionados.png', painel, width = 8, height = 8) #8,12

ggsave('img/graficos/painel_selecionados_1.png', painel, width = 8, height = 12)


#scritp 6_visualizações
# Número total de indivíduos (ajuste se necessário)
total_ind <- 22

# Transformando em proporções
resultados$numero_ind_prim_mov_jus_prop <- resultados$numero_ind_prim_mov_jus / total_ind
resultados$numero_ind_prim_mov_mont_prop <- resultados$numero_ind_prim_mov_mont / total_ind


library(scales)  # para o formato de percentagem

ggplot(resultados, aes(x = tempo_pri_mov)) +
  geom_line(aes(x = tempo_pri_mov_jus, y = numero_ind_prim_mov_jus_prop, col = "Jusante"), lwd = 1.1) +
  geom_line(aes(x = tempo_pri_mov_mont, y = numero_ind_prim_mov_mont_prop, col = "Montante"), lwd = 1.1) +
  scale_color_manual('', values = c("Jusante" = cor_jus, "Montante" = cor_mon)) +
  scale_x_continuous(n.breaks = 12) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +  # transforma em %
  theme_classic(base_size = 12) +
  theme(
    legend.position = 'top',
    axis.line.y = element_blank(),
    plot.margin = margin(20, 10, 4, 4)
  ) +
  labs(
    x = 'Dias desde a soltura',
    y = 'Proporção acumulada de indivíduos (%)'
  )

###################
# Proporcao
total_ind <- 22
resultados$numero_ind_prim_mov_jus_prop <- resultados$numero_ind_prim_mov_jus / total_ind
resultados$numero_ind_prim_mov_mont_prop <- resultados$numero_ind_prim_mov_mont / total_ind

library(scales)

d <- ggplot(resultados, aes(x = tempo_pri_mov)) +
  geom_line(aes(x = tempo_pri_mov_jus, y = numero_ind_prim_mov_jus_prop, col = "Jusante"), lwd = 1.1) +
  geom_line(aes(x = tempo_pri_mov_mont, y = numero_ind_prim_mov_mont_prop, col = "Montante"), lwd = 1.1) +
  scale_color_manual('', values = c("Jusante" = cor_jus, "Montante" = cor_mon)) +
  scale_x_continuous(n.breaks = 14) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = 'top',
    legend.text = element_text(size = 14),
    axis.line.y = element_blank(),
    plot.margin = margin(20, 10, 4, 4)
  ) +
  labs(
    x = 'Dias desde a soltura',
    y = 'Proporção acumulada de indivíduos (%)'
  )

ggsave('img/graficos/movimentos_dias.png', plot = d, width = 8, height = 5)


# deslocamento
dados_longos <- resultados %>%
  dplyr::select(radio_id, taxa_desloc_media_kmrd, jusante_max, montante_max) %>%
  pivot_longer(
    cols = c(taxa_desloc_media_kmrd, jusante_max, montante_max),
    names_to = "tipo",
    values_to = "valor") %>%
  dplyr::mutate(radio_id = factor(radio_id, levels = c("5003", sort(unique(radio_id[radio_id != "5003"])))))

# Gráfico com facetas
f <- ggplot(dados_longos, aes(x = factor(radio_id), y = valor, fill = tipo)) +
  geom_col() +
  facet_wrap(~ tipo, scales = "free_y", labeller = as_labeller(c(
    "taxa_desloc_media_kmrd" = "Média (kmr/dia)",
    "jusante_max" = "Máximo a jusante (kmr)",
    "montante_max" = "Máximo a montante (kmr)"
  ))) +
  scale_fill_manual(
    values = c(
      "taxa_desloc_media_kmrd" = "#1B9E77",
      "jusante_max" = '#5b1e00',
      "montante_max" = '#03405a'
    ),
    guide = "none"  # Remove a legenda (já está separada por facetas)
  ) +
  labs(
    x = "Radio ID",
    y = "Deslocamento (kmr)",
    # title = "Deslocamento máximo e taxa média por indivíduo"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.text = element_text(size = 11),
    strip.background = element_blank(),
    plot.title = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    axis.line.y = element_blank(),
    plot.margin = margin(20, 10, 4, 4)
  );f

ggsave('img/graficos/deslocamento_medio.png', plot = f, width = 8, height = 5)



# teste de correlacao
# ordem original dos radio_ids e total
radio_id_migracao <- c("78026", "78027", "78030", "78031", "78029", "10012", "10023", "10022",
                       "78032", "78028", "78035", "78033", "10020", "10025", "78036", "78034",
                       "10021", "10011", "10013", "10024", "5003", "78037")

total_migracao <- c(36.83031, 52.05563, 24.47487, 25.92644, 226.64434, 809.55297, 137.39106,
                    614.37078, 16.13644, 283.11197, 494.39953, 11.24150, 20.79531, 39.93625,
                    0.65850, 8.01750, 22.68581, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000)

# ordem dos radio_ids com comprimento
radio_id_comp <- c("5003", "10011", "10012", "10013", "10020", "10021", "10022", "10023",
                   "10024", "10025", "78026", "78027", "78030", "78031", "78029", "78028",
                   "78034", "78033", "78032", "78035", "78036", "78037")

comprimento <- c(103, 126, 101, 93, 118, 120, 110, 97, 120, 117,
                 133, 115, 128, 104, 97, 103, 169, 118, 88, 150, 107.5, 150)

# Criar data frame final unificando por radio_id
library(dplyr)

df_migracao <- data.frame(radio_id = radio_id_migracao, total_migracao)
df_comprimento <- data.frame(radio_id = radio_id_comp, comprimento)

dados <- left_join(df_comprimento, df_migracao, by = "radio_id")

library(ggplot2)

ggplot(dados, aes(x = comprimento, y = total_migracao)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Comprimento (mm)", y = "Indicador de Migração (total)",
       title = "Relação entre Comprimento e Migração") +
  theme_minimal()

# Correlação
cor.test(dados$comprimento, dados$total_migracao, method = "pearson")

# Regressão linear
modelo <- lm(total_migracao ~ comprimento, data = dados)
summary(modelo)


# Filtrar apenas indivíduos com migração > 0
dados_filtrados <- dados %>%
  filter(total_migracao > 0)

ggplot(dados_filtrados, aes(x = comprimento, y = total_migracao)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Comprimento (mm)", y = "Indicador de Migração (total)",
       title = "Relação entre Comprimento e Migração (sem zeros)") +
  theme_minimal()

# Correlação
cor.test(dados_filtrados$comprimento, dados_filtrados$total_migracao, method = "pearson")

# Regressão linear
modelo_filtrado <- lm(total_migracao ~ comprimento, data = dados_filtrados)
summary(modelo_filtrado)

# Se p < 0.05: há evidência de relação estatística significativa.
# Se R² baixo (< 0.3): comprimento explica pouca parte da variação → outros fatores influenciam.


#tabela
tab <- resultados %>%
  dplyr::select(radio_id, total, atividade_dias, tempo_pri_mov, tempo_pri_mov_jus, tempo_pri_mov_mont) %>%
  dplyr::mutate(ordem_custom = ifelse(radio_id == "5003", 0, 1)) %>%
  dplyr::arrange(ordem_custom, is.na(radio_id), radio_id) %>%
  dplyr::select(-ordem_custom)

write.csv(tab, "img/graficos/tabela.csv", row.names = FALSE)

# ver a questão de rastreamento móvel, considerar apenas fixo

#------------------------------------------------------------------------------
# Filtra os dados apenas para os indivíduos desejados
ids_desejados <- c(78028, 78029, 78035)
dados_filtrados <- dados_final %>%
  filter(radio_id %in% ids_desejados)

# Define os limites do tempo
dt_min <- min(dados_filtrados$data_hora)
dt_max <- max(dados_filtrados$data_hora)

# Cria o gráfico
grafico <- ggplot(dados_filtrados, aes(x = data_hora, y = distancia, color = factor(radio_id))) +
  geom_line() +
  geom_hline(data = bases, aes(yintercept = dist), linetype = 2, alpha = 0.3, col='red') +
  geom_text(data = bases, aes(x = dt_max, y = dist, label = base_id), hjust = -1, color='red', alpha=0.5) +
  geom_point(data=dados_filtrados %>% filter(!base_id %in% c('EST', 'MAR', 'MOV')),
             aes(x = data_hora, y = distancia), shape = 16, size = 2) +
  geom_point(data=dados_filtrados %>% filter(base_id == 'MOV'),
             aes(x = data_hora, y = distancia), shape = 17, size = 2, col='blue') +
  geom_image(data=dados_filtrados %>% filter(base_id == 'MAR'),
             aes(x = data_hora, y = distancia, image = 'img/piraiba.png'), size = 0.1) +
  coord_cartesian(clip = 'off') +
  scale_y_reverse() +
  theme_classic() +
  theme(
    plot.margin = margin(4,40,4,4),
    axis.line = element_blank()) +
  labs(
    x=NULL,
    y='Distância a partir do km 0 de rio (Km)',
    color = 'Indivíduo',
    title = 'Movimentos dos indivíduos'
  )

# Salva o gráfico
ggsave(filename = 'img/graficos/individuos_agrupados.png', grafico, width = 10, height = 5)


###############
library(ggimage)

# Seleciona apenas os indivíduos desejados
ids_desejados <- c(78028, 78029, 78035)
dados_filtrados <- dados_final %>%
  filter(radio_id %in% ids_desejados)

# Pega apenas o primeiro ponto de soltura (MAR) para colocar a imagem
imagem_ponto <- dados_filtrados %>%
  filter(base_id == 'MAR') %>%
  arrange(data_hora) %>%
  slice(1) %>%
  mutate(image = 'img/piraiba.png')

# Limites de tempo
dt_max <- max(dados_filtrados$data_hora)

# Gráfico
grafico <- ggplot(dados_filtrados, aes(x = data_hora, y = distancia, color = factor(radio_id))) +
  geom_line() +
  geom_hline(data = bases, aes(yintercept = dist), linetype = 2, alpha = 0.3, col = 'red') +
  geom_text(data = bases, aes(x = dt_max, y = dist, label = base_id), hjust = -1, color = 'red', alpha = 0.5) +
  geom_point(data = dados_filtrados %>% filter(!base_id %in% c('EST', 'MAR', 'MOV')),
             shape = 16, size = 3) +
  geom_point(data = dados_filtrados %>% filter(base_id == 'MOV'),
             shape = 17, size = 3, col = 'blue') +
  geom_image(data = imagem_ponto, aes(x = data_hora, y = distancia, image = image),
             inherit.aes = FALSE, size = 0.1) +
  coord_cartesian(clip = 'off') +
  scale_y_reverse() +
  scale_color_manual(
    values = c('78028' = '#B2182B', '78029' = '#006D2C', '78035' = '#67A9CF')) +
  theme_classic() +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    legend.position = 'top',
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    axis.line.y = element_blank(),
    plot.margin = margin(4, 40, 4, 4),
    axis.line = element_blank()) +
  labs(
    x = NULL,
    y = 'Distância a partir do km 0 de rio (Km)',
    color = 'Movimentos dos indivíduos:'
    # title = 'Movimentos dos indivíduos:'
  );grafico

# Salva o gráfico
ggsave(filename = 'img/graficos/individuos_agrupados.png', grafico, width = 8, height = 4)



library(RColorBrewer)
display.brewer.all()
RColorBrewer::brewer.pal(6, "Set1")
"#E41A1C" "#377EB8" "#4DAF4A"
"#B2182B" "#EF8A62" "#FDDBC7" "#D1E5F0" "#67A9CF" "#2166AC"
"#FFFFB2" "#FED976" "#FEB24C" "#FD8D3C" "#F03B20" "#BD0026"
"#41AE76" "#238B45" "#006D2C"
 '#E41A1C','#377EB8','#4DAF4A'
