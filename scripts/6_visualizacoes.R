source('scripts/utils_settings.R')
options(scipen = 999)
resultados <- get_sql('select * from telemetria.resultados r;')


eixo_max <- max(resultados$montante)
eixo_min <- min(resultados$jusante)
quebras <- floor(eixo_max/50)
x_labs <- n_distinct(resultados$radio_id)
cor_jus <- '#5b1e00'
cor_mon <- '#03405a'

p <- ggplot(resultados, aes(x = reorder(radio_id, total))) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_col(aes(y = jusante), fill = cor_jus) +
  geom_col(aes(y = montante), fill = cor_mon) +
  coord_flip(clip='off') +
  #scale_y_continuous(limits = c(-eixo_max, eixo_max), n.breaks = quebras) +
  theme_classic(base_size = 12) +
  labs(
    x="",
    y="km"
  )  +
  annotate("text", x = x_labs + 2, y = eixo_min/2, label = "Jusante", col=cor_jus) +
  annotate("text", x = x_labs + 2, y = (eixo_max/2), label = "Montante", col=cor_mon) +
  annotate("text", x = x_labs + 1, y = eixo_min, label = "Radio ID", hjust=1.2) +
  theme(
    legend.position = 'None',
    axis.line.y = element_blank(),
    plot.margin = margin(20,10,4,4)
  )

ggsave('img/graficos/distancias.png', plot = p, width = 4, height = 6)


ggplot(resultados, aes(x = tempo_pri_mov)) +
  #geom_line(aes(y = numero_ind_prim_mov)) +
  geom_line(aes(x = tempo_pri_mov_jus, y = numero_ind_prim_mov_jus, col = "Jusante"), lwd=1.1) +
  geom_line(aes(x = tempo_pri_mov_mont, y = numero_ind_prim_mov_mont, col = "Montante"), lwd=1.1)  +
  scale_color_manual('', values = c("Jusante" = cor_jus, "Montante" = cor_mon)) +
  scale_x_continuous(n.breaks = 12) +
  theme_classic(base_size = 12) +
  theme(
    legend.position = 'top',
    axis.line.y = element_blank(),
    plot.margin = margin(20,10,4,4)
  ) +
  labs(
    x = 'Dias desde a soltura',
    y = ''
  )


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

