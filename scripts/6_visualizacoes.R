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
