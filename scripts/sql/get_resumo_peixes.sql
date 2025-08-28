select m.transmissor_id
       ,m.data_hora_soltura
       ,m.data_hora_remocao
       ,m.lat
       ,m.long
       ,m.comprimento_total
       ,r.total
       ,r.jusante
       ,r.montante
       ,r.dir_primeiro_mov
       ,r.taxa_desloc_media_kmrd
       ,mov.lat as lat_ud
       ,mov.long as long_ud
       ,mov.data_hora as data_hora_ud
       ,r.dist_da_soltura_max
       ,r.jusante_max_rel_soltura
       ,r.montante_max_rel_soltra
from telemetria.marcacao m
left join telemetria.resultados r on m.transmissor_id = r.radio_id::int
left join lateral (
	select lat, long, data_hora
	from telemetria.dados_consolidados_total dct
	where dct.radio_id::int = m.transmissor_id
	order by data_hora desc
	limit 1
) mov on true
