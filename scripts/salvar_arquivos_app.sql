-- get_bases
select bf.base_id,
       concat(bf.base_id, ' - ', bf.municipio) as label,
       bf.receptor_id,
       bf.data_hora_intalacao,
       bf.lat,
       bf.long,
       bf.dist,
       count(distinct d.radio_id) as n_individuos,
       count(d.base_id) as n_deteccoes
from telemetria.base_fixa bf
left join telemetria.dados_consolidados_total d on bf.base_id = d.base_id
group by bf.base_id, bf.municipio, bf.receptor_id, bf.data_hora_intalacao, bf.lat, bf.long, bf.dist;
