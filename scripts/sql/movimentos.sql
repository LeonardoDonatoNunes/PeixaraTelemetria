with
	set_1 as (
		select *,
			   case when base_id = lag(base_id) over (partition by radio_id order by data_hora) then 0 else 1 end as grupo
		from telemetria.dados_consolidados_total
	),
	set_2 as (
		select *,
		       sum(grupo) over (partition by radio_id order by data_hora) as grupo_
		from set_1
	),
	set_3 as (
		select *,
			row_number() over (partition by radio_id, grupo_ order by data_hora) as ordem_asc,
			row_number() over (partition by radio_id, grupo_ order by data_hora desc) as ordem_desc
		from set_2
	)
	select set_3.radio_id as transmissor_id,
	       set_3.data_hora,
	       set_3.distancia,
	       set_3.base_id,
	       set_3.lat,
	       set_3.long,
	       dct.distancia - set_3.distancia as distancia_rel_soltura
	from set_3
	left join telemetria.dados_consolidados_total dct on set_3.radio_id = dct.radio_id and dct.base_id  = 'MAR'
	where set_3.ordem_asc  = 1 or set_3.ordem_desc = 1
	order by transmissor_id, set_3.data_hora desc;

