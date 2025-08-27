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
	select radio_id as transmissor_id,
	       data_hora,
	       distancia,
	       base_id,
	       lat,
	       long
	from set_3
	where ordem_asc  = 1 or ordem_desc = 1
	order by transmissor_id, data_hora desc;
