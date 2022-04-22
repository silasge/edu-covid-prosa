DROP VIEW media_escolas CASCADE;
CREATE OR REPLACE VIEW media_escolas AS
	SELECT 
		prosa.nu_ano,
		prosa.nu_avaliacao,
		prosa.nm_regional,
		prosa.id_escola,
		prosa.nm_escola,
		prosa.nm_localizacao,
		prosa.id_etapa,
		prosa.nm_disciplina,
		CASE
			WHEN prosa.id_etapa IN (1, 2, 3, 4, 5) THEN 'Anos Iniciais'
			WHEN prosa.id_etapa IN (6, 7, 8, 9) THEN 'Anos Finais'
		END nm_faixa_etapa,
		COUNT(id_aluno) as nu_alunos,
		AVG(prosa.vl_proficiencia) AS vl_nota_media,
		STDDEV_SAMP(prosa.vl_proficiencia) AS vl_nota_sd,
		PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY prosa.vl_proficiencia) AS vl_nota_mediana
	FROM prosa
	GROUP BY nu_ano, nu_avaliacao, nm_regional, id_escola, nm_escola, nm_localizacao, id_etapa, nm_disciplina
	ORDER BY nu_ano;


CREATE OR REPLACE VIEW media_escolas_indicadores AS
	SELECT 
		media_escolas.*,
		atu.atu,
		dsu.dsu,
		had.had,
		icg.icg,
		ideb.ideb,
		ied.ied,
		ird.ird,
		tdi.tdi
	FROM media_escolas
	LEFT JOIN (
		SELECT 
			nu_ano,
			id_escola,
			id_etapa,
			atu
		FROM atu
		WHERE atu IS NOT NULL
	) AS atu
	ON 	atu.nu_ano = media_escolas.nu_ano AND
		atu.id_escola = media_escolas.id_escola AND
		atu.id_etapa = media_escolas.id_etapa
	LEFT JOIN (
		SELECT
			nu_ano,
			id_escola,
			nm_faixa_etapa,
			dsu
		FROM dsu
		WHERE dsu IS NOT NULL
	) AS dsu
	ON  dsu.nu_ano = media_escolas.nu_ano AND
		dsu.id_escola = media_escolas.id_escola AND
		dsu.nm_faixa_etapa = media_escolas.nm_faixa_etapa
	LEFT JOIN (
		SELECT 
			nu_ano,
			id_escola,
			id_etapa,
			had
		FROM had
		WHERE had IS NOT NULL
	) AS had
	ON 	had.nu_ano = media_escolas.nu_ano AND
		had.id_escola = media_escolas.id_escola AND
		had.id_etapa = media_escolas.id_etapa
	LEFT JOIN (
		SELECT 
			nu_ano,
			id_escola,
			icg
		FROM icg
		WHERE icg IS NOT NULL
	) AS icg
	ON 	icg.nu_ano = media_escolas.nu_ano AND
		icg.id_escola = media_escolas.id_escola
	LEFT JOIN (
		SELECT
			nu_ano,
			id_escola,
			nm_faixa_etapa,
			ideb
		FROM ideb
		WHERE ideb IS NOT NULL
	) AS ideb
	ON  ideb.nu_ano = media_escolas.nu_ano AND
		ideb.id_escola = media_escolas.id_escola AND
		ideb.nm_faixa_etapa = media_escolas.nm_faixa_etapa	
	LEFT JOIN (
		SELECT
			nu_ano,
			id_escola,
			nm_faixa_etapa,
			ied
		FROM ied
		WHERE ied IS NOT NULL AND nu_cat_ied = 6
	) AS ied
	ON  ied.nu_ano = media_escolas.nu_ano AND
		ied.id_escola = media_escolas.id_escola AND
		ied.nm_faixa_etapa = media_escolas.nm_faixa_etapa
	LEFT JOIN (
		SELECT 
			nu_ano,
			id_escola,
			ird
		FROM ird
		WHERE ird IS NOT NULL
	) AS ird
	ON 	ird.nu_ano = media_escolas.nu_ano AND
		ird.id_escola = media_escolas.id_escola
	LEFT JOIN (
		SELECT 
			nu_ano,
			id_escola,
			id_etapa,
			tdi
		FROM tdi
		WHERE tdi IS NOT NULL
	) AS tdi
	ON 	tdi.nu_ano = media_escolas.nu_ano AND
		tdi.id_escola = media_escolas.id_escola AND
		tdi.id_etapa = media_escolas.id_etapa;
	
SELECT *
FROM MEDIA_ESCOLAS_INDICADORES;
