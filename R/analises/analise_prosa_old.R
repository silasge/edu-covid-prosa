library(tidyr)
library(dplyr)
library(RPostgres)
library(ggplot2)
library(magrittr)
library(sf)

gre_salvador <- read_sf("./data/gre.gpkg")


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "12345678")



sql_prosa <- "
SELECT *
FROM prosa
WHERE ID_ETAPA IN (5, 9) AND NU_AVALIACAO = 1;
"


prosa <- DBI::dbGetQuery(conn=conn, sql_prosa) %>%
  mutate(nu_ano = as.character(nu_ano),
         nm_regional = stringr::str_replace(nm_regional, "CIDADE BAIXA", "LIBERDADE"))

sql_prosa_por_ano <- "
SELECT 
  NU_ANO,
  NM_REGIONAL,
  ID_ESCOLA,
  NM_ESCOLA,
  NU_AVALIACAO,
  ID_ETAPA,
  NM_DISCIPLINA,
  AVG(VL_PROFICIENCIA) AS NOTA_MEDIA
FROM prosa
WHERE ID_ETAPA IN (5, 9) AND NU_AVALIACAO = 1
GROUP BY NU_ANO, NM_REGIONAL, ID_ESCOLA, NM_ESCOLA, NU_AVALIACAO, ID_ETAPA, NM_DISCIPLINA;
"

prosa_por_ano <- DBI::dbGetQuery(conn=conn, sql_prosa_por_ano) %>%
  mutate(nu_ano = as.character(nu_ano),
         nm_regional = stringr::str_replace(nm_regional, "CIDADE BAIXA", "LIBERDADE"))

writexl::write_xlsx(prosa_por_ano, "prosa_por_escola.xlsx")


tab1_media_escolas <- prosa_por_ano %>%
  group_by(nu_ano, id_etapa, nm_disciplina) %>%
  summarise(
    media_escolas = mean(nota_media, na.rm = TRUE),
    dp_escolas = sd(nota_media, na.rm = TRUE)
  ) %>%
  mutate(
    lower = media_escolas - dp_escolas,
    upper = media_escolas + dp_escolas
  )


ggplot(tab1_media_escolas, aes(x = nu_ano, y = media_escolas)) +
  facet_grid(id_etapa ~ nm_disciplina) +
  geom_linerange(aes(ymin=lower, ymax=upper)) +
  geom_point(size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  labs(
    title = "Média das Escolas no PROSA 2018, 2019 e 2021",
    subtitle = "Por Ano, Disciplina e Etapa",
    caption = "Fonte: Sec. Municipal de Educação de Salvador"
  )

ggsave("./reports/07-02-22_graficos/nota_por_ano.png")

writexl::write_xlsx(tab1_media_escolas, "prosa_media_por_ano.xlsx")

tab2_prosa_gerencias <- prosa %>%
  group_by(nu_ano, nm_regional, id_escola, nm_escola, id_etapa, nm_disciplina) %>%
  summarise(nota_media = mean(vl_proficiencia, na.rm = TRUE)) %>%
  group_by(nu_ano, nm_regional, id_etapa, nm_disciplina) %>%
  summarise(
    media_escolas = mean(nota_media, na.rm = TRUE),
    dp_escolas = sd(nota_media, na.rm = TRUE)
  )



plot_map <- function(ano, etapa, disciplina) {
  tab <- tab2_prosa_gerencias %>%
    filter(nu_ano == ano, id_etapa == etapa, nm_disciplina == disciplina) %>%
    inner_join(gre_salvador)
  
  plt <- ggplot() +
    geom_sf(data = tab, aes(geometry = geom, fill = media_escolas)) +
    theme_void() +
    labs(
      title = glue::glue("Média das Escolas no PROSA {ano} em {stringr::str_to_title(disciplina)}"),
      subtitle = glue::glue("Por Gerência Regional de Educação e Escolas do {etapa}º Ano"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador"
    )
  return(plt)
}


anos <- c(2018, 2019, 2021)
etapas <- c(5, 9)
disciplinas <- c("LÍNGUA PORTUGUESA", "MATEMÁTICA")

for (ano in anos) {
  for (etapa in etapas) {
    for (disciplina in disciplinas) {
      plt <- plot_map(ano, etapa, disciplina)
      ggsave(glue::glue("./reports/07-02-22_graficos/mapa_{ano}_{etapa}ano_{stringr::str_to_lower(disciplina)}.png"), plot = plt)
    }
  }
}


all_perm <- expand.grid(anos, etapas, disciplinas)


purrr::pmap(
  list(all_perm[1], all_perm[2], all_perm[3]),
  ~ plot_map(.x, .y, .z)
)

(plt <- plot_map(2019, 5, "LÍNGUA PORTUGUESA"))

teste_map <- tab2_prosa_gerencias %>%
  filter(nu_ano == 2021, id_etapa == 5, nm_disciplina == "LÍNGUA PORTUGUESA") %>%
  inner_join(gre_salvador)

ggplot() +
  geom_sf(data=teste_map, aes(geometry = geom, fill = media_escolas))

writexl::write_xlsx(tab2_prosa_gerencias, "prosa_media_por_ano_e_gre.xlsx")

ggplot(prosa_por_ano, aes(x = nu_ano, y = nota_media, color = nu_ano)) +
  geom_line() +
  facet_grid(id_etapa ~ nm_disciplina) +
  stat_summary(fun = mean, geom = "point", size = 3)

ggsave("teste.png")

sql_prosa_x_ideb <- "
SELECT
	prosa.*,
	ideb.IDEB,
	tdi.TDI,
	atu.ATU
FROM (SELECT 
	 	NM_REGIONAL,
	 	ID_ESCOLA,
	 	NM_ESCOLA,
	 	ANO_ESCOLAR,
	 	AVG(NOTA_PT) AS NOTA_PT_MEDIA,
	 	AVG(NOTA_MT) AS NOTA_MT_MEDIA,
	  	AVG((NOTA_PT + NOTA_MT)/2) AS NOTA_MEDIA
	 FROM sec_edu_salvador.prosa
	 WHERE ANO_ESCOLAR IN ('5º', '9º')
	 GROUP BY NM_REGIONAL, ID_ESCOLA, NM_ESCOLA, ANO_ESCOLAR) AS prosa
LEFT JOIN (SELECT
		     ID_ESCOLA,
		  	 IDEB,
		  	 CASE
		  		 WHEN ANO_ESCOLAR LIKE '%Iniciais' THEN '5º'
		  		 WHEN ANO_ESCOLAR LIKE '%Finais' THEN '9º'
		  	 END ANO_ESCOLAR
		   FROM inep.ideb
		   WHERE ANO = 2019) AS ideb
ON ideb.ID_ESCOLA = prosa.ID_ESCOLA AND ideb.ANO_ESCOLAR = prosa.ANO_ESCOLAR
LEFT JOIN (SELECT 
		  	 ID_ESCOLA,
		  	 ANO_ESCOLAR,
		     TDI
		   FROM inep.tdi
		   WHERE ANO = 2020) AS tdi
ON ideb.ID_ESCOLA = tdi.ID_ESCOLA AND ideb.ANO_ESCOLAR = tdi.ANO_ESCOLAR
LEFT JOIN (SELECT 
		  	 ID_ESCOLA,
		  	 ANO_ESCOLAR,
		     ATU
		   FROM inep.atu
		   WHERE ANO = 2020) AS atu
ON tdi.ID_ESCOLA = atu.ID_ESCOLA AND tdi.ANO_ESCOLAR = atu.ANO_ESCOLAR;
"

prosa_x_ideb <- DBI::dbGetQuery(conn = conn, statement = sql_prosa_x_ideb) %>%
  filter(ano_escolar == "5º") %>%
  mutate(nm_regional = stringr::str_to_title(nm_regional) %>%
           stringr::str_replace("Ii", "II"))


# grafico de barras ----


(gbarra_nota_media <- ggplot(data = prosa_x_ideb %>%
                               group_by(nm_regional) %>%
                               summarise(nota_media = mean(nota_media, na.rm = TRUE)),
                            aes(x = nota_media, y = reorder(nm_regional, nota_media))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = scales::number(nota_media, accuracy = 0.01)), hjust = 1.2, size = 3.5, color = "white") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    labs(
      title = "Média das Notas - PROSA 2021",
      subtitle = "Por Gerência Regional de Educação e Escolas do 5º Ano",
      caption  = "Fonte: Sec. Municipal de Educação de Salvador"
    ))

ggsave("./reports/graficos_24-01-22/media_notas_por_gre_prosa.png")


# densidade  ----

qt25 <- quantile(prosa_x_ideb$nota_media, c(0.25), na.rm = TRUE)
qt75 <- quantile(prosa_x_ideb$nota_media, c(0.75), na.rm = TRUE)
n_media <- mean(prosa_x_ideb$nota_media, na.rm = TRUE)

(ghist_nota_media <- ggplot(data = prosa_x_ideb, aes(x = nota_media)) +
  geom_density() +
  geom_area(data = ggplot_build(ghist_nota_media)$data[[1]] %>%
              filter(x >= qt25, x <= qt75),
            aes(x = x, y =y), fill = "steelblue", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(nota_media, na.rm = TRUE)),
               color="blue", linetype="dashed", size=1) +
  annotate(
    geom = "text", 
    x = 0,
    y = 0.0073,
    label = glue::glue("Em média as escolas tiveram\n {scales::number(n_media, 0.01, decimal.mark = ',')} de nota"),
    hjust = 0
  ) +
  annotate(
    geom = "curve",
    x = 45,
    y = 0.0071,
    xend = n_media-3,
    yend = 0.005, 
    curvature = .3, 
    arrow = arrow(length = unit(2, "mm"))
  ) + 
  annotate(
    geom = "text",
    x = 215,
    y = 0.0068,
    label = glue::glue("50% das escolas ficaram\n com média entre {scales::number(qt25, 0.01, decimal.mark = ',')} e\n {scales::number(qt75, 0.01, decimal.mark = ',')}"),
    hjust = 1
  ) +
  annotate(
    geom = "curve",
    x = 190,
    y = 0.0062,
    xend = n_media+20,
    yend = 0.004, 
    curvature = -.3, 
    arrow = arrow(length = unit(2, "mm"))
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",")) +
  labs(
    title = "Distribuição das Notas - PROSA 2021",
    subtitle = "Por Escolas do 5º Ano",
    caption  = "Fonte: Sec. Municipal de Educação de Salvador"
  ))
  
ggsave("./reports/graficos_24-01-22/distribuicao_notas_prosa.png")


# Scatter ----


ggplot(data = prosa_x_ideb, aes(x = atu, y = nota_media)) +
  geom_point()
