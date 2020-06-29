#################
################# # Lê o .csv padronizado e cria um df apenas para principais RM's e Brasil Urbano
##### Lucas ##### # Plota 7 gráficos da parcela da renda gasta com transporte em cada RM
################# # 1 linerange agregado para 2002-2017, 3 Cleveland Plots para 2002-2017 (renda, gênero e raça)
################# # e 3 Cleveland Plots para 2017 comparando renda com gênero/raça/Estrato dentro da RM
 
### Setup

source("~/POF/setup.R")

######## 1. Dowload and arrange data ----------------------------------------------------------

pof <- # Recupera base padronizada consolidada
  data.table::fread("pof_total.csv")

pof_urbano <- # Filtra Brasil Urbano e nomeia RM's
  pof %>%
  dplyr::filter(
    Estrato != "Interior Rural" 
  ) %>%
  dplyr::mutate( 
    RM = ifelse(Estrato != "Interior Urbano" & UF == "SP", "São Paulo",
      ifelse(Estrato != "Interior Urbano" & UF == "RJ", "Rio de Janeiro",
      ifelse(Estrato != "Interior Urbano" & UF == "PR", "Curitiba",
      ifelse(Estrato != "Interior Urbano" & UF == "RS", "Porto Alegre",
      ifelse(Estrato != "Interior Urbano" & UF == "BA", "Salvador",
      ifelse(Estrato != "Interior Urbano" & UF == "PE", "Recife",
      ifelse(Estrato != "Interior Urbano" & UF == "CE", "Fortaleza",
      ifelse(Estrato != "Interior Urbano" & UF == "PA", "Belém",
      ifelse(Estrato != "Interior Urbano" & UF == "MG", "Belo Horizonte",
      ifelse(Estrato != "Interior Urbano" & UF == "DF", "Brasília",
             "Brasil Urbano")))))))))))

pof_urbano$RM <- # Ordena as RM's por nome
  pof_urbano$RM %>%
  factor(
    levels = c(
      "Belém",
      "Belo Horizonte",
      "Brasília",
      "Curitiba",
      "Fortaleza",
      "Porto Alegre",
      "Recife",
      "Rio de Janeiro",
      "Salvador",
      "São Paulo",
      "Brasil Urbano"
    )
  )

######## 2. Plot data ----------------------------------------------------------

## 2.1. Linerange ---------------------

pof_urbano_medias <- # Calcula médias, sd, e intervalos de confiança para Linerange
  pof_urbano %>%
  dplyr::group_by(Ano, RM) %>%
  dplyr::summarise(
    prop = weighted.mean(prop_transporte_familia, PESO_FINAL),
    sd = sd(prop_transporte_familia),
    upper = prop + 1.96 * (sd / sqrt(n())),
    lower = prop - 1.96 * (sd / sqrt(n()))
  ) %>%
  dplyr::group_by(Ano) %>%
  dplyr::mutate(media_br = mean(prop)) %>%
  dplyr::ungroup()


pof_urbano_medias %>%
  dplyr::filter(RM != "Brasil Urbano") %>%
  ggplot(aes(RM, prop, color = as.factor(Ano))) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
    position = "dodge", width = .5) +
  geom_point(position = position_dodge(width = .5)) +
  geom_hline(aes(yintercept = media_br, color = as.factor(Ano)),
    linetype = "dashed", size = .9) +
  theme_bw() +
  ggsci::scale_color_uchicago() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "", y = "", color = "Ano",
    title = "Comprometimento da Renda com Transporte, 2002 - 2017",
    subtitle = "Parcela da renda familiar destinada a despesas com transporte por Região Metropolitana\nMédias ponderadas e intervalos de confiança a 95%. A Linha traçejada representa a média nacional",
    x = "", y = "", fill = "Ano", caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  theme(legend.position = "bottom") +
  facet_wrap(~quintil_renda, nrow = 3)

ggsave("linerange.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.2. Cleveland Gênero x Ano ---------------------

pof_urbano_genero_ano <- 
  pof_urbano %>%
  dplyr::filter(Ano != "2008") %>%
  dplyr::group_by(RM, genero, Ano) %>%
  dplyr::summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label1 <- # Calcula variação anual para label
  pof_urbano_genero_ano %>%
  tidyr::spread(Ano, prop) %>%
  dplyr::rename(x2017 = "2017", x2002 = "2002") %>%
  dplyr::mutate(label = round(((x2017 - x2002) / x2002), 2))

ggplot(pof_urbano_genero_ano) +
  geom_point(aes(prop, RM, color = as.factor(Ano)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9 ) +
  geom_text(
    data = label1,
    aes(x = ifelse(x2017 > x2002, x2017, x2002), y = RM, group = genero, label = scales::percent(label)),
    hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent, limits = c(.075, .2)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2002 - 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por gênero\nVariação percentual do comprometimento da renda com despesas de transporte entre 2002 e 2017",
    x = "", y = "", color = "Ano",
    caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~genero)

ggsave("genero_anual.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.3. Cleveland Etnia x Ano ---------------------

pof_urbano_etnia_ano <-
  pof_urbano %>%
  filter(Ano != "2008") %>%
  filter(etnia == "Branca" | etnia == "Parda" | etnia == "Preta") %>%
  group_by(RM, etnia, Ano) %>%
  summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label2 <-
  pof_urbano_etnia_ano %>%
  tidyr::spread(Ano, prop) %>%
  dplyr::rename(x2017 = "2017", x2002 = "2002") %>%
  dplyr::mutate(label = round(((x2017 - x2002) / x2002), 2))

ggplot(pof_urbano_etnia_ano) +
  geom_point(aes(prop, RM, color = as.factor(Ano)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9) +
  geom_text(
    data = label2,
    aes(x = ifelse(x2017 > x2002, x2017, x2002), y = RM, group = etnia, label = scales::percent(label)),
    hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent, limits = c(.075, .2)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2002 - 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por etnia\nVariação percentual do comprometimento da renda com despesas de transporte entre 2002 e 2017",
    x = "", y = "", color = "Ano",
    caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~etnia)

ggsave("etnia_anual.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.4. Cleveland Renda x Ano ---------------------

pof_urbano_renda_ano <-
  pof_urbano %>%
  dplyr::filter(Ano != "2008") %>%
  dplyr::filter(quintil_renda == "Q1 poorest" | quintil_renda == "Q5 richest" | quintil_renda == "Q3") %>%
  dplyr::group_by(RM, quintil_renda, Ano) %>%
  dplyr::summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label3 <-
  pof_urbano_renda_ano %>%
  tidyr::spread(Ano, prop) %>%
  dplyr::rename(x2017 = "2017", x2002 = "2002") %>%
  dplyr::mutate(label = round(((x2017 - x2002) / x2002), 2))

ggplot(pof_urbano_renda_ano) +
  geom_point(aes(prop, RM, color = as.factor(Ano)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9) +
  geom_text(
    data = label3, aes(x = ifelse(x2017 > x2002, x2017, x2002), y = RM, group = quintil_renda, label = scales::percent(label)),
    hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent, limits = c(.05, .275)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2002 - 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por quintil de renda\nVariação percentual do comprometimento da renda com despesas de transporte entre 2002 e 2017",
    x = "", y = "", color = "Ano",
    caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~quintil_renda)

ggsave("renda_anual.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.5. Cleveland Renda x Gênero ---------------------

pof_urbano_genero_renda <-
  pof_urbano %>%
  dplyr::filter(Ano == "2017") %>%
  dplyr::filter(quintil_renda == "Q1 poorest" | quintil_renda == "Q5 richest" | quintil_renda == "Q3") %>%
  dplyr::group_by(RM, quintil_renda, genero) %>%
  dplyr::summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label1b <-
  pof_urbano_genero_renda %>%
  tidyr::spread(genero, prop) %>%
  dplyr::mutate(label = round(((Homem - Mulher) / Mulher), 2))

ggplot(pof_urbano_genero_renda) +
  geom_point(aes(prop, RM, color = as.factor(genero)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9) +
  geom_text(data = label1b, aes(
    x = ifelse(Homem > Mulher, Homem, Mulher), y = RM,
    group = quintil_renda, label = scales::percent(label)
  ), hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent, limits = c(.025, .31)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por quintil de renda\nDiferença percentual do comprometimento da renda com transporte entre quintis de renda conforme o gênero",
    x = "", y = "", color = "Gênero", caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~quintil_renda)

ggsave("genero_renda.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.6. Cleveland Renda x Etnia ---------------------

pof_urbano_etnia_renda <-
  pof_urbano %>%
  dplyr::filter(Ano == "2017") %>%
  dplyr::filter(quintil_renda == "Q1 poorest" | quintil_renda == "Q5 richest" | quintil_renda == "Q3") %>%
  dplyr::filter(etnia == "Branca" | etnia == "Parda" | etnia == "Preta") %>%
  dplyr::group_by(RM, quintil_renda, etnia) %>%
  dplyr::summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label2b <-
  pof_urbano_etnia_renda %>%
  tidyr::spread(etnia, prop) %>%
  dplyr::mutate(label = round(((Preta - Branca) / Branca), 2))

ggplot(pof_urbano_etnia_renda) +
  geom_point(aes(prop, RM, color = as.factor(etnia)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9) +
  geom_text(data = label2b, aes(
    x = ifelse(Preta > Branca, Preta, ifelse(Parda > Branca, Parda, Branca)), y = RM,
    group = quintil_renda, label = scales::percent(label)), hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent,limits = c(.025, .35)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por quintil de renda\nDiferença percentual do comprometimento da renda com transporte entre quintis de renda conforme a etnia",
    x = "", y = "", color = "Etnia", caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~quintil_renda)

ggsave("etnia_renda.png", width = 10, height = 6, units = "in", dpi = 300)

## 2.7. Cleveland Renda x Estrato ---------------------

pof_urbano_estrato_renda <-
  pof_urbano %>%
  dplyr::filter(Ano == "2017") %>%
  dplyr::filter(quintil_renda == "Q1 poorest" | quintil_renda == "Q5 richest" | quintil_renda == "Q3") %>%
  dplyr::filter(RM != "Brasil Urbano" &  RM != "Brasília") %>%
  dplyr::group_by(RM, quintil_renda, Estrato) %>%
  dplyr::summarise(prop = weighted.mean(prop_transporte_ind, PESO_FINAL))

label3b <-
  pof_urbano_estrato_renda %>%
  tidyr::spread(Estrato, prop) %>%
  dplyr::rename(rm = "RM da Capital") %>%
  dplyr::mutate(label = round(((rm - Capital) / rm), 2))

ggplot(pof_urbano_estrato_renda) +
  geom_point(aes(prop, RM, color = as.factor(Estrato)),
    size = 3.5) +
  geom_line(aes(prop, RM, group = RM),
    linetype = "dotted", alpha = 0.75, size = 0.9) +
  geom_text(data = label3b, aes(
    x = ifelse(rm > Capital, rm, Capital), y = RM, 
    group = quintil_renda, label = scales::percent(label)), hjust = -.45, size = 3.5) +
  coord_cartesian() +
  scale_x_continuous(labels = scales::percent, limits = c(.075, .3)) +
  ggsci::scale_color_uchicago() +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    title = "Comprometimento da Renda com Transporte, 2017",
    subtitle = "Parcela da renda individual destinada a despesas com transporte por quintil de renda\nDiferença percentual do comprometimento da renda com transporte entre quintis de renda conforme local na RM",
    x = "", y = "", color = "Local", caption = "Fonte: Pesquisa de Orçamentos Familiares, IBGE. Elaboração: Projeto Acesso a Oportunidades, Ipea"
  ) +
  facet_wrap(~quintil_renda)

ggsave("estrato_renda.png", width = 10, height = 6, units = "in", dpi = 300)
