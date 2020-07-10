
### carregar bibliotecas
source(here::here("R/Acessibilidade", "aop_setup.R"))


# lembrar sempre de usar `pacote::funcao()`

# readr::read_rds()
# data.table::fread()

######## 1. Download data -------------------------------------------
if (!file.exists(here::here("dados", "dados2019_v1.0_20200116.rds"))) {
  download.file(url = "http://repositorio.ipea.gov.br/bitstream/11058/9586/4/dados2019_v1.0_20200116.rds",
                destfile = here::here("dados", "dados2019_v1.0_20200116.rds"))
}




######## 2. Clean and Recode data -------------------------------------------
aop_data <- read_rds(here::here("dados", "dados2019_v1.0_20200116.rds")) %>%
  st_set_geometry(NULL)

## Cidades com transporte público

cidades_tp <- aop_data %>%
  filter(modo == "tp", pico == 1) %>%
  select(sigla_muni, nome_muni) %>%
  distinct()


######## 3. Calculate indicadors -------------------------------------------

## Tempos de viagem a pé
travel_times_tidy <- aop_data %>%
  filter(modo == "caminhada") %>%
  filter(R002 > 0, R003 > 0) %>%
  select(sigla_muni:id_hex, starts_with("P0"), starts_with("R0"), 
         starts_with("TMI")) %>%
  pivot_longer(starts_with("TMI"), names_to="measure", values_to="travel_time") %>%
  separate(measure, sep = c(3), into = c("measure", "opportunity")) %>%
  drop_na() %>%
  mutate(travel_time = if_else(travel_time == Inf, 90, travel_time)) %>%
  mutate(opportunity_name = factor(opportunity,
                                   levels = c("ET", "EI", "EF", "EM",
                                              "ST", "SB", "SM", "SA"),
                                   labels = c("Ensino (Todos)",
                                              "Ensino (Infantil)",
                                              "Ensino (Fundamental)",
                                              "Ensino (Médio)",
                                              "Saúde (Todos)",
                                              "Saúde (Baixa Complexidade)",
                                              "Saúde (Média Complexidade)",
                                              "Saúde (Alta Complexidade)")))

travel_times_renda <- travel_times_tidy %>%
  ungroup() %>%
  filter(!(opportunity %in% c("ET", "ST"))) %>%
  select(sigla_muni:id_hex, P001, renda = R002, measure, 
         opportunity, opportunity_name, travel_time) %>%
  mutate(renda = factor(renda)) %>%
  mutate(distancia = case_when(travel_time <= 15 ~ "menos_15min",
                               travel_time <= 30 ~ "menos_30min",
                               TRUE ~ "mais_30min")) %>%
  mutate(distancia = factor(distancia, levels = c("menos_15min",
                                                  "menos_30min",
                                                  "mais_30min") )) %>%
  group_by(sigla_muni, nome_muni, renda, opportunity, opportunity_name, distancia) %>%
  summarise(populacao = sum(P001), .groups="drop_last") %>%
  mutate(populacao_p = populacao / sum(populacao)) %>%
  filter(renda %in% c(1, 5)) %>%
  mutate(renda = factor(renda, levels = c(1, 5), labels = c("Q1 - Pobres", "Q5 - Ricos"))) %>%
  select(-populacao) %>%
  pivot_wider(names_from = distancia, values_from = populacao_p) %>%
  mutate(menos_30min = menos_30min + menos_15min,
         mais_30min = mais_30min + menos_30min) %>%
  ungroup() %>%
  pivot_longer(cols = menos_15min:mais_30min, names_to = "tempo", values_to = "populacao") %>%
  filter(tempo %in% c("menos_15min")) %>%
  select(sigla_muni, nome_muni, renda, opportunity, populacao) %>%
  mutate(opportunity = factor(opportunity, levels = c("EI", "EF", "EM", "SA", "SM", "SB"))) %>%
  mutate(nome_muni = factor(nome_muni, 
                            levels = c("Brasilia", "Sao Paulo", "Guarulhos", "Campinas",
                                       "Belo Horizonte", 
                                       "Rio de Janeiro", "Duque de Caxias", "Sao Goncalo", 
                                       "Curitiba", "Porto Alegre", "Campo Grande", "Goiania",
                                       "Fortaleza", "Maceio", "Natal", 
                                       "Recife", "Salvador", "Sao Luis",
                                       "Belem", "Manaus"))
  ) %>%
  mutate(renda = fct_rev(renda)) %>%
  arrange(opportunity) 
  


## Razão entre acessibilidade dos Brancos e dos Negros
razao_acess_raca <- aop_data %>%
  filter(sigla_muni %in% cidades_tp$sigla_muni) %>%
  filter(modo %in% c("caminhada", "tp"), pico == 1, R002 > 0)  %>%
  select(sigla_muni:id_hex, P002, P003, modo, CMATT30, CMATT60) %>%
  pivot_longer(cols=c(CMATT30, CMATT60), 
               names_to="medida", values_to="acessibilidade") %>%
  rename(Brancos=P002, Negros=P003) %>%
  pivot_longer(cols=c(Brancos, Negros), names_to = "grupo", values_to = "populacao") %>%
  separate(medida, sep=5, into=c("medida", "tempo")) %>%
  mutate(medida = paste(modo, tempo)) %>%
  filter(medida != "caminhada 60") %>%
  group_by(sigla_muni, nome_muni, grupo, medida) %>%
  summarise(acessibilidade = weighted.mean(acessibilidade, populacao), .groups = "drop") %>%
  pivot_wider(names_from=grupo, values_from=acessibilidade) %>%
  mutate(razao = "B / N", valor = Brancos / Negros) %>%
  # mutate(razao = "Brancos / Negros", valor = Brancos / Negros) %>%
  select(-Brancos, -Negros) %>%
  mutate(medida = factor(medida,
                       levels=c("caminhada 30",
                                "tp 30",
                                "tp 60"),
                       labels=c("Caminhada (30 min)",
                                "Transporte Público (30 min)",
                                "Tansporte Público (60 min)"))) %>%
  mutate(medida = fct_rev(medida)) 
  
razao_acess_raca


######## 4.1 Figure 1 - Radar Plots - TMI -------------------------------------------

travel_times_renda %>%
  # filter(sigla_muni %in% c("rio", "spo","bsb", "poa", "duq", "sgo")) %>%
  
  ggplot(aes(x=opportunity, y=populacao, fill=renda, colour=renda, group=renda)) +
  
  ### geoms
  geom_polygon(aes(y=0.8), fill="grey80", alpha=0.4, colour="grey60", linetype = "dashed",
               show.legend = TRUE) +
  geom_polygon(alpha=0.6) +
  geom_line(size=0.8) +
  geom_point(size = 3) +
  
  ### annotations
  annotate(geom = "text", x=3.5, y = 0.7, label = " 80%", hjust=1, vjust=0.5) +
  annotate(geom = "point", x = "EF", y = 0, shape = 4, size = 2, colour = "grey20") +

  ### coordinate systems
  coord_radar(start = 0 - (pi / 2)) +
  
  ### scales
  scale_fill_manual(values = c("steelblue4", "indianred1")) +
  scale_colour_manual(values = c("steelblue4", "indianred1")) +
  scale_y_continuous(limits=c(0, 1), breaks = c(0.8, 1),  minor_breaks = c(0.5, 0.6, 0.8, 1)) +

  ### facets
  facet_wrap(~nome_muni, ncol = 4, dir = "h") +

  ### themes
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold", vjust=1),
        axis.line = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(colour="grey80", fill=NA),
        strip.text = element_text(face = "bold"),
        legend.title = element_blank(),
        panel.grid.major.x = element_line(size=0.5, colour="grey40"),
        panel.grid.major.y = element_line(size=0.5, colour="grey80"),
        panel.grid.minor.y = element_line(size=1, colour="grey40", linetype = "dashed")
  ) +
  
  ### labels
  labs(title = "Porcentagem da população residente até 15 min de caminhada da oportunidade mais próxima.",
       subtitle = "20 maiores cidades brasileiras.",
       caption = "Oportunidades: EI - Ensino Infantil, EF - Ensino Fundamental, EM - Ensino Médio\nSB - Saúde Básica, SM - Saúde Média Complexidade, SA - Saúde Alta Complexidade")


ggsave(filename = here::here("figures", "aop_radar_tmi.png"), 
       width = 21, height = 29.7, units = "cm", dpi = 300, scale = 1.4)

######## 4.2 Figure 2 - Razão Brancos / Negros -------------------------------------------
razao_acess_raca %>%
  
  ggplot(aes(x=medida, y=valor, fill=medida)) +
  
  ### geoms
  geom_col(position = position_dodge()) +
  
  ### annotations
  geom_hline(yintercept = 1, colour = "grey20", size = 0.8) +
  geom_text(aes(label=comma(valor, accuracy = 0.1, suffix = "x"), y=1.12), 
            vjust=0.5, hjust=0.5) +

  ### scales
  scale_fill_brewer(palette = "Set1", direction = -1) +
  scale_y_log10(breaks = c(1, 1.5, 2), labels = c("1X", "1.5X", "2X")) +

  ### coordinate systems
  coord_flip() +
  
  ### facets
  facet_wrap(~nome_muni, ncol=4) +

  ### themes
  theme_minimal() +
  theme(#legend.position = c(1, 0),
    legend.position = "none",
    # legend.justification = c("right", "bottom"),
    axis.title = element_blank(),
    panel.border = element_rect(colour="grey20", fill=NA)
  ) +

  ### labels
  labs(title="Razão do nível de acessibilidade aos empregos entre a população\nde cor Branca e de cor Negra",
       y="Razão")

ggsave(here::here("figures", "aop_razao_cor_log.png"), 
       width = 16, height = 8, units = "cm", dpi = 300, scale = 1.4)


######## etc -------------------------------------------







