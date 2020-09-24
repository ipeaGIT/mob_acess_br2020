source("./R/setup.R")
source("./R/style.R")
source("./R/colours.R")


##### Passageiros transportados --------------------------

# title: Passageiros equivalentes transportados por veículo por dia no sistema de ônibus urbano (1995-2018)
# nota: Belo Horizonte-MG, Curitiba-PR, Fortaleza-CE, Goiânia-GO, Porto Alegre-RS, Recife-PE, Rio de Janeiro-RJ, Salvador-BA e São Paulo-SP

df_pass <- readxl::read_excel('./data/ntu.xlsx', sheet = 'passageiros')


plot_pass <- 
  ggplot(data=df_pass) +
  geom_point(aes(x=ano, y=volume, color=mes)) + 
  geom_line(aes(x=ano, y=volume, color=mes)) +
  labs(x= 'Ano', y='Passageiros equivalentes\n por veículo por dia') +
  scale_y_continuous(limits = c(100, 650), breaks = seq(100,700, 100) ) +
  scale_x_continuous(breaks = seq(min(df_pass$ano), max(df_pass$ano), by=2) ) +
  theme_minimal() +
  theme(legend.position = 'top') +
  aop_style() +
  scale_colour_aop(palette = 'cartola') +
  theme( panel.grid.major.y = element_line(colour = "gray90"))
  
plot_pass
ggsave(plot_pass, filename = './figures/passageiros_tp.png', width = 16, height = 10, units = 'cm', dpi=200)


##### IPK --------------------------

# title: Evolução do índice de passageiros equivalentes por quilômetro (IPKe) no sistema de ônibus urbano (1994-2018)
# nota: Belo Horizonte-MG, Curitiba-PR, Fortaleza-CE, Goiânia-GO, Porto Alegre-RS, Recife-PE, Rio de Janeiro-RJ, Salvador-BA e São Paulo-SP

df_ipk <- readxl::read_excel('./data/ntu.xlsx', sheet = 'ipk')


plot_ipk <- 
  ggplot(data=df_ipk) +
  geom_point(aes(x=ano, y=ipk, color=mes)) + 
  geom_line(aes(x=ano, y=ipk, color=mes)) +
  labs(x= 'Ano', y='IPK') +
  scale_y_continuous(limits = c(1, 2.6), breaks = seq(1,3, .2) ) +
  scale_x_continuous(breaks = seq(min(df_pass$ano), max(df_pass$ano), by=2) ) +
  theme_minimal() + 
  theme(legend.position = 'none')

plot_ipk
ggsave(plot_ipk, filename = 'ipk.png', width = 16, height = 10, units = 'cm', dpi=200)




ntu <- (plot_pass / plot_ipk) + plot_annotation(tag_levels = 'A')
ntu

ggsave(ntu, filename = 'plot_ntu.png', width = 16, height = 14, units = 'cm', dpi=200)
