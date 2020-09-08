# Libraries ----
source('R/inflacao/inflacao_0_libraries.R')
source("R/style.R")


# Paletas ----

aop_colors <- c(
  # cores (1-4: mais claro-mais escuro)
  
  ## qualitativas
  `petroleo`        = '#00324a',
  `roxo`            = '#5561A8',
  `lilas`           = '#814681',
  `rosa`            = '#b8446b',
  `laranja`         = '#d25942',
  `amarelo`         = '#ffa600',
  `beje`            = '#c88300',
  
  `cor1`        = '#AE8880',
  `cor2`            = '#917b66',
  `cor3`           = '#6c6b50',
  `cor4`            = '#475944',
  `cor5`         = '#24473d',
  `cor6`         = '#013337',

  ## sequenciais
  # petroleo
  `petroleo1`       = '#c0e4ff',
  `petroleo2`       = '#80a4c1',
  `petroleo3`       = '#436883',
  `petroleo4`       = '#00324a',
  
  # roxo
  `roxo1`           = '#d6dcff',
  `roxo2`           = '#9199e6',
  `roxo3`           = '#5360a7',
  `roxo4`           = '#0f2c6b',
  
  # lilas
  `lilas1`          = '#ffcdff',
  `lilas2`          = '#cf87cf',
  `lilas3`          = '#924e93',
  `lilas4`          = '#58165b',
  
  # rosa
  `rosa1`           = '#ff92c0',
  `rosa2`           = '#da4e80',
  `rosa3`           = '#a00a50',
  `rosa4`           = '#540018',
  
  # laranja
  `laranja1`        = '#ffaf8f',
  `laranja2`        = '#f06149',
  `laranja3`        = '#b42b1d',
  `laranja4`        = '#6f0000',
  
  # amarelo
  `amarelo1`        = '#ffa600',
  `amarelo2`        = '#ba6d00',
  `amarelo3`        = '#7a3800',
  `amarelo4`        = '#410000',
  
  #`branco`         = '#f1f1f1',
  `branco`         = '#d9d9d9',
  
  # cinzas
  `cinza_escuro`   = '#626262',
  `cinza_medio`    = '#808080',
  `cinza_claro`    = '#cccccc',
  
  
  # crimson king
  `crimson1`       = '#BF6F67',
  `crimson2`       = '#9E877E',
  `crimson3`       = '#A19097',
  `crimson4`       = '#5F6987',
  `crimson5`       = '#252B4A',
  
  
  # crimson variacao
  `crimsonvar1`       = '#bf7972',
  `crimsonvar2`       = '#9b7a7a',
  `crimsonvar3`       = '#aba091',
  `crimsonvar4`       = '#5F6987',
  `crimsonvar5`       = '#252B4A',
  
  # orpheus
  `orpheu1`       = '#E69E7A',
  `orpheu2`       = '#AB8E89',
  `orpheu3`       = '#AAACB1',
  `orpheu4`       = '#7E95AB',
  `orpheu5`       = '#2F7590',
  `orpheu6`       = '#013337',
  
  
  # cartola
  `cartola1`       = '#D08969',
  `cartola2`       = '#B69186',
  `cartola3`       = '#AAACB1',
  `cartola4`       = '#5E88A0',
  `cartola5`       = '#25677E',
  `cartola6`       = '#003338',
  
  # caqui
  `caqui1`       = '#764F02',
  `caqui2`       = '#CC9A48',
  `caqui3`       = '#CCBC72',
  `caqui4`       = '#8C91A2',
  `caqui5`       = '#6A79A4',
  `caqui6`       = '#1c2c4a',
  
  # post
  `post1`       = '#A3553B',
  `post2`       = '#AB9432',
  `post3`       = '#A3A39E',
  `post4`       = '#70937D',
  `post5`       = '#54778A',
  `post6`       = '#032e3f',
  
  # wrapper
  `wrapper1`       = '#C96526',
  `wrapper2`       = '#FFB330',
  `wrapper3`       = '#BD9F68',
  `wrapper4`       = '#7292D8',
  `wrapper5`       = '#2C578D',
  
  # ipea
  `ipea1`       = '#9E3A26',
  `ipea2`       = '#A17A75',
  `ipea3`       = '#878a97',
  `ipea4`       = '#7696AE',
  `ipea5`       = '#2F6280',
  `ipea6`       = '#063754',

  
      ### animals
  # qualitativo
  `animals1`       = '#632E2D',
  `animals2`       = '#9B3431',
  `animals3`       = '#A85A3F',
  `animals4`       = '#C29B65',
  `animals5`       = '#6c6766',
  `animals6`       = '#6A9BB3',
  `animals7`       = '#326287',
  `animals8`       = '#274370',
  `animals9`       = '#272D4F',
  
  # divergente
  `animals_div1`       = '#632E2D',
  `animals_div2`       = '#8C3D34',
  `animals_div3`       = '#A55940',
  `animals_div4`       = '#B57A51',
  `animals_div5`       = '#558BB5',
  `animals_div6`       = '#4677A6',
  `animals_div7`       = '#3A5F8C',
  `animals_div8`       = '#314463',
  
  # sequencial azul
  `animals_azul1`       = '#7799AF',
  `animals_azul2`       = '#507186',
  `animals_azul3`       = '#1c4358',
  `animals_azul4`       = '#00293A',
  
  # sequencial vermelho
  `animals4`       = '#C29B65',
  `animals3`       = '#A85A3F',
  `animals2`       = '#9B3431',
  `animals1`       = '#632E2D'
  
  
  )

aop_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (aop_colors)
  
  aop_colors[cols]
}

aop_palettes <- list(
  
  `original`           = aop_cols('beje','laranja','rosa','lilas','roxo','petroleo'),
  
  `sequencial_petroleo`   = aop_cols(map('petroleo', ~paste0(., 1:4))[[1]]),
  
  `sequencial_rosa`       = aop_cols(map('rosa', ~paste0(., 1:4))[[1]]),
  
  `divergente`            = aop_cols('petroleo3', 'petroleo1', 'rosa1', 'rosa3'),
  
  `divergente_teste`      = aop_cols(
    map('petroleo', ~paste0(., 3:1))[[1]], map('rosa', ~paste0(., 1:3))[[1]]
  ),
  
  `cinzas`           = aop_cols('cinza_escuro','cinza_medio', 'cinza_claro'),
  
  `nova` = aop_cols(map('cor', ~paste0(., 1:6))[[1]]),
  
  
  `crimson`               = aop_cols(map('crimson', ~paste0(., 1:5))[[1]]),
  
  `crimsonvar`               = aop_cols(map('crimsonvar', ~paste0(., 1:5))[[1]]),
  
  `orpheu`               = aop_cols(map('orpheu', ~paste0(., 1:6))[[1]]),
  
  `cartola`               = aop_cols(map('cartola', ~paste0(., 1:6))[[1]]),
  
  `caqui`               = aop_cols(map('caqui', ~paste0(., 1:6))[[1]]),
  
  `post`               = aop_cols(map('post', ~paste0(., 1:6))[[1]]),
  
  `wrapper`               = aop_cols(map('wrapper', ~paste0(., 1:5))[[1]]),
  
  `ipea`               = aop_cols(map('ipea', ~paste0(., 1:6))[[1]]),
  
  `animals`               = aop_cols(map('animals', ~paste0(., 1:9))[[1]]),
  
  `animals_azul`          = aop_cols(map('animals', ~paste0(., 6:9))[[1]]),
  
  `animals_vermelho`          = aop_cols(map('animals', ~paste0(., 4:1))[[1]]),
  
  `animals_divergente`        = aop_cols(
    c(map('animals', ~paste0(., 1:4))[[1]], map('animals', ~paste0(., 6:9))[[1]])
    )
  
)

# Functions scales_aop ----

# Colour
scale_colour_aop <- function(palette = "original", discrete = TRUE, reverse = FALSE, 
                            values = NULL, ...) {
  
  if (is.null(values)) {
    aop_pal <- function(palette = "original", reverse = FALSE, ...) {
      pal <- aop_palettes[[palette]]
      
      if (reverse) pal <- rev(pal)
      
      colorRampPalette(pal, ...)
    }
    
    pal <- aop_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
      discrete_scale("colour", paste0("aop_", palette), palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
  } else {
    scale_colour_manual(values = values, ...)
  }
  
}

# Fill
scale_fill_aop <- function(palette = "original", discrete = TRUE, reverse = FALSE, 
                           values = NULL, ...) {
  
  if (is.null(values)) {
    aop_pal <- function(palette = "original", reverse = FALSE, ...) {
      pal <- aop_palettes[[palette]]
      
      if (reverse) pal <- rev(pal)
      
      colorRampPalette(pal, ...)
    }
    
    pal <- aop_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
      discrete_scale("fill", paste0("aop_", palette), palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
  } else {
    scale_fill_manual(values = values, ...)
  }
  
}


# Show pallete and its colours

viz_palletes <- function(colours, ...){
  viz_palletes <- scales::show_col(colours = colours, ...)
}

# Example
#viz_palletes(aop_palettes$cartola)

# Remover objetos e funcoes nao uteis
rm(aop_cols, aop_colors)


# Testes graficos ----

# Bar graphic
#ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#  geom_bar() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  scale_fill_aop(palette = 'orpheu')


#ggplot(gapminder::gapminder %>% filter(continent%in%c("Asia",'Africa','Europe'))
#       , 
#       aes(lifeExp, gdpPercap, colour = continent)) +
#  geom_point(size=4) +
#  scale_colour_aop(palette = 'cartola') +
#  aop_style()

# Create df with 6 classes of data

#a <- setDT(gapminder::gapminder)[
#  , 
#  .(lifeExp = mean(lifeExp)),
#  by = .(year, continent)
#  ]
#b <- a[continent == 'Europe',
#       .(lifeExp = sum(lifeExp)),
#       by = .(year, continent)
#       ]
#b <- b %>% mutate(
#  continent = 'Sexta categoria',
#  lifeExp = lifeExp + 5
#  )

#df_linhas <- data.table::rbindlist(list(a,b))


#d <- gapminder::gapminder %>% 
#  filter(continent == "Europe" & year == 2007) %>% 
#  mutate(
#    continent = "Sexta categoria",
#    lifeExp = lifeExp + 10,
#    gdpPercap = gdpPercap * 1.5
#  )

#df_pontos <- data.table::rbindlist(
#  list(gapminder::gapminder %>% filter(year == 2007), d)
#  )

#rm(a, b, d)

# line graph
#linhas <- ggplot(
#  df_linhas,
#  aes(year, lifeExp, colour = continent)
#  ) +
#  geom_line(size=2) +
#  scale_colour_aop('ipea', reverse = T) +
#  aop_style() +
#  labs(subtitle = "Linhas") #+
  
# point graph
#pontos <- ggplot(
#  df_pontos,
#  aes(lifeExp, gdpPercap, colour = continent)) +
#  geom_point(size=4) +
#  scale_colour_aop(palette = 'ipea', reverse = T) +
#  aop_style() +
#  labs(subtitle = "Pontos")

# Patchwork the two together
#linhas + pontos +
#  plot_annotation(title = "Paleta: ipea", theme = aop_style()) +
#  guides(colour = "none") +
#  plot_layout(guides = "collect") & theme(legend.position = 'bottom')


