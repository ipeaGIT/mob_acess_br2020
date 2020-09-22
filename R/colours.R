# Libraries ----
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
  
  #`branco`         = '#f1f1f1',
  `branco`         = '#d9d9d9',
  
  # cinzas
  `cinza_escuro`   = '#626262',
  `cinza_medio`    = '#808080',
  `cinza_claro`    = '#cccccc',
  
  # crimson
  `crimson1`       = '#bf7972',
  `crimson2`       = '#9b7a7a',
  `crimson3`       = '#aba091',
  `crimson4`       = '#5F6987',
  `crimson5`       = '#252B4A',
  
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
  `ipea2`       = '#a15545',
  `ipea3`       = '#9e6e68',
  `ipea4`       = '#878a97',
  `ipea5`       = '#7696AE',
  `ipea6`       = '#2F6280',
  `ipea7`       = '#063754',
  
  
  ### animals
  # qualitativo
  `animals1`       = '#872E2B',
  `animals2`       = '#A85A3F',
  `animals3`       = '#C29365',
  `animals4`       = '#6c6766',
  `animals4.1`     = '#c8c8c8',
  `animals5`       = '#6A9BB3',
  `animals6`       = '#326287',
  `animals7`       = '#274370'
  
  
)

aop_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (aop_colors)
  
  aop_colors[cols]
}

aop_palettes <- list(
  
  `original`           = aop_cols('beje','laranja','rosa','lilas','roxo','petroleo'),
  
  `crimson`               = aop_cols(map('crimson', ~paste0(., 1:5))[[1]]),
  
  `orpheu`               = aop_cols(map('orpheu', ~paste0(., 1:6))[[1]]),
  
  `cartola`               = aop_cols(map('cartola', ~paste0(., 1:6))[[1]]),
  
  `caqui`               = aop_cols(map('caqui', ~paste0(., 1:6))[[1]]),
  
  `post`               = aop_cols(map('post', ~paste0(., 1:6))[[1]]),
  
  `wrapper`               = aop_cols(map('wrapper', ~paste0(., 1:5))[[1]]),
  
  `ipea`                  = aop_cols(map('ipea', ~paste0(., 1:7))[[1]]),
  
  `ipea_vermelho`         = aop_cols(map('ipea', ~paste0(., 4:1))[[1]]),
  
  `ipea_azul`             = aop_cols(map('ipea', ~paste0(., 4:7))[[1]]),
  
  `animals`               = aop_cols(map('animals', ~paste0(., 1:7))[[1]]),
  
  `animals_mod`               = aop_cols(
    c(map('animals', ~paste0(., 1:3))[[1]], 'animals4.1', map('animals', ~paste0(., 5:7))[[1]])
  ),
  
  `animals_azul`          = aop_cols(map('animals', ~paste0(., 4:7))[[1]]),
  
  `animals_vermelho`          = aop_cols(map('animals', ~paste0(., 4:1))[[1]])#,
  
  #`animals_divergente`        = aop_cols(
  #  c(map('animals', ~paste0(., 1:4))[[1]], map('animals', ~paste0(., 6:9))[[1]])
  #  )
  
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

#viz_palletes <- function(colours, ...){
#  viz_palletes <- scales::show_col(colours = colours, ...)
#}

# Example
#viz_palletes(aop_palettes$cartola)

# Remover objetos e funcoes nao uteis
rm(aop_cols, aop_colors)


# Testes graficos ----

# Bar graphic
#ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#  geom_bar() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  scale_fill_aop(palette = 'animals')


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


