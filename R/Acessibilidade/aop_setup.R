Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
library("tidyverse")
library("sf")           # leitura e manipulacao de dados espaciais
library("scales")

# disable scientific notation
options(scipen=10000)

# function provided by Erwan Le Pennec for the radar coordinate system 
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

  