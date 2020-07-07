# This script downloads PNS survey data of 2013 from IBGE website and saves it to your local computer
# Script written by Rafael Pereira (urbandemographics.blogspot.com) and modified 
# by Joao Pedro Bazzo
# Jun. 2020, Brasilia


# 1:Download Pns2013 DATA----------------

dir.create("./data-raw/")
dir.create("./data-raw/PNS")
dir.create("./data-raw/PNS/pns_2013_microdados_2017_03_23/")

download.file(url = "ftp://ftp.ibge.gov.br/PNS/2013/microdados/pns_2013_microdados_2017_03_23.zip",
              destfile = "./data-raw/PNS/pns_2013_microdados_2017_03_23.zip")


unzip(zipfile = "./data-raw/PNS/pns_2013_microdados_2017_03_23.zip",
      exdir = "./data-raw/PNS/pns_2013_microdados_2017_03_23/")

