#
# initial config----------------
#

rm(list=ls())
gc(reset = T)
source("R/PNS/0_loadpackages.R",local = TRUE)
`%nin%` = Negate(`%in%`)
ls_initial_list <- ls()


body <- "https://infraestrutura.gov.br"
link <- "https://infraestrutura.gov.br/component/content/article/115-portal-denatran/8552-estat%C3%ADsticas-frota-de-ve%C3%ADculos-denatran.html"

dir.create("data-raw")
dir.create("data-raw/DENATRAN")

# exclude files if necessary
files_rm <- list.files(path = "data-raw/DENATRAN",all.files = TRUE,
                       recursive = TRUE,include.dirs = TRUE,full.names = TRUE)
unlink(x = files_rm,recursive = FALSE,force = TRUE) 


#
# 1999 - 2012 DENATRAN FILES ---------------
#

html <- paste(readLines(link), collapse="\n")
temp_matched <- stringr::str_match_all(html, "<a href=\"(.*?)\" style")[[1]][,2]
temp_zip <- temp_matched[temp_matched  %like% "RENAVAM"]
years <- 2012:1999
# namesfiles
names_file <- stringr::str_split(temp_zip,"\\/") 
names_file <- do.call(cbind,names_file)
names_file <- names_file[nrow(names_file),]


# download files and save
lapply(seq_along(temp_zip), function(i){ 
  
  message(names_file[i]) 
  
  # create dir
  suppressWarnings(dir.create(paste0("data-raw/DENATRAN/",years[i])))
  
  aux_dest_file <- paste0("data-raw/DENATRAN/",years[i],"/",names_file[i])
  download.file(url = paste0(body,temp_zip[i]),
                destfile = aux_dest_file)
  
})

#
# 2020 ----
#

link2020 <- "/component/content/article/115-portal-denatran/9484"
html2020 <- paste(readLines(paste0(body,link2020)), collapse="\n")
temp_2020 <- stringr::str_match_all(html2020, "<a href=\"(.*?)\" style")[[1]][,2]
# only january
temp_2020 <- temp_2020[temp_2020 %like% ".xlsx" |
                         temp_2020 %like% ".xls"]
temp_2020 <- temp_2020[temp_2020 %like% "jan" | 
                         temp_2020 %like% "JAN" |
                         temp_2020 %like% "Jan"]
names_file <- stringr::str_split(temp_2020,"\\/") 
names_file <- do.call(cbind,names_file)
names_file <- names_file[nrow(names_file),]
# create dir
suppressWarnings(dir.create("data-raw/DENATRAN/2020"))
# download files and save
lapply(seq_along(temp_2020), function(j){ # j = 1
  
  message(names_file[j]) 
  
  aux_dest_file <- paste0("data-raw/DENATRAN/2020/",names_file[j])
  download.file(url = paste0(body,temp_2020[j]),
                destfile = aux_dest_file)
  
})


#
# new years (2013-2019) -------------------
#

temp_html <- temp_matched[temp_matched %like% ".html"] # 2013-2019
years <- 2019:2013

for(i in seq_along(years)){ # i = 4
  message(years[i])
  
  # complete link
  temp_url <- paste0(body,temp_html[i])
  temp_url <- paste(readLines(temp_url,encoding = "UTF-8"), collapse="\n")
  matched <- stringr::str_match_all(temp_url, "<a href=\"(.*?)\" style")[[1]][,2]
  temp_xlsx <- matched[matched  %like% ".xlsx" | 
                         matched  %like% ".xls" ]
  # only january
  temp_xlsx <- temp_xlsx[temp_xlsx %like% "JAN" | 
                           temp_xlsx %like% "jan" | 
                           temp_xlsx %like% "Jan" ]
  
  # check if has .rar files
  if(length(temp_xlsx) == 0){
    
    temp_xlsx <- matched[matched %like% ".rar" |
                           matched %like% ".zip"]
    # only january
    temp_xlsx <- temp_xlsx[temp_xlsx %like% "JAN" | 
                             temp_xlsx %like% "jan" | 
                             temp_xlsx %like% "Jan" ]
  }
  

  
  # namesfiles
  names_file <- stringr::str_split(temp_xlsx,"\\/") 
  names_file <- do.call(cbind,names_file)
  names_file <- names_file[nrow(names_file),]
  
  # create dir
  suppressWarnings(dir.create(paste0("data-raw/DENATRAN/",years[i])))
  
  # download files and save
  lapply(seq_along(temp_xlsx), function(j){ # j = 125
    
    message(names_file[j]) 
    
    aux_dest_file <- paste0("data-raw/DENATRAN/",years[i],"/",names_file[j])
    download.file(url = paste0(body,temp_xlsx[j]),
                  destfile = aux_dest_file)
    
  })
}

#
# extract all files-----
#
# There are zip within zips, so this part of script has 
# to be executed twice
#
#

allfiles <- list.files(path = "data-raw/DENATRAN",all.files = TRUE,recursive = TRUE,
                       full.names = TRUE)
allfiles <- allfiles[allfiles %like% ".rar" | allfiles %like% ".zip"]

# these file has to be executed manually (dont know why)

exclude2015 <- allfiles[allfiles %like% "7_Frota_UF_Municipio_Marca_Modelo_JAN-15.rar" |
                          allfiles %like% "Frota_por_Municipio_e_Tipo_JAN_15.rar"]
allfiles <- allfiles[allfiles %nin% exclude2015]

# names
names_file <- stringr::str_split(allfiles,"\\/") 
names_file <- do.call(cbind,names_file) %>% t() %>% data.table::as.data.table()
names_file[,foldernames := paste0(V1,"/",V2,"/",V3,"/")]
names_file[,allfiles := allfiles]

lapply(1:nrow(names_file),function(i){ # i = 38
  message(names_file$allfiles[i])
  if(names_file$V4[i] %like% ".zip"){
    unzip(zipfile = names_file$allfiles[i],exdir = names_file$foldernames[i])
  }else{ # .rar 
    archive::archive_extract(archive = names_file$allfiles[i],
                             dir = names_file$foldernames[i])
  }
})
