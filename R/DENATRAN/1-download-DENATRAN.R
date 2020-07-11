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
# 2000 - 2012 DENATRAN FILES ---------------
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

temp_html <- temp_matched[temp_matched %like% ".html"] # 2013-2019

#
# new years (2013-2019) -------------------
#

years <- 2020:2013

for(i in seq_along(years)){ # i = 1
  message(years[i])
  
  # complete link
  temp_url <- paste0(body,temp_html[i])
  temp_url <- paste(readLines(temp_url,encoding = "UTF-8"), collapse="\n")
  matched <- stringr::str_match_all(temp_url, "<a href=\"(.*?)\" style")[[1]][,2]
  temp_xlsx <- matched[matched  %like% ".xlsx" | 
                         matched  %like% ".xls" | 
                         matched %like% ".rar" | 
                         matched %like% ".zip"]
  
  # only december
  temp_xlsx <- temp_xlsx[temp_xlsx %like% "DEZ" | 
                           temp_xlsx %like% "dez" | 
                           temp_xlsx %like% "Dez" ]
  
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

allfiles <- list.files(path = "data-raw/DENATRAN",all.files = TRUE,recursive = TRUE,
                       full.names = TRUE)
allfiles <- allfiles[allfiles %like% ".rar" | allfiles %like% ".zip"]
names_file <- stringr::str_split(allfiles,"\\/") 
names_file <- do.call(cbind,names_file) %>% t() %>% data.table::as.data.table()
names_file[,foldernames := paste0(V1,"/",V2,"/",V3,"/")]
names_file[,allfiles := allfiles]

lapply(66:nrow(names_file),function(i){ # i = 38
  message(names_file$allfiles[i])
  if(names_file$V4[i] %like% ".zip"){
    unzip(zipfile = names_file$allfiles[i],exdir = names_file$foldernames[i])
  }
 
  if(names_file$V4[i] %like% ".rar"){
    archive::archive_extract(archive = names_file$allfiles[i],dir = names_file$foldernames[i])
  }
})
