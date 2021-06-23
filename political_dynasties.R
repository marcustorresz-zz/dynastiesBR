########################################################################
######################## Setting up Libraries ##########################
########################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, future, future.apply,
               quanteda, readtext, pdftools, stringr, RCurl, readxl, tm, stringi)


########################################################################
########################### Setting up Dir. ############################
########################################################################

setwd("/media/spinner/br_cand_docs/2020/txt")

future::plan(multisession)

# Reading Town Code CSVs from GitHub into R
town_codes <- read.csv("https://raw.githubusercontent.com/marcustorresz/dynastiesBR/master/town_codes.csv")


########################################################################
######################## Finding State Codes ###########################
########################################################################

# This is useful for declaring the state-specific files

# RO
town_code_ro <- sort(town_codes$codigo_tse[town_codes$uf == "RO"])
town_code_ro <- str_pad(town_code_ro, 5, pad = "0")

# BA
town_code_ba <- sort(town_codes$codigo_tse[town_codes$uf == "BA"])

# SP
town_code_sp <- sort(town_codes$codigo_tse[town_codes$uf == "SP"])

# MG
town_code_mg <- sort(town_codes$codigo_tse[town_codes$uf == "MG"])


########################################################################
########## Loading Sample Files from RO WITH Parent Names ##############
########################################################################


files <- c("00019/220000671881/15_1600384860519.txt",
           "00035/220001081445/14_1601043802048.txt",
           "00051/220000869882/14_1600718143702.txt",
           "00078/220001190805/14_1600972302714.txt",
           "00116/220000773053/13_1600741818781.txt",
           "00159/220000667626/14_1599730865618.txt",
           "00175/220001200095/pje-3b852f93-Certidão criminal da Justiça Estadual de 1º grau.txt",
           "00191/220001056527/13_1600907660839.txt",
           "00310/220001055240/14_1600997520913.txt",
           "00337/220000745320/14_1600518366743.txt",
           "00396/220000744299/14_1600708444384.txt",
           "00035/220001081445/14_1601043802048.txt",
           "00078/220001189299/15_1600548483174.txt",
           "00159/220001130772/pje-730d8d60-Certidão criminal da Justiça Estadual de 2º grau.txt",
           "00396/220001063464/pje-a3ad8580-Certidão criminal da Justiça Estadual de 1º grau.txt",
           "00396/220001064639/12_1600992577678.txt",
           "00434/220000636214/12_1599774979710.txt",
           "00558/220000835374/15_1600814707856.txt",
           "00574/220001213571/13_1601072113729.txt",
           "00582/220000724827/13_1600518520633.txt",
           "00582/220000917846/14_1600903668041.txt",
           "00612/220000653996/11_1600304138632.txt",
           "00612/220001108541/14_1601052681655.txt",
           "00647/220000890308/pje-1abd71eb-Certidão criminal da Justiça Estadual de 2º grau.txt",
           # "00655/220000663093/pje-1e931a45-Certidão criminal da Justiça Estadual de 2º grau.txt",
           "00655/220000737426/14_1600454763067.txt",
           "00663/220000650622/15_1600045951965.txt",
           "00663/220000867126/14_1600783056905.txt",
           "00663/220000936288/14_1600474999644.txt",
           "00701/220000726899/13_1600565673794.txt",
           "00701/220001063746/11_1600977761886.txt",
           "00736/220000986873/14_1600646327789.txt",
           "00809/220000639408/14_1599609318908.txt"
)


########################################################################
###################### Declaring Main Functions ########################
########################################################################


clean_function <- function(x) {
  x$pai = sub("[:,]", "", x$pai)
  x$pai = sub("[.,]", "", x$pai)
  x$pai = sub("Filiação[ 2]", "", x$pai)
  x$pai = sub("[Nn]ome d[eo] [Pp]ai", "", x$pai)
  x$pai = tm::removeNumbers(x$pai)
  x$pai = sub(", natural(.*)", "", x$pai)
  x$pai = sub(" natural(.*)", "", x$pai)
  x$pai = sub("[\\]", "", x$pai)
  x$pai = sub("[Mm][ã]e ", "", x$pai)
  x$pai = sub("[Pp]ai ", "", x$pai)
  
  x$mae = sub("[:,]", "", x$mae)
  x$mae = sub("[.,]", "", x$mae)
  x$mae = sub("Filiação[ 1]", "", x$mae)
  x$mae = sub("[Nn]ome d[eoa] [Ma][ãa]e", "", x$mae)
  x$mae = sub("[Nn]ome [Ma][ãa]e", "", x$mae)
  x$mae = tm::removeNumbers(x$mae)
  x$mae = sub("conforme(.*)", "", x$mae)
  x$mae = sub("[()]", "", x$mae)
  x$mae = sub("[\\]", "", x$mae)
  x$mae = sub("portador(.*)", "", x$mae)
  x$mae = sub(", natural(.*)", "", x$mae)
  x$mae = sub(" natural(.*)", "", x$mae)
  x$mae = sub("[|]", "", x$mae)
  x$mae = sub("[;]", "", x$mae)
  x$mae = sub("[.]", "", x$mae)
  x$mae = sub(" nascid[oa](.*)", "", x$mae)
  x$mae = sub("[Mm][ã]e ", "", x$mae)
  x$mae = sub("[Pp]ai ", "", x$mae)

  x$mae = str_trim(x$mae)
  x$pai = str_trim(x$pai)
  
  x = x
}

## This function extracts parents from candidate TXTs (GENERAL)
get_parents <- function(file) {
  
  txt <- readLines(file)
  
  if(sum(grepl("[Nn]ome [Mm][ãa]e", txt)) > 0 & sum(grepl("[Nn]ome [Pp]ai", txt)) > 0) {
    mae <- gsub(".*:(.*)$", txt[grep("[Nn]ome [Mm][ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub(".*[Nn]ome [Pp]ai(.*)$", txt[grep("[Nn]ome [Pp]ai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Ff][il]lia[çcg][ãa]o", txt)) > 0){
    mae <- gsub(".*- (.*)$", txt[grep("Filiação", txt)], replacement = "\\1")[1]
    pai <- gsub(".*- (.*)$", txt[grep("Filiação", txt) + 1], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("Nome d[ao] M[ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] Pai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("MÃE", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
    mae <- gsub(".*:(.*)$", txt[grep("MÃE", txt)], replacement = "\\1")[1]
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Ff]ilh[oa] [Dd][aoe]", txt)) > 0) {
    mae <- gsub("[Ff]ilh[oa] [Dd][aoe] (.*)$", txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)], replacement = "\\1")[1]
    pai <- gsub("[Ff]ilh[oa] [Dd][aoe] (.*)$", txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  # if no parents' names available, return NAs for both.
  if( !exists("result") ){
    mae <- NA
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = NA, candidate = NA)
  }
  
  unique(clean_function(result))
  # print(file)
}


## Run it one file
# get_parents(paste0("/media/spinner/br_cand_docs/2020/txt/", files[2]))

## Run it on all files
# RO_00019_parents <- future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", files), get_parents)



########################################################################
########################################################################
############## DECLARING STATE-SPECIFIC FUNCTIONS ######################
########################################################################
########################################################################



# This function was tailored to fit SP (though it also works well for BA)
get_parents_sp <- function(file){
  
  txt <- readLines(file)
  
  if(sum(grepl("[Ff]ilh[oa] [Dd][aoe]", txt)) > 0) {
    mae <- txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)]
    pai <- txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)]
    
    mae <- str_match(mae, " e (.*),")[2]
    pai <- str_match(pai, "[Ff]ilh[oa] [Dd][aoe] (.*?) e ")[2]
      
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Nn]ome [Mm][ãa]e", txt)) > 0 & sum(grepl("[Nn]ome [Pp]ai", txt)) > 0) {
    mae <- gsub(".*:(.*)$", txt[grep("[Nn]ome [Mm][ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub(".*[Nn]ome [Pp]ai(.*)$", txt[grep("[Nn]ome [Pp]ai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Ff][il]lia[çcg][ãa]o", txt)) > 0){
    mae <- gsub(".*- (.*)$", txt[grep("Filiação", txt)], replacement = "\\1")[1]
    pai <- gsub(".*- (.*)$", txt[grep("Filiação", txt) + 1], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("Nome d[ao] M[ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] Pai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("MÃE", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
    mae <- gsub(".*:(.*)$", txt[grep("MÃE", txt)], replacement = "\\1")[1]
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  # if no parents' names available, return NAs for both.
  if( !exists("result") ){
    mae <- NA
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = NA, candidate = NA)
  }
  unique(clean_function(result))
}

# Test
# get_parents_sp(all_files_sp[2110])

# get_parents_ba <- function(file){
  
  txt <- readLines(file)
  
  if(sum(grepl("[Nn]ome [Mm][ãa]e", txt)) > 0 & sum(grepl("[Nn]ome [Pp]ai", txt)) > 0) {
    mae <- gsub(".*:(.*)$", txt[grep("[Nn]ome [Mm][ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub(".*[Nn]ome [Pp]ai(.*)$", txt[grep("[Nn]ome [Pp]ai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Ff][il]lia[çcg][ãa]o", txt)) > 0){
    
    mae <- txt[grep("[Ff][il]lia[çcg][ãa]o", txt)]
    pai <- txt[grep("[Ff][il]lia[çcg][ãa]o", txt)]
    
    mae <- gsub(".*- (.*)$", txt[grep("Filiação", txt)], replacement = "\\1")[1]
    pai <- gsub(".*- (.*)$", txt[grep("Filiação", txt) + 1], replacement = "\\1")[1]
    result <- tibble(mae = mae, pai = pai, town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("Nome d[ao] M[ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] Pai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("MÃE", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
    mae <- gsub(".*:(.*)$", txt[grep("MÃE", txt)], replacement = "\\1")[1]
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("[Ff]ilh[oa] [Dd][aoe]", txt)) > 0) {
    mae <- gsub("[Ff]ilh[oa] [Dd][aoe] (.*)$", txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)], replacement = "\\1")[1]
    pai <- gsub("[Ff]ilh[oa] [Dd][aoe] (.*)$", txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  # if no parents' names available, return NAs for both.
  if( !exists("result") ){
    mae <- NA
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = NA, candidate = NA)
  }
  
  unique(clean_function(result))
  # print(file)
}


########################################################################
########################################################################
################ RUNNING FUNCTION ON ALL STATES ########################
########################################################################
########################################################################


all_files_all_states <- list.files(recursive = T, full.name = T)

## This is runpaining our function on A LOT of files, so will take some time.
all_parents <- unique(
                  future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_all_states), get_parents)
                )


# head(all_parents)

# tail(all_parents)


########################################################################
########################################################################
################# RUNNING FUNCTION ON RONDONIA #########################
########################################################################
########################################################################


# Saving all files from all 52 folders within Rondonia
all_files_rondonia = c()

for (i in 0:length(town_code_ro)){
  town_code_ro[i] = toString(town_code_ro[i])
  all_files_rondonia = c(all_files_rondonia, c(list.files(town_code_ro[i], recursive = T, full.name = T)))
}


# Running get_parents() on every file available from Rondonia
all_rondonia_parents <- unique(
                          future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_rondonia), get_parents)
                        )

head(all_rondonia_parents)

all_bahia_parents_complete <- drop_na(all_rondonia_parents)

########################################################################
########################################################################
################### RUNNING FUNCTION ON BAHIA ##########################
########################################################################
########################################################################


# Declaring all files from Bahia using a FOR LOOP
all_files_bahia = c()

for (i in 0:length(town_code_ba)){
  town_code_ba[i] = toString(town_code_ba[i])
  all_files_bahia = c(all_files_bahia, c(list.files(town_code_ba[i], recursive = T, full.name = T)))
}

head(all_files_bahia)
tail(all_files_bahia)


# Running get_parents() on every file available from Bahia
all_bahia_parents <- unique(
                        future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_bahia), get_parents_sp)
                      )

head(all_bahia_parents)
tail(all_bahia_parents)

all_bahia_parents_complete <- drop_na(all_bahia_parents)


########################################################################
########################################################################
################# RUNNING FUNCTION ON SAO PAULO ########################
########################################################################
########################################################################


# Declaring all files from sp using a FOR LOOP
all_files_sp = c()

for ( i in 0:length(town_code_sp) ){
  town_code_sp[i] = toString(town_code_sp[i])
  all_files_sp = c( all_files_sp, c(list.files(town_code_sp[i], recursive = T, full.name = T) ) )
}

head(all_files_sp)
tail(all_files_sp)


# Running get_parents() on every file available from sp
all_sp_parents <- unique(
                    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_sp), get_parents_sp)
                  )

head(all_sp_parents)

all_sp_parents_complete <- drop_na(all_sp_parents)


########################################################################
########################################################################
################# RUNNING FUNCTION ON MINAS GERAIS #####################
########################################################################
########################################################################


# Declaring all files from mg using a FOR LOOP
all_files_mg = c()

for ( i in 0:length(town_code_mg) ){
  town_code_mg[i] = toString(town_code_mg[i])
  all_files_mg = c( all_files_mg, c(list.files(town_code_mg[i], recursive = T, full.name = T) ) )
}

# Running get_parents() on every file available from mg
all_mg_parents <- unique(
  future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_mg), get_parents_sp)
)

head(all_mg_parents)

all_mg_parents_complete <- drop_na(all_mg_parents)
nrow(all_mg_parents_complete)




########################################################################
########################### VIEW DATAFRAMES ############################
########################################################################


view(all_mg_parents_complete)
view(all_sp_parents_complete)
view(all_bahia_parents_complete)
view(all_bahia_parents_complete)






