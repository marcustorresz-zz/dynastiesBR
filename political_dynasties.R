########################################################################
######################## Setting up Libraries ##########################
########################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, future, future.apply, fs,
               furrr, quanteda, readtext, pdftools, stringr, RCurl, readxl, tm, stringi)


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

{
  # RO
  town_code_ro <- sort(town_codes$codigo_tse[town_codes$uf == "RO"])
  town_code_ro <- str_pad(town_code_ro, 5, pad = "0")
  # BA
  town_code_ba <- sort(town_codes$codigo_tse[town_codes$uf == "BA"])
  # SP
  town_code_sp <- sort(town_codes$codigo_tse[town_codes$uf == "SP"])
  # MG
  town_code_mg <- sort(town_codes$codigo_tse[town_codes$uf == "MG"])
  # RS
  town_code_rs <- sort(town_codes$codigo_tse[town_codes$uf == "RS"])
  # PA
  town_code_pa <- sort(town_codes$codigo_tse[town_codes$uf == "PA"])
  town_code_pa <- str_pad(town_code_pa, 5, pad = "0")
  # SC
  town_code_sc <- sort(town_codes$codigo_tse[town_codes$uf == "SC"])
  # PR
  town_code_pr <- sort(town_codes$codigo_tse[town_codes$uf == "PR"])
  # GO
  town_code_go <- sort(town_codes$codigo_tse[town_codes$uf == "GO"])
  # PI
  town_code_pi <- sort(town_codes$codigo_tse[town_codes$uf == "PI"])
  # RJ
  town_code_rj <- sort(town_codes$codigo_tse[town_codes$uf == "RJ"])
  # ES
  town_code_es <- sort(town_codes$codigo_tse[town_codes$uf == "ES"])
  # PE
  town_code_pe <- sort(town_codes$codigo_tse[town_codes$uf == "PE"])
  # CE
  town_code_ce <- sort(town_codes$codigo_tse[town_codes$uf == "CE"])
  # RN
  town_code_rn <- sort(town_codes$codigo_tse[town_codes$uf == "RN"])
  # AL
  town_code_al <- sort(town_codes$codigo_tse[town_codes$uf == "AL"])
  # SE
  town_code_se <- sort(town_codes$codigo_tse[town_codes$uf == "SE"])
  # MA
  town_code_ma <- sort(town_codes$codigo_tse[town_codes$uf == "MA"])
  town_code_ma <- str_pad(town_code_ma, 5, pad = "0")
  # PB
  town_code_pb <- sort(town_codes$codigo_tse[town_codes$uf == "PB"])
  # AM
  town_code_am <- sort(town_codes$codigo_tse[town_codes$uf == "AM"])
  town_code_am <- str_pad(town_code_am, 5, pad = "0")
  # RR
  town_code_rr <- sort(town_codes$codigo_tse[town_codes$uf == "RR"])
  town_code_rr <- str_pad(town_code_rr, 5, pad = "0")
  # TO
  town_code_to <- sort(town_codes$codigo_tse[town_codes$uf == "TO"])
  # AC
  town_code_ac <- sort(town_codes$codigo_tse[town_codes$uf == "AC"])
  town_code_ac <- str_pad(town_code_ac, 5, pad = "0")
  # MT
  town_code_mt <- sort(town_codes$codigo_tse[town_codes$uf == "MT"])
  # MS
  town_code_ms <- sort(town_codes$codigo_tse[town_codes$uf == "MS"])
  # DF
  town_code_df <- sort(town_codes$codigo_tse[town_codes$uf == "DF"])
  # AP
  town_code_ap <- sort(town_codes$codigo_tse[town_codes$uf == "AP"])
}

########################################################################
###################### Declaring Main Functions ########################
########################################################################


# This function cleans the DFs; removes special characters, numbers, etc.
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
  x$pai = sub("portador(.*)", "", x$pai)
  x$pai = sub("feito criminal em nome de(.*)", "", x$pai)
  x$pai = sub("[Cc][Pp][Ff]", "", x$pai)
  x$pai = sub(" nascid[oa](.*)", "", x$pai)
  x$pai = sub(" [Bb]rasileir[oa](.*)", "", x$pai)
  x$pai <- stri_trans_general(str_squish(tolower(x$pai)), "latin-ascii")
  
  x$mae = sub("[Nn]ome [Dd][aeo] ", "", x$mae)
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
  x$mae = sub("feito criminal em nome de(.*)", "", x$mae)
  x$mae = sub("[Cc][Pp][Ff]", "", x$mae)
  x$mae <- stri_trans_general(str_squish(tolower(x$mae)), "latin-ascii")
  
  x$mae = str_trim(x$mae)
  x$mae = str_squish(x$mae)
  x$pai = str_trim(x$pai)
  x$pai = str_squish(x$pai)
  
  # x <- drop_na(unique(x))
  x = x
}

# This function extracts parents from candidate TXTs (GENERAL)
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
  
  if(sum(grepl("Nome d[ao] [Mm][ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] [Pp]ai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("M[AÃ]E", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
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
############## DECLARING STATE-SPECIFIC FUNCTIONS ######################
########################################################################
########################################################################


# This function was tailored to fit SP 
get_parents_sp <- function(file) {
  
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
  
  if(sum(grepl("[Ff][il]lia[çcg][ãa]o", txt)) > 0) {
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

# This function was tailored to fit PA 
get_parents_pa <- function(file) {
  
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
    
    txt <- c(txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)], txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt) + 1])
    txt <- paste(txt, collapse = " ")
    
    mae <- gsub(".* [Ff]ilh[oa] [Dd][aoe] (.*), CPF.*", "\\1", txt)
    pai <- gsub(".* [Ff]ilh[oa] [Dd][aoe] (.*), CPF.*", "\\1", txt)
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

# This function was tailored to fit RS
get_parents_rs <- function(file) {
  
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
    
    txt <- txt[grep("[Ff]ilh[oa] [Dd][aoe]", txt)]
    # txt <- paste(txt, collapse = " ")
    
    mae <- gsub(".* e (.*),.*", "\\1", txt)
    pai <- gsub(".* [Ff]ilh[oa] [Dd][aoe](.*)e .*", "\\1", txt)
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

# This function was tailored to fit PB
get_parents_pb <- function(file) {
  
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
  
  if(sum(grepl("Nome d[ao] [Mm][ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] [Pp]ai", txt)) > 0) {
    mae <- gsub("Nome d[ao] [Mm][ãa]e (.*)$", txt[grep("Nome d[ao] [Mm][ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] [Pp]ai (.*)$", txt[grep("Nome d[ao] [Pp]ai", txt)], replacement = "\\1")[1]
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

# This function was tailored to fit PI
get_parents_pi <- function(file) {
  
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
  
  if(sum(grepl("Nome d[ao] [Mm][ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] [Pp]ai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("M[AÃ]E", txt)) > 0 & sum(grepl("PAI", txt)) > 0){
    
    x = txt[grep("[PM][AÃ][EI]", txt)]
    x = paste0(txt[grep("[PM][AÃ][EI]", txt) - 1], txt[grep("[PM][AÃ][EI]", txt)], txt[grep("[PM][AÃ][EI]", txt) + 1], collapse = " ")
    
    mae <- str_match(x, "M[AÃ]E: (.*?) [PM][AÃ][EI]")[2]
    pai <- str_match(x, "PAI: (.*?)[PM][AÃ][EI]")[2]
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

# This function was tailored to fit GO
get_parents_go <- function(file) {
  
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
  
  if(sum(grepl("Nome d[ao] [Mm][ãa]e", txt)) > 0 | sum(grepl("Nome d[ao] [Pp]ai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai), town = str_match(file, "txt/(.*?)/")[2], candidate = gsub(".*./.*/([0-9]{1,})/.*", replacement = "\\1", x = file))
  }
  
  if(sum(grepl("M[AÃ]E", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
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


# BA      # MOSTLY WORKING; FIX MINOR THINGS
{
  ########################################################################
  ########################################################################
  ################### RUNNING FUNCTION ON BAHIA ##########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from ba using a FOR LOOP
  all_files_ba = c()
  
  for (i in 0:length(town_code_ba)){
    town_code_ba[i] = toString(town_code_ba[i])
    all_files_ba = c(all_files_ba, c(list.files(town_code_ba[i], recursive = T, full.name = T)))
  }
  
  
  # Running get_parents() on every file available from ba
  all_ba_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_ba), get_parents)
  )
  
  # Removing all NAs
  all_ba_parents <- drop_na(unique(all_ba_parents))
}
# PI      # FIXED
{
  ########################################################################
  ########################################################################
  ###################### RUNNING FUNCTION ON PIAUI #######################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_pi = c()
  
  for ( i in 0:length(town_code_pi) ){
    town_code_pi[i] = toString(town_code_pi[i])
    all_files_pi = c( all_files_pi, c(list.files(town_code_pi[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_pi_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_pi), get_parents_pi)
  )
  
  # Removing all NAs
  all_pi_parents <- drop_na(unique(all_pi_parents))
}
# PE      # MOSTLY WORKING; FIX MINOR THINGS
{
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON PE #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_pe = c()
  
  for ( i in 0:length(town_code_pe) ){
    town_code_pe[i] = toString(town_code_pe[i])
    all_files_pe = c( all_files_pe, c(list.files(town_code_pe[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_pe_parents <- unique(
                      future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_pe), get_parents)
                      )
  
  # Removing all NAs
  all_pe_parents <- drop_na(unique(all_pe_parents))
}
# CE      # NEED TO FIX
{
########################################################################
########################################################################
####################### RUNNING FUNCTION ON CE #########################
########################################################################
########################################################################


# Declaring all files from PR using a FOR LOOP
all_files_ce = c()

for ( i in 0:length(town_code_ce) ){
  town_code_ce[i] = toString(town_code_ce[i])
  all_files_ce = c( all_files_ce, c(list.files(town_code_ce[i], recursive = T, full.name = T) ) )
}

# Running get_parents() on every file available from rs
all_ce_parents <- unique(
  future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_ce), get_parents)
)

# Removing all NAs
all_ce_parents <- drop_na(unique(all_ce_parents))
}
# RN      # NEED TO FIX
{
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON RN #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_rn = c()
  
  for ( i in 0:length(town_code_rn) ){
    town_code_rn[i] = toString(town_code_rn[i])
    all_files_rn = c( all_files_rn, c(list.files(town_code_rn[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_rn_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_rn), get_parents)
  )
  
  # Removing all NAs
  all_rn_parents <- drop_na(unique(all_rn_parents))
}
# AL      # NEED TO FIX
{  
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON AL #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_al = c()
  
  for ( i in 0:length(town_code_al) ){
    town_code_al[i] = toString(town_code_al[i])
    all_files_al = c( all_files_al, c(list.files(town_code_al[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_al_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_al), get_parents)
  )
  
  # Removing all NAs
  all_al_parents <- drop_na(unique(all_al_parents))
  }
# SE      # NEED TO FIX
{
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON SE #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_se = c()
  
  for ( i in 0:length(town_code_se) ){
    town_code_se[i] = toString(town_code_se[i])
    all_files_se = c( all_files_se, c(list.files(town_code_se[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_se_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_se), get_parents)
  )
  
  # Removing all NAs
  all_se_parents <- drop_na(unique(all_se_parents))
}
# MA      # NEED TO FIX
{  
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON MA #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_ma = c()
  
  for ( i in 0:length(town_code_ma) ){
    town_code_ma[i] = toString(town_code_ma[i])
    all_files_ma = c( all_files_ma, c(list.files(town_code_ma[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_ma_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_ma), get_parents)
  )
  
  # Removing all NAs
  all_ma_parents <- drop_na(unique(all_ma_parents))
}
# PB      # FIXED
{
  ########################################################################
  ########################################################################
  ####################### RUNNING FUNCTION ON pb #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PB using a FOR LOOP
  all_files_pb = c()
  
  for ( i in 0:length(town_code_pb) ){
    town_code_pb[i] = toString(town_code_pb[i])
    all_files_pb = c( all_files_pb, c(list.files(town_code_pb[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_pb_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_pb), get_parents_pb)
  )
  
  # Removing all NAs
  all_pb_parents <- drop_na(unique(all_pb_parents))
}

# RO      # FIXED
{
  ########################################################################
  ########################################################################
  ################# RUNNING FUNCTION ON RONDONIA #########################
  ########################################################################
  ########################################################################
  
  
  # Saving all files from all 52 folders within Rondonia
  all_files_ro = c()
  
  for (i in 0:length(town_code_ro)){
    town_code_ro[i] = toString(town_code_ro[i])
    all_files_ro = c(all_files_ro, c(list.files(town_code_ro[i], recursive = T, full.name = T)))
  }
  
  
  # Running get_parents() on every file available from Rondonia
  all_ro_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_ro), get_parents)
  )
  
  # Removing all NAs
  all_ro_parents <- drop_na(unique(all_ro_parents))
}
# GO      # NEED TO FIX
{
  ########################################################################
  ########################################################################
  ###################### RUNNING FUNCTION ON GOIAS #######################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_go = c()
  
  for ( i in 0:length(town_code_go) ){
    town_code_go[i] = toString(town_code_go[i])
    all_files_go = c( all_files_go, c(list.files(town_code_go[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_go_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_go), get_parents_go)
  )
  
  # Removing all NAs
  all_go_parents <- drop_na(unique(all_go_parents))
}
# PA      # MOSTLY WORKING; FIX MINOR THINGS
{
  ########################################################################
  ########################################################################
  ##################### RUNNING FUNCTION ON PARÁ #########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PA using a FOR LOOP
  all_files_pa = c()
  
  for ( i in 0:length(town_code_pa) ){
    town_code_pa[i] = toString(town_code_pa[i])
    all_files_pa = c( all_files_pa, c(list.files(town_code_pa[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_pa_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_pa), get_parents_pa)
  )
  
  # Removing all NAs
  all_pa_parents <- drop_na(unique(all_pa_parents))
}

# SP      # FIXED
{
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
  
  
  # Running get_parents() on every file available from sp
  all_sp_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_sp), get_parents_sp)
  )
  
  
  # Removing all NAs
  all_sp_parents <- drop_na(unique(all_sp_parents))
}
# MG      # FIXED
{
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
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_mg), get_parents)
  )
  
  # Removing all NAs
  all_mg_parents <- drop_na(unique(all_mg_parents))
}
# RJ      # NEED TO FIX
{
  ########################################################################
  ###################### RUNNING FUNCTION ON RJ ##########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_rj = c()
  
  for ( i in 0:length(town_code_rj) ){
    town_code_rj[i] = toString(town_code_rj[i])
    all_files_rj = c( all_files_rj, c(list.files(town_code_rj[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_rj_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_rj), get_parents)
  )
  
  # Removing all NAs
  all_rj_parents <- drop_na(unique(all_rj_parents))
}
# ES      # NEED TO FIX
{
  ########################################################################
  ###################### RUNNING FUNCTION ON ES ##########################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_es = c()
  
  for ( i in 0:length(town_code_es) ){
    town_code_es[i] = toString(town_code_es[i])
    all_files_es = c( all_files_es, c(list.files(town_code_es[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_es_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_es), get_parents)
  )
  
  # Removing all NAs
  all_es_parents <- drop_na(unique(all_es_parents))
}

# RS      # FIXED
{
  ########################################################################
  ########################################################################
  ############### RUNNING FUNCTION ON RIO GRANDE DO SUL ##################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from RS using a FOR LOOP
  all_files_rs = c()
  
  for ( i in 0:length(town_code_rs) ){
    town_code_rs[i] = toString(town_code_rs[i])
    all_files_rs = c( all_files_rs, c(list.files(town_code_rs[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_rs_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_rs), get_parents_rs)
  )
  
  # Removing all NAs
  all_rs_parents <- drop_na(unique(all_rs_parents))
}
# SC      # FIXED
{
  ########################################################################
  ########################################################################
  ################ RUNNING FUNCTION ON Santa Catarina ####################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from ps using a FOR LOOP
  all_files_sc = c()
  
  for ( i in 0:length(town_code_sc) ){
    town_code_sc[i] = toString(town_code_sc[i])
    all_files_sc = c( all_files_sc, c(list.files(town_code_sc[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_scrents() on every file available from rs
  all_sc_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_sc), get_parents)
  )
  
  # Removing all NAs
  all_sc_parents <- drop_na(unique(all_sc_parents))
}
# PR      # NEED TO FIX
{
  ########################################################################
  ########################################################################
  ##################### RUNNING FUNCTION ON PARANA #######################
  ########################################################################
  ########################################################################
  
  
  # Declaring all files from PR using a FOR LOOP
  all_files_pr = c()
  
  for ( i in 0:length(town_code_pr) ){
    town_code_pr[i] = toString(town_code_pr[i])
    all_files_pr = c( all_files_pr, c(list.files(town_code_pr[i], recursive = T, full.name = T) ) )
  }
  
  # Running get_parents() on every file available from rs
  all_pr_parents <- unique(
    future_map_dfr(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_pr), get_parents)
  )
  
  # Removing all NAs
  all_pr_parents <- drop_na(unique(all_pr_parents))
}


########################################################################
########################################################################
####################### PCTG OF DATA AVAILABLE #########################
########################################################################
########################################################################


# Getting all Directories
all_dirs <- fs::dir_ls("/media/spinner/br_cand_docs/2020/txt/", type = "directory", recurse = TRUE)

# Only keep candidate directories
all_dirs <- all_dirs[grepl(".*/txt/[0-9]{1,}/[0-9]{1,}$", all_dirs)]
all_cands <- tibble(cand_code = as.character(gsub(".*/([0-9]{1,})$", "\\1", all_dirs)),
                    codigo_tse = as.integer(gsub(".*/txt/([0-9]{1,})/.*", "\\1", all_dirs)))
all_cands <- left_join(all_cands, town_codes)

# Getting total of candidates by State
table(all_cands$uf)
# AC   AL   AM   AP   BA   CE   ES   GO   MA   MG   MS   MT   PA   PB   PE   PI   PR   RJ   RN   RO   RR   RS   SC   SE   SP   TO 
# 90  341  282   92 1366  608  378  881  789 2794  293  482  662  649  660  608 1356  603  521  215   66 1352  921  257 2702  435 


# Getting pctg. of candidates with parents data available, by state
t(
  as.matrix(
    c(    
      round((length(unique(all_pi_parents$candidate)) / table(all_cands$uf)["PI"]), 3),
      round((length(unique(all_ba_parents$candidate)) / table(all_cands$uf)["BA"]), 3),
      round((length(unique(all_rn_parents$candidate)) / table(all_cands$uf)["RN"]), 3),
      round((length(unique(all_es_parents$candidate)) / table(all_cands$uf)["ES"]), 3),
      round((length(unique(all_sc_parents$candidate)) / table(all_cands$uf)["SC"]), 3),
      round((length(unique(all_rs_parents$candidate)) / table(all_cands$uf)["RS"]), 3),
      round((length(unique(all_pe_parents$candidate)) / table(all_cands$uf)["PE"]), 3),
      round((length(unique(all_pb_parents$candidate)) / table(all_cands$uf)["PB"]), 3),
      round((length(unique(all_se_parents$candidate)) / table(all_cands$uf)["SE"]), 3),
      round((length(unique(all_sp_parents$candidate)) / table(all_cands$uf)["SP"]), 3),
      round((length(unique(all_mg_parents$candidate)) / table(all_cands$uf)["MG"]), 3),
      round((length(unique(all_pa_parents$candidate)) / table(all_cands$uf)["PA"]), 3),
      round((length(unique(all_pr_parents$candidate)) / table(all_cands$uf)["PR"]), 3),
      round((length(unique(all_rj_parents$candidate)) / table(all_cands$uf)["RJ"]), 3),
      round((length(unique(all_ce_parents$candidate)) / table(all_cands$uf)["CE"]), 3),
      round((length(unique(all_ma_parents$candidate)) / table(all_cands$uf)["MA"]), 3),
      round((length(unique(all_ro_parents$candidate)) / table(all_cands$uf)["RO"]), 3),
      round((length(unique(all_go_parents$candidate)) / table(all_cands$uf)["GO"]), 3)
    )
  )
)

#         PI    BA    RN    ES    SC    RS    PE    PB    SE    SP    MG   PA   PR    RJ    CE    MA    RO    GO
# [1,] 0.967 0.963 0.956 0.921 0.876 0.874 0.873 0.814 0.813 0.774 0.722 0.63 0.35 0.277 0.225 0.219 0.233 0.116


########################################################################
########################################################################
################# TIBBLES TO CHECK FOR MISSING NAMES ###################
########################################################################
########################################################################


# Tibble with unique candidates and whether parents data is available or not (MG)
all_cands_mg <- filter(all_cands, uf == "MG")
all_cands_mg$found_parents <- ifelse(all_cands_mg$cand_code %in% all_mg_parents$candidate, TRUE, FALSE)
head(all_cands_mg)

# Removing state town_codes - no longer need them
rm(list = ls(pattern = "town_code_"), i, all_dirs)


























