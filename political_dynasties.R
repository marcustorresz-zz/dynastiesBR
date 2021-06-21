########################################################################
######################## Setting up Libraries ##########################
########################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, #tesseract,
               quanteda, readtext, pdftools, stringr, RCurl, readxl)


########################################################################
########################### Setting up Dir. ############################
########################################################################

setwd("/media/spinner/br_cand_docs/2020/txt")


# Reading Town Code CSVs from GitHub into R
town_codes <- read.csv("https://raw.githubusercontent.com/marcustorresz/dynastiesBR/master/town_codes.csv")


########################################################################
######################## Finding State Codes ###########################
########################################################################

# RO
town_code_ro <- sort(town_codes$codigo_tse[town_codes$uf == "RO"])

# BA
town_code_ba <- sort(town_codes$codigo_tse[town_codes$uf == "BA"])

# SP
town_code_sp <- sort(town_codes$codigo_tse[town_codes$uf == "SP"])

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
###################### Declaring Main Function #########################
########################################################################

## This function extracts parents from candidate PDFs from RO
get_parents <- function(file) {
  
  txt <- readLines(file)
  
  if(sum(grepl("[Nn]ome [Mm][ãa]e", txt)) > 0 & sum(grepl("[Nn]ome [Pp]ai", txt)) > 0) {
    mae <- gsub(".*:(.*)$", txt[grep("[Nn]ome [Mm][ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub(".*[Nn]ome [Pp]ai(.*)$", txt[grep("[Nn]ome [Pp]ai", txt)], replacement = "\\1")[1]
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai))
    result$pai = sub("[:,]", "", result$pai)
    result$pai = sub("[.,]", "", result$pai)
    result$pai = str_trim(result$pai)
    result$mae = sub("[:,]", "", result$mae)
    result$mae = sub("[.,]", "", result$mae)
    result$mae = str_trim(result$mae)
  }
  
  if(sum(grepl("[Ff][il]lia[çcg][ãa]o", txt)) > 0){
    mae <- gsub(".*- (.*)$", txt[grep("Filiação", txt)], replacement = "\\1")[1]
    pai <- gsub(".*- (.*)$", txt[grep("Filiação", txt) + 1], replacement = "\\1")[1]
    result <- tibble(mae = mae, pai = pai)
    result$pai = sub("[:,]", "", result$pai)
    result$pai = sub("[.,]", "", result$pai)
    result$pai = str_trim(result$pai)
    result$mae = sub("[:,]", "", result$mae)
    result$mae = sub("[.,]", "", result$mae)
    result$mae = str_trim(result$mae)
  }
  
  if(sum(grepl("Nome d[ao] M[ãa]e", txt)) > 0 & sum(grepl("Nome d[ao] Pai", txt)) > 0) {
    mae <- gsub("Nome d[ao] M[ãa]e (.*)$", txt[grep("Nome d[ao] M[ãa]e", txt)], replacement = "\\1")[1]
    pai <- gsub("Nome d[ao] Pai (.*)$", txt[grep("Nome d[ao] Pai", txt)], replacement = "\\1")[1]
    mae <- str_remove(mae, ":")
    pai <- str_remove(pai, ":")
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai))
    result$pai = sub("[:,]", "", result$pai)
    result$pai = sub("[.,]", "", result$pai)
    result$pai = str_trim(result$pai)
    result$mae = sub("[:,]", "", result$mae)
    result$mae = sub("[.,]", "", result$mae)
    result$mae = str_trim(result$mae)
  }
  
  if(sum(grepl("MÃE", txt)) > 0 & sum(grepl("PAI", txt)) == 0){
    mae <- gsub(".*:(.*)$", txt[grep("MÃE", txt)], replacement = "\\1")[1]
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai))
    result$pai = sub("[:,]", "", result$pai)
    result$pai = sub("[.,]", "", result$pai)
    result$pai = str_trim(result$pai)
    result$mae = sub("[:,]", "", result$mae)
    result$mae = sub("[.,]", "", result$mae)
    result$mae = str_trim(result$mae)
  }
  
  # if no parents' names available, return NAs for both.
  if( !exists("result") ){
    mae <- NA
    pai <- NA
    result <- tibble(mae = str_trim(mae), pai = str_trim(pai))
    #result$pai = sub("[:,]", "", result$pai)
    #result$pai = sub("[.,]", "", result$pai)
    #result$pai = str_trim(result$pai)
    #result$mae = sub("[:,]", "", result$mae)
    #result$mae = sub("[.,]", "", result$mae)
    #result$mae = str_trim(result$mae)
  }
  
  unique(result)
}


## Run it one file
get_parents(paste0("/media/spinner/br_cand_docs/2020/txt/", files[3]))

## Run it on all files
RO_00019_parents <- map_df(paste0("/media/spinner/br_cand_docs/2020/txt/", files), get_parents)


########################################################################
######## Loading ALL Files from Rondonia to see what happens ###########
########################################################################


# Saving all files from all 52 folders within Rondonia

# all_files_rondonia = c()
# 
# for ( i in 0:length(town_code_ro) ){
#   town_code_ro[i] = toString(town_code_ro[i])
#   all_files_rondonia = c( all_files_rondonia, c(list.files(town_code_ro[i], recursive = T, full.name = T) ) )
# }

all_files_rondonia <-   c( list.files("00019", recursive = T, full.name = T),
                           list.files("00035", recursive = T, full.name = T),
                           list.files("00051", recursive = T, full.name = T),
                           list.files("00078", recursive = T, full.name = T),
                           list.files("00094", recursive = T, full.name = T),
                           list.files("00116", recursive = T, full.name = T),
                           list.files("00132", recursive = T, full.name = T),
                           list.files("00159", recursive = T, full.name = T),
                           list.files("00175", recursive = T, full.name = T),
                           list.files("00191", recursive = T, full.name = T),
                           list.files("00213", recursive = T, full.name = T),
                           list.files("00230", recursive = T, full.name = T),
                           list.files("00256", recursive = T, full.name = T),
                           list.files("00272", recursive = T, full.name = T),
                           list.files("00299", recursive = T, full.name = T),
                           list.files("00310", recursive = T, full.name = T),
                           list.files("00337", recursive = T, full.name = T),
                           list.files("00353", recursive = T, full.name = T),
                           list.files("00370", recursive = T, full.name = T),
                           list.files("00396", recursive = T, full.name = T),
                           list.files("00418", recursive = T, full.name = T),
                           list.files("00434", recursive = T, full.name = T),
                           list.files("00450", recursive = T, full.name = T),
                           list.files("00477", recursive = T, full.name = T),
                           list.files("00493", recursive = T, full.name = T),
                           list.files("00515", recursive = T, full.name = T),
                           list.files("00531", recursive = T, full.name = T),
                           list.files("00558", recursive = T, full.name = T),
                           list.files("00566", recursive = T, full.name = T),
                           list.files("00574", recursive = T, full.name = T),
                           list.files("00582", recursive = T, full.name = T),
                           list.files("00590", recursive = T, full.name = T),
                           list.files("00604", recursive = T, full.name = T),
                           list.files("00612", recursive = T, full.name = T),
                           list.files("00620", recursive = T, full.name = T),
                           list.files("00639", recursive = T, full.name = T),
                           list.files("00647", recursive = T, full.name = T),
                           list.files("00655", recursive = T, full.name = T),
                           list.files("00663", recursive = T, full.name = T),
                           list.files("00671", recursive = T, full.name = T),
                           list.files("00680", recursive = T, full.name = T),
                           list.files("00698", recursive = T, full.name = T),
                           list.files("00701", recursive = T, full.name = T),
                           list.files("00710", recursive = T, full.name = T),
                           list.files("00728", recursive = T, full.name = T),
                           list.files("00736", recursive = T, full.name = T),
                           list.files("00744", recursive = T, full.name = T),
                           list.files("00752", recursive = T, full.name = T),
                           list.files("00779", recursive = T, full.name = T),
                           list.files("00787", recursive = T, full.name = T),
                           list.files("00795", recursive = T, full.name = T),
                           list.files("00809", recursive = T, full.name = T)
)


# Running get_parents() on every file available from Rondonia
all_rondonia_parents <- unique(
  map_df(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_rondonia), get_parents
  )
)

head(all_rondonia_parents, 9)


# # A tibble: 9 x 2
# mae                                  pai                       
# <chr>                                <chr>                     
# 1 NA                                 NA                        
# 2 ROSA LIMA DE OLIVEIRA              RAIMUNDO PINTO DE OLIVEIRA
# 3 Alcimira de Souza Barbosa Alves    Adão José Alves           
# 4 Elite Feitosa Brasil do Carmo      Valter José do Carmo      
# 5 Glaucia Mendes da Silva Farias     Cesar Augusto Nunes Farias
# 6 Gláucia Mendes da Silva Farias     César Augusto Nunes Farias
# 7 Nely Rigo Pinto                    José Pinto                
# 8 JOSELITA DOS ANJOS PINTO           JOAO BISPO PINTO          
# 9 Lucineia Pereira Gonçalves Rezende Paulo Ferreira Rezende



########################################################################
########################################################################
############### GETTING ALL DATA FROM ALL STATES #######################
########################################################################
########################################################################


all_parents_all_states <- list.files(recursive = T, full.name = T)

# This is running our function on A LOT of files, so will take some time.
all_parents <- unique(
  map_df(
    all_parents_all_states, get_parents
  )
)

head(all_parents)
tail(all_parents)


########################################################################
########################################################################
################## GETTING ALL DATA FROM BAHIA #########################
########################################################################
########################################################################


all_files_bahia = c()

for ( i in 0:length(town_code_ba) ){
  town_code_ba[i] = toString(town_code_ba[i])
  all_files_bahia = c( all_files_bahia, c(list.files(town_code_ba[i], recursive = T, full.name = T) ) )
}


head(all_files_bahia)
tail(all_files_bahia)

# Running get_parents() on every file available from Bahia
all_bahia_parents <- unique(
  map_df(paste0("/media/spinner/br_cand_docs/2020/txt/", all_files_bahia), get_parents
  )
)

head(all_bahia_parents, 9)
# 
# # A tibble: 9 x 2
# mae                                     pai                                       
# <chr>                                   <chr>                                     
# 1 NA                                    NA                                        
# 2 Filiação 1 WASHINGTON DUQUE DE RANGEL Filiação 2 MARIA BERNADETH REBOUÇAS RANGEL
# 3 Filiação 1 MIRIAM SILVA SOUZA BORGES  Filiação 2 ONDUMAR FERREIRA BORGES        
# 4 Filiação 1 Jose Joaquim De Oliveira   Filiação 2 Odete Alves De Oliveira        
# 5 Filiação 1 JOSE LIMA ALMEIDA          Filiação 2 GISELIA TORRES DE ALMEIDA      
# 6 Filiação 1 ZULMIRA VENTURINI CHECON   Filiação 2 NELSON CHECON                  
# 7 Filiação 1 Adelice Santos Joaquim     Filiação 2 José Joaquim Filho             
# 8 Filiação 1 Alzira Batista de Oliveira Filiação 2 Antonio Batista dos Santos     
# 9 Filiação 1 PEDRO PIRES NOGUEIRA       Filiação 2 NILVANDA FERREIRA DE SOUZA     

tail(all_bahia_parents, 9)
# 
# # A tibble: 9 x 2
# mae                                        pai                                  
# <chr>                                      <chr>                                
# 1 Filiação 1 SILEUZA SANTOS SALES RIOS     Filiação 2 DERALDO SALES RIOS        
# 2 Filiação 1 Josias de Souza Rios          Filiação 2 Judite dos Santos Rios    
# 3 Filiação 1 Oldaque de Souza Rios         Filiação 2 Anatilde de Oliveira Rios 
# 4 Filiação 1 MARINALVA ALEXANDRE BARBOSA   Filiação 2                           
# 5 Filiação 1 tereza joana do nascimento    Filiação 2 luis pedro do nascimento  
# 6 Filiação 1 FRANCISCO ANTONIO BENTO       Filiação 2 CLEUSA GOMES SAMPAIO BENTO
# 7 Filiação 1 MARIA PEREIRA RIBEIRO         Filiação 2 CELSO FRANCISCO RIBEIRO   
# 8 Filiação 1 CLEONISSE CRISOSTOMO DA SILVA Filiação 2 ANTÔNIO ALDINO SÁ TELES   
# 9 Filiação 1 CLEONISSE CRISÓSTOMO DA SILVA Filiação 2 ANTÔNIO ALDINO SÁ TELES   






