########################################################################
######################## Setting up Libraries ##########################
########################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, httr, lubridate, hrbrthemes, janitor, #tesseract,
               quanteda, readtext, pdftools, stringr, RCurl)


########################################################################
########################### Setting up Dir. ############################
########################################################################

<<<<<<< HEAD
=======

setwd("/media/spinner/br_cand_docs/2020/txt")

>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5

setwd("/media/spinner/br_cand_docs/2020/txt")


########################################################################
##############################             #############################
############################## TOWN 00019  #############################
##############################             #############################
########################################################################


town_00019_01 <- readLines("00019/220000671881/15_1600384860519.txt") # PARENTS DATA FOUND!

<<<<<<< HEAD
match <- "Filiação|Filiacao|filiação|filiacao|Nome da mãe|Nome da mãe|Nome da mae|nome da mãe|nome da mae|Nome Mãe|Nome mae|Nome do pai|Nome do Pai|nome do pai|nome pai|Nome Pai|mãe|mae|Mae|Mãe|Pai|pai|filho de|Filho de|Filha de|filha de|Filho(a) de|filho(a) de"
=======
match <- "Filiação|Filiacao|filiação|filiacao|Nome da mãe|
          Nome da mãe|Nome da mae|nome da mãe|nome da mae|Nome Mãe|Nome mae|
          Nome do pai|Nome do Pai|nome do pai|nome pai|Nome Pai|
          mãe|mae|Mae|Mãe|Pai|pai|
          filho de|Filho de|Filha de|filha de|Filho(a) de|filho(a) de"
>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5

filiacao_line <- grepl(match, town_00019_01)

town_00019_01[filiacao_line == TRUE]

mae <- gsub(pattern = ".*: - (.*)$", x = town_00019_01[filiacao_line == TRUE],
            replacement = "\\1")

pai <- gsub(pattern = ".*- (.*)$", x = town_00019_01[which(filiacao_line == TRUE) + 1],
            replacement = "\\1")

town_00019_01 <- matrix(c("Nome Mae", "Nome Pai", mae, pai), 2,2)

#       [,1]       [,2]                        
# [1,] "Nome Mae" "ROSA LIMA DE OLIVEIRA"     
# [2,] "Nome Pai" "RAIMUNDO PINTO DE OLIVEIRA"






########################################################################
##############################            ##############################
############################## TOWN 00035 ##############################
##############################            ##############################
########################################################################


town_00035_04 <- readLines("00035/220001081445/14_1601043802048.txt") # PARENTS DATA FOUND!
town_00035_05 <- readLines("00035/220001082524/15_1601047417682.txt") # PARENTS DATA FOUND!



########################## town_00035_04  ##############################

filiacao_line <- grepl(match, town_00035_04)

filiacao <- unique(town_00035_04[filiacao_line == TRUE])
filiacao <- gsub("\\s+"," ",filiacao)

parents <- unique(gsub(pattern = ".* :(.*)$", x = filiacao,
                       replacement = "\\1"))

town_00035_04 <- matrix(c("Nome Mae", "Nome Pai", parents[2], parents[1]), 2,2)

#       [,1]       [,2]                             
# [1,] "Nome Mae" "Alcimira de Souza Barbosa Alves"
# [2,] "Nome Pai" "Adão José Alves"  



<<<<<<< HEAD
=======

>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5
########################## town_00035_05  ##############################

filiacao_line <- grepl(match, town_00035_05)

filiacao <- unique(town_00035_05[filiacao_line == TRUE])
filiacao <- gsub("\\s+"," ",filiacao)

parents <- unique(gsub(pattern = ".* :(.*)$", x = filiacao,
                       replacement = "\\1"))

town_00035_05 <- matrix(c("Nome Mae", "Nome Pai", parents[2], parents[1]), 2, 2)

#       [,1]       [,2]                           
# [1,] "Nome Mae" "Elite Feitosa Brasil do Carmo"
# [2,] "Nome Pai" "Valter José do Carmo"   

<<<<<<< HEAD




########################################################################
###############################            #############################
############################### TOWN 68390 #############################
###############################            #############################
########################################################################


town_68390_02 <- readLines("68390/250001160406/13_1601006735581.txt")  # PARENTS DATA FOUND! 

########################## town_68390_02  ##############################

filiacao_line <- grepl(match, town_68390_02)

filiacao <- unique(town_68390_02[filiacao_line == TRUE])
filiacao

# [1] "natural de Mirassol - SP, filho de SEBASTIAO JOSÉ DA SILVA FILHO e ANA DE JESUS DA SILVA,"

parents <- gsub(pattern = "(\\S+)\\s.*filho de |filha de ", x = filiacao, replacement = "")
parents

# [1] "SEBASTIAO JOSÉ DA SILVA FILHO e ANA DE JESUS DA SILVA,"

parents <- str_split(parents, " e ")
parents <- c(parents[[1]])

parents
# [1] "SEBASTIAO JOSÉ DA SILVA FILHO" "ANA DE JESUS DA SILVA,"       

=======



>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5


########################################################################
###############################            #############################
############################### TOWN 80896 #############################
###############################            #############################
########################################################################


town_80896_02 <- readLines("80896/240000981115/14_1600524737452.txt")  # PARENTS DATA FOUND! 
town_80896_03 <- readLines("80896/240000982188/14_1600381085383.txt")  # PARENTS DATA FOUND! 
town_80896_04 <- readLines("80896/240000982783/14_1600885691769.txt")  # PARENTS DATA FOUND! 
town_80896_06 <- readLines("80896/240000987167/14_1600964947198.txt")  # PARENTS DATA FOUND! 
town_80896_07 <- readLines("80896/240001158893/14_1601044521753.txt")  # PARENTS DATA FOUND!



########################## town_80896_02  ##############################

filiacao_line <- grepl(match, town_80896_02)

filiacao <- unique(town_80896_02[filiacao_line == TRUE])

# filiacao <- gsub("\\s+"," ",filiacao)

<<<<<<< HEAD
parents <- gsub(pattern = ".*: (.*)$", x = filiacao,
                replacement = "\\1")

parents

=======
parents <- gsub(pattern = ".* :(.*)$", x = filiacao,
                replacement = "\\1")

>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5


########################## town_80896_03  ##############################

filiacao_line <- grepl(match, town_80896_03)

filiacao <- unique(town_80896_03[filiacao_line == TRUE])

# filiacao <- gsub("\\s+"," ",filiacao)

<<<<<<< HEAD
parents <- gsub(pattern = ".*: (.*)$", x = filiacao,
                replacement = "\\1")

town_80896_03 <- matrix(c("Nome Mae", "Nome Pai", parents[1], parents[2]), 2, 2)

#       [,1]       [,2]                         
# [1,] "Nome Mae" "CARLA ROSANE PRATES PEDROSO"
# [2,] "Nome Pai" "FERNANDO ZANATTA FILHO"

########################## town_80896_04  ##############################

filiacao_line <- grepl(match, town_80896_04)

filiacao <- unique(town_80896_04[filiacao_line == TRUE])

# filiacao <- gsub("\\s+"," ",filiacao)

parents <- gsub(pattern = ".*: (.*)$", x = filiacao,
                replacement = "\\1")

town_80896_04 <- matrix(c("Nome Mae", "Nome Pai", parents[1], parents[2]), 2, 2)

#       [,1]       [,2]                    
# [1,] "Nome Mae" "Maria Zenaide da Silva"
# [2,] "Nome Pai" "Ageu Antônio da Silva" 







=======
parents <- gsub(pattern = ".* :(.*)$", x = filiacao,
                replacement = "\\1")



########################## town_80896_04  ##############################





########################################################################
###############################            #############################
############################### TOWN 68390 #############################
###############################            #############################
########################################################################


town_68390_02 <- readLines("68390/250001160406/13_1601006735581.txt")  # PARENTS DATA FOUND! 


########################## 68390  ##############################

filiacao_line <- grepl(match, town_68390_02)

filiacao <- unique(town_68390_02[filiacao_line == TRUE])

# filiacao <- gsub("\\s+"," ",filiacao)

parents <- gsub(pattern = ".* :(.*)$", x = filiacao,
                replacement = "\\1")
>>>>>>> dadda76fb575883f72cfe2e4e49432acb85a5dd5






















