############### Text Analysis - Hidalgo ################
rm(list = ls())


if (!require('tesseract')) install.packages('tesseract'); library('tesseract')
if (!require('quanteda')) install.packages('quanteda'); library('quanteda')
if (!require('readtext')) install.packages('readtext'); library('readtext')
if (!require('pdftools')) install.packages('pdftools'); library('pdftools')
if (!require('stringr')) install.packages('stringr'); library('stringr') # para o str_split
if (!require('RCurl')) install.packages('RCulr'); library('RCurl') 


################################## Set Wd ###################
setwd("C:/Users/marvi/OneDrive - Universidade Federal de Pernambuco/UFPE/DOUTORADO/dynastiesBR/files")
######################## TESSERACT #############


#Usando o tesseact para baixar
tesseract_download('por')
por <- tesseract("por")
eng <- tesseract("eng")


#O problema do OCR é que ele baixa o doc, enquanto que o pdf não.
text <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PE/23019/426/candidatos/286275/12_1600293102187.pdf", engine = por)%>% 
  str_split (pattern ="\n") %>% unlist()
cat(text)


# Essa extração está usando os meus dados
#O grep serve para que se consiga encontrar um match para um argumento
#Nesse caso, o que fiz foi colocar a Filiação como padrão para encontrar os casos que estou interessado


# A vantagem de usar o meu frente ao de Hidalgo é a limpeza dos dados. Contudo, demora um pouco mais

text

Orgao <- grep(pattern = "TRIBUNAL SUPERIOR ELEITORAL", x= text)
Candidato <- grep(pattern = "Eleitor", x = text)
Filiacao <- grep(pattern = "Filiação:", x = text)



#Aqui, estou pedindo a linha do padrão interessado, mais a próxima linha (+1).
#Note que você pode brincar com essa função, escolhendo mais ou menos casos


#Selecionar o órgão
gsub(pattern = "JUSTIÇA ELEITORAL",
     replacement = "\\1", x = text[Orgao[1]])

# Selecionar o Eleitor. 
# Usar text[Candidato[1]], ele somente retorna o primeiro
gsub(pattern = "Eleitor(a)",
     replacement = "\\1", x = text[Candidato[1]])


# Selecionar a Mãe
gsub(pattern = "Filiação: -",
     replacement = "\\1", x = text[Filiacao])


#Selecionar o Pai
gsub(pattern = "Filiação: -",
     replacement = "\\1", x = text[Filiacao + 1])


########################    filho(a) de  (TJCE)  ############################################

filho_a_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/CE/13897/2030402020/60000737869/pje-0d6673ab-Certid%C3%A3o%20criminal%20da%20Justi%C3%A7a%20Estadual%20de%201%C2%BA%20grau.pdf", engine = por)%>% 
  str_split (pattern ="\n") %>% unlist()
cat(filho_a_de)
filho_a_de

Orgao <- grep(pattern = "ESTADO", x= filho_a_de)
Candidato <- grep(pattern = "em nome", x = filho_a_de)
#Filiacao <- grep(pattern = "filho (a)", x = filho_a_de) # A variável candidato já faz isso


#Selecionar o órgão

gsub(pattern = "",
     replacement = "\\1", x = filho_a_de[Orgao[1]])

# Selecionar o Candidato
# Usar text[Candidato[1]], ele somente retorna o primeiro
gsub(pattern = "",
     replacement = "\\1", x = filho_a_de[Candidato + 1])


# Selecionar os pais
gsub(pattern = "",
     replacement = "\\1", x = filho_a_de[Candidato + 2])





########################    filho de   ############################################


#####################################(TJBA) 1a instância########

filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/BA/38490/426/candidatos/389567/13_1600444133409.pdf", engine = por)%>% 
str_split (pattern ="\n") %>% unlist()
cat(filho_de)
filho_de

Orgao <- grep(pattern = "PODER JUDICIARIO", x= filho_de)
Candidato <- grep(pattern = "em nome de", x = filho_de)
#Filiacao <- grep(pattern = "filho (a)", x = filho_a_de) # A variável candidato já faz isso


#Selecionar o órgão

gsub(pattern = "",
     replacement = "\\1", x = filho_de[Orgao + 1])

# Selecionar o Candidato
# Usar text[Candidato[1]], ele somente retorna o primeiro
gsub(pattern = "",
     replacement = "\\1", x = filho_de[Candidato + 1])


# Selecionar os pais
 gsub(pattern = "",
     replacement = "\\1", x = filho_de[Candidato + 2])

#FALTA SEPARAR OS PAIS

 
############ TJAC ########
 
 
 
 filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/AC/01120/426/candidatos/10000854328/pje-c18bf358-Certid%C3%A3o%20criminal%20da%20Justi%C3%A7a%20Estadual%20de%202%C2%BA%20grau.pdf", engine = por) %>% 
 str_split (pattern ="\n") %>% unlist()
 cat(filho_de)
 filho_de
 
 Orgao <- grep(pattern = "PODER JUDICIARIO", x= filho_de)
 Candidato <- grep(pattern = "em nome de", x = filho_de)
 Filiacao <- grep(pattern = "filho de", x = filho_de) # A variável candidato já faz isso
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Orgao + 1])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato + 1])
 
 
 # Selecionar os pais. Solta o mesmo.
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Filiacao[1]])
 
 
 
 ############## TJ PA #########################
 
 filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PA/04278/426/candidatos/544580/13_1600806890390.pdf", engine = por) %>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filho_de)
 filho_de
 
 Orgao <- grep(pattern = "TRIBUNAL", x= filho_de)
 Candidato <- grep(pattern = "em nome de", x = filho_de)
 Filiacao <- grep(pattern = "residente", x = filho_de) # A variável candidato já faz isso
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Orgao[1]])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato])
 
 
 # Selecionar os pais. O problema nesse caso é que o nome da pessoa está em duas linhas
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Filiacao[1]])
 
 
 
 
 
 
 ############### TJ AL ( 1 a instância)
 
 
 
 filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/AL/27855/426/candidatos/508138/14_1600630854237.pdf", engine = por) %>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filho_de)
 filho_de
 
 Orgao <- grep(pattern = "TRIBUNAL", x= filho_de)
 Candidato <- grep(pattern = "em nome de", x = filho_de)
 Filiacao <- grep(pattern = "filho de", x = filho_de) # A variável candidato já faz isso
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Orgao[1]])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro. Tem uma linha acima que também tem o nome do candidato
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato + 1])
 
 
 # Selecionar os pais. O problema nesse caso é que o nome da pessoa está em duas linhas
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Filiacao[1]])
 
 
 
###################################################################### 
 
 

 
 ################# TJ RN ##
 ## Tecncamente esse doc é parecido com o TJBA
 
 
 filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/RN/17612/426/candidatos/673578/13_1601077067571.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filho_de)
 filho_de
 
 Orgao <- grep(pattern = "PODER JUDICIARIO", x= filho_de)
 Candidato <- grep(pattern = "em nome de", x = filho_de)
 #Filiacao <- grep(pattern = "filho (a)", x = filho_a_de) # A variável candidato já faz isso
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Orgao + 1])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato + 1])
 
 
 ## Selecionar os pais. NÃO FUNCIONA
 #gsub(pattern = "",
 #     replacement = "\\1", x = filho_de[Candidato + 2])
 
 #FALTA SEPARAR OS PAIS
 
 ####################################################
 
 
 ################# TJ CE ##################
 ## Essa parte serve tanto para a 1 e 2 instancia
 ## Tecncamente esse doc é parecido com o TJBA
 
 
 filho_de <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/CE/15598/426/candidatos/447440/14_1600484061213.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filho_de)
 filho_de
 
 Orgao <- grep(pattern = "PODER JUDICIARIO", x= filho_de)
 Candidato <- grep(pattern = "em nome", x = filho_de)
 Filiacao <- grep(pattern = "filho (a)", x = filho_a_de) # A variável candidato já faz isso
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Orgao+1])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato+1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "",
      replacement = "\\1", x = filho_de[Candidato + 2])
 
 #FALTA SEPARAR OS PAIS
 
 
#####################Filiação 1: (NOME) \n Filiação 2: (NOME) #######################################################
 
 ## TJRN 2a Instancia ##
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/RN/17612/426/candidatos/673578/14_1600886126435.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "PODER JUDICIARIO", x= filia)
 Candidato <- grep(pattern = "em nome de", x = filia)
 Filiacao1 <- grep(pattern = "Filiação 1:", x = filia) 
 Filiacao2 <- grep(pattern = "Filiação 2:", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "",
      replacement = "\\1", x = filia[Filiacao2])
 
 
 
 #### TJBA 2a 

 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/BA/38490/426/candidatos/389567/14_1600444133933.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "PODER JUDICIARIO", x= filia)
 Candidato <- grep(pattern = "Nome:", x = filia)
 Filiacao1 <- grep(pattern = "Filiação 1:", x = filia) 
 Filiacao2 <- grep(pattern = "Filiação 2:", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "Nome:",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Filiação 1:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Filiação 2:",
      replacement = "\\1", x = filia[Filiacao2])
 
 
 #### TJ MA ##
 
 
 
 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/MA/09210/426/candidatos/500873/14_1600747134001.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "PODER JUDICIÁRIO", x= filia)
 Candidato <- grep(pattern = "NOME:", x = filia)
 Filiacao <- grep(pattern = "FILIAÇÃO:", x = filia) 

 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "NOME:",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "FILIAÇÃO",
      replacement = "\\1", x = filia[Filiacao])
 

 
 ## TJPB 1a e 2a
 

 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PB/20516/426/candidatos/408253/13_1600451457068.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "JUSTIÇA", x= filia)
 Candidato <- grep(pattern = "Nome:", x = filia)
 Filiacao1 <- grep(pattern = "Nome da mãe:", x = filia) 
 Filiacao2 <- grep(pattern = "Nome do pai", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "Nome:",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome da mãe:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome do pai:",
      replacement = "\\1", x = filia[Filiacao2])
 
 
 
 
 ## TJPE 1a e 2a ##
 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PE/25313/426/candidatos/447583/13_1600480334420.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "PODER JUDICIÁRIO", x= filia)
 Candidato <- grep(pattern = "Nome:", x = filia)
 Filiacao1 <- grep(pattern = "Nome da Mãe:", x = filia) 
 Filiacao2 <- grep(pattern = "Nome do Pai", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "Nome:",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome da Mãe:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome do Pai:",
      replacement = "\\1", x = filia[Filiacao2])
 
 
 
 #### TJSE ####
 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/SE/31054/426/candidatos/408038/15_1600440754684.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "COMARCA", x= filia)
 Candidato <- grep(pattern = "Nome:", x = filia)
 Filiacao1 <- grep(pattern = "Nome da Mãe:", x = filia) 
 Filiacao2 <- grep(pattern = "Nome do Pai", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "Nome:",
      replacement = "\\1", x = filia[Candidato])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome da Mãe:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome do Pai:",
      replacement = "\\1", x = filia[Filiacao2]) 
 
 
 
 
 
 ### TJES ####
 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/ES/57037/426/candidatos/491153/12_1600602984953.pdf", engine = por)%>% 
   str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "TRIBUNAL DE JUSTIÇA", x= filia)
 Candidato <- grep(pattern = "Nome:", x = filia)
 Filiacao1 <- grep(pattern = "Nome da Mãe:", x = filia) 
 Filiacao2 <- grep(pattern = "Nome do Pai", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "Nome:",
      replacement = "\\1", x = filia[Candidato])
 
 ############### AJUSTAR ESSA ESTRUTURA
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome da Mãe:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "Nome do Pai:",
      replacement = "\\1", x = filia[Filiacao2]) 
 
 
 #### TJPI ##
 
 
 
 filia <- tesseract::ocr("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PI/12190/426/candidatos/770776/13_1600992214333.pdf", engine = por)%>% 
    str_split (pattern ="\n") %>% unlist()
 cat(filia)
 filia
 
 Orgao <- grep(pattern = "PODER JUDICI", x= filia)
 Candidato <- grep(pattern = "NOME:", x = filia)
 Filiacao1 <- grep(pattern = "MÃE:", x = filia) 
 Filiacao2 <- grep(pattern = "PAI", x = filia) 
 
 
 #Selecionar o órgão
 
 gsub(pattern = "",
      replacement = "\\1", x = filia[Orgao])
 
 # Selecionar o Candidato
 # Usar text[Candidato[1]], ele somente retorna o primeiro
 gsub(pattern = "NOME:",
      replacement = "\\1", x = filia[Candidato])
 
 ############### AJUSTAR ESSA ESTRUTURA
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "MÃE:",
      replacement = "\\1", x = filia[Filiacao1])
 
 
 ## Selecionar os pais. Ainda preocupado com não conseguir pegar duas linhas
 gsub(pattern = "PAI:",
      replacement = "\\1", x = filia[Filiacao2]) 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
########################### GOOGLE DRIVE TEST #####################


if (!require('googledrive')) install.packages('googledrive'); library('tesseract')

url <- "https://www.dropbox.com/s/ete9t35itacvxif/br_cand_docs_2020.tar.gz?dl=0&file_subpath=%2Ftxt"

drive_browse(url)
drive_find(url)
