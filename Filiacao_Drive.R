############### Text Analysis - Hidalgo ################
rm(list = ls())


if (!require('tesseract')) install.packages('tesseract'); library('tesseract')
if (!require('quanteda')) install.packages('quanteda'); library('quanteda')
if (!require('readtext')) install.packages('readtext'); library('readtext')
if (!require('pdftools')) install.packages('pdftools'); library('pdftools')
if (!require('stringr')) install.packages('stringr'); library('stringr') # para o str_split
if (!require('RCurl')) install.packages('RCulr'); library('RCurl') 


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



########################### GOOGLE DRIVE TEST #####################


