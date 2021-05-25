############### Text Analysis - Hidalgo ################
rm(list = ls())


if (!require('tesseract')) install.packages('tesseract'); library('tesseract')
if (!require('quanteda')) install.packages('quanteda'); library('quanteda')
if (!require('readtext')) install.packages('readtext'); library('readtext')
if (!require('pdftools')) install.packages('pdftools'); library('pdftools')
if (!require('stringr')) install.packages('stringr'); library('stringr') # para o str_split




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
Candidato <- grep(pattern = "Eleitor", x = text)
Filiacao <- grep(pattern = "Filiação:", x = text)



#Aqui, estou pedindo a linha do padrão interessado, mais a próxima linha (+1).
#Note que você pode brincar com essa função, escolhendo mais ou menos casos


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





####################### PDFTOOLS ##############




# Isso na verdade não é tão importante agora, já que não queremos saber quais são as palavras que estão sendo focadas
# Caso você tenha interesse nisso, é só retirar os comentários

#results <- tesseract::ocr_data("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PE/23019/426/candidatos/286275/12_1600293102187.pdf", engine = eng)
#doc <- corpus(text)
#head(summary(doc))

#limpo <- dfm(doc, remove = stopwords("pt"), remove_punct = TRUE,
#              remove_numbers = TRUE, remove_symbols = TRUE) %>%
#  dfm_wordstem(language = "portuguese")
#limpo



############################################# TRIBUNAL SUPERIOR ELEITORAL #######################

### Extract names :) (Finally)
# Essa extração está usando os dados de Hidalgo. Depois vou tentar com os meus de fato
#Using his code line

NadaConsta <- pdftools::pdf_text("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PE/23019/426/candidatos/286275/12_1600293102187.pdf") %>% 
  str_split(pattern ="\n") %>% unlist()

#O grep serve para que se consiga encontrar um match para um argumento
#Nesse caso, o que fiz foi colocar a Filiação como padrão para encontrar os casos que estou interessado

Filia <- grep(pattern = "Filiação: -", x = NadaConsta)
Candidato <- grep(pattern = "Eleitor", x = NadaConsta)


#Selecionar

#Aqui, estou pedindo a linha do padrão interessado, mais a próxima linha (+1).
#Note que você pode brincar com essa função, escolhendo mais ou menos casos

NadaConsta[Filia:(Filia + 1)]
NadaConsta[Candidato[1]]


# Selecionar a Mãe
gsub(pattern = "Eleitor",
     replacement = "\\1", x = NadaConsta[Filia])


# Selecionar a Mãe
gsub(pattern = "Filiação: -",
     replacement = "\\1", x = NadaConsta[Filia])


#Selecionar o Pai
gsub(pattern = "Filiação: -",
                    replacement = "\\1", x = NadaConsta[Filia+1])



############### Tribunal de Justiça (AM) ##################

#Teste com modelo diferente



TJAM <- pdftools::pdf_text("https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/AM/02550/426/candidatos/467946/13_1600872244282.pdf") %>% 
  str_split (pattern ="\n") %>% unlist()

TJAM
#O grep serve para que se consiga encontrar um match para um argumento
#Nesse caso, o que fiz foi colocar a Filiação como padrão para encontrar os casos que estou interessado


Filia <- grep(pattern = "filho de", x = TJAM)


#Selecionar

#Aqui, estou pedindo a linha do padrão interessado, mais a próxima linha (+1).
#Note que você pode brincar com essa função, escolhendo mais ou menos casos
# Nesse caso, caso queira, pode-se pegar outras inforações
TJAM[Filia:(Filia+2)]



# Seleciona o candidato (Não está dando muito certo)
gsub(pattern = "",
     replacement = "\\", x = TJAM[Filia+1])

??gsub()
# Selecionar a Mãe
gsub(pattern = "filho de ",
     replacement = "\\1", x = TJAM[Filia+2])


#Selecionar o Pai
gsub(pattern = "em nome de:",
     replacement = "\\1", x = TJAM[Filia+2])


######## O que eu pensei 
# Tentar fazer um algoritmo que consiga selectionar os docs, mas apenas pegue os do TSE. Desse modo, conseguiríamos as filiações de maneira sistemática
# Problema: é possível que o candidato não possua esse documento - Exemplo: https://divulgacandcontas.tse.jus.br/divulga/#/candidato/2020/2030402020/06017/30000817792
# Nos casos que só tem mãe, é porque só tem mãe mesmo ou é parte da estrutura? Exemplo: https://divulgacandcontas.tse.jus.br/candidaturas/oficial/2020/PA/04278/426/candidatos/544580/13_1600806890390.pdf
# Candidatos mais propensos a ganhar colocam mais documentos?


