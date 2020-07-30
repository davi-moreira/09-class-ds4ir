# ---title:"DS4IR"
# subtitle:"Text as Data"
# author:-Professor Davi Moreira
# - Professor Rafael Magalhães
# date:"`r format(Sys.time(), '%d-%m-%Y')`"
# output:revealjs::revealjs_presentation:theme:simple
# highlight:haddock
# transition:slide
# center:true
# css:stylesheet.css
# reveal_options:controls:false  # Desativar botões de navegação no slide
# mouseWheel:true # Passar slides com o mouse---```{
# r echo = FALSE, message = FALSE, warning = FALSE, results = 'hide'
# }
# # pacotes necessários
library(xml2)
library(httr)
library(rvest)
library(tesseract)
library(stringi)
library(stringr)
library(pdftools)
library(scales)
library(topicmodels)
library(readtext)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.corpora)
library(tidytext)
library(tidyverse)
library(here)

# ```
# 
# ## Programa
# 1. Text as data:o texto como dado
# 2. R e o Processamento de Linguagem Natural
# 3. Strings no R
# 4. Obtenção de conteúdo
# 5. Processamento dos dados
# 6. Mineração e estatísticas básicas
# 7. Escalonamento
# 8. Classificação
# 
# ## Impeachment: análise de votos proferidos
# 
# < center >
#   ![impeachment](images / impeachment - dilma - ebc.jpg)
# 
# <  / center >
#   
#   ## Text as data: o texto como dado
#   
#   A análise de conteúdo possui grande relevância para as ciências sociais. Contudo,
# sua abordagem manual sempre limitou o volume de documentos sob análise. São raros
# os projetos que analisam grandes bases de dados.
# 
# < center >
#   
#   ![MP](images / manifesto_project.png) {
#     width = 450px
#   }
# 
# [Manifesto Research Group](https: /  / manifestoproject.wzb.eu / )
# 
# <  / center >
#   
#   ## Text as data: oportunidades
#   
#   O avanço tecnológico e científico permitiu que técnicas automatizadas de análise
# de conteúdo fossem desenvolvidas e aplicadas de forma simples a grandes acervos.
# 
# < center >
#   
#   ![Biblioteca Florestan Fernandes](images / livros.jpg)
# 
# <  / center >
#   
#   ## Novos acervos
#   
#   < center >
#   
#   ![CIA](images / cia_2.png) {
#     width = 600px
#   }
# 
# [CIA](https: /  / www.cia.gov / library / readingroom / advanced - search -
#         view)
# 
# <  / center >
#   
#   ## Olhar o passado com as lentes do presente
#   
#   < center >
#   
#   ![RF](images / rev_francesa.jpg) {
#     width = 600px
#   }
# 
# <  / center >
#   
#   ##
#   
#   < center >
#   
#   >  “automated content methods are * incorrect * models of language"
# >
# > --- (Grimmer and Stewart 2013, 2)
# 
# </center>
# 
# ## Text as data: premissas
# 
# 1. Todos os modelos quantitativos de análise de conteúdo estão errados, mas alguns são úteis;
# 
# 2. Métodos quantitativos de análise de conteúdo amplificam a capacidade humana, mas não a substitui;
# 
# 3. Não há um método global para a análise automatizada de conteúdo;
# 
# 4. Validar, validar, validar.
# 
# ## Quadro geral de metodologias
# 
# <center>
# ![Grimmer](images/quadro_grimmer.png){width=700px}
# 
# [(Grimmer and Stewart, 2013)](https://www.cambridge.org/core/journals/political-analysis/article/text-as-data-the-promise-and-pitfalls-of-automatic-content-analysis-methods-for-political-texts/F7AAC8B2909441603FEB25C156448F20)
# 
# </center>
# 
# ## Fluxo de análise
# 
# <center>
# 
# ![flow](images/tidyflow-ch-6.png){width=700px}
# 
# [Text Mining with R: a tidy approach](https://www.tidytextmining.com/topicmodeling.html)
# 
# </center>
# 
# ## R e o Processamento de Linguagem Natural
# 
# - O [processamento de linguagem natural (NLP)](https://en.wikipedia.org/wiki/Natural_language_processing)
# é um subcampo da ciência da computação relacionado às interações entre computadores e a linguagem humana.
# 
# - O `R` dispõe de uma série de pacotes dedicados a essa área e apresenta grande potencial
# ao conectar o processamento de linguagem natural a todo seu arcabouço de pacotes estatísticos.
# 
# ## Encoding - Codificação de caracteres
# 
# Caracteres são representados por algum tipo de sistema de codificação ([Wiki](https://en.wikipedia.org/wiki/Character_encoding#cite_note-1)).
# 
# **Exemplo: [código Morse](https://morsedecoder.com/)** que codifica as letras do
# alfabeto latino e os numerais como sequências de pulsos elétricos de longa e curta duração. Outro exemplo é o
# sistema de codificação [UTF-8](https://en.wikipedia.org/wiki/UTF-8), capaz de codificar todos os 1.112.064 pontos
# de código válidos em Unicode usando até 8 bits.
# 
# ## Encoding - Codificação de caracteres
# 
# O R fornece funções para lidar com diferentes sistemas de codificação. Isso é útil se você lida
# com arquivos de texto que foram criados com outro sistema operacional e especialmente se
# o idioma não for o inglês e tiver muitos acentos e caracteres específicos.
# 
# Exemplo: o esquema de codificação padrão no Linux é [UTF-8](https://en.wikipedia.org/wiki/UTF-8),
# enquanto o esquema de codificação padrão no Windows é [Latin1](https://en.wikipedia.org/wiki/Latin-1_Supplement_(Unicode_block)).
# 
# 
# ## Encoding - Codificação de caracteres
# 
# A função `Encoding()` retorna a codificação de uma sequência de caracteres.
# Por sua vez, a função `iconv()` é usada para converter a codificação. Vejamos:
# 
# ```{r echo = TRUE, results='hide', message=FALSE, warning=FALSE}
chr <- "Amigo sinhô, Saravá. Xangô me mandou lhe dizer. Se é canto de Ossanha, Não vá!"
Encoding(chr) <- "UTF - 8"
Encoding(chr)

chr <- iconv(chr, from = "UTF - 8", to = "latin1")
Encoding(chr)

stri_trans_general(chr, "Latin - ASCII")
# ```
# 
# ## Strings no `R`
# 
# Chamamos uma sequência de caracteres de string. Três coisas são importantes de serem
# lembradas aqui:
# 
# 1. **Computadores não interpretam letras**. No limite, todos os caracteres são
# transformados em sequências compostas por zeros e uns. Logo, é através de padrões
# que caracteres são interpretados e os computadores armazenam os dados que retornam
# a nossos olhos.
# 
# ## Strings no `R`
# 
# 2. **Programar é escrever!** Não é à toa que chamamos as formas de escrita em programação
# de linguagens de programação. O desafio de se trabalhar com o texto como dado é o de fazer
# com que o computador diferencie código escrito do "texto como dado" que ele precisará
# processar de acordo com os interesses do analista.
# 
# ## Strings no `R`
# 
# 3. **O código e o texto é processado pelo computador: da esquerda para a direita e de cima para baixo**.
# Logo, ao desenvolver seu script é importante ter atenção em relação à ordem de escrita para
# que o computador possa desempenhar corretamente suas tarefas.
# 
# ## Strings e vetores
# 
# Para declarar uma string, utilizamos aspas simples `'` ou aspas dupla `"`.
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # Vetores de caracteres
caracter1 <- "a"
caracter2 <- 'A'

class(caracter1)
class(caracter2)
# ```
# 
# ## O `R` é case sensitive
# 
# O `R` diferencia letras maiúsculas de letras minúsculas. Se compararmos os dois
# objetos criados acima, temos:```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
caracter1 == caracter2
# ```
# 
# ## Sequências de caracteres
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # string
txt <- "uma string é uma sequência de caracteres"
txt <- 'também pode ser utilizada com aspas simples'

txt <- "no caso de aspas dupla, usa-se 'aspas simples' na string"
txt <- 'no caso de aspas simples, usa-se "aspas dupla" na string'

txt <- "para usar \"aspas dupla\" na string é necessário usar \\"
cat(txt)
# ```
# 
# ## Operações básicas com vetores de strings
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # vetor de caracteres com 5 strings vazias
palmeiras <- character(5)
palmeiras

# vejamos seu tamnho
length(palmeiras)  # verificando o tamanho do vetor

# incluindo string no primeiro e terceiro elementos do vetor
palmeiras[1] <- "Quando surge o alviverde imponente"
palmeiras[3] <- "Sabe bem o que vem pela frente"
palmeiras
# ```
# 
# ## Processamento básico-`nchar()`
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
nchar(c("Quantos", "caracteres?"))

nchar("Quantos caracteres?")

str_length(c("Quantos", "caracteres?"))
# ```
# 
# -`toupper()`, `tolower()`
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
tolower(c("TUdo eM MinúsCuLA", "ABCDE"))

toupper(c("TUdo eM mAiúsCula", "ABCDE"))
# ```
# 
# ## Processamento básico-Identificando caracter numa posição específica
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # vetor de strings
txt <-
  c("O Palmeiras é o time da virada", "o Palmeiras é o time do amor.")

# identificando terceira letra
str_sub(txt, 3, 3)

# Selecionando do segundo caracter de trás pra frente.
str_sub(txt, 2,-2)

# ```
# 
# ## Processamento básico
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
#Incluindo caracter ou string numa posicao específica.

str_sub(txt, 3, 11) <- "PALMEIRAS"
txt

# Preencher uma string em tamanho fixo.
str_pad(txt, 50) # por padrão: left

# Remove espaço extra.
txt <- str_pad(txt, 50) # por padrão: left
str_trim(txt)

# ```
# 
# ## Processamento básico
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # Recortando uma string para obter parte da sequência de caracteres.
str_sub(txt, start = 3, end = 11)

# É possível fazer o recorte usando índices de trás pra frente.
str_sub(txt, start = -14, end = -1)

# Extração de palavras.
word(txt, 2)
word(txt,-1)

# ```
# 
# 
# ## Regular Expressions no `R`
# 
# Vimos funções básicas e intermediárias para o processamento de sequências de caracteres
# no `R`. Para avançar,  é necessário aprender o uso de expressões regulares ( *
#                                                                                Regular Expressions * ).
# 
# ** Regular Expressions ** :uma expressão regular é um conjunto de símbolos que descreve
# um padrão de texto. Mais formalmente, uma expressão regular é um padrão que descreve
# um conjunto de cadeias de caracteres.
# 
# - Determinar cadeias de caracteres correspondentes a um padrão.
# - Encontrar as posições de padrões correspondentes.
# - Extrair o conteúdo de padrões correspondentes.
# - Substituir o padrão correspondente por novos valores.
# - Dividir uma sequência com base na correspondência de um padrão determinado.
# 
# ## Regular Expressions no `R`
# 
# O uso de expressões regulares pode se tornar uma tarefa realmente complexa. Veja
# esta discussão do [StackOverflow](
#   https: /  / stackoverflow.com / questions / 201323 / how - to - validate -
#     an - email - address - using - a - regular - expression / 201378#201378)
#   a respeito de seu uso para identificação de endereços de e -
#     mail,
#   por exemplo.
#   
#   ## Identificação e Extração de padrão
#   
#   ```{
#     r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
#   }

txt <-c("O Palmeiras é o time da virada", "o Palmeiras é o time do amor.")

str_extract(txt, "amor")

str_detect(txt, "amor")

# Utilizando o operador | (“OU”):

str_detect(
  c(
    "presidente",
    "presidencialismo",
    "presidencialista",
    "parlamentarismo"
  ),
  "ente|ismo"
)

# #```
# 
# ## Identificação e Extração de padrão
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# # Para identificar o “.” de fato, usamos “\.”. Para poder usar a “\”, adicionamos mais uma e temos:

txt <-
  c("O Palmeiras é o time da virada", "o Palmeiras é o time do amor.")
str_detect(txt, "\\.")

# Para identificar a “\” de fato, usamos “\\”:

txt <-
  c("O Palmeiras é o time da virada \\ o Palmeiras é o time do amor.")
writeLines(txt)

# ```
# 
# ## Substituição
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }

txt <-
  c("O Palmeiras é o time da virada", "o Palmeiras é o time do amor.")

str_replace(txt, "Palmeiras", "PALMEIRAS")

# ```
# 
# ## Âncoras
# 
# Por padrão,
# expressões regulares buscam por correspondência em qualquer parte de
# uma sequência de caracteres. Porém,
# é extremamente útil poder ancorar a busca pela
# correspondência no início ou no final de uma string. Podemos usar:-`“^”` para coincidir com o **
#   início ** da string.
# - `“$”` para coincidir com o ** final ** da string.
# 
# ```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }
# 
txt <-
  c("O Palmeiras é o time da virada", "o Palmeiras é o time do amor.")

str_detect(txt, "^O")

str_detect(txt, "\\.$")

# ```
# 
# 
# ## Exercício
# 
# Utilizando o operador `|` (“OU”),
# desenvolva código que identifique os elementos do
# vetor abaixo que contêm as palavras presidencialismo e parlamentarismo:```{
#   r echo = TRUE, message = FALSE, warning = FALSE
# }
vetor <-
  c(
    "presidente",
    "presidencialismo",
    "presidencialista",
    "parlamentarismo"
  )

# ```
# 
# ## Exercício: resposta
# # Obtenção de conteúdo: Webscraping
#   
#   Etapas para raspagem de dados na web:1. Conhecer detalhadamente o caminho para acesso aos dados
# 2. Armazenar todos os caminhos de acesso aos dados de forma amigável ao programa
# 3. Obter os dados
# 4. Processar os dados obtidos
# 
# ## Obtenção de conteúdo: Webscraping
# 
# É possível conhecer o código fonte de um site ao clicar com o botão direito do mouse
# no conteúdo da página.
# 
# < center >
#   ![deputados](images / deputados.png)
# 
# [Discursos Deputados](
#   https: /  / www2.camara.leg.br / atividade - legislativa / discursos - e -
#     notas - taquigraficas
# )
# 
# <  / center >
#   
#   ## Obtenção de Código Fonte:
#   
#   Podemos facilmente obter o código fonte de um endereço na internet com o uso da
# função `readLines`.
# 
# ```{
#   r echo = FALSE, results = 'hide', eval = FALSE
# }
# # ETAPA 1. Conhecer detalhadamente o caminho para acesso aos dados
# # ETAPA 2. Armazenar todos os caminhos de acesso aos dados de forma amigável ao programa
# 
# # defnindo endereço da web

link <-
  "https://www.camara.leg.br/internet/SitaqWeb/TextoHTML.asp?etapa=5&nuSessao=174.4.53.O&nuQuarto=56&nuOrador=2&nuInsercao=0&dtHorarioQuarto=11:46&sgFaseSessao=BC&Data=17/08/2010&txApelido=JAIR%20BOLSONARO,%20PP-RJ&txFaseSessao=Breves%20Comunica%C3%A7%C3%B5es&txTipoSessao=Extraordin%C3%A1ria%20-%20CD&dtHoraQuarto=11:46&txEtapa="

# ETAPA 3. Obter os dados
conteudo <-
  readLines(link)  # obtem o codigo fonte
# head(conteudo)
conteudo[328]

# ETAPA 4. Processar os dados obtidos
conteudo <- conteudo[328]

# ```
# 
# ## Exercício
# 
# Com base no exemplo que vimos,
# obtenha o código fonte da página do [Chico Buarque na Wikipédia](https: /
#                                                                    / pt.wikipedia.org / wiki / Chico_Buarque).
# 
# ## Exercício: resposta


# ## Obtenção de conteúdo em arquivo `.pdf`:
#   
#   ```{
#     r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
#   }

rdg2017 <-
  pdf_text(here("data/2017.pdf"))
rdg2017[1]

# ```
# 
# ## Obtenção de conteúdo em Imagens
# 
# No caso de textos em imagem é possível utilizar o _optical character recognition_ (OCR).
# OCR é o processo de encontrar e reconhecer texto dentro de imagens,
# por exemplo,
# de uma
# captura de tela,
# texto digitalizado. A imagem abaixo tem um texto de exemplo:< center >
#   ![ocr](images / testocr.png) {
#     width = 400px
#   }
# 
# <  / center >
#   
#   
#   ## Obtenção de conteúdo em Imagens
#   
#   Com o pacote [`Tesseract`](
#     https: /  / cran.r - project.org / web / packages / tesseract / vignettes /
#       intro.html
#   ) e
# o uso da [Interface de Programação de Aplicativos (API) do Google](https: /
#                                                                      / cloud.google.com / vision / ) é
# possível capturar seu conteúdo:```{
#   r echo = TRUE, results = 'hide', message = FALSE, warning = FALSE
# }

eng <- tesseract("eng")
text <-
  tesseract::ocr(here("data/testocr.png"), engine = eng)
cat(text)
# ```
# 
# ## Exercício
# 
# Utilizando a imagem do Tweet Fake atribuído ao Presidente Bolsonaro,
# obtenha seu conteúdo.
# Para tanto,
# é necessário instalar o acervo de treinamento em português com o seguinte
# comando `tesseract_download('por')`.
# 
# ## Exercício: resposta

# ## Obtenção de conteúdo: outras fontes-Download de arquivos:[TCE](
#   https: /  / www.tce.pe.gov.br / internet / index.php / relatorios - de -
#   gestao - fiscal - 2
# )
# - Webservices:[Câmara dos Deputados](
# https: /  / www2.camara.leg.br / transparencia / dados - abertos / dados -
#   abertos - legislativo / webservices
# )
# - APIs:[`twitteR`](
# https: /  / cran.r - project.org / web / packages / twitteR / twitteR.pdf
# )
# -  Áudio Transcrição:[`googleLanguageR](
# https: /  / cran.r - project.org / web / packages / googleLanguageR / index.html
# )
# 
# ## Processamento dos dados
# 
# < center >
# ![Grimmer](images / quadro_grimmer.png) {
#   width = 700px
# }
# 
# [(Grimmer and Stewart, 2013)](
# https: /  / www.cambridge.org / core / journals / political - analysis /
#   article / text - as - data - the - promise - and - pitfalls - of - automatic -
#   content - analysis - methods - for -political - texts / F7AAC8B2909441603FEB25C156448F20
# )
# 
# <  / center >
# 
# ## Mineração e estatísticas básicas

# ```{
#   r, results = 'hide', echo = FALSE, eval = T, warning = F, message = F
# }

rm(list=ls())

load(here(
"./data/impeachment-dilma-dados-filter.rda"
))

# removendo stop words
stop_w <-
tibble(word = stopwords(source = "stopwords-iso", language = "pt"))

# frequencia de palavras
impeachment_dilma %>%
unnest_tokens(word, text) %>%
anti_join(stop_w) %>%
count(word, sort = TRUE)

# gráfico
impeachment_dilma %>%
unnest_tokens(word, text) %>%
anti_join(stop_w) %>%
filter(!word %in% c("sr", "voto", "votos")) %>%
count(word, sort = TRUE) %>%
filter(n > 200) %>%
mutate(word = reorder(word, n)) %>%
ggplot(aes(word, n)) +
geom_col() +
xlab(NULL) +
coord_flip()

# ```
# 
# ## Mineração e estatísticas básicas
# 
# Avançando um pouco mais,
# já seria possível comparar o uso de palavras por diferentes
# deputados. Vamos comparar os deputados do PSOL,
# PT e do PSDB.
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = TRUE, warning = F, message = F, fig.align =
#   "center", out.width = "55%"
# }

impeachment_pt <-
impeachment_dilma %>% filter(partido == "PT")
impeachment_psdb <-
impeachment_dilma %>% filter(partido == "PSDB")
impeachment_psol <-
impeachment_dilma %>% filter(partido == "PSOL")

impeachment_pt <-
tibble(line = 1:nrow(impeachment_pt),
       text = impeachment_pt$text)

impeachment_psdb <-
tibble(
  line = 1:nrow(impeachment_psdb),
  text = impeachment_psdb$text
)

impeachment_psol <-
tibble(
  line = 1:nrow(impeachment_psol),
  text = impeachment_psol$text
)

tidy_impeachment_pt <-
impeachment_pt %>%
unnest_tokens(word, text) %>%
anti_join(stop_w)

tidy_impeachment_psdb <-
impeachment_psdb %>%
unnest_tokens(word, text) %>%
anti_join(stop_w)

tidy_impeachment_psol <-
impeachment_psol %>%
unnest_tokens(word, text) %>%
anti_join(stop_w)

# frequencia de palavras
frequency <-
bind_rows(
  mutate(tidy_impeachment_psol, author = "PSOL"),
  mutate(tidy_impeachment_psdb, author = "PSDB"),
  mutate(tidy_impeachment_pt, author = "PT")
) %>%
mutate(word = str_extract(word, "[a-z']+")) %>%
count(author, word) %>%
group_by(author) %>%
mutate(proportion = n / sum(n)) %>%
select(-n) %>%
spread(author, proportion) %>%
gather(author, proportion, `PSOL`:`PSDB`)

# gráfico
ggplot(frequency, aes(
x = proportion,
y = `PT`,
color = abs(`PT` - proportion)
)) +
geom_abline(color = "gray40", lty = 2) +
geom_jitter(
  alpha = 0.1,
  size = 2.5,
  width = 0.3,
  height = 0.3
) +
geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
scale_x_log10(labels = percent_format()) +
scale_y_log10(labels = percent_format()) +
scale_color_gradient(
  limits = c(0, 0.001),
  low = "darkslategray4",
  high = "gray75"
) +
facet_wrap( ~ author, ncol = 2) +
theme(legend.position = "none") +
labs(y = "PT", x = NULL)

# # ```
# # 
# # ## tf-idf
# # 
# # A quantificação de texto pode ser feito através da análiseda frequência das palavras
# # que compõem o documento. Logo,
# # uma medida seria a frequência
# # de um termo (tf) em um documento.
# # 
# # Contudo,
# # há palavras que ocorrem muitas vezes,
# # mas podem não ser importantes. Podemos
# # removê-las antes da análise,
# # mas é possível que algumas dessas palavras sejam mais
# # importantes em alguns documentos do que em outros.
# # 
# # ## tf-idf
# # 
# # Uma alternativa seria examinar a frequência de documento inversa de um termo (idf),
# # o que diminui o peso das palavras comumente usadas e aumenta o peso das palavras
# # que não são muito usadas em uma coleção de documentos.
# # 
# # Combinando as duas alternativas,
# # calcula - se o * tf - idf * de um termo. Em outras palavras,
# # a frequência de um termo ajustada pela frequência com que é usado no acervo.
# # 
# # ## tf-idf
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = TRUE, warning = F, message = F, fig.align =
#   "center", out.width = "65%"
# }

party_tfidf <-
impeachment_dilma %>% filter(partido %in% c("PT", "PSDB", "PSOL", "PMDB")) %>%
unnest_tokens(word, text) %>%
count(partido, word, sort = TRUE) %>%
bind_tf_idf(word, partido, n) %>%
arrange(desc(tf_idf)) %>%
mutate(word = factor(word, levels = rev(unique(
  word
)))) %>%
mutate(partido = factor(
  partido, levels = c("PT", "PSDB", "PSOL", "PMDB")
))

party_tfidf %>%
group_by(partido) %>%
top_n(15, tf_idf) %>%
ungroup() %>%
mutate(word = reorder(word, tf_idf)) %>%
ggplot(aes(word, tf_idf, fill = partido)) +
geom_col(show.legend = FALSE) +
labs(x = NULL, y = "tf-idf") +
facet_wrap( ~ partido, ncol = 2, scales = "free") +
coord_flip()

# ```
# 
# ## Corpus
# 
# O pacote `quanteda` permite trabalhar com Tokens,
# Corpus e DFMs através do seguinte fluxograma:< center >
# ![quanteda_flow](images / quanteda -
#                    workflow.png) {
#   width = 500px
# }
# <  / center >
# 
# ## Corpus
# 
# ```{
#   r echo = TRUE, eval = F, message = FALSE, warning = FALSE
# }
# # transformando data.frame em corpus ----
corp <- corpus(impeachment_dilma)
summary(corp, 5)

# # verifique as 4 primeiras colunas do corpus!!!!
# 
# ```
# 
# ## Filtrando corpus: `corpus_subset()`
# 
# A função `corpus_subset()` permite selecionar documentos em um corpus com base em variáveis no nível do documento.
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }
# 
ndoc(corp)  # n de documentos do corpus completo

corp_ptpsdb <-
corpus_subset(corp, partido %in% c('PT', 'PSDB'))

ndoc(corp_ptpsdb)
summary(corp_ptpsdb, 5)

# ```
# 
# ## Trocando a unidade de texto no corpus: `corpus_reshape()`
# 
# A função `corpus_reshape()` permite alterar a unidade de textos entre documentos,
# parágrafos e frases. Os textos podem ser restaurados para a unidade original mesmo
# que o corpus seja modificado por outras funções.
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }
ndoc(corp)  # n de documentos do corpus completo

# mudando para sentenças ----
corp_sent <-
corpus_reshape(corp, to = 'sentences')
ndoc(corp_sent)
summary(corp_sent, 5)

# restaurando documentos originais ----
corp_documents <-
corpus_reshape(corp_sent, to = 'documents')
ndoc(corp_documents)

# ```
# 
# ## Tokens e Corpus
# 
# ## Obtendo Tokens a partir de um Corpus: `tokens()`
# 
# A função `tokens()` segmenta o texto.
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }
# obtendo tokens ----
toks <- tokens(corp)
head(toks[[1]], 50)

# obtendo tokens sem pontuação ----
toks <-
tokens(corp, remove_punct = T)
head(toks[[1]], 50)

# ```
# 
# ## Palavras-chave e seu contexto: `kwic()`
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }
# # caso de uma palavra-chave ----
toks <- tokens(corp)
kw <-
kwic(toks, pattern =  'golp*')
head(kw, 30)
texts(corp)[6]

# caso de uma frase inteira ----
kw <-
kwic(toks, pattern =  phrase('não vai ter golp*'))
head(kw, 30)
texts(corp)[6]

# ```
# 
# ## DFM
# 
# A função `dfm()` constrói uma matriz de documentos e termos /
# palavras / stems / tokens / features (DFM)
# a partir de um objeto de tokens.
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }

dfmat <-
tokens(corp, remove_punct = TRUE, remove_numbers = T) %>%
tokens_select(
  pattern = stopwords(source = "stopwords-iso", language = "pt"),
  selection = 'remove'
) %>%
dfm()

dfmat
ndoc(dfmat)  # numero de documentos
nfeat(dfmat)  # numero de features
head(docnames(dfmat), 20)  # ids dos documentos
head(featnames(dfmat), 20)  # algumas features
topfeatures(dfmat, 10)  # features mais frequentes

# ```
# 
# ## Refinando a seleção de features
# 
# ```{
# r echo = TRUE, eval = FALSE, message = FALSE, warning = FALSE
# }
# # selecionando tokes que aparecem numa proporção específica de documentos ----
dfmat_docfreq <-
dfm_trim(dfmat, min_docfreq = 0.01, docfreq_type = "prop")

nfeat(dfmat)
nfeat(dfmat_docfreq)
# ```
# 
# ## Agrupando e separando documentos numa DFM
# 
# ```{
# r, results = 'hide', echo = TRUE, eval = F, warning = F, message = F
# }
head(docvars(dfmat_docfreq))

# por autor ----
dfmat_autor <-
dfm(dfmat_docfreq, groups = "nomeOrador")
ndoc(dfmat_autor)
head(docvars(dfmat_autor))

# por partido ----
dfmat_party <-
dfm(dfmat_docfreq, groups = "partido")
ndoc(dfmat_party)
docvars(dfmat_party)

# selecionando partidos
dfmat_party_pt <-
dfm_subset(dfmat_party, subset = partido %in% c("PT"))

# ```
# 
# 
# ## Stemming
# 
# Até aqui,
# mesmo que os procedimentos adotados tenham diminuído a dimensionalidade
# do acervo de documentos ao transformá-lo em uma sacola de palavras ( *
#                                                                      bag of words * ),
# ainda é necessária a adoção de procedimentos que possam reduzir a complexidade do conteúdo
# a ser analisado.
# 
# ## Stemming
# 
# Com esse objetivo,
# podemos pensar que determinado documento tenha em sua composição
# as seguintes palavras únicas:trabalho,
# trabalhador,
# trabalhista. Apesar de seus
# diferentes signficados,
# cada uma dessas palavras pode ser reduzida ao seu radical,
# _trabalh_,
# dando ao pequisador informações suficientes para sua
# análise e,
# assim reduzindo,
# o _n_ de três palavras únicas para uma palavra que tem
# a soma das frequências anteriores.
# 
# ## Stemming
# 
# Chamamos esse procediment de ** Stemming **
# . O pacote `quanteda` possui a opção de
# stemming da DFM em português que usa o projeto Snowball através da função `dfm_wordstem()`.
# 
# Vamos aplicar o ** stemming ** para produzir a `dfm` que nos será útil daqui em diante.
# 
# ```{
# r echo = FALSE, eval = TRUE, results = 'hide', message = FALSE, warning =
#   FALSE
# }

rm(list = ls())

load(here(
"./data/impeachment-dilma-dados-filter.rda"
))

impeachment_dfm <-
impeachment_dilma %>%
mutate(text = stri_trans_general(text, "Latin-ASCII")) %>%
mutate(text = str_remove_all(text, "[[:digit:]]")) %>%
corpus(docid_field = "doc_id", text_field = "text") %>%
tokens(remove_punct = TRUE) %>%
tokens_tolower() %>%
tokens_remove(stopwords(source = "stopwords-iso", language = "pt"), min_nchar = 2) %>%
tokens_wordstem(language = "pt") %>%
dfm() %>%
dfm_select(
  pattern = c(
    "sr",
    "total",
    "deput",
    "vot",
    "president",
    "bet",
    "mansur",
    "palm"
  ),
  selection = "remove"
) %>%
dfm_trim(min_docfreq = 0.01, docfreq_type = "prop")

impeachment_dfm
ndoc(impeachment_dfm)  # numero de documentos
nfeat(impeachment_dfm)  # numero de features
head(docnames(impeachment_dfm), 20)  # ids dos documentos
head(featnames(impeachment_dfm), 20)  # algumas features
topfeatures(impeachment_dfm, 10)  # features mais frequentes
# ```
# 
# ## Nuvem de palavras
# 
# Uma comum da visualização de frequência na análise de texto é a nuvem de palavras.
# 
# ```{
# r echo = FALSE, eval = TRUE, message = FALSE, warning = FALSE, fig.align =
#   "center"
# }

set.seed(132)
textplot_wordcloud(impeachment_dfm, max_words = 100)

# ```
# 
# ## Escalonamento
# 
# < center >
# ![quadro - grimmer](images / quadro_grimmer.png) {
#   width = 700px
# }
# 
# <  / center >
# 
# ## Escalonamento
# 
# < center >
# ![voteview](images / voteview.png) {
#   width = 400px
# }
# 
# [Projeto Voteview](https: /  / voteview.com /
# )
# <  / center >
# 
# ## Escalonamento
# 
# A utilização da análise quantitativa de textos para a extração de posições
# políticas / ideológicas de partidos,
# políticos e eleitores é uma área extremamente
# promissora. O teste de modelos de competição partidária,
# por exemplo,
# depende do
# conhecimento das posições dos principais atores envolvidos no jogo político.
# 
# As duas técnicas mais populares são o [_Wordscores_](https: /
#                                                      / tutorials.quanteda.io / machine - learning / wordscores / ) e
# o [_Wordfish_](https: /  / tutorials.quanteda.io /
#                machine - learning / wordfish / ).
# 
# ## Wordscores
# 
# O _Wordscores_ é um algoritmo supervisionado para estimar posições políticas.
# Nessa família de algoritmos,
# são apresentados ao computador alguns dados de entrada
# e as saídas esperadas. Chamamos esse conjunto de entradas e saídas de _training set_.
# A partir desse conjunto de informações o algoritmo “aprende” a classificar novos
# documentos,
# o conjunto do _test set_.
# 
# Para nosso exemplo,
# vamos usar manifestos das eleições federais alemãs de 2013 e 2017.
# Para as eleições de 2013,
# temos as avaliações médias de especialistas para os cinco
# principais partidos e prevemos as posições dos partidos nos manifestos de 2017.
# 
# ## Wordscores
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = TRUE, warning = F, message = F, fig.align =
#   "center", out.width = "75%"
# }
# carregando dados ----
corp_ger <-
readRDS(here("./data/data_corpus_germanifestos.rds"))

# summary(corp_ger)
texts(corp_ger)[1]  # em alemão! possibilidade de pesquisas em perspectiva comparada.

# criando dfm e aplicando o modelo
dfmat_ger <-
dfm(corp_ger, remove = stopwords("de"), remove_punct = TRUE)

# aplicando modelo e gerando visualização ----
textmodel_wordscores(
dfmat_ger,
y = docvars(corp_ger, "ref_score"),
smooth = 1
) %>%
predict(se.fit = TRUE, newdata = dfmat_ger) %>%
textplot_scale1d()

# ```
# 
# ## Wordscores: Críticas e potenciais problemas
# 
# Embora o _Wordscores_ constitua um grande avanço na análise quantitativa de textos,
# ele não é livre de problemas.
# 
# 1. O principal é o fato de ele depender fortemente da escolha dos textos de referência ( _training set_). Em situações
# extremas é possível que,
# com a escolha diferente de textos de referência,
# um mesmo
# pesquisador encontre resultados diferentes para um mesmo conjunto de dados.
# 
# 2. A segunda limitação é a possibilidade de as diferenças entre os textos estarem
# mais relacionadas com o estilo linguístico do autor do que com as posições políticas.
# Como todas as palavras adicionam a mesma quantidade de informação sobre o documento,
# temos que palavras politicamente relevantes em um contexto sejam igualmente ponderadas
# a palavras pouco informativas.
# 
# ## Wordfish
# 
# A segunda técnica mais popular para estimar posições políticas a partir de textos
# é o _Wordfish_ (SLAPIN
#               PROKSCH, 2008). Ao contrário do _Wordscores_,
# esse é um
# algoritmo não supervisionado,
# pois não depende da escolha de textos de referência,
# ou seja,
# da construção de um _training set_. Dessa forma,
# diminui muito a chance de obtenção de
# resultados diferentes a partir do mesmo conjunto de dados em razão da amostra de treinamento.
# 
# Vamos novamente usar um exemplo do pacote `quanteda`. Nesse exemplo,
# serão utilzados
# os os discursos orçamentários irlandeses de 2010.
# 
# ## Wordfish
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = TRUE, warning = F, message = F, fig.align =
#   "center", out.width = "75%"
# }

# carregando dados, construindo dfm e visualização
dfmat_irish <-
dfm(data_corpus_irishbudget2010, remove_punct = TRUE)

dfmat_irish %>% textmodel_wordfish(dir = c(6, 5)) %>%
textplot_scale1d()
# ```
# 
# ## Wordfish
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = TRUE, warning = F, message = F, fig.align =
#   "center", out.width = "75%"
# }

# agrupando resultados na visualização ----
dfmat_irish %>% textmodel_wordfish(dir = c(6, 5)) %>%
textplot_scale1d(groups = docvars(dfmat_irish, "party"))

# ```
# 
# ## Críticas e potenciais problemas
# 
# A limitação desse modelo é o fato de ele necessitar que os documentos cubram uma
# grande quantidade de temas para extrair as posições ideológicas. Assim,
# se os
# parlamentares focarem seus discursos em determinadas áreas temáticas,
# provavelmente
# não teremos resultados consistentes ao aplicarmos o _Wordfish_. Como a variação no
# uso das palavras não será determinada pelas preferências políticas,
# mas pelos tópicos,
# a
# diferença nas posições estimadas também refletirá essa diferença.
# Outro aspecto,
# compartilhado com o _Wordscore_,
# é a possível mudança nos resultados a
# depender do tratamento dado aos textos no pré-processamento.
# 
# ## Classificação
# 
# A classificação automatizada organiza o acervo de documentos em categorias,
# sejam
# elas conhecidas ou não.
# 
# < center >
# ![quadro - grimmer](images / quadro_grimmer.png) {
#   width = 700px
# }

# <  / center >
# 
# ## Categorias conhecidas
# 
# Ancorados na teoria,
# na experiência ou especialidade em determinado assunto pode - se
# desejar classificar um acervo de documentos em categorias já conhecidas.
# 
# - Métodos de aprendizagem supervisionada
# 
# Métodos de aprendizado supervisionado replicam a familiar tarefa de codificação manual,
# porém com enorme redução de custos e grande ganho de escala. Sua implementação pressupõe
# a classificação manual de uma amostra do acervo em um conjunto predeterminado de categorias.
# Essa amostra classificada,
# conhecida como conjunto de treinamento ou ** training set ** ,
# é usada para treinar modelos estatísticos,
# cuja principal aplicação é a classificação
# do restante do acervo,
# conjunto de teste ou ** test set ** ,
# nas categorias predeterminadas.
# Ao final da classificação,
# procedimentos de validação devem ser adotados para se
# averiguar a performance do modelo utilizado.
# 
# ## Categorias desconhecidas
# 
# Não é difícil encontrar situações nas quais o conjunto de categorias
# não seja conhecido. Pode,
# por exemplo,
# ser do interesse de um pesquisador identificar
# quais tópicos são enfatizados pelos deputados federais nos discursos proferidos ao longo
# de legislaturas. Uma vez que a atividade do representante político se debruça sobre
# inúmeras esferas da sociedade e da vida,
# predeterminar categorias temáticas de fala
# dos deputados federais pode limitar o conhecimento a ser obtido sobre o acervo.
# Para enfrentar esse desafio,
# veremos exemplos de aplicação do aprendizado não
# supervisionado ( ** unsupervised learning methods **
# ).
# 
# ## Topic Models
# 
# Os modelos de tópicos possuem duas principais características:1. definem estatisticamente um tópico como função densidade de probabilidade sobre palavras.
# Para um tópico$k$$(k = 1, \dots, K)$,
# essa função de probabilidade é representada com um
# vetor$M\times 1$,
# $\theta_k$,
# em que$\theta_{
# mk
# }$descreve a probabilidade de
# o$k - ésimo$tópico usar a$m - ésima palavra$. Logo,
# para estimar um tópico,
# os
# modelos usam a ocorrência de palavras entre documentos e pressupõem,
# em sua grande
# maioria,
# o uso da DTM / DFM obtida através do pré-processamento dos dados.
# 
# ## Topic Models
# 
# 2. os modelos de tópicos compartilham uma estrutura hierárquica básica.
# 
# < center >
# 
# ![topic_models](images / topic_models.png)
# 
# [O texto como dado:desafios e oportunidades para as ciências sociais](
# https: /  / www.anpocs.com / index.php / bib - pt / bib - 86 / 11215 - o -
#   texto - como - dado - desafios - e - oportunidades - para - as - ciencias -
#   sociais / file
# )
# 
# <  / center >
# 
# ## Topic Models
# 
# Como apontam Grimmer e Stewart (2013),
# todos os métodos de aprendizagem supervisionada
# pressupõem duas etapas básicas após os procedimentos de pré-processamento:**
# 1. Definindo o número$k$de categorias **
# 
# ** 2. Validação ** :Um exemplo de aplicação desses dois tópicos será visto na apresentação do _Expressed Agenda Model_.
# 
# ## Latent Dirichlet Allocation - LDA
# 
# O Latent Dirichlet Allocation (LDA) [(BLEI
#                                     NG
#                                     JORDAN, 2003)](http: /  / jmlr.org / papers / volume3 / blei03a / blei03a.pdf)
# é um método popular [Probabilistic Topic Models](http: /
#                                                  / www.cs.columbia.edu /  ~ blei / papers / Blei2012.pdf)
# para modelagem de tópicos. Ele trata cada documento como uma mistura de tópicos e cada
# tópico como uma mistura de palavras. Isso permite que os documentos "se sobreponham" uns
# aos outros em termos de conteúdo,
# em vez de serem separados em grupos distintos.
# 
# Para aplicação do LDA,
# faremos uso do pacote `topicmodels`,
# que implementa o modelo
# desenvolvido por BLEI,
# NG e JORDAN (2003) através de um exemplo do pacote `quanteda`.
# 
# ## Latent Dirichlet Allocation - LDA
# 
# Vamos para o código.
# 
# ```{
# r, results = 'hide', echo = FALSE, eval = F, warning = F, message = F
# }

load(here("data", "data_corpus_guardian.rda"))

ndoc(corp_news_subset)

dfmat_news <-
corp_news %>% dfm(remove_punct = TRUE, remove = stopwords('en')) %>%
dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST')) %>%
dfm_trim(
  min_termfreq = 0.95,
  termfreq_type = "quantile",
  max_docfreq = 0.1,
  docfreq_type = "prop"
)

dfmat_news <-
dfmat_news[ntoken(dfmat_news) > 0, ]

dtm <-
convert(dfmat_news, to = "topicmodels")

# rodando o modelo
# lda <- LDA(dtm, k = 10)

load(here("data", "lda_guardian.rda"))

terms(lda, 10)

dfmat_news$topic <- topics(lda)
head(topics(lda), 20)

# ```
# 
# 
# ## Expressed Agenda Model
# 
# < center >
# ![leviatan](images / leviathan.jpg)
# 
# <  / center >
# 
# ## Expressed Agenda Model
# 
# < center >
# ![romario - jogador](images / romario_1994.jpg) {
#   width = 300px
# }
# 
# <  / center >
# 
# ## Expressed Agenda Model
# 
# < center >
# ![romario - deput](images / romario.jpg) {
#   width = 300px
# }
# 
# <  / center >
# 
# ## Expressed Agenda Model
# 
# < center >
# ![exp - agenda](images / exp - agenda.png)
# 
# <  / center >
# 
# ## Expressed Agenda Model
# 
# < center >
# ![paper - dados](images / paper - dados.png)
# 
# [Com a Palavra os Nobres Deputados](
# https: /  / www.scielo.br / scielo.php ? script = sci_arttext &
#   pid = S0011 - 52582020000100202 & lng = pt & nrm = iso & tlng = pt
# )
# 
# <  / center >
# 
# ## Expressed Agenda Model
# 
# < center >
# 
# ![retorica - parlamentar](images / retorica_parlamentar.png) {
#   width = 400px
# }
# 
# [Retórica Parlamentar](http: /  / retorica.labhackercd.leg.br /
#                        2015.html)
# 
# <  / center >
# 
# ## Material adicional
# 
# < center >
# ![txt4cs](images / txt4cs.png)
# 
# [txt4cs](https: /  / bookdown.org / davi_moreira /
#          txt4cs / )
# 
# <  / center >
# 
# ## Material adicional-[Political Analysis:Special Issue](
# https: /  / www.cambridge.org / core / journals / political - analysis /
# article / introduction - to - the - special - issue - the - statistical -
# analysis - of - political - text / E3D4575845083A506B2177F3F1152100
# )
# - [Political Analysis:Virtual Issue](
# https: /  / www.cambridge.org / core / journals / political - analysis /
#   issue / FF88EF06ABD5D421202E8284F67DE2F7
# )
# - [Image as Data](
# https: /  / www.cambridge.org / core / elements / an - introduction - to -
#   images - as - data - for -social - science - research / 0376EE8A7A21F5B47FC4EC24DF07EFE9
# )
# - [Handling and Processing Strings in R](
# https: /  / www.gastonsanchez.com / Handling_and_Processing_Strings_in_R.pdf
# )
# - [Handling Strings with R](https: /  /
#                             www.gastonsanchez.com / r4strings / )
# - [R Wikibook:Programming and Text Processing](http: /
#                                                / en.wikibooks.org / wiki / R_Programming / Text_Processing)
# - [stringr:modern, consistent string processing](
# https: /  / journal.r - project.org / archive / 2010 - 2 / RJournal_2010 -
#   2_Wickham.pdf
# )
# - [Quanteta Tutorials](https: /  / tutorials.quanteda.io /
# )
# - [Text Mining with R](https: /  / www.tidytextmining.com /
# )
# 
# ## Tarefa da aula
# 
# As instruções da tarefa estão no arquivo `NN-class-ds4ir-assignment.rmd` da pasta
# `assignment` que se encontra na raiz desse projeto.
# 
# 









