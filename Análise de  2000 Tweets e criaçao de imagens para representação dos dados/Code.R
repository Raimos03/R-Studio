# install.packages("glue")
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("gsubfn")
# install.packages("tidytext")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("ggplot2")
# install.packages("cluster")
# install.packages("fpc")

library(glue)
library(twitteR)
library(SnowballC)
library(wordcloud)
library(twitteR)
library(gsubfn)
library(tm)
library(ggplot2)
library(cluster)
library(fpc)

#Chave de Autorizaçao do Twitter


###############################################  
#a)Leia do Twitter mensagens que contenham a palavra covid a partir da #
#data 01/07/2020 e que estejam em um raio de 30Km da latitude 22°58'45.8"S (-22.97939) e
#longitude 43°13'58.5"W (-43.233452).

tcovid=searchTwitter('covid',n=2000,since='2020-07-01',geocode ='-22.97939,-43.233452,30km' )
print(tcovid)

tcovidb=tcovid #backup
tcovid2=tcovidb

dftcovid=twListToDF(tcovid)


###############################################
#b)A partir dos textos lidos no item 1 realize a limpeza removendo caracteres 
#     removendo caracteres como ".-'´:", pontuação, números e URLs;
#     converta os caracteres de todas as palavras para minúsculo; remova stopwords; remova espaços;
# 

tcovidbedit = Corpus(VectorSource(dftcovid$text))


tcovid= tm_map(tcovidbedit,content_transformer(function(tcovidbedit) gsub("(RT|via)\n n...((?:\\b\\W*@\\w+)+)", "", tcovidbedit )))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("\n ^\\s+|\\s+$ ...", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("@\\w+ \n \\n", "", tcovid )))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("[[:punct:]]", "", tcovid ) ))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("[[:digit:]]", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("http\\w+", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("[\t]{2,}", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("[0-9]", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("(RT ...RTn ...\n)", "", tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("(...' \n .-`;:)", "",tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("(kkkkkkk rs hahaha ...)", "",tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("(n...rt nrt)", "",tcovid)))
tcovid= tm_map(tcovid,content_transformer(function(tcovid)gsub("nrt,\nrt,\rt,rt", "",tcovid)))

tcovid<- tm_map(tcovid, tolower)
tcovid<- tm_map(tcovid, stripWhitespace)
########################################
#c)A seguir, realize o stemming (eliminação de variações morfológicas) dos textos; 

tcovid<- tm_map(tcovid,removeWords,stopwords('portuguese'))



#########################################
#d)Apresente os 25 termos mais encontrados e as respectivas frequências;

corp=VCorpus(VectorSource(tcovid))

dtmcovid=DocumentTermMatrix(corp)
tdm=TermDocumentMatrix(corp) 

freq=colSums(as.matrix(tdm))
freq <- sort(colSums(as.matrix(dtmcovid)), decreasing=TRUE) 
print(head(freq,25))


######################################
#e)Apresente um histograma (termo x frequência) para os temos que apareceram pelo menos 600 vezes;

wf=data.frame(word=names(freq), freq=freq)

ggplot(subset(wf, freq>600),aes(word,freq))+geom_bar(stat="identity",color='blue',alpha=0.5,fill='blue')+labs(x="Palavras",y="Frequencia")+
  theme(axis.text.x=element_text(angle=45, hjust=1))
  
#********** Tomei a liberdade de colocar outro filtro para visualizar uma maior variaçao da ocorrência pelo gráfico.

ggplot(subset(wf, freq>100),aes(word,freq))+geom_bar(stat="identity",color='blue',alpha=0.5,fill='blue')+labs(x="Palavras",y="Frequencia")+
  theme(axis.text.x=element_text(angle=45, hjust=1))


###################################
#f)Apresente os termos associados com o termo Brasil segundo uma correlação maior ou igual a 70%;


findAssocs(dtmcovid, c("brasil","Brasil"), corlimit=0.7)



#testando uma correlaçao menor:

findAssocs(dtmcovid, c("brasil","Brasil"), corlimit=0.3)


####################################
#g)Apresente um wordcloud com termos cuja frequência seja de pelo menos 100; 

wordcloud(names(freq),freq,min.freq = 100,colors=brewer.pal(6, "Dark2"))


###################################
#h)Usando o método k-means apresente o cluster de termos;


cluster_Covid=as.data.frame(freq)
cluster_Covid=wf[wf$freq>90,]

kdata= kmeans(cluster_Covid$freq,3)

clusplot(cluster_Covid, kdata$cluster, color=T, shade=T, labels=2, lines=0)   

   

