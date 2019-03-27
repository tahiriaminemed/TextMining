rm(list = ls())
setwd("~/tm_tahiri/tm")

library(NLP)
library(tm)
library(igraph)
source('includes/manualSagaProcessing.R',print.eval=TRUE)

#Chargement
saga = readChar("data/[Rois Maudits-1] Le Roi de fer - Druon,Maurice.txt", nchars = 1000000)

#Eliminer les lignes vides, metadonnees et le repertoire (passages entre corpus)
saga = manualSagaProcessing(saga)

#Traitement des personnage au nom compose
names <- tolower(readLines("data/Personnages",encoding = "UTF-8"))
for (name in names) {
  saga = gsub(name, gsub(" ", "", name), saga)
}
names = gsub(" ", "", names)

#Transformation du text en Corpus
saga.corpus <- Corpus(VectorSource(saga))

#Netoyage de la donnees (lowercasing, removing numbers and punctuation)
saga.corpus <- tm_map(saga.corpus, removeNumbers)
saga.corpus <- tm_map(saga.corpus, removePunctuation)
saga.corpus <- tm_map(saga.corpus, removeWords, stopwords('french'))

#TFxIDF
tfidf <- TermDocumentMatrix(saga.corpus, control = list(weighting = weightTf))
tfidf <- removeSparseTerms(tfidf, sparse = 0.8)

#netoyage de RAM#######
rm(saga.corpus)
gc()
########################

tfidf = as.matrix(tfidf)
tfidf = cbind(tfidf, words = rownames(tfidf))


#Recuperation de la matrice TFxIDF pour les noms
tfidf.names = merge.data.frame(data.frame(tfidf), data.frame(names), by.x = "words", by.y = "names")

#netoyage de RAM####
rm(tfidf)
gc()
######################

row.names(tfidf.names) = tfidf.names[,1]
tfidf.names = tfidf.names[,2:ncol(tfidf.names)]

#tfid * t(tfidf) = la matrice d'adjacence (co-occurance)
tmp <- as.matrix((tfidf.names), 2, as.numeric)
tmp[tmp > 0] = 1

adjMatrix = apply(as.matrix(tmp), 2, as.numeric) %*% t(apply(as.matrix(tmp), 2, as.numeric))
rownames(adjMatrix) = rownames(tfidf.names)
colnames(adjMatrix) = rownames(tfidf.names)
diag(adjMatrix)<-0



#netoyage de RAM####
gc()
######################

#build graph
graph <- graph.adjacency(adjMatrix, weighted=T, mode = 'undirected')
set.seed(3952)
graph <- simplify(graph)
V(graph)$label <- V(graph)$name
V(graph)$degree <- degree(graph)
plot(graph, layout=layout.kamada.kawai)



