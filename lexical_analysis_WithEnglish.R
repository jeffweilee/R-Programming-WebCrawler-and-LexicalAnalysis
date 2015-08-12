library(tm)
#library(data.table)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)

####################
# Initialize Path
####################
#wordcloud.eng.pic.path = "mobile01_android_wordcloud_eng.png"
#cluster.eng.pic.path = "mobile01_android_cluster_eng.png"

####################
# Functions ExtractWords(); n: noun; a: adj; v: verb; ......
####################
# Extract noun and adj here
ExtractWords <- function(sentence) {
  wordstring<-lapply(sentence, function(w) {w[names(w) == "n" | names(w) == "a" ]})
  return 
  unlist(wordstring)}

####################
# Term Segment Analysis
####################
# Label and Pick adj, verb, noun, ...... 
# e.seglst <- segmentCN(mobile01.android.data[,c("mobile01.android.title", "mobile01.android.content")], nature=TRUE)
e.seglst <- segmentCN(mobile01.android.data[,c("mobile01.android.content")], nature=TRUE)
e.corpus <- Corpus(VectorSource(ExtractWords(c.seglst)))
e.corpus <- tm_map(e.corpus, removePunctuation)
e.corpus <- tm_map(e.corpus, removeNumbers)
e.corpus <- tm_map(e.corpus, content_transformer(function(word) gsub(c("wrote", "the", "yzx", "一直", "差不多","現在", "樓主", "安安", "你好", "分享", "小弟", "XD", "xd", "lol", "LOL", "btw", ""), "", word)))
# Remove all English words and numbers
#e.corpus <- tm_map(e.corpus, content_transformer(function(word) gsub("[A-Za-z0-9]", "", word)))

####################
# Term Frequency Analysis
####################
# Matrix Row: Terms; Col:Documents
e.tdm = TermDocumentMatrix(e.corpus)
# Matrix Row: Documents; Col:Terms
#e.tdm = DocumentTermMatrix(e.corpus)
freqency <- sort(rowSums(as.matrix(e.tdm)), decreasing = TRUE)
e.dm <- data.frame(word = names(freqency), freq = freqency)
View(e.dm)

####################
# Wordcloud Picture Based on Term Frequency
####################
# Draw wordcloud based on e.corpus TermDocument; family="Kaiti TC", "STKaiti"
wordcloud(e.corpus, family="STKaiti", rot.per=0.05, scale=c(4,.2), min.freq=0, max.words=Inf, random.order=FALSE, ordered.colors = FALSE, colors=brewer.pal(8, "Dark2"))
# Draw png wordcloud base on word/freq to file
png(file = wordcloud.eng.pic.path, bg="transparent", width=1000, height=1000, res=300)
wordcloud(e.dm$word, e.dm$freq, family="STKaiti", rot.per=0.05, scale=c(4,.2), min.freq=0, max.words=Inf, random.order=FALSE, ordered.colors = FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

####################
# Cluster Diagram Based on Cluster Analysis
####################
e.dtm.cluster <- removeSparseTerms(e.tdm, sparse=0.99)
e.dtm.df.cluster <- as.data.frame(inspect(e.dtm.cluster))
e.dtm.df.cluster.scale <- scale(e.dtm.df.cluster)
d <- dist(e.dtm.df.cluster.scale, method = "euclidean") 
fit <- hclust(d, method="ward")
plot(fit, family="Kaiti TC")
# Draw Cluster Dendrogram
png(file = cluster.eng.pic.path, bg="transparent", width=3000, height=1000, res=300)
plot(fit, family="Kaiti TC") 
dev.off()

# textplot(X-axis-NumVector, Y-axis-NumVector, TextVector, ... )
