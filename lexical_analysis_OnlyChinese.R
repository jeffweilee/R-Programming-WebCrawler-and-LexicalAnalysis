library(tm)
#library(data.table)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(RColorBrewer)

####################
# Initialize Path
####################
#wordcloud.cht.pic.path = "mobile01_android_wordcloud_cht.png"
#cluster.cht.pic.path = "mobile01_android_cluster_cht.png"

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
# c.seglst <- segmentCN(mobile01.android.data[,c("mobile01.android.title", "mobile01.android.content")], nature=TRUE)
c.seglst <- segmentCN(mobile01.android.data[,c("mobile01.android.content")], nature=TRUE)
c.corpus <- Corpus(VectorSource(ExtractWords(c.seglst)))
c.corpus <- tm_map(c.corpus, removePunctuation)
c.corpus <- tm_map(c.corpus, removeNumbers)
c.corpus <- tm_map(c.corpus, content_transformer(function(word) gsub(c("是不是|沒什麼|不錯|差別|時候|現在|樓主|安安|你好|小弟|大大"), "", word)))
# Remove all English words
c.corpus <- tm_map(c.corpus, content_transformer(function(word) gsub("[A-Za-z]", "", word)))

####################
# Term Frequency Analysis
####################
# Matrix Row: Terms; Col:Documents
c.tdm = TermDocumentMatrix(c.corpus, control = list(wordLengths = c(2, Inf)))
# Matrix Row: Documents; Col:Terms
#c.tdm = DocumentTermMatrix(c.corpus)
freqency <- sort(rowSums(as.matrix(c.tdm)), decreasing = TRUE)
c.dm <- data.frame(word = names(freqency), freq = freqency)
View(c.dm)

####################
# Wordcloud Picture Based on Term Frequency
####################
# Draw wordcloud based on c.corpus TermDocument; family="Kaiti TC", "STKaiti"
wordcloud(c.corpus, family="STKaiti", rot.per=0.05, scale=c(4,.2), min.freq=0, max.words=Inf, random.order=FALSE, ordered.colors = FALSE, colors=brewer.pal(8, "Dark2"))
# Draw png wordcloud base on word/freq to file
png(file = wordcloud.cht.pic.path, bg="transparent", width=1000, height=1000, res=300)
wordcloud(c.dm$word, c.dm$freq, family="STKaiti", rot.per=0.05, scale=c(4,.2), min.freq=0, max.words=Inf, random.order=FALSE, ordered.colors = FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()

####################
# Cluster Diagram Based on Cluster Analysis
####################
c.dtm.cluster <- removeSparseTerms(c.tdm, sparse=0.99)
c.dtm.df.cluster <- as.data.frame(inspect(c.dtm.cluster))
c.dtm.df.cluster.scale <- scale(c.dtm.df.cluster)
d <- dist(c.dtm.df.cluster.scale, method = "euclidean") 
fit <- hclust(d, method="ward.D")
plot(fit, family="Kaiti TC")
# Draw Cluster Dendrogram
png(file = cluster.cht.pic.path, bg="transparent", width=3000, height=1000, res=300)
plot(fit, family="Kaiti TC")
dev.off()

# textplot(X-axis-NumVector, Y-axis-NumVector, TextVector, ... ) like bubble chart
