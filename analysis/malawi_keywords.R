#cleaning paclages
library (tidyverse) #cleaning; includes dpylr, stringr, tidyr, tibble, ggplot2
library(tidytext) #cleaning strings
library(qdap) #string revisions such as removing patterns
library (lubridate) #time date library
library (readtext) #reading txt files

#analysis packages
library(quanteda) #nlp library
library(quanteda.textplots) #quanteda plots
library (corpustools) #for corpus/dfms

#plots packages
library (wordcloud) #other word cloud functions
library (RColorBrewer) #for plot colours
library(tm) #under quanteda as well

set.seed(19) # for reproducibility 

### 1) 2019 KEYWORDS
keys19 = read.csv (file = '~/Downloads/malawi_news/data/csv_versions/keywords2019.csv')
df19 = readtext("~/Downloads/malawi_news/data/headlines_dates_2019/*.txt") #import header files
df19 = separate(df19, text, into = c("title", "log"), sep = " (?=[^ ]+$)") #splitting text to get dates
df19[c('date', 'time')] = str_split_fixed(df19$log, 'T', 2)
df19 = df19[,-c(3,5)] #keep dates only
df19$date = as.Date(parse_date_time(df19$date, "%Y-%m-%d")) #date as.Date
df19$doc_id<-gsub(".txt","",as.character(df19$doc_id)) #removing .txt extensions to match

keys19 = left_join (keys19, df19) #combine with header files

### 1.1) General keyword cloud

gen = keys19$keyword
doc = Corpus(VectorSource(gen))

doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removeWords, stopwords("english"))
doc = tm_map(doc, removeWords, c('malawi', 'flood','floods', 'said'))

dtm = TermDocumentMatrix(doc) 
matrix = as.matrix(dtm) 
words = sort(rowSums(matrix),decreasing=TRUE) 
gen_freq =  data.frame(word = names(words),freq=words)
clipr::write_clip(gen_freq) #copy to word/excel for analysis

pal = c('#67a9cf', '#02818a', '#016c59') #manually choose colours

wordcloud(words = gen_freq$word, freq = gen_freq$freq, min.freq = 2,           
          max.words=25, scale = c(3, 0.6), random.order=FALSE, rot.per=0,
          colors = pal)



### 1.2) Flash flood keywords
d2 = filter(keys19, date >= "2019-01-01", date <= "2019-02-07")

flash = d2$keyword
doc2 = Corpus(VectorSource(flash))

doc2 = tm_map(doc2, content_transformer(tolower))
doc2 = tm_map(doc2, removeWords, stopwords("english"))
doc2 = tm_map(doc2, removeWords, c('malawi', 'flood','floods', 'said'))

dtm2 = TermDocumentMatrix(doc2) 
matrix2 = as.matrix(dtm2) 
words2 = sort(rowSums(matrix2),decreasing=TRUE) 
flash_freq =  data.frame(word = names(words2),freq=words2)
clipr::write_clip(flash_freq) #copy to word/excel for analysis

pal2 = c('#8c96c6', '#8c6bb1', '#016450') #manually choose colours

wordcloud(words = flash_freq$word, freq = flash_freq$freq, min.freq = 2,           
          max.words=20, scale = c(3, 0.5), random.order=FALSE, rot.per=0,            
          colors=pal2)


### 1.3) River flood keywords
d3 = filter(keys19, date >= "2019-02-08", date <= "2019-04-30")

river = d3$keyword
doc3 = Corpus(VectorSource(river))

doc3 = tm_map(doc3, content_transformer(tolower))
doc3 = tm_map(doc3, removeWords, stopwords("english"))
doc3 = tm_map(doc3, removeWords, c('malawi', 'flood','floods', 'said'))

dtm3 = TermDocumentMatrix(doc3) 
matrix3 = as.matrix(dtm3) 
words3 = sort(rowSums(matrix3),decreasing=TRUE) 
river_freq =  data.frame(word = names(words3),freq=words3)
clipr::write_clip(river_freq) #copy to word/excel for analysis

pal3 = c('#c994c7', '#df65b0', '#238443') #manually choose colours

wordcloud(words = river_freq$word, freq = river_freq$freq, min.freq = 2,           
          max.words=20, scale = c(3, 0.5), random.order=FALSE, rot.per=0,            
          colors = pal3)




