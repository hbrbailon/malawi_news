#cleaning packages
library (tidyverse) #cleaning; includes dpylr, stringr, tidyr, tibble, ggplot2
library(tidytext) #cleaning strings
library(qdap) #string revisions such as removing patterns
library (lubridate) #time date library
library (readtext) #reading txt files

#analysis packages
library(quanteda) #nlp library
library(quanteda.textplots) #quanteda plots
library (corpustools) #for corpus/dfms
library(SentimentAnalysis)
library(quanteda.textplots)
library(quanteda.textstats)
library(qdapDictionaries)


#plots packages
library (wordcloud) #other word cloud functions
library (RColorBrewer) #for plot colours
library(tm) #under quanteda as well
library(ggplot2)

set.seed(19) # for reproducibility 

### 1) 2019 SENTIMENTS
txt = readLines ('~/Downloads/malawi_news/data/sentiment_2019/sentmerged2019.txt')

#insert a blank line before each line having "doc_id" so that the subsequent line is identified as a new record
record_num <- grep("doc_id", txt)
for (i in seq(length(record_num))) {
  txt <- append(txt, "", after = grep("doc_id", txt)[i] - 1)
}
#now read dcf file and convert it to a data frame and re-read to fix classes
d19 = as.data.frame(read.dcf(textConnection(txt)))
write.csv(d19, 'd19.csv', row.names = FALSE) #so manual and annoying, i know
d19 = read.csv (file = 'd19.csv', sep = ',')

#headlines and dates
df19 = readtext("~/Downloads/malawi_news/data/headlines_dates_2019/*.txt")
df19 = separate(df19, text, into = c("title", "log"), sep = " (?=[^ ]+$)")
df19[c('date', 'time')] = str_split_fixed(df19$log, 'T', 2)
df19 = df19[,-c(3,5)]
df19$date = as.Date(parse_date_time(df19$date, "%Y-%m-%d"))

d19 = left_join (df19, d19)

#checking number of negative tagged articles
neg = d19 %>% filter (d19$polarity < 0)
min_range = 0
max_range = 0.5
sum(d19$subjectivity>min_range & d19$subjectivity<max_range)


### 1.1) 2019 average sentiment
mean(d19$polarity)
mean(d19$subjectivity)

### 1.2) flash flood
d1 = filter(d19, date >= "2019-01-01", date <= "2019-03-01")
mean(d1$polarity)
mean(d1$subjectivity)

### 1.3) river flood
d2 = filter(d19, date >= "2019-03-01", date <= "2019-04-30")
mean(d2$polarity)
mean(d2$subjectivity)

### 1.4) malawi24 average sentiment and subjectivity
m19 = d19 %>% filter (grepl('malawi24', doc_id)) 
mean(m19$polarity)
mean(m19$subjectivity)

### 1.5) nyasatimes average sentiment
n19 = d19 %>% filter (grepl('nyasa', doc_id)) 
mean(n19$polarity)
mean(n19$subjectivity)

### 1.6) mwnation average sentiment
mw19 = d19 %>% filter (grepl('mwnation', doc_id)) 
mean(mw19$polarity)
mean(mw19$subjectivity)

### 1.7) times.mw average sentiment
t19 = d19 %>% filter (grepl('times.mw', doc_id)) 
mean(t19$polarity)
mean(t19$subjectivity)


### 2) Validation
sample_ids = sample(docnames(d19), size = 50)

## convert quanteda corpus to data.frame
d19 %>%
  filter(doc_id %in% sample_ids) %>% 
  mutate(manual_sentiment="") %>%
  write_csv("to_code.csv")

validation = read.csv("/Users/hannahbailon/Downloads/malawi_news/data/csv_versions/to_code.csv", sep = ';')

cor.test(validation$manual_sentiment, validation$polarity)

validation = validation %>% 
  mutate(sent_nom = cut(polarity, breaks=c(-1, -0.1, 0, 1), labels=c("-", "0", "+")))
cm = table(manual = validation$manual_sentiment, dictionary = validation$sent_nom)
cm

sum(diag(cm)) / sum(cm)
