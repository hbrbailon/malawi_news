#cleaning packages
library (tidyverse) #cleaning; includes dpylr, stringr, tidyr, tibble, ggplot2
library(tidytext) #cleaning strings
library(qdap) #string revisions such as removing patterns
library (lubridate) #time date library
library (readtext) #reading txt files
library (writexl)

#analysis packages
library(quanteda) #nlp library
library(quanteda.textplots) #quanteda plots
library (corpustools) #for corpus/dfms

#plots packages
library (wordcloud) #other word cloud functions
library (RColorBrewer) #for plot colours
library(tm) #under quanteda as well
library(ggplot2)

set.seed(19) # for reproducibility 


### 2018 RELEVANT TWEETS
twit = read.csv (file = 'malawitwitter2018.csv', sep = ',')
twit$text = str_to_lower(twit$text)

twit = twit %>% mutate(water = str_detect(text, "water"), 
                       rain = str_detect(text, c('rain', 'heavy rain+')),
                       flood = str_detect (text, 'flood+'),
                       storm = str_detect (text, 'storm'),
                       flash = str_detect (text, 'flash'),
                       emergency = str_detect (text, 'emergency'),
                       river = str_detect (text,'river'))
#NOT USEFUL words: blockage, drain, flow

water = twit %>% filter (water == 'TRUE') #23 is relevant
rain = twit %>% filter (rain == 'TRUE')
flood = twit %>% filter (flood == 'TRUE')  #28, 30 not
storm = twit %>% filter (storm == 'TRUE') #10 relevant
flash = twit %>% filter (flash == 'TRUE') # 1,2 only
emerg = twit %>% filter (emergency == 'TRUE') #1
river = twit %>% filter (river == 'TRUE')

water = water [-c(1:22,24:61),]
rain = rain [-c(1:4,6,13:17,19:22,25:38,40:42,44,45,47,48,49,50,52:55),]
flood = flood [-c(28,30),]
storm = storm [-c(1:10,12:16),]
flash = flash [-c(3:8),]
emerg = emerg [-c(2:4),]
river = river [-c(3,6,7,10:24),]

keys = rbind (water, rain, flood, storm, flash, emerg,river)

length(unique(keys$text)) #identify unique
duplicated(keys) #identify duplicates
newkeys = keys[!duplicated(keys$text), ] #make new without duplicates

write.csv(newkeys, "keys2018.csv", row.names = FALSE)
keys2018 = read.csv(file = 'keys2018.csv', sep = ',')




### 2019 RELEVANT TWEETS
twit2 = read.csv (file = 'malawitwitter2019.csv', sep = ',')
twit2$text = str_to_lower(twit2$text)

#heavy rain keyword instead of rain is better
twit2 = twit2 %>% mutate(water = str_detect(text, "water"), 
                       rain = str_detect(text, 'heavy rain'),
                       flood = str_detect (text, 'flood+'),
                       storm = str_detect (text, 'storm'),
                       flash = str_detect (text, 'flash'),
                       emergency = str_detect (text, 'emergency'),
                       river = str_detect (text,'river'))

water2 = twit2 %>% filter (water == 'TRUE') #nothing relevant for 2019
rain2 = twit2 %>% filter (rain == 'TRUE') #heavy rains keywords are more relevant
flood2 = twit2 %>% filter (flood == 'TRUE') #all relevant
storm2 = twit2 %>% filter (storm == 'TRUE') #hailstorm is included
flash2 = twit2 %>% filter (flash == 'TRUE')
emerg2 = twit2 %>% filter (emergency == 'TRUE') #nothing relevant
river2 = twit2 %>% filter (river == 'TRUE')

storm2 = storm2 [-c(1:8,10:20,23,25,26,27,29:33),]
flash2 = flash2 [-c(3,4,5,6),]
river2 = river2 [-c(4,5,6,9:15,17:25,27:34),]

keys2 = rbind (rain2, flood2, storm2, flash2, river2)

length(unique(keys2$text)) #identify unique
duplicated(keys2) #identify duplicates
newkeys2 = keys2[!duplicated(keys2$text), ] #make new without duplicates

write.csv(newkeys2, "keys2019.csv", row.names = FALSE)
keys2019 = read.csv(file = 'keys2019.csv', sep = ',')

### EXTRACTING URLS
names (keys2018)
names (keys2019)

urls2018 = subset(keys2018, select = c("entities.urls"))
urls2019 = subset(keys2019, select = c("entities.urls")) 

write.csv(urls2018, "urls2018.csv", row.names = FALSE)
write.csv(urls2019, "urls2019.csv", row.names = FALSE)




#HEADLINES FINAL 2018
df18 = readtext("~/Downloads/malawi_news/data/headlines_dates_2018/*.txt")
df18 = separate(df18, text, into = c("title", "log"), sep = " (?=[^ ]+$)")
df18[c('date', 'time')] = str_split_fixed(df18$log, 'T', 2)
df18 = df18[,-c(3,5)]
df18$date = as.Date(parse_date_time(df18$date, "%Y-%m-%d"))

min(df18$date)
max(df18$date)

table (df18$date)

df18 %>% 
  group_by(date) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) #there are only 10 newspapers left and these may not be viable for comparison with 2019 floods


#HEADLINES FINAL 2019
df19 = readtext("~/Downloads/malawi_news/data/headlines_dates_2019/*.txt")
df19 = separate(df19, text, into = c("title", "log"), sep = " (?=[^ ]+$)")
df19[c('date', 'time')] = str_split_fixed(df19$log, 'T', 2)
df19 = df19[,-c(3,5)]
df19$date = as.Date(parse_date_time(df19$date, "%Y-%m-%d"))

write_xlsx (df19, 'headlines_2019.xlsx')

min(df19$date)
max(df19$date)
table (df19$date)

df19 %>% 
  group_by(date) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

d = df19 %>% arrange(df19$date)
d2 = filter(d, date >= "2019-01-01", date <= "2019-03-01")
d3 = filter(d, date >= "2019-03-01", date <= "2019-04-30")


#flash flood
m24 = d2 %>% filter (grepl('malawi24', doc_id)) %>%
  mutate (newspaper = 'Malawi 24')
ny = d2 %>% filter (grepl('nyasatimes', doc_id))%>%
  mutate (newspaper = 'Nyasa Times')
tm = d2 %>% filter (grepl('times.mw', doc_id)) %>%
  mutate (newspaper = 'MW Times')

d2 = rbind (m24, ny, tm)

#overall flash flood per newspaper
ggplot() + 
  geom_freqpoly(data = d2, aes (date, group = newspaper, colour = newspaper), 
                size = 0.5, alpha = 0.75) + 
  scale_x_date(date_labels = "%b-%d", date_breaks = '5 days',
               limits = as.Date (c('2019-01-01', '2019-03-01'))) +
  scale_color_manual(name="", values = c("#FFD700","#228B22","#9932CC")) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0,5, by = 2)) +
  theme_minimal ()


#river flood
m24_2 = d3 %>% filter (grepl('malawi24', doc_id))%>%
  mutate (newspaper = 'Malawi 24')
ny2 = d3 %>% filter (grepl('nyasatimes', doc_id))%>%
  mutate (newspaper = 'Nyasa Times')
mw2 = d3 %>% filter (grepl('mwnation', doc_id)) %>%
  mutate (newspaper = 'MW Nation')
tm2 = d3 %>% filter (grepl('times.mw', doc_id))%>%
  mutate (newspaper = 'MW Times')

d3 = rbind (m24_2, ny2, tm2, mw2)

#overall river flood
ggplot() + 
  geom_freqpoly(data = d3, aes (date, group = newspaper, colour = newspaper),
                size = 0.5, alpha = 0.75) + 
  scale_x_date(date_labels = "%b-%d", date_breaks = '12 days',
               limits = as.Date (c('2019-03-01', '2019-05-10'))) +
  scale_color_manual(name="", values = c("#FFD700","#228B22",'#FF8C00', "#9932CC")) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0,10, by = 2)) +
  theme_minimal ()

