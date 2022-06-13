#cleaning packages 
library (tidyverse) #cleaning; includes dpylr, stringr, tidyr, tibble, ggplot2
library(tidytext) #cleaning strings
library(qdap) #string revisions such as removing patterns
library (lubridate) #time date library
library (readtext) #reading txt files
library (stringr)
library(writexl)

#analysis packages
library(quanteda) #nlp library
library(quanteda.textplots) #quanteda plots
library (corpustools) #for corpus/dfms

#plots packages
library (wordcloud) #other word cloud functions
library (RColorBrewer) #for plot colours
library(tm) #under quanteda as well

#maps packages
library(sf)
library(raster)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package
library(viridis) #scale options
library(ggrepel)

set.seed(19) # for reproducibility 

### 2019 Entities
### 1) Initial cleaning and loading data
d19 = read.csv(file = '~/Downloads/malawi_news/data/csv_versions/uniqueEntities_2019_cleaned.csv', sep = ';', fill=TRUE)
d19 = d19[,-(5:6)] #remove unneccessary columns
d19[d19 == ""] = NA #placing NA
d19 = d19 %>%na.omit() #removing NAs

#incorporating dates for the flash vs river filter
df19 = readtext("~/Downloads/malawi_news/data/headlines_dates_2019/*.txt")
df19 = separate(df19, text, into = c("title", "log"), sep = " (?=[^ ]+$)")
df19[c('date', 'time')] = str_split_fixed(df19$log, 'T', 2)
df19 = df19[,-c(3,5)]
df19$date = as.Date(parse_date_time(df19$date, "%Y-%m-%d"))
df19$doc_id<-gsub(".txt","",as.character(df19$doc_id)) #removing .txt extensions to match

#new data frame
new = left_join (d19, df19)
new = new %>%na.omit() #removing NAs

### 1.1) Filtering Entities and Labels for Cleaning

#ordinal
#ordinal = new %>% filter (label == 'ORDINAL') #not needed and it's hard without context

#quantity
#quant = new %>% filter (label == 'QUANTITY') #can be used for economic damages

#percent
#percent = new %>% filter (label == 'PERCENT') #what's the use?

#dates
#date = new %>% filter (label == 'DATE') #might not need this for analysis?

#time
#time = new %>% filter (label == 'TIME')


#cardinal
#cardinal = new %>% filter (label == 'CARDINAL')
#cardinal = cardinal [-c(12,409,410,448,449),]

#money1 = cardinal %>% filter (grepl("K|MK", entity))#money from cardinal; to be transferred to money
#cardinal = anti_join(cardinal, money1) #new cardinal df; but still for cleaning
#money1$label = recode (money1$label, 'CARDINAL' = 'MONEY')
#money1$entity = str_replace_all (money1$entity, c('MK290 million' = 'K290 million'))

#cardinal$entity = str_to_lower(cardinal$entity)
#cardinal$entity = str_replace_all (cardinal$entity, c(" " = "", 'about' = '', 'approximately' = '', 'morethan' = '', 'over' ='', 'atleast' = '','almost' = '', 'anestimated' = '','nearly' = '','upto' = ''))

#event
event = new %>% filter (label == 'EVENT') #not good tagged
gpe1 = event %>% filter (entity == "Ndirande")
event = anti_join(event, gpe1)
gpe1$label = recode(gpe1$label, 'EVENT' = "GPE") #to be transferred to gpe

org1 = event %>% filter (entity == 'Marine Rescue Teams')
event = anti_join(event, org1)
org1$label = recode (org1$label, 'EVENT' = 'ORG') #transfer to org
recode1 = event %>% filter (entity == 'the Social Cash Transfer and the Decent and Affordable Housing Programme')

event = event [-1,]

#gpe
gpe = new %>% filter (label == 'GPE') 
recode2 = gpe %>% filter (entity %in% c('DC', 'AU.Otengo', 'Lifebuoy', 
                                      'Danish', "‘On", 'RC', '“UNFPA', 
                                      'MT', "respond’", 'K1.2', 'k1.2', 
                                      'Wotchi Mose 17', "tarpaulin'", 
                                      'tarpaulin', 'Tapsoba',
                                      'Sugar', 'Prisca Chisala', "Plem", 'El Nino',
                                      'Group Village', 'Dausi', 'Mutharika', 
                                      'Bona', 'DfID', 'Black', 'Emanuelle', 
                                      'Abida', 'Airtel', 'Chii Il', 'Chilima', 'Emmanuelle', 'Kaliati',
                                      'Chisi Isl', 'the Chisi Isl'))

gpe = anti_join(gpe, recode2) 

#location
loc = new %>% filter (label == 'LOC') 
recode3 = loc %>% filter (entity %in% c('Group Village', 'a Tropical Cyclone', 
                                        'BPC', 'Embassy', 'Capital Hill', 'Ameer Jakhura'))
loc = anti_join(loc, recode3)
loc$label = recode(loc$label, 'LOC' = "GPE")

gpe = rbind (gpe, loc)


#law
law = new %>% filter (label == 'LAW') #all wrongly tagged
#money2 = law %>% filter (grepl('K', entity))
#money2$label = recode (money2$label, 'LAW' = 'MONEY') #to be transferred to money
org2 = law %>% filter (grepl('Electricity Generation Company', entity))
org2$label = recode (org2$label, 'LAW' = 'ORG') #to be transferred to money

#money
#money = new %>% filter (label == 'MONEY') #all tagged right but not detecting Malawi currency
#money$entity = str_replace_all (money$entity, c('3million' = '$3 million',  'up to £6 million' = '£6 million', '\\w*k' ='K','US\\w*' = '','approximately' = '',  'up to*' = '','only' = '','about' = ''))
                                        

#norp: Nationalities or religious or political groups
norp = new %>% filter (label == 'NORP')
recode4 = norp %>% filter (entity %in% c('“Plans', 'K2500', 'Khonjeni', 
                                        'Nsanje', "Anati", 'Tropical Cyclone Idai', 'Central and Southern', 
                                        'Southern', "Malawi", 'Cyclone Idai', 
                                        'Unitrans', 'Mozambique', 'Black Missionaries'))
norp = anti_join(norp, recode4) 

#org
org = new %>% filter (label == 'ORG')
recode5 = org %>% filter (entity %in% c('“Out', 'Kanache','Sant’ Egidio', 'Malawi', 'K20', 'Soya', 
                                        'K10', 'K38', 'Malawi VP', 
                                        'Gender', 'Lilongwe City', 
                                        'cholera', 'Biwi', 'Emmanuella Bwekere', 
                                        'Nsanje', 'Flames', 'Chikwawa', 'Mpama', 
                                        'house', 'Ntchena', 'Nkalo', 
                                        'DC) Malango Bottomani', 'Chikuse', 
                                        'the Social Cash Transfer', 'Paramount',
                                        'the State of Disaster', 'Mashiba', 'Mussa',
                                        'Mwanza', 'Dedza', 'Kaleso', 
                                        'Mileme Primary School', 'Nchalo Estate Marc Pousson',
                                        'Total', 'Mtambo', 'a Health Surveillance Assistant',
                                        'Constable Foster Benjamin', 'Thyolo',
                                        'Patuma Welemu', 'St Joseph’s Mission Hospital',
                                        'Koche Hospital', 'Chiradzulu District Hospital',
                                        'Elizabeth Hospital', 'Cement', 'Phiri',
                                        'Neno', 'House', 'Koche Health Centre',
                                        'Kwacha', 'Operations', 'Chidzanja', 'Ndirande',
                                        'Malawi’s', 'K50', 'Blantyre', 'State of Disaster',
                                        'K15', 'K25', 'Club', 
                                        'Sub/TA Kalimanjira and Acting District',
                                        'Chipasula Kaliyeka and', '“Thank', 'Botswana',
                                        'Chipasula Secondary School', 'Ireen Ngwira',
                                        'Balaka', 'Tengani', 
                                        'the east bank of the Shire River',
                                        'Wilson Moleni', 'Mana', 'Otengo',
                                        'Displaced Persons', 'Chakwera',
                                        'the Inter-Tropical Convergence Zone',
                                        'Msungama', 'Kawale-Nchesi', 'Dausi', 
                                        'Grey Natoto Mphithi', 'Non Food', 
                                        'Chimulirenj', 'Malawi  Sergey V. Bakharev', 
                                        'the Republics of Zimbabwe', 
                                        'Ambassadorof the Russian Federation',
                                        'Perwais', 'Chakwera-Mia', 'Kunkuyu',
                                        'KalindeVillage', 'Adams Chavula', 
                                        'Falakeza', 'State', 'K13', 'Bank', 'Kaliati',
                                        'Inter-Tropical Convergence Zone', 'Country', 
                                        'Chide', 'Clemence', 
                                        'Khulubvi Primary School Head', 'Manyowe',
                                        'Queen Elizabeth Central Hospital', 
                                        'Tate Private Secondary School', 'Nkhokwe',
                                        'Karonga', 'Kalambule', 'Olipa Moses',
                                        'Tsangano-Proper', 'Kachale.', 
                                        'State of National Disaster', 'K8.7',
                                        'the Tropical Cyclone', 'Mwanguku.', 'Nthole',
                                        'Kamdar', 'Nsanje Boma', ' Mozambican Channel', 
                                        'Mozambique Channel', ' Mozambique Channel', 'Mozambique',
                                        'Tropical Cyclone Kenneth','Tropical Storm', 
                                        'Lindiwe Chide', 'Bachelor of Accountancy', 'Fund', 'La Caverna','Lancha','Mafale 2 Camp','Magombo Camp',
                                        'Idai', 'K36', 'Malawi a State of Emergency', 'Manifesto',
                                        'Kanyenda Community Day Secondary School (CDSS',
                                        'Kanyenda Primary School', 'Kapichira Power Stations', 'Kawale-Mchesi',
                                        'Khulubvi Primary School',
                                        'Kadantot','Kaliwo', 'Kanyoza', 'Kipandura', 'Lidia Fadweck','Lundu', 'Malata', 'MargretNkhalamba', 'Mkandawire','Nyathi',
                                        'Kamuzu', 'Kamzota', 'Likangala','Lower Shire Valley', 'Machinga',
                                        'Machinjiri', 'Nthondo', 'Nyachikadza  Tengani', 'People Republic of China',
                                        'Nyachikadza and Mankhokwe',
                                        'Nyachilenda',
                                        'Kachere','Kalongolera', 'Kamanga','Katenga', 'Katsalambande'
                                        ))
org = anti_join(org, recode5)

#persons
pers = new %>% filter (label == 'PERSON')
recode6 = pers %>% filter (entity %in% c('Malawi', 'Ntowe', 'Illovo Malawi', 'Chikwawa District', 
                                         'Chikwawa', 'Blantyre', 'Nyachikadza', 'Kenya 0-0', 'Jumbe', 'Mpama', 
                                         'St Joseph’s Hospital', 'Qech', 'maize', 'Chitera', 'Molele', 'Manda', 'Zomba',
                                         'Mulanje', 'GVH Makombe', 'IDA', 'Sant’ Egidio', 'Ramadhan',
                                         'Malawi24', 'Malawi Red Cross', 'K20', '“On behalf', 
                                         'Lilongwe', 'Ufa', 'Bangula ADMARC Camp', 
                                         'K223', 'K1', 'K150', 'Dodma', 'Christian Aid', 'Medrum Camps',
                                         'Maize', 'Fisp', 'Shire', 'Shaker', 'K2', 'K32', 'K32 million', 'K18.7 million',
                                         'Sana', 'Sana Cash', 'Nsanje', 
                                         'Nsanje South', 'Chipasula',
                                         'Chikwawa Nkombedzi Constituency',
                                         'Malemia', 'Kilupula', 'Chisala',
                                         'Mkanda', 'IDAI', 'Mlolo', 'Egenco',
                                         'Unilever Malawi', 'Phalombe', 
                                         'Cyclone Kenneth', 'Airtel Malawi', 
                                         'K3 Million','Kalima', 'Kalimba',
                                         'Kalimbo', 'Fao', 'Kwataine', 'Machinga', 
                                         'Malata Subsidy', 'Malawi Red Cross',
                                         'Southern Malawi', 'Nsanje Districts',
                                         'mosquito', 'Mosquito', 'Mibawa', 'Mibawa Hall',
                                         'Mibawa Multi', 'Malawi Red Cross', 'Nkhokwe', 'Nsanje Boma',
                                         'Village$', 'Soap', 'Subsidy Programme', 'Tengani', 'Thabwa',
                                         'Black Missionaries'))
pers = anti_join(pers, recode6)

#fac: Buildings, airports, highways, bridges, etc
fac = new %>% filter (label == 'FAC') 
recode7 = fac %>% filter (entity %in% c('Idai', 'Ufa 20', 'K1 500', 
                                        'Bereu in', 'Group Village','Traditional Authority', 'TA Katuli'))

fac = anti_join(fac, recode7)

#product
prod = new %>% filter (label == 'PRODUCT')
recode8 = prod %>% filter (entity %in% c("Idai", 'Cyclone Idai', 'the Shire Vallay',
                                         'Makhanga', 'Airtel','Chikwawa', 'Mwalija',
                                         'Chimombo', 'Sawa'))


### 1.2) Recoding mistagged variables
recodes = rbind (recode1, recode2, recode3, recode4, recode5, recode6, recode7, recode8)

#gpe
gpe2 = recodes %>% filter (entity %in% c('Malawi', 'Nsanje', 'Biwi', 'Lilongwe City', 
                                         'Chikwawa', 'Chikwawa District', 'Blantyre', 
                                         'Jumbe', 'Lilongwe', 'GVH Makombe', 
                                         'Zomba', 'Mpama', 'Ntowe', 
                                         'Nyachikadza', 'Chitera', 'Manda', 
                                         'Mulanje', 'Bereu in', 'Bona', 
                                         'Capital Hill', 'Mozambique', 
                                         'Malawi’s', 'Southern', 'Kaleso', 'Chidzanja',
                                         'Ndirande', 'Thyolo', 
                                         'the east bank of the Shire River', 
                                         'Chipasula Kaliyeka and',
                                         'Botswana', 'Balaka',
                                         'the Republics of Zimbabwe', 'Perwais', 
                                         'KalindeVillage', 'Nkhokwe', 'Manyowe',
                                         'Karonga', 'Kalimbo', 'Chipasula', 
                                         'Malemia', 'Kenya 0-0', 'Molele', 'Chisala',
                                         'Mkanda', 'Mlolo', 'Machinga',
                                         'Phalombe', 'Nsanje South', 'Southern Malawi',
                                         'Shire', 'Kwataine', 'Khonjeni', 'Kwacha',
                                         'Kamdar', 'Kalambule', 'Kilupula',
                                         'Kalima', 'Kalimba', 'Neno',
                                         'Tsangano-Proper', 'Mtambo', 'Ntchena', 
                                         'Nkalo', 'Central and Southern', 'Mwanza',
                                         'Chikuse', 'Mashiba', 'Mwanza', 
                                         'Nsanje Districts', 'Nsanje Boma', ' Mozambican Channel', 
                                         'Mozambique Channel', ' Mozambique Channel', 'the Shire Vallay',
                                         'Makhanga','Chikwawa', 'Mwalija',
                                         'Chimombo', 'Kanache', 'TA Katuli', 'Nkhokwe', 
                                         'Nsanje Boma', 'Tengani', 'Thabwa', 'Kamuzu', 'Kamzota', 'Likangala','Lower Shire Valley', 'Machinga',
                                         'Machinjiri', 'Nthondo', 'Nyachikadza  Tengani', 'People Republic of China',
                                         'Nyachikadza and Mankhokwe',
                                         'Nyachilenda',
                                         'Kachere','Kalongolera', 'Kamanga','Katenga', 'Katsalambande'))

recodes = anti_join (recodes, gpe2)

gpe2$label = recode (gpe2$label, 'ORG' = 'GPE', 'PERSON' = 'GPE', 
                     'FAC' = 'GPE', 'LOC' = 'GPE', 'NORP' = 'GPE', 'PRODUCT' = 'GPE') 


gpe = rbind(gpe, gpe2)

gpe$entity = str_replace_all (gpe$entity, c('the Republic of Malawi' = 'Malawi', 
                                            'Lilongwe\\s*' = 'Lilongwe', 
                                            'LilongweCity' = 'Lilongwe', 
                                            '\\“Lilongwe' ='Lilongwe',
                                            'Blantyre\\s*' = 'Blantyre',
                                            'BlantyreCity*' = 'Blantyre', 
                                            'Blantyre\\w*' = 'Blantyre',
                                            'Chikwawa\\s*' = 'Chikwawa',
                                            'Mangochi\\s*' = 'Mangochi',
                                            'Balaka\\s*' = 'Balaka',
                                            'Nsanje\\s*' = 'Nsanje',
                                            'Ntchisi\\s*' = 'Ntchisi',
                                            'InTizola Village' = 'Tizola Village',
                                            'the Russian Federation' = 'Russia',
                                            'the United States\\s*\\w*' = 'USA',
                                            'The United States\\s*\\w*' = 'USA',
                                            'America' = 'USA',
                                            'USA\\s*\\w*' = 'USA',
                                            'the Republic of South Africa' = 'South Afric',
                                            'United Kingdom\\W*' = 'UK',
                                            'UK\\w*' = 'UK',
                                            'United Republic of Tanzania' = 'Tanzania',
                                            'the Tanzania' = 'Tanzania',
                                            'the United Republic of Tanzania' = 'Tanzania',
                                            'the Niassa Province' = 'Niassa',
                                            'District' = '',
                                            'Township' = '',
                                            ' Malawi' = 'Malawi',
                                            'Chikhwawa' = 'Chikwawa',
                                            'Nsanje ' = 'Nsanje',
                                            'Nsanje South' = 'Nsanje',
                                            'Chipaula' = 'Chipasula',
                                            ' Makombe' = 'Makombe',
                                            'Chipasula ' = 'Chipasula',
                                            'Chipaula Kaliyeka and'= 'Chipasula',
                                            'Lilongwe ' = 'Lilongwe',
                                            'east bank$'= 'East Bank',
                                            'South' = 'Southern Region',
                                            'the Southern Region' = 'Southern Region',
                                            'Nsinje Village' = 'Nsanje',
                                            'Village' = '',
                                            'village' = '',
                                            'Lower ShireBasin' = 'Lower Shire',
                                            'the Shire\\s*' = 'Lower Shire',
                                            'the Lower Shire\\s*' = 'Lower Shire',
                                            'the Lower Shire\\w*' = 'Lower Shire',
                                            'the Indian Ocean' = 'Indian Ocean', 'District' = '',
                                            'City' = '',
                                            'Vallay' = 'Valley',
                                            '’s' = '',
                                            'the east bank of the'= '',
                                            'the Republics of ' = '',
                                            'Village' = '',
                                            '-Proper' = '',
                                            '0-0' = '',
                                            'GVH' = '',
                                            'Nsanje South$' = 'Nsanje',
                                            'Nsanje Southern Region$' = 'Nsanje',
                                            'Nsanje Districts$' = 'Nsanje',
                                            'Nsanje Boma$' = 'Nsanje',
                                            'Nanje' = 'Nsanje',
                                            ' Malawi$' = 'Malawi',
                                            'Chikhwawa' = 'Chikwawa',
                                            'Nsanje ' = 'Nsanje',
                                            '^South$' = 'Southern Region',
                                            'the Southern Region$' = 'Southern Region',
                                            'Chipaula' = 'Chipasula',
                                            ' Makombe' = 'Makombe',
                                            'Chipasula ' = 'Chipasula',
                                            'Chipaula Kaliyeka and'= 'Chipasula',
                                            'east bank'= 'East Bank',
                                            'Lilongwe ' = 'Lilongwe',
                                            'and' = '',
                                            'Mozambique Channel' = 'Mozambique', 
                                            ' Mozambique Channel' = 'Mozambique',
                                            ' Mozambican Channel' = 'Mozambique',
                                            'the east bank of the Shire River' = 'East Bank',
                                            'the Republics of Zimbabwe' = 'Zimbabwe',
                                            'the Republic of Zimbabwe' = 'Zimbabwe',
                                            'the Shire Valley' = 'Shire Valley',
                                            'TA Katuli' = 'Katuli',
                                            'the eat bank of the Shire River$' = 'East Bank',
                                            'Eat$' = 'East Bank',
                                            'Eat Bank$' = 'East Bank',
                                            'Chika ' = 'Chika',
                                            'ChipasulaKaliyeka ' = 'Chipasula',
                                            'Bereu in' = 'Bereu',
                                            '^Lower Shire Valley$' = 'Lower Shire', 
                                         'Nyachikadza  Tengani' = 'Nyachikadza', '^People Republic of China$' = 'China',
                                         'Nyachikadza and Mankhokwe' = 'Nyachikadza'))



#org
org3 = recodes %>% filter (entity %in% c('DfID', '“UNFPA', 
                                        'Dodma', 'Christian Aid', 'Illovo Malawi', 
                                        'Ufa', 'Ufa 20', 'BPC', 
                                        'Embassy', 'Plem', 'Malawi24', 
                                        'Traditional Authority', 'Egenco', 'Sana',
                                        'Sana Cash', 'Airtel Malawi', 
                                        'Unilever Malawi', 
                                        'Chikwawa Nkombedzi Constituency', 'Mana', 
                                        'Fao', 'Unitrans', 'Mana', 'Malawi Red Cross', 'Airtel',
                                        'Black Missionaries'))
recodes = anti_join (recodes, org3)
org3$label = recode (org3$label, 'GPE' = 'ORG', 'PERSON' = 'ORG', 
                     'FAC' = 'ORG', 'LOC' = 'ORG', 'NORP' = 'ORG', 'PRODUCT' = 'ORG') 

org = rbind (org, org1, org2, org3)

org$entity = str_replace_all (org$entity, c('’s' = '', 
                                            'Public Relations Officer' = '', 
                                            'Dodma' ='DoDMA',
                                            'DODMA' = 'DoDMA',
                                            'The Department of Disaster Management Affairs' = 'DoDMA', 
                                            'the Department of Disaster Management Affairs' = 'DoDMA',
                                            'Department of Disaster Management Affairs' = 'DoDMA',
                                            ' Department of Disaster and Management Affairs' = 'DoDMA',
                                            'Department of Disaster and Management Affairs' = 'DoDMA',
                                            'Disaster Management Affairs' = 'DoDMA',
                                            'DoDMA Department' = 'DoDMA',
                                            ' Department DoDMA' = 'DoDMA',
                                            'Department of Disaster and Management \\(DoDMA' = 'DoDMA',
                                            'Department of Disaster Preparedness and Management Affairs \\(DoDMA' = 'DoDMA',
                                            'Plc' = '',
                                            'AU' = 'African Union',
                                            'The' = '',
                                            'the' = '',
                                            'LilongweCity Council' = 'Lilongwe City Council',
                                            'WORLD Vision' = 'World Vision',
                                            'World Vision Malawi' = 'World Vision',
                                            'World Vision District Programs' = 'World Vision District Programs',
                                            'Illovo Sugar Malawi\\W*' = 'Illovo',
                                            '“ US Government' = 'US Government',
                                            'DPP\\w*\\W*' = 'Democratic Progressive Party',
                                            'DPP' = 'Democratic Progressive Party',
                                            'UTM$' = 'UTM Party',
                                            'initiative' = '',
                                            'Electricity Generation Company \\(Egenco' = 'Electricity Generation Company',
                                            'every Traditional Authority \\(TA' = 'Traditional Authority',
                                            'DfID' = 'DFID',
                                            'NBM\\) plc' = 'NBM',
                                            '“Lilongwe City Council' = 'Lilongwe City Council',
                                            'Malawi Red Cross Society' = 'Malawi Red Cross',
                                            'MCP Shadow$' = 'MCP',
                                            ' Department of Climate Change and Meteological Services' = 'Department of Climate Change and Meteorological Services',
                                            ' Department of Climate Change and Meteorological Services \\(DCCMS' = 'Department of Climate Change and Meteorological Services',
                                            ' Department of Climate Change Management and Meteorological Services' = 'Department of Climate Change and Meteorological Services',
                                            ' Department of Climate Change and Meteorological Services' = 'Department of Climate Change and Meteorological Services',
                                            ' Department of Meteorology and Climate Change' = 'Department of Climate Change and Meteorological Services',
                                            ' Meteorological and Climate Change Department'= 'Department of Climate Change and Meteorological Services',
                                            '320MW. Department of Climate Change and Meteorological Services' = 'Department of Climate Change and Meteorological Services',
                                            'Blantyre Press Club' =  'BPC',
                                            'Christian Aid Country' = 'Christrian Aid',
                                            'DoDMA Commissioner' = 'DoDMA',
                                            'ECG Church' = 'ECG', 
                                            'ECG Malawi' = 'ECG',
                                            'Group Villages Chaweza' = 'Group Village Chaweza',
                                            'Habitat' = 'Habitat for Humanity International',
                                            'Illovo Malawi' = 'Illovo',
                                            'DoDMA ' = 'DoDMA',
                                            "Malawi Congress Party" = 'MCP', 'DfID' = 'DFID',
                                            '“UNFPA' = 'UNFPA',
                                            'Ufa 20' = 'UFA',
                                            'Ufa' = 'UFA',
                                            'Dodma' = 'DoDMA', 'Sana' = 'Sana Cash',
                                            "Malawi Congress Party" = 'MCP',
                                            'Malawi Red Cross' = "Malawi Red Cross"))

#pers
pers2 = recodes %>% filter (entity %in% c('Mutharika','Chilima',  'Prisca Chisala', 'Wotchi Mose 17',
                                          'Malawi VP', 'DC', 'Tapsoba', 'AU.Otengo',
                                          'Chakwera', 'Emmanuella Bwekere',
                                          'Patuma Welemu', 
                                          'Otengo', 'Chakwera-Mia','Chakwera',
                                          'Dausi', 'Phiri', 'Nkhokwe', 'Ireen Ngwira',
                                          'Tengani', 'Grey Natoto Mphithi',
                                          'Ambassadorof the Russian Federation', 
                                          'Chimulirenj', 'Kunkuyu',
                                          'Falakeza', 'Kaliati', 
                                          'Khulubvi Primary School Head', 
                                          'Kachale.', 'Nchalo Estate Marc Pousson',
                                          'Paramount', 'Constable Foster Benjamin',
                                          'a Health Surveillance Assistant',
                                          'Mussa',
                                          'Dedza', 'Mwanguku.', 'Nthole', 'Wilson Moleni',
                                          'Olipa Moses', 'Adams Chavula',
                                          'Clemence', 'Anati', 'Msungama', 
                                          'Malawi  Sergey V. Bakharev', 'Chide', 'Ameer Jakhura',
                                          'Lindiwe Chide', 'Abida', 'Kadantot','Kaliwo', 
                                          'Kanyoza', 'Kipandura', 'Lidia Fadweck','Lundu', 
                                          'Malata', 'MargretNkhalamba', 'Mkandawire','Nyathi'))
recodes = anti_join (recodes, pers2)
pers2$label = recode (pers2$label, 'GPE' = 'PERSON', 'ORG' = 'PERSON', 'NORP' = 'PERSON', 
                      'LOC' = 'PERSON') 

pers = rbind(pers, pers2)

pers$entity = str_replace_all (pers$entity, c('^Kaliati' = 'Patricia Kaliati', 
                                              '^Chakwera' = 'Lazarus Chakwera', 
                                              '^Mkandawire' = 'Isaac Mkandawire',
                                              '^Mutharika' = 'Peter Mutharika',
                                              '^Lundu$' = 'Paramount Lundu',
                                              '^Dausi' = 'Nicholas Dausi',
                                              'Jappie' = 'Jappie Mhango',
                                              'Arthur' = '',
                                              'Said' = '',
                                              '^Mia' = 'Sidik Mia',
                                              '^Mauwa' = 'Dorothy Mauwa',
                                              '^Nhlane' = 'Lusizi Nhlane',
                                              '^Phiri' = 'Ben Phiri',
                                              '^Mchacha' = 'Charles Mchacha',
                                              '^Khamula' = 'Chipiliro Khamula',
                                              'Ackson Kalaile Banda' = 'Ackson Kalaile',
                                              '^Kalaile' = 'Ackson Kalaile',
                                              '^Guterres' = 'António Guterres',
                                              '^Mumderanji' = 'Mavuto Mumderanji',
                                              '^Xiusheng' = 'Wang Xiusheng',
                                              '^Botolo' = 'Ben Botolo',
                                              '^Moleni' = 'Wilson Moleni',
                                              '^Duwa' = 'Aubrey Duwa',
                                              '^Chavula' = 'Rengard Chavula',
                                              '^Jiya' = 'Harold Jiya',
                                              '^Changole' = 'Levison Changole',
                                              'Pope' = 'Pope Francis',
                                              '^Francis' = 'Pope Francis',
                                              '^Madula' = 'Sam Madula',
                                              'Recovery' = '',
                                              '^Kachale' = 'Harris Kachale',
                                              'Prophet TB Joshua’s'= 'Prophet TB Joshua',
                                              'prophet T. B Joshua' = 'Prophet TB Joshua',
                                              '^Joshua' = 'Prophet TB Joshua',
                                              '^Banda' = 'Jimmy Banda',
                                              '^Mazoni' = 'William Mazoni',
                                              '^Fatchi' = 'Ceaser Fatchi',
                                              '^Bamusi' = 'Gloria Bamusi',
                                              '^Gadi' = 'Patricia Gadi',
                                              '^Chathyoka' = 'Dorrah Chathyoka',
                                              '^Gondwe'= 'Goodall Gondwe',
                                              '^Jakhura' = 'Aamir Jakhura',
                                              '^Bamusi' = 'Mavuto Bamusi',
                                              '^Chigalu' = 'Hastings Chigalu',
                                              '^Chipoya' = 'Esther Chipoya',
                                              '^Kimu' = 'Rhoda Kimu',
                                              'likuni phala' = 'Likuni Phala',
                                              '^Likuni' = 'Likuni Phala',
                                              'Chikwawa Francis Kadzokota' = 'Francis Kadzokota',
                                              ' Peter Mutharika' = 'Peter Mutharika',
                                              ' Fatchi' = 'Ceaser Fatchi',
                                              '^Chikwama' = 'Chikwawa District Commissioner',
                                              '17' = '',
                                              'AU.Otengo' = 'Otengo',
                                              'Paramount' = 'Paramount Lundu',
                                              'Anati' = 'Anati Jere',
                                              'Mussa' = 'Henry Mussa',
                                              '\\.' = '',
                                              '^Dausi' = 'Nicholas Dausi',
                                              '^Kachale' = 'Harris Kachale',
                                              'Phiri' = 'Ben Phiri',
                                              '^Mutharika' = 'Peter Mutharika',
                                              '^Chilima$' = 'Saulos Chilima',
                                               '^Abida$' = 'Sidik Mia', 'Kadantot' = 'Joseph Kadantot',
                                              'Kaliwo' = 'Gustave Kaliwo', 
                                              'Kanyoza' = 'Charles Kanyoza', 'Kipandura' = 'Tennyson Kipandura', 
                                              'Malata' = 'Lucky Malata', 'MargretNkhalamba' = 'Margret Nkhalamba', 
                                              'Mkandawire' = 'Thandika Mkandawire',
                                              'Nyathi'= 'Hazel Nyathi'))


#event
event1 = recodes %>% filter (entity %in% c('Sant’ Egidio', 'Ramadhan', 
                                           'Idai', 'a Tropical Cyclone', 
                                           'El Nino', 
                                           'Cyclone Idai',
                                           'Tropical Cyclone Idai',
                                           'the Tropical Cyclone', 'IDAI', 
                                           'Cyclone Kenneth',
                                           'Sant’ Egidio', 'Tropical Cyclone Kenneth','Tropical Storm'))
recodes = anti_join (recodes, event1) 
event1$label = recode (event1$label, 'GPE' = 'EVENT', 'PERSON' = 'EVENT', 
                       'LOC' = 'EVENT', 'FAC' = 'EVENT', 'NORP' = 'EVENT', 
                       'ORG' = 'EVENT', 'PRODUCT' = 'EVENT') 

event1$entity = str_replace_all (event1$entity, c('^IDAI' = 'Cyclone Idai',
                                                '^Idai' = 'Cyclone Idai',
                                                'Tropical Cyclone Idai' = 'Cyclone Idai',
                                                'the Tropical Cyclone'= 'Cyclone Idai',
                                                'Tropical Cyclone Kenneth' = 'Cyclone Kenneth',
                                                'a Tropical Cyclone' = 'Cyclone Idai'))

event = rbind (event, event1)

#fac
fac1 = recodes %>% filter (entity %in% c('St Joseph’s Hospital', 
                                         'Bangula ADMARC Camp', 'Medrum Camps', 
                                         'Kanyenda CDSS', 
                                         'the CDSS.Ntchisi', 
                                         'Kawale-Nchesi',
                                         'Chiradzulu District Hospital',
                                         'Elizabeth Hospital',
                                         'Koche Health Centre', 
                                         'Chipasula Secondary School', 
                                         'Tate Private Secondary School',
                                         'Queen Elizabeth Central Hospital',
                                         'Medrum Camps',
                                         'Koche Hospital',
                                         'St Joseph’s Mission Hospital',
                                         'Mileme Primary School',
                                         'Qech', 'Chileka Airport', 'Kapichira Power Stations',
                                         'Kanyenda Community Day Secondary School (CDSS', 'Kanyenda Primary School',
                                         'Kawale-Mchesi', 'Khulubvi Primary School'))
recodes = anti_join (recodes, fac1) 
fac1$label = recode (fac1$label, 'PERSON' = 'FAC', 'ORG' = 'FAC') 

fac = rbind(fac, fac1)

fac$entity = str_replace_all (fac$entity, c('^Qech' = 'Queen Elizabeth Central Hospital',
                                              'St Joseph’s Mission Hospital' = 'St Joseph’s Hospital',
                                              'the CDSS.Ntchisi' = 'Kanyenda CDSS',
                                              'Elizabeth Hospital' = 'Queen Elizabeth Central Hospital', 
                                            'Chileka International Airport' = 'Chileka Airport',
                                            'Kawale Bridge' = 'Kawale-Nchesi',
                                            'the Mchesi Bridge' = 'Kawale-Nchesi',
                                            'the CDSS.Ntchisi' = 'Kanyenda CDSS',
                                            'the Mchesi Bridge' = 'Kawale-Nchesi',
                                            'Kanyenda Community Day Secondary School \\(CDSS' = 'Kanyeda CDSS',
                                            'Kawale-Mchesi' = 'Kawale-Nchesi'))

#norp
norp1 = recodes %>% filter (entity %in% c('Danish', 'Black Missionaries'))
recodes = anti_join (recodes, norp1)
norp1$label = recode (norp1$label, 'GPE' = 'NORP', 'PERSON' = 'NORP') 

norp = rbind (norp, norp1)

norp$entity = str_replace_all (norp$entity, c('Malawian$' = 'Malawians'))
norp = norp [-43,] #removing computers

#money
#money3 = recodes %>% filter (grepl('^K', entity))
#recodes = anti_join (recodes, money3)
#money3$label = recode (money3$label, 'ORG' = 'MONEY', 'GPE' = 'MONEY', 'FAC' = 'MONEY', 'PERSON' = 'MONEY', 'NORP' = 'MONEY') 
#money3$entity = str_replace_all (money3$entity, c('Million' = 'million',
#                                                  '\\K1.2$*' = 'K1.2 million',
#                                                  '\\K13$*' = 'K13 million',
#                                                  '\\K8.7*' = 'K8.7 million',
#                                                  '\\K10$' = 'K10 million',
#                                                  '\\K50$' = 'K50 million',
#                                                  '\\K15$' = 'K15 million',
#                                                  '\\K20$' = 'K20 million',
#                                                  '\\K25$' = 'K25 million',
#                                                  '\\K38$' = 'K38 million',
#                                                  '\\K223$' = 'K223 million',
#                                                  '\\K1$' = 'K1 million',
#                                                  '\\K2$' = 'K2 million',
#                                                  '\\K32$' = 'K32 million',
#                                                 '\\K150$' = 'K150 million'))
#money = rbind (money, money1, money2, money3)

#last recognisable group; all rows in recodes are not relevant

### 3) Top Entities Pivots
data19 = rbind (event, fac, gpe, org, norp, pers)

table (data19$label)

#cardinal pivot
#cp = cardinal %>%
#  group_by(entity) %>% 
#  summarize(sum_frequency = sum(frequency)) %>% 
#  arrange (desc(sum_frequency))
#clipr::write_clip(cp)

#fac pivot
fp = fac %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(fp)

#event pivot
ep = event %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(ep)

#gpe pivot
gp = gpe %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))

#recode missed items
gpe$entity = str_replace_all (gpe$entity, c('Southern Regionern$' = 'Southern Region',
                                          'Southern Regionern Region$' = 'Southern Region',
                                          'Southern RegionernMalawi$' = 'Southern Region',
                                          'Southern Region Afric$' = 'Southern Africa',
                                          'Southern Region Africa$' = 'Southern Africa',
                                          'the eat bank of Lower ShireRiver$' = 'East Bank',
                                          'Shire$' = 'Shire River',
                                          'U$S'= 'USA',
                                          'Zomba ' = 'Zomba',
                                          'Lower ShireBasin$' = 'Lower Shire',
                                          'NsanjeSouthern Region$' = 'Nsanje',
                                          'Lower ShireValley$' = 'Lower Shire',
                                          'the East Bank of Lower ShireRiver$' = 'East Ban',
                                          'Lower Shire River$' = 'Lower Shire',
                                          'ChipasulaRiver'= 'Chipasula River',
                                          'the Kawale River' = 'Kawale River',
                                          'Rwa'= 'Rowanda'))
#run gp pivot again after recode
gp = gpe %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(gp)

#money pivot
#mp = money %>%
#  group_by(entity) %>% 
#  summarize(sum_frequency = sum(frequency)) %>% 
#  arrange (desc(sum_frequency))
#clipr::write_clip(mp)

#norp pivot
np = norp %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(np)

#org pivot
op = org %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))

#recode missed items
org$entity = str_replace_all (org$entity, c('^ ' = '',
                                            '320MW.Department of Climate Change and Meteorological Services' = 'Department of Climate Change and Meteorological Services',
                                            'Department of Disaster Management$' = 'DoDMA',
                                            'Disaster Management$' = 'DoDMA',
                                            'ECG and$' = 'ECG',
                                            'Electricity Generation Company$' = 'Egenco',
                                            'FCB Acting Chief Executive Officer$' = 'FCB',
                                            'First Capital Bank $' = 'First Capital Bank',
                                            'Group Village Chaweza$' = 'Group Village',
                                            'Group Village Head' = 'Group Village',
                                            'Genesis Nutritional Company Products' = 'Genesis Nutritional Company',
                                            'Habitat for Humanity International for Humanity International' = 'Habitat for Humanity International',
                                            'Home Land Security' = 'Homeland Security',
                                            'Homeland Security Nicholus' = 'Homeland Security',
                                            'Homeland Security Responsible for Disaster Management' = 'Homeland Security',
                                            'HRDC Gift Trapence' = 'HRDC',
                                            'Lilongwe Technical College Suzika Mvalo' = 'Lilongwe Technical College',
                                            'Red Cross Malawi$' = 'Malawi Red Cross',
                                            'Red Cross Malawi Society' = 'Malawi Red Cross',
                                            'UN Food and Agriculture Organisation$' = 'FAO',
                                            'Fao$' = 'FAO',
                                            'Food and Agriculture Organisation$' = 'FAO',
                                            'UN Food and Agriculture Organization$' = 'FAO',
                                            'UN Childrens’ Fund$' = 'UNICEF',
                                            'Unicef $' = 'UNICEF',
                                            'Unilever Malawi$' = 'Unilever',
                                            'Unilever Malawi Country$' = 'Unilever',
                                            'United Nation High Commission for Refugees' = 'UNHCR',
                                            'United Nations' = 'UN',
                                            'United Nations Children Fund$' = 'UNICEF',
                                            'United Nations Population Fund$' = 'UNFPA',
                                            'World Bank Senior Disaster Risk Management Specialist' = 'World Bank',
                                            'World Food Programme' = 'WFP',
                                            'World Vision  ' = 'World Vision',
                                            'World Vision District Programs' = 'World Vision',
                                            '^Zomba$' = 'Zomba City Council',
                                            '10 Traditional Authorities' = 'Traditional Authorities',
                                            'Airtel Legal' = 'Airtel',
                                            'Airtel Malawi' = 'Airtel',
                                            'AMRA' = 'Asian Muslim Relief Aid',
                                            'Asian Muslim ReliefAid' = 'Asian Muslim Relief Aid',
                                            'Bangula' = 'Bangula ADMARC',
                                            'Blantyre City Council Public Relations' = 'Blantyre City Council',
                                            'board of FDH Group' = 'FDH Bank',
                                            'Japan Tobacco International' = 'JTI',
                                            'Malawi DoDMA' = 'DoDMA',
                                            'TNM ' = 'TNM',
                                            'Tobacco processors Association' = 'Tobacco Processors Association',
                                            'World Food Program' = 'WFP',
                                            '^World$' = 'World Vision'))

#run op pivot again after recode
op = org %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(op)


#pers pivot
pp = pers %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))

#recode missed items
pers$entity = str_replace_all (pers$entity, c('^ ' = '',
                                            'CeaserCeaser Fatchi$' = 'Ceaser Fatchi',
                                            'Dr Saulos Dr Saulos Chilima$' = 'Saulos Chilima',
                                            'Saulos Dr Saulos Chilima$'= 'Saulos Chilima',
                                            'Dr Saulos Chilima$' = 'Saulos Chilima',
                                            '^Fabiano$' = 'Emmanuel Fabiano',
                                            'Malawi VP' = 'Saulos Chilima',
                                            'Pope Francis Francis$' = 'Pope Francis',
                                            'Pope Francis Liyati$' = 'Pope Francis',
                                            'Pope Francis Mkoka$' = 'Pope Francis',
                                            'Prophet Joshua$' = 'Prophet TB Joshua',
                                            'Willam Mazoni' = 'William Mazoni', 
                                            'Lazarus Chakwera-Mia$' = 'Lazarus Chakwera',
                                            'Kondowe$' = 'Overstone Kondowe',
                                            '^Kaliati$' = 'Patricia Kaliati',
                                            '^Chide$' = 'Lindiwe Chide'))

#run pp pivot again after recode
pp = pers %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(pp)


### 2.1) General Word Clouds of Top Entities
#org words
pal2 = c( '#66CDAA','#008080','#6B8E23', '#016450')

wordcloud(words=op$entity, freq = op$sum_frequency, size = 1.5, min.freq = 2,
          max.words = 25, scale = c(3, 0.6), random.order = FALSE, rot.per = 0,
          colors = pal2)

#pers words
pal3 = c('#6B8E23','#3CB371', '#40E0D0', '#016450')

wordcloud(words=pp$entity, freq = pp$sum_frequency, size = 1.5, min.freq = 2,
          max.words = 25, scale = c(2, 0.6), random.order = FALSE, rot.per = 0,
          colors = pal3)

### 2.2) Combining all
data19 = rbind (fac, event, gpe, norp, org, pers) 
#not included: cardinal, money, date, time, ordinal, percent, product, quantity yet

data19$entity = trimws(data19$entity)

#general pivot
pivot = data19 %>% 
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(pivot)

### 2.3) Flash Flood Word Cloud
d1 = filter(data19, date >= "2019-01-01", date <= "2019-02-07")
table (d1$label)

d1p = d1 %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(d1p)

pal5 =  c('#8FBC8F', '#20B2AA', '#016450')


#plots the simple word cloud
wordcloud(words=d1p$entity, freq = d1p$sum_frequency, size = 1.5, min.freq = 2,
          max.words = 25, scale = c(3, 0.6), random.order = FALSE, rot.per = 0,
          colors = pal5)


### 2.4) River Flood Word Cloud
d2 = filter(data19, date >= "2019-02-08", date <= "2019-04-30")
table (d2$label)

d2p = d2 %>%
  group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
clipr::write_clip(d2p)

d2p = d2p [-1,]
pal6 = c('#556B2F', '#9ACD32', '#016450')

#plots the simple word cloud
wordcloud(words=d2p$entity, freq = d2p$sum_frequency, size = 1.5, min.freq = 2,
          max.words = 20, scale = c(3, 0.6), random.order = FALSE, rot.per = 0,
          colors = pal6)


### 3) Location Map for Visualisation
gpmap = gp %>% filter (entity %in% c("Balaka","Blantyre", "Chikwawa", "Chiradzulu", "Chitipa","Dedza",
                                     "Dowa","Karonga","Kasungu","Likoma", "Lilongwe", "Machinga", 
                                     "Mangochi","Mchinji","Mulanje", "Mwanza","Mzimba" , "Neno", "Nkhata Bay", "Nkhotakota", 
                                     "Nsanje", "Ntcheu","Ntchisi","Phalombe", "Rumphi", "Salima", "Thyolo", "Zomba"))
#note: includes only Malawian districts and not other entities (e.g., other nations, bodies of water)

#load district shapefiles for malawi
district = read_sf ('~/Downloads/malawi_news/data/maps/districts/MWI_adm1.shp')
district = district[,-c(1:4, 6:12)]
district = district %>% rename (entity = NAME_1)

#load region shapefiles
region = read_sf ('~/Downloads/malawi_news/data/maps/region/mwi_admbnda_adm2_nso_20181016.shp')
region = region[-c(3,13,20,32),-c(1,2,4:7,9:14,15)] #removing cities and other values to keep region
region = region %>% rename (entity = ADM2_EN)
region = region %>% rename (region = ADM1_EN)
region = as_tibble (region)

nkhata = region %>% filter (entity == 'Nkhata Bay') #new and better geometry for nkhata 
nkhata$entity = str_replace_all(nkhata$entity, c('Nkhata Bay' = 'Nkhata Bay_2'))
nkhata = nkhata %>% mutate (sum_frequency = 5)
region = region [,-3]

#load bodies of water shapefiles for visualisation
water = read_sf ('~/Downloads/malawi_news/data/maps/water/ECHO2_prioritization.shp')
water = water %>% filter (DISTRICT %in% c ('Lake Chilwa', 'Lake Malombe', 'Lake malawi'))
water = water [-c(1,3),-c(1, 3:26)]
water = water %>% rename (entity = TA)
water = water %>% mutate (region = NA) 
water = water %>% mutate (sum_frequency = NA)#new column just to mutate with districts
water$entity = str_replace_all (water$entity, c('Lake malawi$' = 'Lake Malawi'))

### 3.1) Overall 2019 Location Map                                        
#match the data in gpmap and malawi
mw = left_join(district, gpmap, by = "entity")
mw = inner_join (mw, region, by = 'entity')
mw= rbind (nkhata, mw)
mw = mw [-20,]
mw$entity = str_replace_all (mw$entity, c('Nkhata Bay_2' = 'Nkhata Bay'))
mw = st_as_sf(x = mw)

#malawi base map
map = tm_shape (mw) +
  tm_polygons ('sum_frequency', id = 'entity', palette = 'Greens', lwd = 0.5,
               colorNA = '#F5F5F5', textNA = 'NA', title = 'Frequency') +
  tm_layout (legend.text.size = 0.6,
             frame = FALSE,
             legend.width = 10) +
  tm_shape (water) +
  tm_polygons (col = "lightblue", lwd = 0) +
  tm_add_legend(type = "fill", 
                labels = 'Water Bodies',
                col = "lightblue",
                border.lwd = 0.5) +
  tm_layout (legend.text.size = 0.6, 
             frame = FALSE,
             legend.width = 5) +
  tm_shape (mw) +
  tm_text (text = 'entity', size = 0.5)
map

#malawi map for methods overview
colors = c('#8FBC8F', '#20B2AA', '#5F9EA0')
method = tm_shape (mw) +
  tm_polygons ('region', id = 'entity', palette = colors, lwd = 0.5,
               title = 'Regions') +
  tm_shape (water) +
  tm_polygons (col = "lightblue", lwd = 0) +
  tm_add_legend(type = "fill", 
                labels = 'Water Bodies',
                col = "lightblue",
                border.lwd = 0.5) +
  tm_layout (legend.text.size = 0.6,
             frame = FALSE) +
  tm_shape (mw) +
  tm_text (text = 'entity', size = 0.5)
method

tmap_mode ('view') #to view with world map and layers

### 3.2) Flash Flood Locations
flash = filter(gpe, date >= "2019-01-01", date <= "2019-03-01")
flash = flash %>% group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
flash = left_join(district, flash, by = "entity")
flash = inner_join (flash, region, by = 'entity')
flash = rbind (nkhata, flash)
flash = flash [-20,]
flash$entity = str_replace_all (flash$entity, c('Nkhata Bay_2' = 'Nkhata Bay'))
flash = st_as_sf(x =flash)


fmap = tm_shape (flash) +
  tm_polygons ('sum_frequency', id = 'entity', palette = 'Greens', lwd = 0.5,
               colorNA = '#F5F5F5', textNA = 'NA', title = 'Frequency') +
  tm_layout (legend.text.size = 0.6,
             frame = FALSE,
             legend.width = 10) +
  tm_shape (water) +
  tm_polygons (col = "lightblue", lwd = 0) +
  tm_add_legend(type = "fill", 
                labels = 'Water Bodies',
                col = "lightblue",
                border.lwd = 0.5) +
  tm_layout (legend.text.size = 0.6, 
             frame = FALSE,
             legend.width = 5) +
  tm_shape (flash) +
  tm_text (text = 'entity', size = 0.5)
fmap



### 3.3) River Flood Locations
river = filter(gpe, date >= "2019-03-01", date <= "2019-04-30")
river = river %>% group_by(entity) %>% 
  summarize(sum_frequency = sum(frequency)) %>% 
  arrange (desc(sum_frequency))
river = left_join(district, river, by = "entity")
river = inner_join (river, region, by = 'entity')
river = rbind (nkhata, river)
river = river [-20,]
river$entity = str_replace_all (river$entity, c('Nkhata Bay_2' = 'Nkhata Bay'))
river = st_as_sf(x =river)


rmap = tm_shape (river) +
  tm_polygons ('sum_frequency', id = 'entity', palette = 'Greens', lwd = 0.5,
               colorNA = '#F5F5F5', textNA = 'NA', title = 'Frequency') +
  tm_layout (legend.text.size = 0.6,
             frame = FALSE,
             legend.width = 10) +
  tm_shape (water) +
  tm_polygons (col = "lightblue", lwd = 0) +
  tm_add_legend(type = "fill", 
                labels = 'Water Bodies',
                col = "lightblue",
                border.lwd = 0.5) +
  tm_layout (legend.text.size = 0.6, 
             frame = FALSE,
             legend.width = 5) +
  tm_shape (river) +
  tm_text (text = 'entity', size = 0.5)
rmap

