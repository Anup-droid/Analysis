#Library installation
library(ggplot2)
library(tidyverse)
library(stringr)
library(mice)
library(tm)
library(wordcloud)
library(wordcloud2)
library(caTools)
kypsa <- read.csv("C:/Users/Anup/Desktop/Kypsa/kypsa.csv")


#Data Management
#arrange the data
kypsa=kypsa %>% select(ResultNumber,UserID,Country,Gender,Agegroup,DeoUser,
                       Deotype,Deofreq,Sweating,Children,Activity.level,
                       Average.Time,everything())
#Take only those user who give the consent to participate
kypsa=kypsa%>% filter(Consent==1,na.rm=TRUE) 
#Country filter
kypsa=kypsa%>% filter(Country %in% c("South Africa","India","US","Philippines","Australia","UK"))
#Remove duplicate value based on the userID
kypsa=kypsa %>% distinct(UserID, .keep_all = TRUE)
#Missing value describe
p=function(x){
  sum(is.na(x))/length(x)*100
}
apply(kypsa,2,p)
md.pattern(kypsa)

#Question number 3
#Appeal
kypsa %>% select(Country,Appeal) %>% 
  filter(Appeal %in% c(1,2,3,4,5)) %>%
  mutate(Appeal = factor(Appeal, 
                         levels = 1:5,
                         labels = c("It is not appealing at all",
                                    "It is not appealing", 
                                    "Neutral", 
                                    "It is appealing", 
                                    "It is very appealing"))) %>% 
  ggplot(aes(Country,fill=Appeal))+
  geom_bar(position="dodge",alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="Country and Appeal",x="Country",y="Number")
#Trust
kypsa %>% select(Country,Trust) %>% 
  filter(Trust %in% c(1,2,3,4,5)) %>%
  mutate(Trust = factor(Trust, 
                        levels = 1:5,
                        labels = c("It is not appealing at all",
                                   "It is not appealing", 
                                   "Neutral", 
                                   "It is appealing", 
                                   "It is very appealing"))) %>% 
  ggplot(aes(Country,fill=Trust))+
  geom_bar(position="dodge",alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="Country and Trust",x="Country",y="Number")


#Question number 4
#1.Country and Gender
kypsa %>% select(Country,Gender) %>% 
  filter(Gender %in% c("Male","Female"),
         Country %in% c("South Africa","India","US","Philippines","Australia","UK")) %>% 
  ggplot(aes(Country,fill=Gender))+
  geom_bar(position="dodge",alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="Gender Representation in country",x="Country",y="Number")

#2.Country and Age Group
kypsa %>% select(Country,Agegroup) %>%
  filter(Country %in% c("South Africa","India","US","Philippines","Australia","UK")) %>% 
  ggplot(aes(Country,fill=Agegroup))+
  geom_bar(position="dodge",alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="Age Group Representation in country",x="Country",y="Number")

#Text Analysis
#Build corpus
corpus=iconv(kypsa$Likes,to="utf-8")
corpus=Corpus(VectorSource(corpus))
inspect(corpus[1:5])

#Clean text
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
cleanset=tm_map(corpus,removeWords,stopwords('english'))
cleanset=tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Text document matrix
tdm=TermDocumentMatrix(cleanset)
tdm
tdm=as.matrix(tdm)
tdm[1:10,1:20]

#Bar plot
w=rowSums(tdm)
w=subset(w,w>=50)
barplot(w,las=2,col=rainbow(50))

#Word cloud
w=sort(rowSums(tdm),decreasing = TRUE)
set.seed(222)
wordcloud(words=names(w),freq=w,max.words =50,
          random.order = F,min.freq =10,
          colors=brewer.pal(8,'Dark2'),
          scale=c(5,0.3))
wrd=data.frame(names(w),w)
colnames(wrd)=c('word','freq')
wordcloud2(wrd,size=0.7,shape='circle')
