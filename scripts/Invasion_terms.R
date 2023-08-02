
# Invasion terms 
# Ismael Soto & Corey Bradshaw

#####################################   Formatting   ################################################

df = read_xlsx('WoS_Search_Terms_31July2023.xlsx') #From Fran 

names(df)

df1 = df[,c(3,17,26,48, 51, 66)]
head(df1)
unique(df1$`WoS Categories`) #

unique(df1$Term)

keywords <- c("Ecology", "Environmental", "Biodiversity", 'Plant Sciences','Fisheries',
              "Marine & Freshwater Biology", "Zoology", "Botany", "Agricultur")

df_filtered <- df1 %>%
  filter(str_detect(`WoS Categories`, str_c(keywords, collapse = "|")))


#Extract the ecosystem
df_filtered <- df_filtered %>% 
  mutate(Ecosystem = case_when(
    str_detect(Abstract, "marine") ~ "marine",
    str_detect(Abstract, "freshwater") ~ "freshwater",
    str_detect(Abstract, "terrestrial") ~ "terrestrial",
    TRUE ~ "NA"
  ))

table(df_filtered$Ecosystem) # many NAs...
table(df_filtered$Language)

names(df_filtered)

df2 <- df_filtered %>%
  group_by(Term, `Publication Year`) %>%
  summarise(N_papers = n(), .groups = 'drop')

#Total number of papers
Total= df2 %>% group_by(Term)  %>% summarise(sum = sum(N_papers))

#Relative n papers
df3 <- df2 %>%
  group_by(`Publication Year`) %>%
  mutate(Relative_N_papers = N_papers / sum(N_papers))

terms= c('alien species',"exotic species","non-native species","invasive/invader species","non-indigenous species")
df4= df3 %>% filter(Term %in% terms)


df4 %>%
  ggplot(aes(x = `Publication Year`, y = Relative_N_papers, color = Term)) +
  geom_line(size= 1.5) +
  labs(title = "Temporal Trends in the Relative Importance of Terms",
       x = "Publication Year",
       y = "Relative Number of Papers",
       color = "Term") +
  theme_minimal() + xlim(1990,2020)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################################################

## call libraries
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)

## import data
setwd("~/Documents/Papers/Invasive species/Terminology/data")


head(df2)
unique(df2$Term) #Note that vermin and xenobiota species has only one paper (no detected by my previous filters in WOS, 
# because their category is History and Microbiology, respectively). However as they have only paper makes no sense to plot it 

# I need to make these change to the wordcloud
df2$Term[df2$Term=='invasive/invader species' ] = 'invasive species'
df2$Term[df2$Term=='colonizer/colonist' ] = 'colonizer'

colnames(df2) <- c('term', 'year', 'count')
unique(df2$term)

df2$term <- gsub(" species|$", "", df2$term)
df2$term <- trimws(df2$term)


#dat <- readxl::read_excel("terms.xlsx") From Corey 
dat <- df2


## term diversity with time
termfreq <- table(dat$term, dat$year)
termfreqTS <- as.data.frame(apply(termfreq, MARGIN=2, sum, na.rm=T))
colnames(termfreqTS) <- c("nterms")
termfreqTS$year <- as.numeric(rownames(termfreqTS))
head(termfreqTS)
plot(termfreqTS$year, termfreqTS$nterms, type="l", xlab="year", ylab="term diversity")
termfreqTS$ntermR <- c(NA,log(termfreqTS$nterms[2:dim(termfreqTS)[1]] / termfreqTS$nterms[1:(dim(termfreqTS)[1] - 1)]))
head(termfreqTS)
plot(termfreqTS$year, termfreqTS$ntermR, type="l", xlab="year", ylab="term diversity r")
write.table(termfreqTS,"termfreqTS.csv", row.names = F, sep=",")

## r vs. n
plot(termfreqTS$nterms[1:(dim(termfreqTS)[1]-1)], termfreqTS$ntermR[2:dim(termfreqTS)[1]], pch=19, xlab="term diversity", ylab="terms rate of change")

## single terms
term.vec <- attr(table(dat$term), "names")
for (i in 1:length(term.vec)) {
  assign(paste0(term.vec[i],".dat"), subset(dat, term == term.vec[i]))
}

termDat.vec <- paste0(term.vec,".dat")

## plots
attach(get(termDat.vec[1]))
dat.use <- data.frame(year,count)
datuse <- dat.use[order(dat.use[,1], decreasing=F),]
plot(datuse$year, log10(datuse$count), type="l", xlab="year", ylab="log10 count", 
     ylim=c(0,log10(max(dat$count,na.rm=T))), col="grey")
detach(get(termDat.vec[1]))

for (j in 2:length(termDat.vec)) {
  attach(get(termDat.vec[j]))
  dat.use <- data.frame(year,count)
  datuse <- dat.use[order(dat.use[,1], decreasing=F),]
  lines(datuse$year, log10(datuse$count), col="grey")
  detach(get(termDat.vec[j]))
}

sumcounts <- as.data.frame(xtabs(dat$count ~ dat$year))
dim(sumcounts)
dim(termfreqTS)
termfreqTS$countSum <- sumcounts$Freq
termfreqTS$countMn <- termfreqTS$countSum/termfreqTS$nterms
head(termfreqTS)
lines(termfreqTS$year, log10(termfreqTS$countMn), lwd=3, lty=1, col="black")

## create list with single-term data
N <- length(termDat.vec)
term.list <- vector("list", N)
for (s in 1:N) {
  Ps <- get(termDat.vec[s])
  term.list[[s]] <- Ps
}
setNames(term.list, term.vec)

sink("termList.txt")
print(term.list)
sink()


## word cloud
term.mat <- as.matrix(table(dat$count, dat$term))
dim(term.mat)
termWt.mat <- sort(unique(dat$count)) * term.mat
head(termWt.mat)

termcounts.mat <- as.matrix(apply(termWt.mat, MARGIN=2, sum))
ISterms <- sort(rowSums(termcounts.mat), decreasing=T)
IStermsDF <- data.frame(term = names(ISterms), freq=ISterms)
set.seed(1234) # for reproducibility 
wordcloud(words = IStermsDF$term, freq = IStermsDF$freq, min.freq = 1, max.words=35,
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))


