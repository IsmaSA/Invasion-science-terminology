# Invasion terms 
# Ismael Soto & Corey Bradshaw
# August 2023

##################################
## without 'species' tag included 
##################################
# import data
dat1 <- read.csv("termsWOspp.csv", header=T)
head(dat1)
tail(dat1)

# fix term names
year.seq <- seq(1,117,2)
count.seq <- seq(2,118,2)

terms.vec <- colnames(dat1)[count.seq]
  terms.vec[1] <- "acclimatized"
  terms.vec[10] <- "colonizer"
  terms.vec[12] <- "domestic (iea)"
  terms.vec[22] <- "intra-country established alien"
  terms.vec[26] <- "invasive alien"
  terms.vec[27] <- "invasive non-native"
  terms.vec[28] <- "invasive super-dominant"
  terms.vec[30] <- "naturalized"
  terms.vec[33] <- "new non-native"
  terms.vec[36] <- "non-indigenous"
  terms.vec[37] <- "non-native"
  terms.vec[38] <- "non-resident"
  terms.vec[44] <- "range-expanding"
colnames(dat1)[count.seq] <- terms.vec
head(dat1)
colnames(dat1)[year.seq] <- "year"
head(dat1)

# separate terms into unique data.frames
for (i in 1:length(terms.vec)) {
  assign(paste0(terms.vec[i],".dat"), dat1[,year.seq[i]:count.seq[i]])
}

# merge data frames by year
min.yr <- range(dat1[,year.seq], na.rm=T)[1]
max.yr <- range(dat1[,year.seq], na.rm=T)[2]
year.vec <- seq(min.yr, max.yr, 1)
year.dat <- data.frame(year.vec, rep(NA, length(year.vec)))
colnames(year.dat) <- c("year", "dummy")

terms.dat.vec <- paste(terms.vec, ".dat", sep="")
for (s in 1:length(terms.dat.vec)) {
  dat1.it <- get0(terms.dat.vec[s])
  year.dat <- merge(year.dat, dat1.it, by="year", all.x=T, no.dups=T)
}
head(year.dat)
tail(year.dat)

# average & interquartile range of # papers by year
dat.seq <- year.dat[,-2]
mean.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, mean, na.rm=T)
up.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, quantile, probs=0.75, na.rm=T)
lo.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, quantile, probs=0.25, na.rm=T)
sd.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, sd, na.rm=T)
mean.dat <- data.frame(year.vec, mean.count, up.count, lo.count, sd.count)

write.table(mean.dat, "meanOut.csv", sep=",", col.names = T, row.names = F)

# term diversity through time
term.bin <- ifelse(dat.seq >= 1, 1, NA)
term.div <- apply(term.bin, MARGIN=1, sum, na.rm=T)
term.div.r <- c(NA, log(term.div[2:length(term.div)] / term.div[1:(length(term.div)-1)]))
termDiv.out <- data.frame(year.vec, term.div, term.div.r)

write.table(termDiv.out, "termDiv.csv", sep=",", col.names = T, row.names = F)

# cumulative term diversity
term.cum <- term.bin
for (j in 1:length(terms.vec)) {
  first.appear <- which(is.na(term.bin[,j]) == F)[1]
  if (is.na(first.appear) == F) {
    term.cum[first.appear:dim(term.bin)[1], j] <- 1
  }
}
term.cumsum <- apply(term.cum, MARGIN=1, sum, na.rm=T)
termDivCS.out <- data.frame(year.vec, term.cumsum)

write.table(termDivCS.out, "termDivCS.csv", sep=",", col.names = T, row.names = F)

# word cloud
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(pals)

dat2 <- read.csv("totalTermCount.csv", header=T)

set.seed(1234) # for reproducibility 
wordcloud(words = dat2$term, freq = dat2$count, min.freq = 5, max.words=40,
          random.order=FALSE, random.color=T, rot.per=0.20, colors=alphabet2(n = 26))


################################
## with 'species' tag included 
################################
dat3 <- read.csv("termsWspp.csv", header=T)
head(dat3)
tail(dat3)
str(dat3)

# fix term names
dim(dat3)
year.seq <- seq(1,97,2)
count.seq <- seq(2,98,2)

terms.vec <- colnames(dat3)[count.seq]
terms.vec[22] <- "invasive alien"
terms.vec[23] <- "invasive non-native"
terms.vec[27] <- "new non-native"
terms.vec[30] <- "non-indigenous"
terms.vec[31] <- "non-native"
terms.vec[32] <- "non-resident"
terms.vec[38] <- "range-expanding"
colnames(dat1)[count.seq] <- terms.vec
head(dat3)
colnames(dat3)[year.seq] <- "year"
head(dat3)

# separate terms into unique data.frames
for (i in 1:length(terms.vec)) {
  assign(paste0(terms.vec[i],".dat"), dat3[,year.seq[i]:count.seq[i]])
}

# merge data frames by year
min.yr <- range(dat3[,year.seq], na.rm=T)[1]
max.yr <- range(dat3[,year.seq], na.rm=T)[2]
year.vec <- seq(min.yr, max.yr, 1)
year.dat <- data.frame(year.vec, rep(NA, length(year.vec)))
colnames(year.dat) <- c("year", "dummy")

terms.dat.vec <- paste(terms.vec, ".dat", sep="")
for (s in 1:length(terms.dat.vec)) {
  dat3.it <- get0(terms.dat.vec[s])
  year.dat <- merge(year.dat, dat3.it, by="year", all.x=T, no.dups=T)
}
head(year.dat)
tail(year.dat)

# average & interquartile range of # papers by year
dat.seq <- year.dat[,-2]
mean.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, mean, na.rm=T)
up.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, quantile, probs=0.75, na.rm=T)
lo.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, quantile, probs=0.25, na.rm=T)
sd.count <- apply(dat.seq[2:dim(dat.seq)[2]], MARGIN=1, sd, na.rm=T)
mean.dat <- data.frame(year.vec, mean.count, up.count, lo.count, sd.count)

write.table(mean.dat, "meanOutWspp.csv", sep=",", col.names = T, row.names = F)

# term diversity through time
term.bin <- ifelse(dat.seq >= 1, 1, NA)
term.div <- apply(term.bin, MARGIN=1, sum, na.rm=T)
term.div.r <- c(NA, log(term.div[2:length(term.div)] / term.div[1:(length(term.div)-1)]))
termDiv.out <- data.frame(year.vec, term.div, term.div.r)

write.table(termDiv.out, "termDivWspp.csv", sep=",", col.names = T, row.names = F)

# cumulative term diversity
term.cum <- term.bin
for (j in 1:length(terms.vec)) {
  first.appear <- which(is.na(term.bin[,j]) == F)[1]
  if (is.na(first.appear) == F) {
    term.cum[first.appear:dim(term.bin)[1], j] <- 1
  }
}
term.cumsum <- apply(term.cum, MARGIN=1, sum, na.rm=T)
termDivCS.out <- data.frame(year.vec, term.cumsum)

write.table(termDivCS.out, "termDivCSwSPP.csv", sep=",", col.names = T, row.names = F)

# word cloud
dat4 <- read.csv("totalTermCountWspp.csv", header=T)

set.seed(1234) # for reproducibility 
wordcloud(words = dat4$term, freq = dat4$count, min.freq = 5, max.words=40,
          random.order=FALSE, random.color=T, rot.per=0.20, colors=alphabet2(n = 26))

