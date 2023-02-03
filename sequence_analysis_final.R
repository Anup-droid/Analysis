library(ggplot2)
library(tidyverse)
library(stringr)
library(haven)
library(TraMineR)
library(cluster)
library(pacman)
library(factoextra)
library(ggpubr)
library(WeightedCluster)
# load and install packages
pacman::p_load(TraMineR, TraMineRextras, downloader, cluster, RColorBrewer, devtools, haven, 
               tidyverse, reshape2, WeightedCluster, nnet, plyr)

#Data Import Bihar data
cont <- read_dta("C:/Users/Anup/Desktop/fdata.dta")

max(str_count(cont$trim_cal))
min(str_count(cont$trim_cal))

#Remove extra space in sequence
#cont$vcal_1=str_trim(cont$vcal_1)
#str_count(cont$vcal_1)
#max(str_count(cont$vcal_1))

#To remove recent two month because women don not know about their pregnancy
cont$trim_cal=str_sub(cont$trim_cal,3)
max(str_count(cont$trim_cal))

numb = seq(1:nrow(cont))
expl = lapply(numb, function(i){
  k = cont[i,'trim_cal']
  k = str_split(k,"")[[1]]
  k
})
calc = do.call('rbind.data.frame',expl)
colnames(calc) = paste('CM', rev(seq(66:1)), sep="")
calc=rev(calc)

#Make group of contraceptive use

calc[calc == 0] <- "NU"
calc[calc == "B"] <- "PR"
calc[calc == "P"] <- "PR"
calc[calc == "T"] <- "PR"
calc[calc == 1] <- "SR"
calc[calc == 3] <- "SR"
calc[calc == 5] <- "SR"
calc[calc == "L"] <- "SR"
calc[calc == "E"] <- "SR"
calc[calc == "S"] <- "SR"
calc[calc == 2] <- "LP"
calc[calc == 6] <- "LP"
calc[calc == 7] <- "LP"
calc[calc == "N"] <- "LP"
calc[calc == 8] <- "TR"
calc[calc == 9] <- "TR"
calc[calc == "W"] <- "OT"
calc[calc == "C"] <- "OT"
calc[calc == "M"] <- "OT"
calc[calc == "F"] <- "OT"
calc[calc == 4] <- "OT"


#Factor convert
calc=calc %>% mutate_if(is.character,as.factor)
str(calc)

cdata=cbind(cont,calc)

#Combine two data
new.alphab <- c("NU","PR","SR",
              "LP","TR","OT")
new.lab <- c("NU","PR","SR",
             "LP","TR","OT")
new.scode <- c("NU","PR","SR",
               "LP","TR","OT")

#making the sequence data
new.seq <- seqdef(cdata,21:86,alphabet = new.alphab, xtstep = 5)

#Sequence index plot
seqiplot(new.seq, border = NA, with.legend = "right",idxs=1:6)

#Sequence desity plot

seqdplot(new.seq, border = NA, with.legend = "right", legend.prop=0.4)

#Tabulation of sequence
seqtab(new.seq, idxs = 1:6)

seqfplot(new.seq, border = NA, with.legend = FALSE,
         main = "Weighted frequencies")

seqfplot(new.seq, weighted = FALSE, border = NA,
         with.legend = FALSE, main = "Unweighted frequencies")

new.om <- seqdist(new.seq, method = "OM", indel = 1, sm = "TRATE")

#Clustering Using ward's hirarical clustering
clusterward <- agnes(new.om, diss = TRUE, method = "ward")

wardtest <- as.clustrange(clusterward,
                          diss = new.om, weights=cdata$v005, 
                          ncluster = 4)
summary(wardtest)
ward=plot(wardtest, stat = c("ASW", "HC", "PBC","HG","R2","R2sq"), norm = "zscore", lwd = 4)
ward
new.cl4 <- cutree(clusterward, k = 4)
cl4.lab <- factor(new.cl4, labels = paste("Cluster", 1:4))
seqdplot(new.seq, group = cl4.lab, border = NA)

#K-mediod clustering

pamrange=wcKMedRange(new.om,kvals=2:10,weights=cdata$v005)
summary(pamrange,max.rank = 2)
seqdplot(new.seq,group=pamrange$clustering$cluster4,border=NA)

pam=plot(pamrange, stat = c("ASW", "HC", "PBC","HG","R2","R2sq"), norm = "zscore", lwd = 4)
      
        
#Data extraction from cluster 
clust1=cdata[new.cl4==1, ]
clust2=cdata[new.cl4==2, ]
clust3=cdata[new.cl4==3, ]
clust4=cdata[new.cl4==4, ]

#Age distribution in each cluster
prop.table(table(clust1$v013))*100
prop.table(table(clust2$v013))*100
prop.table(table(clust3$v013))*100
prop.table(table(clust4$v013))*100

























