## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----table genotypes----------------------------------------------------------
genotyp<-c("AA", "Aa", "aa")
gentable.names<-c("offspring","dam","sire","hot.dam","hot.sire","hot.parents", "hiphop","hothiphop.parents")
gentable<-as.data.frame(array(NA,dim = c(length(genotyp)^3,length(gentable.names)), dimnames=list(NULL,gentable.names)))
gentable$offspring<-rep(genotyp, each=length(genotyp)*length(genotyp))
gentable$dam<-rep(genotyp, length(genotyp)*length(genotyp))
gentable$sire<-rep(genotyp, length(genotyp), each=length(genotyp))
# the HOT test for the offspring-dam dyad
gentable$hot.dam<-ifelse( (gentable$offspring=="AA" & gentable$dam=="aa") | (gentable$offspring=="aa" & gentable$dam=="AA"), 1,0)
# the HOT test for the offspring-sire dyad 
gentable$hot.sire<-ifelse( (gentable$offspring=="AA" & gentable$sire=="aa") | (gentable$offspring=="aa" & gentable$sire=="AA"), 1,0)
# the HOT test for the offspring-dam-sire triad
gentable$hot.parents<-ifelse(((gentable$offspring=="AA" & (gentable$sire=="aa" | gentable$dam=="aa")) | (gentable$offspring=="aa" & (gentable$sire=="AA" | gentable$dam=="AA"))),1,0)
# the hiphop for the dam-sire combination
gentable$hiphop<-ifelse( (gentable$offspring=="Aa" & gentable$sire=="aa" & gentable$dam=="aa") | (gentable$offspring=="Aa" & gentable$sire=="AA" &  gentable$dam=="AA"), 1,0)
gentable$hothiphop.parents<-gentable$hot.parents+gentable$hiphop
print(gentable)

## ----library------------------------------------------------------------------
library(hiphop)

## ----individuals--------------------------------------------------------------
individuals[23:33,]

## ----dummy brood id-----------------------------------------------------------
tail(individuals)

## ----genotypes----------------------------------------------------------------
genotypes[1:5,1:13]  

## ----select one cohort--------------------------------------------------------
ind2018<-subset(individuals, individuals$year==2018)

## ----inspect data-------------------------------------------------------------
inspection<-inspect(ind=ind2018, gen=genotypes)
head(inspection)
inspection[which(inspection$sampled==0),]

## ----setup--------------------------------------------------------------------
print(c(length(unique(ind2018$individual[which(ind2018$type=="offspring")])), length(unique(ind2018$individual[which(ind2018$type=="adult female")])), length(unique(ind2018$individual[which(ind2018$type=="adult male")]))))

## ----hothiphop, eval=FALSE, echo=TRUE-----------------------------------------
#  combinations<-hothiphop(ind=ind2018, gen=genotypes)

## ----hothiphop head-----------------------------------------------------------
head(combinations)

## ----topmatch-----------------------------------------------------------------
best.hothiphop<-topmatch(x=combinations, ranking="hothiphop.parents")
best.hothiphop[1:8,]

## ----topmatch thres-----------------------------------------------------------
best<-topmatch(x=combinations, ranking="hothiphop.parents", thres=62)
best[9:12,c(1:9, 16,17)]

## ----topmatch thres2----------------------------------------------------------
best<-topmatch(x=combinations, ranking="hothiphop.parents", thres=0.045)
best[9:12,c(1:9, 16,17)]

## ----topmatch thres3----------------------------------------------------------
best<-topmatch(x=combinations, ranking="hothiphop.parents", thres=-0.99)
best[9:12,c(1:9, 16,17)]

## ----topmatch ranking---------------------------------------------------------
best.hiphop<-topmatch(x=combinations, ranking="hiphop")
best.hiphop[1:4,1:13]

## ----topmatch ranking 2-------------------------------------------------------
best.hot.dam<-topmatch(x=combinations, ranking="hot.dam")
best.hot.dam[1:3,1:13]

## ----topmatch ranking 3-------------------------------------------------------
best.hot.dam<-topmatch(x=combinations, ranking=c("hot.dam","hiphop"), unique="dam")
best.hot.dam[1:4,1:13]

## ----topmatch condition-------------------------------------------------------
best<-topmatch(x=combinations, ranking="hothiphop.parents", condition="mother")
best[1:4,1:13]

## ----topmatch top-------------------------------------------------------------
best<-topmatch(x=combinations, ranking="hothiphop.parents", top=5)
best[1:6,1:7]

## ----topmatch case 1----------------------------------------------------------
best.dams<-topmatch(x=combinations, ranking=c("hot.dam","hiphop"), unique="dam")
best.dams[1:8,1:13]
which(best.dams$rank==1 & best.dams$dam.type!="social parent")

## ----topmatch case 1b, eval=FALSE, echo=TRUE----------------------------------
#  best.triad<-best.dams[which(best.dams$rank==1),]
#  best.triad<-best.triad[!duplicated(best.triad[c("offspring","dam")]),] #we remove the duplicates, because if the dam with the lowest hot.dam score is the social mother she can be listed twice: once with the social father and one with the sire that gives the lowest HIPHOP score
#  plot(best.triad$hot.dam,best.dams$hot.dam[which(best.dams$rank==2)], xlim=c(0,100), ylim=c(0,100), xlab="Hot.dam score 1st ranked dams (here the social mothers)", ylab="Hot.dam score 2nd ranked dams", main="149 offspring of 2018 cohort")
#  abline(0,1)

## ----image 4, echo=FALSE, out.width='100%'------------------------------------
knitr::include_graphics('./hotdam_scatter.png')

## ----topmatch case 1c---------------------------------------------------------
best.dams[which(best.dams$offspring=="A59419-iNgn"),1:13]

## ----topmatch case 1e, eval=FALSE, echo=TRUE----------------------------------
#  # first run the hothiphop on all cohorts
#  comb.18<-hothiphop(ind=subset(individuals,individuals$year==2018), gen=genotypes)
#  comb.17<-hothiphop(ind=subset(individuals,individuals$year==2017), gen=genotypes)
#  comb.16<-hothiphop(ind=subset(individuals,individuals$year==2016), gen=genotypes)
#  comb.15<-hothiphop(ind=subset(individuals,individuals$year==2015), gen=genotypes)
#  comb.14<-hothiphop(ind=subset(individuals,individuals$year==2014), gen=genotypes)
#  # then run topmatch function on all cohorts
#  best.18<-topmatch(x=comb.18, ranking=c("hot.dam","hiphop"), unique="dam")
#  best.17<-topmatch(x=comb.17, ranking=c("hot.dam","hiphop"), unique="dam")
#  best.16<-topmatch(x=comb.16, ranking=c("hot.dam","hiphop"), unique="dam")
#  best.15<-topmatch(x=comb.15, ranking=c("hot.dam","hiphop"), unique="dam")
#  best.14<-topmatch(x=comb.14, ranking=c("hot.dam","hiphop"), unique="dam")
#  #combine results
#  combinations.all<-do.call("rbind", list(comb.18, comb.17, comb.16,comb.15, comb.14))
#  best.dams<-do.call("rbind", list(best.18, best.17, best.16,best.15, best.14))
#  #remove the cases where the social mother was the first ranked dam, as then she is listed twice with rank 1
#  best.triad<-best.dams[which(best.dams$rank==1),]
#  best.triad<-best.triad[!duplicated(best.triad[c("offspring","dam")]),]
#  # plot the data
#  plot(best.triad$hot.dam ,best.triad$hiphop, col = "red", xlab="HOT.dam", ylab="HIPHOP", xlim=c(0,100), ylim=c(0,120))
#  points(best.dams$hot.dam[which(best.dams$rank==2 & best.dams$dam.type!="social parent")],best.dams$hiphop[which(best.dams$rank==2 &  best.dams$dam.type!="social parent")], col = "blue")
#  points(best.dams$hot.dam[which(best.dams$rank==1 & best.dams$dam.type!="social parent")],best.dams$hiphop[which(best.dams$rank==1 & best.dams$dam.type!="social parent")], col = "orange", pch = 19)
#  points(best.dams$hot.dam[which(best.dams$rank==2 & best.dams$dam.type=="social parent")],best.dams$hiphop[which(best.dams$rank==2 &  best.dams$dam.type=="social parent")], col = "green", pch = 19)
#  legend("bottomright",c("1st ranked social mother", "1st ranked not social mother", "2nd ranked social mother", "2nd ranked not social mother"), fill = c("red", "orange", "green", "blue"))

## ----image 1, echo=FALSE, out.width='100%'------------------------------------
knitr::include_graphics('./allcohorts1.png')

## ----topmatch case 1f---------------------------------------------------------
best.parents.hiphop<-topmatch(x=combinations, condition="mother", ranking="hothiphop.parents")

## ----topmatch case 1ff, eval=FALSE, echo=TRUE---------------------------------
#  plot(best.parents.hiphop$hiphop[which(best.parents.hiphop$rank==1)],best.parents.hiphop$hothiphop.parents[which(best.parents.hiphop$rank==1)], col = "red", xlab="HIPHOP", ylab="HOTHIPHOP.parents", main="149 offspring of 2018 cohort")
#  points(best.parents.hiphop$hiphop[which(best.parents.hiphop$rank==2)],best.parents.hiphop$hothiphop.parents[which(best.parents.hiphop$rank==2)], col = "blue")
#  legend("topleft",c("1st ranked", "2nd ranked"), fill = c("red", "blue"))

## ----image 5, echo=FALSE, out.width='100%'------------------------------------
knitr::include_graphics('./hiphop_scatter.png')

## ----topmatch case 1g---------------------------------------------------------
best.parents.hiphop[which(best.parents.hiphop$rank==1 & best.parents.hiphop$hiphop>20),c(1:13,22,23)]

## ----simulation, eval=FALSE, echo=TRUE----------------------------------------
#  ff<-which(individuals$type=="adult female")
#  individuals.rem<-individuals[-ff[seq(5,length(ff),5)],] #this removes every fifth female in the individuals dataframe
#  mm<-which(individuals.rem$type=="adult male")
#  individuals.rem<-individuals.rem[-mm[seq(5,length(mm),5)],] #this removes every fifth male in the individuals dataframe

## ----simulation plot, eval=FALSE, echo=TRUE-----------------------------------
#  combinations.rem<-hothiphop(ind=individuals.rem, gen=genotypes)
#  best.hhhh<-topmatch(x=combinations.rem, ranking="hothiphop.parents")
#  h1 <- hist(best.hhhh[which(best.hhhh$rank==1), "hothiphop.parents"], breaks = 100)
#  h2 <- hist(best.hhhh[which(best.hhhh$rank==2), "hothiphop.parents"], breaks = 50)
#  plot(h2, col = "blue", xlim = c(0, 200),  xlab="HOTHIPHOP.parents", main=NULL)
#  plot(h1, col = "red", xlim = c(0, 200), add = T)
#  legend("topright",c("1st ranked", "2nd ranked"), fill = c("red", "blue"))

## ----image 2, echo=FALSE, out.width='100%'------------------------------------
knitr::include_graphics('./allcohorts2.png')

## ----topmatch case 2c, eval=FALSE, echo=TRUE----------------------------------
#  plot(best.hhhh[which(best.hhhh$rank==1 & best.hhhh$hiphop<12),"hot.dam"] ,best.hhhh[which(best.hhhh$rank==1 & best.hhhh$hiphop<12),"hot.sire"], col ="orange" , xlab="HOT.dam", ylab="HOT.sire", xlim=c(0,100), ylim=c(0,100), pch = 19, main="1st ranked triads (on HIPHOP) 2018 cohort")
#  points(best.hhhh[which(best.hhhh$rank==1 & best.hhhh$hiphop>=12),"hot.dam"] ,best.hhhh[which(best.hhhh$rank==1 & best.hhhh$hiphop>=12),"hot.sire"], col ="purple", pch = 19 )
#  abline(v=37, h=35, col="green")
#  legend("topright",c("HIPHOP < 12", "HIPHOP >= 12"), fill = c("orange", "purple"))

## ----image 3, echo=FALSE, out.width='100%'------------------------------------
knitr::include_graphics('./allcohorts3.png')

