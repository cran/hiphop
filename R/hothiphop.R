#' A function that calculates the genetic mismatches according to the hiphop and hot
#' test
#'
#' This function  calculates the number genetic mismatches according to the hiphop and hot
#' test for any combination of offspring-potential.dam-potential.sire.
#' The HOT test (Homozygous Opposite Test; Huisman 2017) compares the genotype of an offspring with a potential parent:
#' a mismatch is scored when both the offspring and parent are homozygous, but for different alleles.
#' The HIPHOP test (Homozygous Identical Parents, Heterozygous Offspring are Precluded; Cockburn et al. in revision)
#' compares the genotype of an offspring with both potential parents:
#' a mismatch is scored when the offspring is heterozygous and both parent are homozygous for the same allele
#' The resulting output can next be summarized using the 'topmatch()' function.
#' @param ind The input file with individuals, which should contain at least the columns
#' brood, individual, type, social.parent, year.
#' @param gen The input file with genotypes, which should contain the loci-names as column headers
#' and the individual  names as row-header and should only contain the values 0, 1, 2 or NA.
#' @return a dataframe with all possible  offspring-potential.dam-potential.sire combinations
#' and their mismatch scores according to the HOT and HIPHOP test, the number of loci this was based on,
#' and some additional relevant information about the social parents and potential dam and sires
#' \describe{
#'   \item{year}{ the year or cohort that is being considered, adults can be potential dam or sire in some years, but no in others}
#'   \item{brood}{ an identifier of the brood to which the offspring and adults belong/are associated with}
#'   \item{offspring}{ an identifier of the offspring }
#'   \item{potential.dam}{ an identifier of the potential dam }
#'   \item{potential.sire}{ an identifier of the potential sire }
#'   \item{hothiphop.parents}{ the sum of the hiphop and hot.parents mismatch score }
#'   \item{hiphop}{ the hiphop mismatch score of the offspring with the potential dam and potential sire, expressed as the number of loci giving mismatches }
#'   \item{hot.parents}{ the hot score of the offspring with both the potential dam and sire, expressed as the number of loci giving mismatches}
#'   \item{hot.dam}{ the hot score of the offspring with the potential dam, expressed as the number of loci giving mismatches}
#'   \item{hot.sire}{ the hot mismatch score of the offspring with the potential sire , expressed as the number of loci giving mismatches}
#'   \item{hothiphop.dam}{ the sum of the hot.dam and hiphop mismatch score }
#'   \item{hothiphop.sire}{ the sum of the hot.sire and hiphop mismatch score }
#'   \item{loci.dyad.dam}{ the number of loci at which both the offspring and dam were not NA }
#'   \item{loci.dyad.sire}{the number of loci at which both the offspring and sire were not NA }
#'   \item{loci.triad}{ the number of loci at which the offspring, dam and sire were not NA }
#'   \item{offspring.heterozygosity}{ proportion of loci at which the offspring was heterozygous }
#'   \item{social.mother.sampled}{if the social.mother genotypic data is in the genotypes file then equal to 1, else 0}
#'   \item{social.father.sampled}{if the social.father genotypic data is in the genotypes file then equal to 1, else 0}
#'   \item{is.dam.social}{if the potential dam is the social mother then equal to 1, else 0}
#'   \item{is.sire.social}{if the potential sire is the social father then equal to 1, else 0}
#'   \item{is.dam.within.group}{if the potential dam is part of the same group (i.e. associated with the same brood) as the offspring, then equal to 1, else 0}
#'   \item{is.sire.within.group}{if the potential sire is part of the same group (i.e. associated with the same brood) as the offspring, then equal to 1, else 0}
#'   \item{social.mother}{ identity of the social mother of the offspring}
#'   \item{social.father}{ identity of the social father of the offspring}
#' }
#' @author Martijn van de Pol, \email{martijn@myscience.eu}
#' @references Cockburn et al. (2020) HIPHOP: improved paternity assignment among close relatives using a simple exclusion method for biallelic markers.
#' Molecular Ecology Resources, in revision.
#' @references Huisman, J. (2017). Pedigree reconstruction from SNP data: parentage assignment, sibship clustering and beyond. Molecular ecology resources, 17(5), 1009-1024.
#' @source Cockburn et al. (2020) HIPHOP: improved paternity assignment among
#' close relatives using a simple exclusion method for biallelic markers.
#' Molecular Ecology Resources, in revision.
#' @examples
#' results<-hothiphop(ind=individuals[1:22,], gen=genotypes)
#' head(results)
#' best<-topmatch(x=results, ranking="hothiphop.parents")
#' head(best)
#' @export
hothiphop<-function(ind, gen) {
  # first check for correct input format of ind and gen data
  ind.types<-c("offspring", "adult female", "adult male")
  ind.col.names<-c("brood", "individual", "type", "social.parent", "year")
  if( (length(ind.col.names)-sum(colnames(ind) %in% ind.col.names))>0) {stop("In the individual file there should be the following columns: ", ind.col.names) }
  if( (length(ind$type)-sum(ind$type %in% ind.types)) >0) {stop("In the individual file there should only be the following types: ", ind.types)  }
  if( (length(ind$social.parent)-sum(ind$social.parent %in% c(0,1))) >0) {stop("In the individual file there should only be the following value for social parent: 0 or 1")}
  broods<-unique(ind$brood)
  for(b in 1:length(broods)) { if(length(which(ind$brood==broods[b] & ind$social.parent==1 & ind$type=="adult male"))>1) {stop("brood ", broods[b], " has multiple social fathers")}     }
  for(b in 1:length(broods)) { if(length(which(ind$brood==broods[b] & ind$social.parent==1 & ind$type=="adult female"))>1) {stop("brood ", broods[b], " has multiple social mothers")} }
  # we next identify all offspring-potential.dam-potential.sire combinations and define array to store scores
  offspring.records<-which(ind$type=="offspring")
  offspring<-ind$individual[offspring.records]
  genotyped.individuals<-unique(rownames(gen))
  potential.sires<- unique(ind$individual[which(ind$type=="adult male")])
  potential.dams<- unique(ind$individual[which(ind$type=="adult female")])
  potential.sires<-potential.sires [! potential.sires %in% setdiff(potential.sires, genotyped.individuals)]   # we remove the non-genotyped dams and sires
  potential.dams<-potential.dams [! potential.dams %in% setdiff(potential.dams, genotyped.individuals)]
  offspring<-offspring [! offspring %in% setdiff(offspring, genotyped.individuals)]
  hot.dams.mismatch<-as.data.frame(array(NA,dim = c(length(offspring),length(potential.dams)), dimnames=list(offspring,potential.dams)))
  hot.sires.mismatch<-as.data.frame(array(NA,dim = c(length(offspring),length(potential.sires)), dimnames=list(offspring,potential.sires)))
  hiphop.mismatch.names<-c("hiphop","hot.parents","hot.dam", "hot.sire","loci.triad","loci.dyad.dam","loci.dyad.sire", "dam.within.brood", "sire.within.brood")
  hiphop.mismatch<-array(NA,dim = c(length(potential.dams), length(potential.sires),length(offspring),length(hiphop.mismatch.names)), dimnames=list(potential.dams,potential.sires, offspring, hiphop.mismatch.names))
  #define a dataframe that tracks some data
  triad<-ind[offspring.records,c("year","brood")]
  triad$offspring<-ind[offspring.records,"individual"]
  triad[,c( "dam", "sire",  "social.mother.sampled", "social.father.sampled", "offspring.heterozygosity")]<-triad$year*NA
  #define the genotype matrices of individuals
  gen.offspring<-as.matrix(gen[triad$offspring,])
  gen.potential.sires<-as.matrix(gen[potential.sires,])
  gen.potential.dams<-as.matrix(gen[potential.dams,])
  # some more data checks
  if(length(setdiff(offspring, genotyped.individuals))>0) { stop("there are one or more offspring without genotypes, please remove from individual list") }
  if(length(which(gen.offspring==0))+length(which(gen.offspring==1))+length(which(gen.offspring==2))+length(which(is.na(gen.offspring)==TRUE))!=(dim(gen.offspring)[1]*dim(gen)[2])) {
    stop("ERROR, there should only be 0, 1,2 and possible NA values in the genotypes")
  }
  # start scoring each offspring-potential.dam-potential.sire combination
  for(o in 1:length(offspring)) {
    if(o%%10==0) { print(paste0(round((100*o/length(offspring)),0),"%, ", o," offspring finished")) }
    for(d in 1:length(potential.dams)) {
      for(s in 1:length(potential.sires)) {
        # the hot mismatch part for offspring and single parent dyad: are offspring and parent opposite homozygotes (o=0,p=1 or o=1,p=0, in both cases their sum=1, as homozygotes are scored as 0 or 1)
        if(d==1) {
          combined.os<-gen.offspring[o,]+gen.potential.sires[s,]
          hot.sires.mismatch[o,s]<-sum(combined.os==1, na.rm=TRUE)
        }
        if(s==1) {
          combined.od<-gen.offspring[o,]+gen.potential.dams[d,]
          hot.dams.mismatch[o,d]<-sum(combined.od==1, na.rm=TRUE)
        }
        hiphop.mismatch[d,s,o,"hot.dam"]<-hot.dams.mismatch[o,d]
        hiphop.mismatch[d,s,o,"hot.sire"]<-hot.sires.mismatch[o,s]
        # the HOT test for offspring with the parents (triad)
        hiphop.mismatch[d,s,o,"hot.parents"]<-sum((((gen.offspring[o,]+gen.potential.sires[s,])==1)+((gen.offspring[o,]+gen.potential.dams[d,])==1))>=1, na.rm=TRUE)
        # the HIPHOP test for offspring with the parents (triad)
        gen.mismatch<-(gen.offspring[o,]==2)+(gen.potential.dams[d,]<2)+(gen.potential.dams[d,]==gen.potential.sires[s,])
        hiphop.mismatch[d,s,o,"hiphop"]<-sum(gen.mismatch==3, na.rm=TRUE)
        # calculate how many loci could be compared for each test (i.e.had no missing data)
        hiphop.mismatch[d,s,o,"loci.triad"]<-sum(((!is.na(gen.offspring[o,]))*(!is.na(gen.potential.dams[d,]))*(!is.na(gen.potential.sires[s,]))), na.rm=T)
        hiphop.mismatch[d,s,o,"loci.dyad.dam"]<-sum(((!is.na(gen.offspring[o,]))*(!is.na(gen.potential.dams[d,]))), na.rm=T)
        hiphop.mismatch[d,s,o,"loci.dyad.sire"]<-sum(((!is.na(gen.offspring[o,]))*(!is.na(gen.potential.sires[s,]))), na.rm=T)
        # score if the potential.dam and potential.sire are within brood or extra brood
        dam.broods<-ind$brood[which(ind$individual==potential.dams[d])]
        sire.broods<-ind$brood[which(ind$individual==potential.sires[s])]
        offspring.brood<-ind$brood[which(ind$individual==offspring[o] & ind$type=="offspring")]
        hiphop.mismatch[d,s,o,"dam.within.brood"]<-ifelse(length(which(dam.broods==offspring.brood))>0,TRUE,FALSE)
        hiphop.mismatch[d,s,o,"sire.within.brood"]<-ifelse(length(which(sire.broods==offspring.brood))>0,TRUE,FALSE)
      }
    }
    # calculate the genotype of offspring
    triad$offspring.heterozygosity[o]<-sum(gen.offspring[o,]==2, na.rm=T)/sum(!is.na(gen.offspring[o,]), na.rm=T)
    #identify who the social parents were and if they were sampled (have genotype data)
    selection.mother<-which(ind$brood==triad$brood[o]  & ind$type=="adult female" & ind$social.parent==1)
    selection.father<-which(ind$brood==triad$brood[o]  & ind$type=="adult male" & ind$social.parent==1)
    if(length(selection.mother)>0) {  triad$dam[o]<-ind$individual[selection.mother[1]]
    } else { triad$dam[o]<-NA }
    if(length(selection.father)>0) {  triad$sire[o]<-ind$individual[selection.father[1]]
    } else {triad$sire[o]<-NA}
    triad$social.mother.sampled[o]<-ifelse(length(which(rownames(gen)==triad$dam[o]))==0,FALSE,TRUE)
    triad$social.father.sampled[o]<-ifelse(length(which(rownames(gen)==triad$sire[o]))==0,FALSE,TRUE)
  }
  # now calculate the raw output overview
  names.scores<-c("year", "brood", "offspring", "potential.dam","potential.sire","hothiphop.parents","hiphop","hot.parents","hot.dam", "hot.sire","hothiphop.dam","hothiphop.sire", "loci.dyad.dam","loci.dyad.sire","loci.triad","offspring.heterozygosity","social.mother.sampled","social.father.sampled", "is.dam.social","is.sire.social","is.dam.within.group","is.sire.within.group","social.mother","social.father")
  scores<-as.data.frame(array(NA,dim = c(length(offspring)*length(potential.dams)*length(potential.sires),length(names.scores)), dimnames=list(seq(1,length(offspring)*length(potential.dams)*length(potential.sires),1) , names.scores)))
  # the identify of the triad of offspring-potential.dam-potential.sire being compared
  scores[, "offspring"]<-rep(offspring,each=length(potential.dams)*length(potential.sires))
  scores[, "potential.dam"]<-rep(potential.dams,length(offspring)*length(potential.sires))
  scores[, "potential.sire"]<-rep(potential.sires,each=length(potential.dams))
  # the number of loci with data available for each comparison (i.e. not NA)
  scores[, "loci.dyad.dam"]<-as.numeric(as.vector(hiphop.mismatch[,,,"loci.dyad.dam"]))
  scores[, "loci.dyad.sire"]<-as.numeric(as.vector(hiphop.mismatch[,,,"loci.dyad.sire"]))
  scores[, "loci.triad"]<-as.numeric(as.vector(hiphop.mismatch[,,,"loci.triad"]))
  scores[, "offspring.heterozygosity"]<-rep(triad$offspring.heterozygosity,each=length(potential.dams)*length(potential.sires))
  # the mismatch scores
  scores[, "hot.dam"]<-as.numeric(as.vector(hiphop.mismatch[,,,"hot.dam"]))
  scores[, "hot.sire"]<-as.numeric(as.vector(hiphop.mismatch[,,,"hot.sire"]))
  scores[, "hot.parents"]<-as.numeric(as.vector(hiphop.mismatch[,,,"hot.parents"]))
  scores[, "hiphop"]<-as.numeric(as.vector(hiphop.mismatch[,,,"hiphop"]))
  scores[, "hothiphop.parents"]<-scores[, "hot.parents"]+scores[, "hiphop"]
  scores[, "hothiphop.dam"]<-scores[, "hot.dam"]+scores[, "hiphop"]
  scores[, "hothiphop.sire"]<-scores[, "hot.sire"]+scores[, "hiphop"]
  # variables needed for interpretation
  scores[, "year"]<-rep(triad$year,each=length(potential.dams)*length(potential.sires))
  scores[, "brood"]<-rep(triad$brood,each=length(potential.dams)*length(potential.sires))
  scores[, "social.mother"]<-rep(triad$dam,each=length(potential.dams)*length(potential.sires))
  scores[, "social.father"]<-rep(triad$sire,each=length(potential.dams)*length(potential.sires))
  scores[, "social.mother.sampled"]<-rep(triad$social.mother.sampled,each=length(potential.dams)*length(potential.sires))
  scores[, "social.father.sampled"]<-rep(triad$social.father.sampled,each=length(potential.dams)*length(potential.sires))
  scores[, "is.dam.social"]<-ifelse(is.na(scores[, "social.mother"])==TRUE, 0, ifelse(scores[, "social.mother"]==scores[, "potential.dam"],1, 0))
  scores[, "is.sire.social"]<-ifelse(is.na(scores[, "social.father"])==TRUE, 0, ifelse(scores[, "social.father"]==scores[, "potential.sire"],1, 0))
  scores[, "is.dam.within.group"]<-as.vector(hiphop.mismatch[,,,"dam.within.brood"])
  scores[, "is.sire.within.group"]<-as.vector(hiphop.mismatch[,,,"sire.within.brood"])
  return(scores)
}
