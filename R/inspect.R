#' A function that inspects the genotypes of individuals
#'
#' This function allows one to inspect the genotypes of all individuals in your input file to see
#' whether genetic data is available, and if so what the proportion of
#' homozygotes, heterozygotes fraction of missing values is.
#' @param ind The input file with individuals, which should contain at least the columns
#' brood, individual, type, social.parent, year.
#' @param gen The input file with genotypes, which should contain the loci-names as column headers
#' and the individual  names as row-header and should only contain the values 0, 1, 2 or NA.
#' @return the individuals file with a summary of the genotypes attached.
#'#' \describe{
#'   \item{brood}{ an identifier of the brood to which the offspring and adults belong/are associated with}
#'   \item{individual}{ an identifier of the offspring }
#'   \item{type}{ denotes whether the individual is an offspring, adult female (potential dam) or adult male (potential sire)}
#'   \item{social.parent}{ if the individual is the social parent of the brood then equal to 1, else 0}
#'   \item{year}{ the year or cohort that is being considered, adults can be potential dam or sire in some years, but no in others}
#'   \item{sampled}{if the individual's genotypic data is in the genotypes file then equal to 1, else 0}
#'   \item{homozygote.0}{ the proportion of loci that was scored as 0 (homozygotes at common allele) }
#'   \item{homozygote.1}{ the proportion of loci that was scored as 1 (homozygotes at rare allele) }
#'   \item{homozygote.2}{ the proportion of loci that was scored as 2 (heterozygote) }
#'   \item{missing}{ the proportion of loci that has missing values (NA) }
#'   \item{number.loci}{ the number of loci that has genotypic information  (i.e. not NA) }
#' }
#' @author Martijn van de Pol, \email{martijn@myscience.eu}
#' @examples
#' overview<-inspect(ind=individuals[1:22,], gen=genotypes)
#' head(overview)
#' @export
inspect<-function(ind, gen) {
  # first check for correct input format of ind and gen data
  ind.types<-c("offspring", "adult female", "adult male")
  ind.col.names<-c("brood", "individual", "type", "social.parent", "year")
  if( (length(ind.col.names)-sum(colnames(ind) %in% ind.col.names))>0) {stop("In the individual file there should be the following columns: ", ind.col.names) }
  if( (length(ind$type)-sum(ind$type %in% ind.types)) >0) {stop("In the individual file there should only be the following types: ", ind.types)  }
  if(length(which(gen==0))+length(which(gen==1))+length(which(gen==2))+length(which(is.na(gen)==TRUE))!=(dim(gen)[1]*dim(gen)[2])) {
    stop("there should only be 0, 1,2 and possible NA values in the genotypes")
  }
  broods<-unique(ind$brood)
  for(b in 1:length(broods)) { if(length(which(ind$brood==broods[b] & ind$social.parent==1 & ind$type=="adult male"))>1) {stop("brood ", broods[b], " has multiple social fathers")}     }
  for(b in 1:length(broods)) { if(length(which(ind$brood==broods[b] & ind$social.parent==1 & ind$type=="adult female"))>1) {stop("brood ", broods[b], " has multiple social mothers")} }
  if( (length(ind$social.parent)-sum(ind$social.parent %in% c(0,1))) >0) {stop("In the individual file there should only be the following value for social parent: 0 or 1")}
  # next summarize the genotypic information for each individual
  output<-ind
  output[,c("sampled","homozygote.0","homozygote.1","heterozygote.2","missing", "number.loci")]<-output$year*NA
  for(i in 1:length(output$individual)) {
    selected.gen<-as.numeric(gen[which(rownames(gen)==output$individual[i]),])
    output$homozygote.0[i]<-length(which(selected.gen==0))/ dim(gen)[2]
    output$homozygote.1[i]<-length(which(selected.gen==1))/ dim(gen)[2]
    output$heterozygote.2[i]<-length(which(selected.gen==2))/ dim(gen)[2]
    output$missing[i]<-length(which(is.na(selected.gen)==TRUE))/ dim(gen)[2]
    ifelse(output$missing[i]==1,   output$sampled[i]<-0,   output$sampled[i]<-1)
    }
  output$number.loci<-dim(gen)[2]-dim(gen)[2]*output$missing
  rownames(output) <- NULL
  return(output)
}
