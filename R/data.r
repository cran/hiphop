#' Genotypes of individuals scored at different loci
#'
#' A dataset containing genotypes of 1407 superb fairy wrens (Malurus cyaneus)
#' from five cohorts (breeding seasons 2014-2018) from the Australian National Botanic Garden.
#' Each individual (rows) is scored at 1376 loci (columns) with the scores meaning:
#' 0: homozygotes at common allele; 1: homozygote at rare allele and
#' 2: heterozygotes; NA: locus could not be scored.
#'
#' @format A data frame with 1407 rows (individuals) and 1376 variables (loci)
#' @author Andrew Cockburn, \email{andrew.cockburn@anu.edu.au}
#' @source Cockburn et al. (2020) HIPHOP: improved paternity assignment among
#' close relatives using a simple exclusion method for biallelic markers.
#' Molecular Ecology Resources, in revision.
"genotypes"
#' List of individuals to be compared for parentage assignment.
#'
#' A list of 2527 individuals (superb fairy wrens; Malurus cyaneus)) of which their genetics are to be compared to determine parentage.
#' The dataset consists of 1153 offspring, 469 adult females that are potential dams and 905 adult males that are potential sires.
#' Data is from five cohorts (breeding seasons 2014-2018) from the Australian National Botanic Garden.
#' Note that individuals can occur multiple times in the dataset, as adults can have parentage in multiple years.
#' Also offspring can become adults in future years.
#' Also if an individual is associated with multiple broods as social parent this means there can be multiple records per year.
#' Note that the columns social parent and brood are only used to determine whether a potential dam or sire is the social parent,
#' an extra-group (technically extra-brood) parent or within-group parent that is not the social parent (a subordinate).
#' @format A data frame with 2527 rows and 6 variables:
#' \describe{
#'   \item{brood}{ an identifier of the brood to which the offspring and adults belong/are associated with}
#'   \item{individual}{ an identifier of individual }
#'   \item{type}{ denotes whether the individual is an offspring, adult female (potential dam) or adult male (potential sire)}
#'   \item{social.parent}{ if the individual is the social parent of the brood then equal to 1, else 0}
#'   \item{year}{ the year or cohort that is being considered, adults can be potential dam or sire in some years, but no in others}
#' }
#' @author Andrew Cockburn, \email{andrew.cockburn@anu.edu.au}
#' @source Cockburn et al. (2020) HIPHOP: improved paternity assignment among
#' close relatives using a simple exclusion method for biallelic markers.
#' Molecular Ecology Resources, in revision.
"individuals"
#' List of triads to be ranked for parentage assignment.
#'
#' An example list of how the output from the hothiphop function looks like.
#' Listed are all possible triad combinations for 8 offspring from the 2018 cohort
#' This dataframe is also used to generate the vignette.
#' See the help file of the hothiphop function for an explanation of columns.
"combinations"
