
<!-- README.md is generated from README.Rmd. Please edit that file -->

### Authors

Martijn van de Pol, Andrew Cockburn and Lyanne Brouwer 12 July 2020

## What is R package hiphop for?

R package hiphop is based on the hiphop test that can be used for
parentage assignment using a simple exclusion method for biallelic
markers, even when comparing among close relatives. The rationale behind
this parentage assignment method is described in detail in Cockburn et
al. in revision Molecular Ecology Resources (for full reference use
citation(“hiphop”)).

### The basic idea

The basic idea behind the hiphop parentage assignment is that we can
compare the genotypes of any combination of offspring-potential dam and
sire (i.e. genetic mother or fathers) by comparing the exclusion scores
of these individuals at bi-allelic markers. Previous methods focused on
exclusion based on the Homozygous Opposite Test (HOT; Huisman 2017;
<https://doi.org/10.1111/1755-0998.12665>). HOT compares the genotype of
an offspring with a potential parent: a mismatch is scored when both the
offspring and parent are homozygous, but for different alleles. Cockburn
et al. suggested an additional exclusion criterion HIPHOP (Homozygous
Identical Parents, Heterozygous Offspring are Precluded), which compares
the genotype of an offspring with both potential parents (ie. a dyad of
potential dam and potential sire): a mismatch is scored when the
offspring is heterozygous and both parent are homozygous for the same
allele. The HOT and HIPHOP scores are by default calculated as ratios:
the HOT score is the number of HOT mismatches divided by the number of
loci at which the offspring is homozygote, while the HIPHOP score is the
number of HIPHOP mismatches divided by the number of loci the offspring
is heterozygotes (this standardization is important as individuals can
differ substantially in the number of loci at which they are homo- or
hetero-zygote). The HOT and HIPHOP scores can be combined to provide
information about the total of mismatches and is proposed as an improved
exclusion criterion for parentage assignment. Finally, in species where
the there is contextual information (e.g. seen egg laying, lactating)
that the social mother (father) is always the genetic mother (father) it
is possible to condition the paternity (maternity) assignment on the dam
(sire) being the social mother (father).

## Installation

You can install the released version of hiphop from
[CRAN](https://CRAN.R-project.org) using install.packages(“hiphop”) and
start using it with library(hiphop)

## Vignette with worked examples

The introduction vignette associated with the package gives a worked
example on the data used in Cockburn et al. paper.

## Questions about the package and reporting bugs

Please ask all questions about the package and report bugs or
suggestions for improvement on:
<https://groups.google.com/d/forum/r-hiphop> We aim to answer these
questions as quickly as possible.
