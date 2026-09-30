library(rgbif)
library(dplyr)
library(tidyr)
library(taxize)
library(magrittr)
veg_survey<- read.csv("data/event_ipt_VegGroup_v2(Associated Occurrences - South).csv")
veg_survey

get_gbif_taxonomy <- function(sp_list) {
  
  records <- lapply(sp_list, function(sp) {
    
    res <- name_backbone(name = sp)
    
    data.frame(
      Species = sp,
      kingdom = ifelse(is.null(res$kingdom), NA, res$kingdom),
      phylum = ifelse(is.null(res$phylum), NA, res$phylum),
      class = ifelse(is.null(res$class), NA, res$class),
      order = ifelse(is.null(res$order), NA, res$order),
      family = ifelse(is.null(res$family), NA, res$family),
      genus = ifelse(is.null(res$genus), NA, res$genus),
      species = ifelse(is.null(res$species), NA, res$species)
    )
  })
  
  bind_rows(records)
}


# Get the unique species names from survey
sp_list <- unique(veg_survey$scientificName)

# Get GBIF taxonomy
df <- get_gbif_taxonomy(sp_list)


# Match each species in your survey to its GBIF taxonomy
match_index <- match(veg_survey$scientificName, df$Species)


# Replace ONLY the existing taxonomy columns
veg_survey$Kingdom <- df$kingdom[match_index]
veg_survey$Phylum  <- df$phylum[match_index]
veg_survey$Class   <- df$class[match_index]
veg_survey$Order   <- df$order[match_index]
veg_survey$Family  <- df$family[match_index]
veg_survey$Genus   <- df$genus[match_index]


# Keep exactly your original columns and their original order
veg_survey <- veg_survey[, c(
  "featurePoint",
  "eventID",
  "occurrenceID",
  "basisOfRecord",
  "eventDate",
  "Habitat.type",
  "Phase.1.Habitat.Code..JNCC.",
  "Common.Name",
  "Abundance..DAFOR.",
  "Abundance.Percentage..DAFOR.scale.",
  "scientificName",
  "Kingdom",
  "Phylum",
  "Class",
  "Order",
  "Family",
  "Genus",
  "Species",
  "HighesttaxonRank",
  "What3Words",
  "decimalLatitude",
  "decimalLongitude",
  "geodeticDatum",
  "countryCode",
  "TargetNotes",
  "GroupID",
  "ResearcherID",
  "Lookup.Matrix.DAFOR"
)]


# Check the result
names(veg_survey)

write.csv(
  veg_survey,
  "results/1_clean_veg_survey.csv",
  row.names = FALSE
)

veg_tax <- read.csv("results/1_clean_veg_survey.csv") 
veg