#' ---
#' title: Unrolling Genetic Group Functions
#' date:  2020-01-09
#' ---
#'
#' Specify parameters for testing
setwd("/Volumes/data_projekte/projekte/ketose/work/rh")
opt<- list()
opt$missingParentsFile <- "statGenGroup_4gg.txt"
opt$numberOfYears <- 5
opt$minGroupSize <- 250
opt$diffYear<- 3
opt$shiftYear <- 2
opt$breedsFile <- "../../par/gg_brd_grp_rh.txt"
opt$countryFile <- "../../par/gg_ctry_grp_rh.txt"
# Set global parameters
#======================
global_minyear <- 1900
global_maxyear <- 2100
# Read, alter and purge the statistics on missing parents
#========================================================
d.gg <- read.table(file=opt$missingParentsFile, sep=";", header = T, skip = 2)
d.gg[is.na(d.gg)] <- 0
d.gg <- d.gg[d.gg$GebJahr != "unbekannt",]
d.gg$GebJahr <- as.numeric(as.character(d.gg$GebJahr))
present_brd_ctry_comb <- unique(paste(d.gg$Rasse, d.gg$Land, sep ="_"))
present_brd_ctry_comb
!is.null(opt$breedsFile)
d.breeds <- read.table(file=opt$breedsFile, sep=";", header = T)
d.breeds$Gruppe <- as.factor(paste("bg_",d.breeds$Gruppe,sep="")) # Make the groups to factor, prefix is added for readability
dim(d.breeds)
d.breeds
vec_breeds <- readLines(con = file(opt$breedsFile))
vec_breeds
length(unique(d.breeds$Rasse)) != nrow(d.breeds)
! levels(d.gg$Rasse) %in% levels(d.breeds$Rasse)
levels(d.gg$Rasse)
levels(d.breeds$Rasse)
breeds2add <- levels(d.gg$Rasse)[! levels(d.gg$Rasse) %in% levels(d.breeds$Rasse)]
df_breeds2add <- data.frame(Rasse=breeds2add, Gruppe=breeds2add)
d.breeds <- rbind(d.breeds, df_breeds2add)
d.gg$Rasse <- factor(d.gg$Rasse, levels=d.breeds$Rasse, labels=d.breeds$Gruppe)
d.gg$Rasse
d.breeds
d.gg[1:10,]
head(d.gg)
tail(d.gg)
df_breeds2add
!is.null(opt$countryFile)
d.countries <- read.table(file=opt$countryFile, sep=";", header = T)
d.countries$Gruppe <- as.factor(paste("cg_",d.countries$Gruppe,sep="")) # Make the groups to factor, prefix is added for readability
if(length(unique(d.countries$Land)) != nrow(d.countries)){
  n_occur <- data.frame(table(d.countries$Land))
  n_occur[n_occur$Freq > 1,1]
  cat(as.character(n_occur[n_occur$Freq > 1,1]))
  message("")
  stop("Countries above appear more than once in ", opt$countryFile, " please delete the duplicates!")
}
d.countries
ctry2add <- levels(d.gg$Land)[! levels(d.gg$Land) %in% levels(d.countries$Land)]
df_ctry2add <- data.frame(Land=ctry2add, Gruppe=ctry2add)
d.countries <- rbind(d.countries, df_ctry2add)
d.gg$Land <- factor(d.gg$Land, levels=d.countries$Land, labels=d.countries$Gruppe)
head(d.gg)
tail(d.gg)
# Reduce to sires
#============================
# The deduction of the genetic groups will be done only on missing sire information
# Marius hatte diese Idee --> ich hoffe sie funktioniert genügend gut
t.gg.sires <- d.gg[,c(1:5)]
# Identify breeds that have to few missing sires for an own genetic group
#========================================================================
t.counts_per_brd <- aggregate(cbind(SP_SB, SP_SC) ~ Rasse, data = t.gg.sires, FUN=sum)
n_missing_per_brd <- data.frame(breed=t.counts_per_brd$Rasse, nmiss=t.counts_per_brd$SP_SB + t.counts_per_brd$SP_SC)
brd_to_small <- n_missing_per_brd[n_missing_per_brd$nmiss < opt$minGroupSize, "breed"]
brd_big_enough <- n_missing_per_brd[! n_missing_per_brd$breed %in% brd_to_small, "breed"]
brd_to_small
brd_big_enough
n_missing_per_brd
# Define dummy genetic group for breeds that have to few missing sires
#=====================================================================
n_dummy_gg_def <- 0
dummy_gg_def_list = list()
for(breed_l in brd_to_small) {
  counts_of_this_brd <- t.gg.sires[t.gg.sires$Rasse == breed_l,]
  for(country_l in unique(counts_of_this_brd$Land)) {
    n_dummy_gg_def <- n_dummy_gg_def + 1
    gg_def <- data.frame(Rasse = breed_l, Land = country_l, GebJahrUnten = global_minyear, GebJahrOben = global_maxyear)
    dummy_gg_def_list[[n_dummy_gg_def]] <- gg_def
  }
}
dummy_gg_def_list
counts_of_this_brd
# Identify genetic groups for breeds with enough missing sires
#=============================================================
n_gg_def <- 0
gg_code <- 0
gg_def_list = list()

#' Start unrolling of loop across breeds which have enough missing sires
# for(breed_l in brd_big_enough) {
breed_l <- brd_big_enough[1]
breed_l
#breed_l = "MO"
counts_of_this_brd <- t.gg.sires[t.gg.sires$Rasse == breed_l,]
counts_of_this_brd

t.counts_per_ctry <- aggregate(cbind(SP_SB, SP_SC) ~ Land, data = counts_of_this_brd, FUN=sum)
n_missing_per_ctry <- data.frame(country=t.counts_per_ctry$Land, nmiss=t.counts_per_ctry$SP_SB + t.counts_per_ctry$SP_SC)
ctry_to_small <- n_missing_per_ctry[n_missing_per_ctry$nmiss < opt$minGroupSize, "country"]
ctry_big_enough <- n_missing_per_ctry[! n_missing_per_ctry$country %in% ctry_to_small, "country"]

nmissing <- sum(n_missing_per_ctry[n_missing_per_ctry$country %in% ctry_to_small, "nmiss"])

levels(counts_of_this_brd$Land) <- c(levels(counts_of_this_brd$Land), "icg")  # "icg" steht für internal country group
counts_of_this_brd[counts_of_this_brd$Land %in% ctry_to_small, "Land"] <- "icg"
ctry_in_icg <- ctry_to_small

counts_of_this_brd[] <- lapply(counts_of_this_brd, function(x) if(is.factor(x)) factor(x) else x) # Remove unused factor levels

n_gg_def_within_breed <- 0
gg_def_within_breed_list = list()

country_l <- levels(counts_of_this_brd$Land)[1]

counts_of_this_brd_and_ctry <- counts_of_this_brd[counts_of_this_brd$Land == country_l,]

year <- global_minyear
minyear <- global_minyear
maxyear <- global_minyear

# while (year < global_maxyear) {

nr_years <- 0
nmissing <- 0

# while ((nmissing < opt$minGroupSize | nr_years < opt$numberOfYears) & year < global_maxyear){

tmpyear <- counts_of_this_brd_and_ctry[counts_of_this_brd_and_ctry$GebJahr==year,]
if (nrow(tmpyear) > 0) {
  nmissing <- nmissing + sum(tmpyear[,c(4:5)])
}
nr_years <- nr_years + 1
year <- year + 1

n_gg_def_within_breed <- n_gg_def_within_breed + 1
gg_def <- data.frame(Rasse = breed_l, Land = country_l, GebJahrUnten = minyear, GebJahrOben = maxyear)

