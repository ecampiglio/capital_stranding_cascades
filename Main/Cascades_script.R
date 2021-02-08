
############### Cascades 2020 script ####################

### This is the main code conducting the analysis for "Capital stranding cascades: The impact of decarbonisation on productive asset utilisation"

# Authors: Louison Cahen-Fourot, Emanuele Campiglio, Antoine Godin, Eric Kemp-Benedict & Stefan Trsek

# Note: This script loads the file WIOT2014_disaggregated.Rdata, which is itself a result of the preparatory scripts "WIOD_MiningDisaggregation.R" and "ICIO_FossilRatios.R". 
# It also requires the functions from "Cascades_function_library.R"


###  Structure:
# 0. Preamble
# 1. Derivation of the matrix of stranding multipliers (S) and associated matrices
# 2. Analysis of S
# 3. Stranding rounds
# 4. Stranding cascade networks
# 5. Stranding exposure networks


# 0. Preamble ----------------------------------------------------------------

# load required packages
# package names
packages <- c("igraph","expm","plotrix","scales","RColorBrewer", "visNetwork","ggsci","plotly","ggplot2","ggthemes", "ggrepel", "dplyr","data.table","gplots","openxlsx", "Rfast", "xtable")

# install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# load packages (wrapped into invisible to suppress the list output of lapply)
invisible(lapply(packages, library, character.only = TRUE))

# favor standard numeric notation over scientific notation
options(scipen = 999) 

# load the functions written in the library file
source("Main/Cascades_function_library.R")

# load the disaggregated WIOD data (see script "WIOD_MiningDisaggregation")
load("Main/WIOT2014_disaggregated.Rdata")

# declare folder where to save figures
fig_path <- getwd()

# FOR REF: NACE default sectors: 
sectors_EU <-  c("A01", "A02", "A03", "B", "C10-12", "C13-15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_32", "C33" , "D", "E36", "E37-39", "F", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I", "J58", "J59_60", "J61", "J62_63", "K64", "K65", "K66", "L", "M69_70", "M71", "M72", "M73", "M74_75", "N77", "N78", "N79", "N80-82", "O", "P", "Q86", "Q87_88", "R90-92", "R93", "S94", "S95", "S96")
# the WIOD aggregation of NACE sectors
sectors_old <- c("A01", "A02", "A03", "B", "C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_C32", "C33", "D35", "E36", "E37-E39", "F", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I", "J58", "J59_J60", "J61", "J62_J63", "K64", "K65", "K66", "L68", "M69_M70", "M71", "M72", "M73", "M74_M75", "N", "O84", "P85", "Q", "R_S", "T", "U")
# naming of WIOD sectors: base version 
sectors_base <- c("AGRagr", "AGRfor", "AGRfis", "MIN+", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+")
# naming of WIOD sectors: final version, where the MIN sector is disaggregated
sectors <- c("AGRagr", "AGRfor", "AGRfis", "MINfos", "MINoth", "MINsup", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+")

countries <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "SWE","TUR", "TWN", "USA", "ROW")
countries_noRoW <- countries[-length(countries)]

country_sec_base <- paste0(rep(countries, each=length(sectors_base)),"_",rep(sectors_base, length(countries)))
country_sec <- paste0(rep(countries, each=length(sectors)),"_",rep(sectors, length(countries)))
country_sec_noRoW <- country_sec[1:(43*length(sectors))]

# define here the sector of focus (usually "MINfos")
sect_focus <- "MINfos"

# generate summation matrices for easy aggregation along sectors and countries
sum_sec <- matrix(diag(length(sectors)), nrow = length(sectors), ncol = length(country_sec))
sum_sec_noRoW <- matrix(diag(length(sectors)), nrow = length(sectors), ncol = length(country_sec_noRoW))
sum_country <- matrix(rep(diag(length(countries)), each = length(sectors)), nrow = length(countries), ncol = length(country_sec), byrow=T)
sum_country_noRoW <- sum_country[1:43,1:(43*length(sectors))]


# 1. Derivation of matrix S of stranding multipliers ----------------------

# 1.1 Z  ====

# create version of Z without the rest of the world (RoW)
Z_noRoW <- Z[1:(43*length(sectors)),1:(43*length(sectors))]

# create an aggregate global Z matrix (by sector): Note that RoW is not included because there's no capital stock data
Z_worldsec <- sum_sec_noRoW %*% Z_noRoW %*% t(sum_sec_noRoW); dimnames(Z_worldsec) <- list(sectors, sectors)
Z_worldsec_ext <- Z_worldsec; diag(Z_worldsec_ext) <- 0

# create an aggregate global Z matrix (by country)
Z_country <- sum_country %*% Z %*% t(sum_country); dimnames(Z_country) <- list(countries, countries)
Z_country_ext <- Z_country; diag(Z_country_ext) <- 0
Z_country_ext_noRoW <- Z_country_ext[-44,-44]

# extract all fossil mining rows from Z (all uses of MINfos products)
Z_fos_rows <- Z[grepl(paste0("_",sect_focus), rownames(Z)),]
# compute Z_fos matrix of cross-country fossil mining trade
Z_fos <- Z_fos_rows%*%t(sum_country); colnames(Z_fos) <- countries
Z_fos_noRoW <- Z_fos[-44,-44]
# compute Z_fos_ext (same as Z_fos but with diagonal set to zero)
Z_fos_ext <- Z_fos; diag(Z_fos_ext) <- 0
Z_fos_ext_noRoW <- Z_fos_ext[-44,-44]


# 1.2 output ====

# create versions of output without the rest of the world (row)
output_noRoW <- output[1:(43*length(sectors))]

# create aggregate output vectors (by sector and by country)
output_worldsec <- as.numeric(sum_sec_noRoW%*%output_noRoW); names(output_worldsec) <- sectors # RoW is EXcluded here 
output_country <- as.numeric(sum_country%*%output); names(output_country) <- countries # RoW is INcluded here


# 1.3 k ====

# create aggregate k vectors (by sector and by country)
k_worldsec <- as.numeric(sum_sec%*%k); names(k_worldsec) <- sectors
k_country <- as.numeric(sum_country%*%k); names(k_country) <- countries

# extract capital stock for fossil mining sectors
k_fos <- k[grepl(paste0("_",sect_focus), names(k))]


# 1.4 k_int ==== 

# capital intensity (sector x country): Note that RoW has 0's here due to lack of capital stock data
k_int <- k/output
# capital intensity (aggregate sectors, does not contain RoW)
k_int_worldsec <- k_worldsec/output_worldsec
# capital intensity (aggregate countries, including 0 for RoW)
k_int_country <- c(k_country/output_country)
# extract capital intensity for fossil mining sectors
k_int_fos <- k_int[grepl(paste0("_",sect_focus), names(k_int))]


# 1.5 Summary of fossil data ====

fossil_data_summary <-cbind(
  "share of global production (% excl. RoW)"= round(rowSums(Z_fos_noRoW)/sum(Z_fos_noRoW)*100, digits=3),
  "fossil prod as share of national output (%)"=round(rowSums(Z_fos)/output_country*100, digits=3)[-44],
  "share of global exports (% excl. RoW)"= round(rowSums(Z_fos_ext_noRoW)/sum(Z_fos_ext_noRoW)*100, digits=3),
  "exports as share of production (%)"= round(rowSums(Z_fos_ext)/rowSums(Z_fos)*100, digits=3)[-44],
  "share of global imports (% excl. RoW)"= round(colSums(Z_fos_ext)/sum(Z_fos_ext[,-44])*100, digits=3)[-44],
  "imports as share of consumption"= round(colSums(Z_fos_ext)/colSums(Z_fos)*100, digits=3)[-44]
)


# 1.6 B and G ====

# calculate the B matrix and the Ghosh Inverse G
B <- Z/output
B[!is.finite(B)] <- 0
G <- solve(diag(nrow(B))-B)
Gt <- t(G)
#Gt <- Gt[1:(43*length(sectors)),1:(43*length(sectors))] # omit rest of the world 

# B & G for aggregate sectors
B_worldsec <- Z_worldsec/output_worldsec # does not contain RoW
B_worldsec[!is.finite(B_worldsec)] <- 0 # not necessary
G_worldsec <- solve(diag(nrow(B_worldsec))-B_worldsec)
Gt_worldsec <- t(G_worldsec)

# B & G for aggregate countries
B_country <- Z_country/output_country
B_country[!is.finite(B_country)] <- 0 # not necessary
G_country <- solve(diag(nrow(B_country))-B_country)
Gt_country <- t(G_country) #[-44,-44]

# extract MINfos rows (for matrix B) and columns (for Gt)
B_fos_rows <- B[grepl(paste0("_",sect_focus), rownames(B)),]
B_fos_rows_rank <- sort(rowSums(B_fos_rows), decreasing = TRUE)
Gt_fos_cols <- Gt[,grepl(paste0("_",sect_focus), colnames(Gt))]


# 1.7 S ====

# compute the S matrix of stranding multipliers
S <- Gt*k_int # same as S_alt<-diag(k_int)%*%Gt but faster!
sum(!is.finite(S)) # How many NA's & Inf's?
S[!is.finite(S)] <- 0 

# calculate the total stranding multiplier of sector j
strand_tot <- colSums(S)
# calculate the internal stranding multiplier 
strand_int <- diag(S)  
# calculate the external (net) stranded assets multiplier
strand_ext <- strand_tot-strand_int
# calculate the extent to which sectors are affected by external impacts (exposure)
S_diagoff <- S; diag(S_diagoff) <- 0
strand_exp <- rowSums(S_diagoff)

# compute S & corresponding multipliers for aggregate world sectors
# using Gt_worldsec
S_worldsec <- Gt_worldsec*k_int_worldsec
sum(!is.finite(S_worldsec))
S_worldsec_ext <- S_worldsec; diag(S_worldsec_ext) <- 0
strand_tot_worldsec <- colSums(S_worldsec)
strand_int_worldsec <- diag(S_worldsec)  
strand_ext_worldsec <- strand_tot_worldsec-strand_int_worldsec
strand_exp_worldsec <- rowSums(S_worldsec)
strand_ext_exp_worldsec <- rowSums(S_worldsec_ext)

# compute S & corresponding multipliers for aggregate countries
# using Gt_country 
S_country <- Gt_country*k_int_country
sum(!is.finite(S_country)) 
S_country[!is.finite(S_country)] <- 0 
S_country_ext <- S_country; diag(S_country_ext) <- 0
strand_tot_country <- colSums(S_country)
strand_int_country <- diag(S_country)  
strand_ext_country <- strand_tot_country-strand_int_country
strand_exp_country <- rowSums(S_country_ext)

# extract all MINfos columns from S
S_fosCols <- S[, grepl(paste0("_",sect_focus), colnames(S))]
# remove internal stranding to get S_fosCols_ext
S_fosCols_ext <- S_diagoff[, grepl(paste0("_",sect_focus), colnames(S))]

# compute total & external multipliers for all MINfos sectors
strand_tot_fos <- colSums(S_fosCols)
strand_ext_fos <- colSums(S_fosCols_ext)

# compute S_fos_country matrix of cross-country fossil stranding
S_fos_country <- sum_country %*% S_fosCols; rownames(S_fos_country) <- countries
# compute S_fos_country_ext (same as S_fos_country excluding stranding within the same country)
S_fos_country_ext <- S_fos_country; diag(S_fos_country_ext) <-0
# compute total & external stranding multipliers from cross-country perspective
strand_tot_fos_country <- colSums(S_fos_country) # equivalent to strand_tot_fos!
strand_ext_fos_country <- colSums(S_fos_country_ext) 
strand_exp_fos_country <- rowSums(S_fos_country)
strand_exp_ext_fos_country <- rowSums(S_fos_country_ext)

# compute S_fos_worldsect matrix of cross-sector fossil stranding
S_fos_worldsec <- sum_sec %*% S_fosCols; rownames(S_fos_worldsec) <- sectors # includes RoW as a column (stranding caused by the RoW fossil sector)
# compute S_fos_worldsec_ext (same as S_fos_worldsec but with row MINfos set to zero --> eliminates effects within the same SECTOR (=B))
S_fos_worldsec_ext <- S_fos_worldsec ; S_fos_worldsec_ext[sect_focus,] <-0 
# compute total & external stranding multipliers from cross-sector perspective
strand_tot_fos_worldsec <- colSums(S_fos_worldsec)  # equivalent to strand_tot_fos & strand_tot_fos_country!
strand_ext_fos_worldsec <- colSums(S_fos_worldsec_ext)
strand_exp_fos_worldsec <- rowSums(S_fos_worldsec)
strand_exp_ext_fos_worldsec <- rowSums(S_fos_worldsec_ext)



# 2. Analysis of S --------------------------------------------------------

# 2.1 Global sectoral stranding ====

## Tables for S_worldsec: Compare world-sector stranding results (total, internal, external & exposure) from S_worldsec
# --> How much stranding is each aggregated world-sector causing / exposed to?
strand_worldsec_comp <- cbind(strand_tot_worldsec, strand_int_worldsec, strand_ext_worldsec, strand_exp_worldsec, strand_ext_exp_worldsec)
colnames(strand_worldsec_comp) <- c("Total stranding", "Internal stranding", "External stranding", "Total exposure", "External exposure")
# Create rankings for each column of this matrix 
rank_strand_worldsec_comp <- apply(strand_worldsec_comp, 2, rank_and_name)
# generate table for LaTex
# xtable(rank_strand_worldsec_comp[1:15,-2])
# export
# write.xlsx(rank_strand_worldsec_comp, file = "sectoral_stranding.xlsx")

# 2.2 Cross-country stranding ====

## Tables for cross-country & cross-sector stranding from the fossil sector:
# S_fos_country: rank each column to see IN WHICH COUNTRIES the individual fossil sectors cause most stranding
rank_S_fos_country <- apply(S_fos_country, 2, rank_and_name)
# S_fos_worldsec: rank each column to see IN WHICH GLOBAL SECTORS the most stranding is caused
rank_S_fos_worldsec <- apply(S_fos_worldsec, 2, rank_and_name)

## Tables for aggregate stranding multipliers from each country's MINfos sector:
# Comparing total & 3 forms of external stranding (excl. sector itself / all MINfos sectors / all national sectors)  
strand_fos_comp <- cbind("Total Stranding" = strand_tot_fos, "External Stranding (excl. sector itself)" = strand_ext_fos, "External Stranding (excl. all fossil sectors)" = strand_ext_fos_worldsec, "External Stranding (excl. own country)" = strand_ext_fos_country)
rownames(strand_fos_comp) <- countries
# Exposure of countries to fossil stranding from all global MINfos sectors (total) and all foreign MINfos sectors (external)
strand_fos_exp <- cbind("Total Exposure" = strand_exp_fos_country, "External Exposure (excl. domestic fossil sector)" = strand_exp_ext_fos_country)
# combine and rank to see which countries' fossil sectors create most stranding & which countries are most exposed to fossil stranding (excluding ROW)
rank_strand_fos_comp <- cbind(apply(strand_fos_comp[-44,], 2, rank_and_name), apply(strand_fos_exp[-44,] ,2, rank_and_name))
# Extract the columns for Total Stranding, External Stranding (excl. own country) and External Exposure
rank_strand_fos_comp_fin <- rank_strand_fos_comp[,c(1,4,6)]
colnames(rank_strand_fos_comp_fin) <- c("Total stranding", "External stranding", "External exposure")
# generate table for LaTex
# xtable(rank_strand_fos_comp_fin[1:15,-2])
# export
# write.xlsx(rank_strand_fos_comp_fin, file = "country_stranding_and_exposure.xlsx")

# 2.2a Lollipop charts ====

# for S_fos_country (stranding caused by national MINfos sectors in other countries)
# excluding internal stranding within the same country

# reformat data for plotting
S_fos_country_long <- t(S_fos_country_ext)
rownames(S_fos_country_long) = countries
S_fos_country_long <- reshape2::melt(S_fos_country_long)
names(S_fos_country_long) <- c("orig_country", "aff_country", "value") # originating & affected country
# add a column with ranks by originating country
S_fos_country_long <- S_fos_country_long %>% group_by(orig_country) %>%  
  mutate(rank_by_orig_country = ntile(value,length(sectors)))
# add a column with ranks by affected country
S_fos_country_long <- S_fos_country_long %>% group_by(aff_country) %>%
  mutate(rank_by_aff_country = ntile(value,length(sectors)))
# add column with total external (excluding stranding in the originating the country itself)
S_fos_country_long <- S_fos_country_long %>% group_by(orig_country) %>% mutate(orig_country_total = sum(value)) %>% ungroup() %>% 
  mutate(orig_country_total_rank = dense_rank(desc(orig_country_total)))
# do the same thing for mean stranding caused in each affected country (for exposure plot)
S_fos_country_long <- S_fos_country_long %>% group_by(aff_country) %>%  mutate(aff_country_total = sum(value)) %>% ungroup() %>% 
  mutate(aff_country_total_rank = dense_rank(desc(aff_country_total)))

# select top countries
top <- 10
S_fos_country_plot <- filter(S_fos_country_long, orig_country_total_rank %in% 1:top)

# this is a function that will be used in the plot to add the country totals to the right border of the plot (with stat_summary)
totals_df <- function(y, position) {return(data.frame(y = position, label = paste0(round(sum(y),2))))}

#plot
ggplot(S_fos_country_plot, aes(x = reorder(orig_country,value, mean, na.rm = TRUE), value, group = orig_country)) + # you can also reorder according to median or max
  coord_flip() +
  geom_line(color="grey", size = 1.5, alpha = 0.5) +
  # add jittered dots, making the maximum value transparent
  geom_jitter(size = 1.3, position = position_jitter(width = 0.25, height = 0, seed = 1), alpha = 1/5, 
              aes(color = I(ifelse(rank_by_orig_country == max(rank_by_orig_country), "transparent", "black")))) + # if color/shape should be country-specific, add aes(color = aff_country)
  ## add large dot for the maximum value (optionally also for mean, see below)
  stat_summary(fun = max, color=rgb(0.1,0.7,0.9), geom = "point", size = 3.5, alpha = 1) + 
  #stat_summary(fun = mean, color="yellow", geom = "point", shape = 18, size = 3, alpha = 1) +  # adds point for the mean
  ##add label to maximum value and ggrepel labels to the 2nd and 3rd largest (using the same seed as above ensures correct positioning)
  geom_text(aes(label=ifelse(rank_by_orig_country == max(rank_by_orig_country), paste(aff_country, round(value,2)),'')), 
            size = 2.5, hjust = -0.25, vjust = 0.25, color = "black") +
  geom_text_repel(aes(label=ifelse(rank_by_orig_country %in% ((length(countries)-2):(length(countries)-1)), paste(aff_country, round(value,2)),'')), 
                  size = 2, hjust = 0.2, vjust = 0.5, color = "black", 
                  segment.colour = "grey", min.segment.length	= 0.1, force = 0.1, max.overlaps = 100,   
                  position = position_jitter(width = 0.25, height = 0, seed = 1)) + # make sure that the same seed as in the geom_jitter is used!
  # add total value at the border, controlling the horizontal position with "position argument"
  stat_summary(geom="label", fun.data=totals_df, fun.args = list(position = 2.2))+
  ## add axis labels and a subtitle used as a heading for the total values (see hjust positioning argument in theme())
  labs(x = "Originating country (fossil sector)", y = "Stranding in affected countries", subtitle = "total")+
  scale_y_continuous(breaks = seq(0, round(max(S_fos_country_plot$value),0), by = 1))+
  theme_minimal() + # theme_hc works too
  theme(axis.title = element_text(), plot.subtitle = element_text(hjust = 0.975, vjust = -1), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(margin = margin(r = 0)))

# export plot
# ggsave(filename = paste0(path.expand(fig_path),"/","Lollipop_country",".pdf"), height = 6)


# for countries' exposure to all (foreign) MINfos sectors

# select top countries
top <- 10
S_fos_country_plot <- filter(S_fos_country_long, aff_country_total_rank %in% 1:top)

ggplot(S_fos_country_plot, aes(x = reorder(aff_country, value, mean, na.rm = TRUE), value, group = aff_country)) + # you can also reorder according to median or max
  coord_flip() +
  geom_line(color="grey", size = 1.5, alpha = 0.5) +
  # add jittered dots, making the maximum value transparent
  geom_jitter(size = 1.3, position = position_jitter(width = 0.25, height = 0, seed = 1), alpha = 1/5, 
              aes(color = I(ifelse(rank_by_aff_country == max(rank_by_aff_country), "transparent", "black")))) + 
  ## add large dot for the maximum value (optionally also for mean, see below)
  stat_summary(fun = max, color=rgb(0.1,0.7,0.9), geom = "point", size = 3.5, alpha = 1) + 
  #stat_summary(fun = mean, color="yellow", geom = "point", shape = 18, size = 3, alpha = 1) +  # adds point for the mean
  ##add label to maximum value and ggrepel labels to the 2nd and 3rd largest 
  geom_text(aes(label=ifelse(rank_by_aff_country == max(rank_by_aff_country), paste(orig_country, round(value,2)),'')), 
            size = 2.5, hjust = -0.25 ,vjust = 0.25, color = "black") +
  geom_text_repel(aes(label=ifelse(rank_by_aff_country %in% ((length(countries)-2):(length(countries)-1)), paste(orig_country, round(value,2)),'')),
                  size = 2, hjust = 0.2, vjust = 0.5, color = "black", 
                  segment.colour = "grey", min.segment.length	= 0.1, box.padding = 0.25, force = 1,	point.padding	= 0,
                  position = position_jitter(width = 0.25, height = 0, seed = 1)) +  # make sure that the same seed as in the geom_jitter is used!
  # add total value at the border, controlling the horizontal position with "position argument"
  stat_summary(geom="label", fun.data=totals_df, fun.args = list(position = 2.1))+
  ## add axis labels and a subtitle used as a heading for the total values (see hjust positioning argument in theme())
  labs(x = "Affected countries", y = "Stranding exposure to fossil sectors", subtitle = "total")+
  theme_minimal() + # theme_hc works too
  theme(axis.title = element_text(), plot.subtitle = element_text(hjust = 0.975, vjust = -1), 
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.y = element_text(margin = margin(r = 0)))

# export plot
# ggsave(filename = paste0(path.expand(fig_path),"/","Lollipop_country_exposure",".pdf"), height = 6)


# 3. Stranding rounds  -----------------------------------------------------

# 3.1 Rounds computation ----

# Computation of the power series & stranding rounds matrices

## for S_worldsec (from B_worldsec)

# first create a list containing the first 8 powers of B_worldsec
B_powers_worldsec <- list(diag(length(sectors)), B_worldsec) # the first two elements are given by the identity matrix and B(^1)
# the loop adds elements to the series by multiplying the previous element with B
for (j in 3:9){
 B_powers_worldsec[[j]] <- mat.mult(B_powers_worldsec[[j-1]], B_worldsec) # mat.mult from rfast package makes multiplication much faster
 dimnames(B_powers_worldsec[[j]]) <- list(sectors, sectors)
}
names(B_powers_worldsec) <- paste0(rep("Round",9),0:8)
B_powers_worldsec <- lapply(B_powers_worldsec, function(x){dimnames(x) <- dimnames(Z_worldsec); return(x)})

# then pre-multiply the transpose of each power matrix with diagonalised k_int_worldsec (same but faster: t(x)*k_int_worldsec )
Rounds_worldsec <- lapply(B_powers_worldsec, function(x) {
  Round <- t(x)*k_int_worldsec; dimnames(Round)<-dimnames(Gt_worldsec); return(Round) })
Rounds_worldsec <- lapply(Rounds_worldsec, function(x){ 
  x[is.na(x)] <-0 ; return(x)}) # set NA's to zero


## for S (from B)
B_powers <- list(diag(length(country_sec)), B)
for (j in 3:9){
  B_powers[[j]] <- mat.mult(B_powers[[j-1]], B) 
  dimnames(B_powers[[j]]) <- list (country_sec, country_sec)
}
names(B_powers) <- paste0(rep("Round",9),0:8)
B_powers <- lapply(B_powers, function(x){dimnames(x) <- dimnames(Z); return(x)})

Rounds <- lapply(B_powers, function(x) {Round <- t(x)*k_int; dimnames(Round)<-dimnames(Z); return(Round)})
Rounds <- lapply(Rounds, function(x){ x[is.na(x)] <- 0 ; return(x)}) # set NA's to zero


# 3.2 Rounds barcharts: Shock in aggregate world sectors----

## for world sector level (S_worldsec)

# create a matrix containing the first 8 rounds of stranding from the MINfos sector
fos_Rounds_worldsec <- sapply(Rounds_worldsec, function(x){return(x[,sect_focus])})
# append a column of total stranding (column MINfosof S_worldsec)
fos_Rounds_worldsec <- cbind(fos_Rounds_worldsec, "Total" = S_worldsec[,sect_focus])

# set plotting colors
barchart_cols <- c("initial_shock" = 'rgba(13, 115, 108, 0.9)', "round1" = 'rgba(38, 24, 74, 0.9)', "round2" = 'rgba(38, 24, 74, 0.6)', "round3" = 'rgba(38, 24, 74, 0.4)', "further_rounds" = 'rgba(38, 24, 74, 0.2)')

# plot the rounds as ordered barcharts, displaying first 3 rounds separately and remaining rounds as aggregate segment
# it can be defined whether internal stranding (referring to the sector of the shock origin) and the initial shock should be displayed

bar_worldsec <- plotly_barchart (Rounds_matrix = fos_Rounds_worldsec, sector = sect_focus, aggregation = "none", internal_strand = T, initial_shock = F, top = 10, title = "Exposure to global fossil stranding", cols = barchart_cols)
bar_worldsec

# save plots (NOTE: saving plotly charts as static image requires manual installation of the orca app. See https://github.com/plotly/orca for more information)
# orca(bar_worldsec, file = "Rounds_worldsectors.pdf")


# 3.3 Rounds vis: Shock from a single country-sector  ----

# country-sector level results

cntry_focus <- "CAN"
country_sect_focus <- paste0(cntry_focus,"_",sect_focus)
# create a matrix containing the first 8 rounds of stranding from the focus country MINfos sector
fos_Rounds_single <- sapply(Rounds, function(x){return(x[,country_sect_focus])})
# append a column of total stranding
fos_Rounds_single <- cbind(fos_Rounds_single, "Total" = S[,country_sect_focus])
#plot
bar_single <- plotly_barchart(Rounds_matrix = fos_Rounds_single, sector = country_sect_focus, aggregation = "none", internal_strand = T, initial_shock = T, top = 10, title = paste0("Exposure to stranding from the ",cntry_focus, " fossil sector"))
bar_single

# country-level  
bar_single_country <- plotly_barchart(Rounds_matrix = fos_Rounds_single, sector = country_sect_focus, aggregation = "country", internal_strand = T, initial_shock = T, top = 10, title = paste0("Exposure to stranding from the ",cntry_focus, " fossil sector"))
bar_single_country

# world-sector level 
bar_single_worldsec <- plotly_barchart(Rounds_matrix = fos_Rounds_single, sector = country_sect_focus, aggregation = "sector", internal_strand = T, initial_shock = T, top = 10, title = paste0("Exposure to stranding from the ",cntry_focus, " fossil sector"))
bar_single_worldsec

# save plots
#orca(bar_single, file = "Rounds_single.pdf"))
#orca(bar_single_country, file = "Rounds_single_country.pdf"))
#orca(bar_single_worldsec, file = "Rounds_single_worldsec.pdf"))



# 4. Stranding cascade networks  -----------------------------------------------------

# 4.1 World-sector cascades ----

# compute network for S_worldsec: stranding from world-level MINfos sector on other world-sectors

# define parameters
node <- sect_focus # originating sector 
depth <- 3 # maximum number of layers
n_top <- 3 # threshold number "n" of top stranding links to be selected for each node

# generate network 
# the cascade_network function takes only the first stranding Round matrix and uses Bt to compute power-series-like input losses for each node 
S1_worldsec <- Rounds_worldsec$Round1
cn_worldsec <- cascade_network_fin(matrix = S1_worldsec, node = node, n_top = n_top, depth = depth, B_matrix = B_worldsec) 

# the layout_network function below transforms the igraph network object to a visNetwork dataframe and sets basic layout parameters (node / edge labels & attributes) within this dataframe
# the type of the input network needs to be defined (either "worldsec" for global sectors or "standard" for country-level sectors)
# it also requires a "Rounds" argument according to this type, which is the Rounds list containing the total stranding multipliers per round, which are displayed in the nodes
cn_worldsec_vis_dat <- layout_network(network = cn_worldsec,  type = "worldsec", strand_rounds = Rounds_worldsec, edgewidth_factor = 30)
# optional: remove edge values lower than a given threshold (improve readability)
cn_worldsec_vis_dat$edges$label[cn_worldsec_vis_dat$edges$label<0.001] <- NA

# plot network: this function plots the visNetwork, defining title, node spacing & size, as well as color attributes if they aren't already defined in the network dataframe
cn_worldsec_plot <- plot_network(network = cn_worldsec_vis_dat, # title = "World sectors stranding from MINfos", 
             node_background = "rgba(190,190,190,0.75)", node_border = "rgba(190,190,190,0.75)", node_name_color = "black", node_value_color = "darkblue",
             edge_color = "rgba(120,120,120,0.5)", edge_vale_color = "rgba(0, 127, 127, 0.85)", stroke_color = "rgba(255,255,255,0.85)", highlight_color = "gold", 
             node_height = 80, node_dist = 140, layer_sep = 250, physics = T)
cn_worldsec_plot

# export (NOTE: the visExport command will open a window in which you can manually drag nodes as desired and then export it to png by clicking the export button)
# visExport(cn_worldsec_plot, type = "png") # or pdf

# optional: save as html
# visSave(cn_worldsec_plot, file = "cn_worldsec.html")


# 4.2 Cross-country cascades ----

# compute network for S: stranding from an individual country MINfos sector on other sectors within and outside the country

# define parameters
cntry <-"AUS" 
node <- paste0(cntry,"_",sect_focus)
depth <- 3
n_top <- 3
S1 <- Rounds$Round1

# generate network
cn <- cascade_network_fin(matrix = S1, node = node, n_top = n_top, depth = depth, B_matrix = B)

# transform the igraph network object to a visNetwork data frame and set layout parameters
cn_vis_dat <- layout_network(network = cn, type = "standard", strand_rounds = Rounds, edgewidth_factor = 50)

# plot network
cn_plot <- plot_network(network = cn_vis_dat, title = paste0("Stranding from ", node), node_height = 100 )
cn_plot

# export
#visExport(cn_plot, type = "png") # or pdf


# 5. Stranding exposure networks  -----------------------------------------------------

# generate exposure networks for the most fossil-exposed sectors of selected countries

# define parameters 
cntry <- "USA" # country to be investigated 
n_exp <- 3 # number of top exposed sectors to include
# the "m_top" argument specifies how many ("m") of the most important 1-, 2- and 3-step linkages arriving in the exposed sectors should be displayed
m_top <- 2
# the depth argument (possible values: 2 or 3) defines whether only 2 or 3 steps should be displayed
## NOTE: if depth 3 is used, the computation can take several minutes (depending on your machine and the number of n_exp and m_top)
depth <- 2

# extract sectors that are most exposed to fossil stranding (total or external, i.e. only to foreign fossil sectors)
S_fos_exp_tot <- rowSums(S_fosCols[grepl(paste0(cntry,"_"), rownames(S_fosCols)),])
S_fos_exp_ext <- rowSums(S_fosCols[grepl(paste0(cntry,"_"), rownames(S_fosCols)), colnames(S_fosCols) != paste0(cntry,"_",sect_focus)])
exposed <- names(head(sort(S_fos_exp_ext[names(S_fos_exp_ext)!=paste0(cntry,"_",sect_focus)], decreasing = TRUE),n_exp))

# generate network
cn_exp <- exposure_network_fin(exposed = exposed, m_top = m_top, depth = depth, S1 = S1, B = B, color_1 = 'rgba(0,0,102,0.75)', color_2 = 'rgba(255,0,0,0.75)', color_3 = 'rgba(250,210,0,0.75)')
# transform the igraph network object to a visNetwork data frame and set layout parameters
cn_exp_vis_dat <- layout_network(network = cn_exp,  type = "exposure", edgewidth_factor = 30, edgelabel = TRUE)
# plot (deactivating physics lets you drag nodes manually)
cn_exp_plot <- plot_network(network = cn_exp_vis_dat, node_height = 70, node_dist = 180, layer_sep = 200, physics = T)
cn_exp_plot
# export
# visExport(cn_exp_plot, type = "png") # or pdf  

