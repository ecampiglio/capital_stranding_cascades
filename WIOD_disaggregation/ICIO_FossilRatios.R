
######## Computation of fossil ratios from the OECD ICIO ##########

# Structure of code: Ratios for...
# 1. Total Output
# 2. Final Demand
# 3. Value Added
# 4. Transaction Matrix Z

### NOTE ####
# This script requires the OECD ICIO table for the year 2014. To run the code, please download the ICIO table in csv format from https://www.oecd.org/sti/ind/inter-country-input-output-tables.htm.
# It also requires the ICIO-WIOD sector correspondence sheet, contained in the Data folder

# load required packages
library(openxlsx)

# read in ICIO and extract total output column
ICIO <- read.csv("WIOD_disaggregation/Data/ICIO2018_2014.csv", row.names = 1) ## file not contained, see note above
ICIO <- as.matrix(ICIO)

# 1. total output -----------------------------------------------------

bottom <- (which(rownames(ICIO) == "AUS_TAXSUB")-1)
ICIO_output <- ICIO[1:bottom, "TOTAL"]
names(ICIO_output) <- rownames(ICIO)[1:bottom]

# extract countries
countries_ICIO <- unique(substr(names(ICIO_output),1,3))

# extract fossil and non-fossil mining sectors
ICIO_output_fos <- ICIO_output[grepl("05T06", names(ICIO_output))]
ICIO_output_nonfos <- ICIO_output[grepl("07T08", names(ICIO_output))]
ICIO_output_supp <- ICIO_output[grepl("09", names(ICIO_output))]

# bind them to a matrix
ICIO_MIN_output <- cbind(ICIO_output_fos, ICIO_output_nonfos, ICIO_output_supp)
dimnames(ICIO_MIN_output) <- list(countries_ICIO, c("fos", "oth", "sup"))

# assign the ICIO countries to the corresponding WIOD countries / regions
countries_ICIOtoWIOD <- cbind("ICIO" = countries_ICIO, 
                              "WIOD" = c("AUS", "AUT", "BEL", "CAN", "CHE", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "ROW", "IRL", "ROW", "ITA", "JPN", "KOR", "LVA", "LTU", "LUX", "MEX", "NLD", "ROW", "NOR", "POL", "PRT", "SVK",
                                         "SVN", "ESP", "SWE", "CHE", "TUR", "GBR", "USA", "ROW", "BRA", "ROW", "BGR", "ROW", "CHN", "ROW", "ROW", "HRV", "CYP", "IND", "IDN", "CHN", "ROW", "ROW", "MLT", "ROW", "ROW", "ROW", "ROU", "RUS", "ROW",
                                         "ROW", "ROW", "TWN", "ROW", "ROW", "ROW", "ROW", "MEX", "MEX", "CHN", "CHN")) 

# aggregate countries according to WIOD
ICIO_MIN_output_agg <- rowsum(ICIO_MIN_output, group = countries_ICIOtoWIOD[,2], reorder = FALSE)

# reorder according to WIOD 
countries_true <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "SWE","TUR", "TWN", "USA", "ROW")
ICIO_MIN_output_agg <- ICIO_MIN_output_agg[match(countries_true, rownames(ICIO_MIN_output_agg)),]

# compute fossil ratios
FossilRatiosOutput_ICIO <- ICIO_MIN_output_agg/rowSums(ICIO_MIN_output_agg)


# 2. final demand -----------------------------------------------------

ICIO_fd <- ICIO[1:bottom,(which(colnames(ICIO)=="CN2_97T98")+1):(which(colnames(ICIO)=="TOTAL")-1)]
ICIO_fd <- rowSums(ICIO_fd)

# extract fossil and non-fossil mining sectors
ICIO_fd_fos <- ICIO_fd[grepl("05T06", names(ICIO_fd))]
ICIO_fd_nonfos <- ICIO_fd[grepl("07T08", names(ICIO_fd))]
ICIO_fd_supp <- ICIO_fd[grepl("09", names(ICIO_fd))]

# bind them to a matrix
ICIO_MIN_fd <- cbind(ICIO_fd_fos, ICIO_fd_nonfos, ICIO_fd_supp)
dimnames(ICIO_MIN_fd) <- list(countries_ICIO, c("fos", "oth", "sup"))

#aggregate countries according to WIOD
ICIO_MIN_fd_agg <- rowsum(ICIO_MIN_fd, group = countries_ICIOtoWIOD[,2], reorder = FALSE)
# reorder according to WIOD 
ICIO_MIN_fd_agg <- ICIO_MIN_fd_agg[match(countries_true, rownames(ICIO_MIN_fd_agg)),]

# compute fossil ratios
FossilRatiosFD_ICIO <- ICIO_MIN_fd_agg/rowSums(ICIO_MIN_fd_agg)
# replace 0-ratios by 10e-10 (necesary for TRAS balancing algorithm to converge!)
FossilRatiosFD_ICIO[FossilRatiosFD_ICIO == 0] <- 10e-10
min(FossilRatiosFD_ICIO)


# 3. value Added -----------------------------------------------------

ICIO_va_block <- ICIO[(which(rownames(ICIO) == "AUS_TAXSUB"):which(rownames(ICIO) == "VALU")), 1:bottom]
ICIO_va <- colSums(ICIO_va_block)
names(ICIO_va) <- names(ICIO_output)

# extract fossil and non-fossil mining sectors
ICIO_va_fos <- ICIO_va[grepl("05T06", names(ICIO_va))]
ICIO_va_nonfos <- ICIO_va[grepl("07T08", names(ICIO_va))]
ICIO_va_supp <- ICIO_va[grepl("09", names(ICIO_va))]

# bind them to a matrix
ICIO_MIN_va <- cbind(ICIO_va_fos, ICIO_va_nonfos, ICIO_va_supp)
dimnames(ICIO_MIN_va) <- list(countries_ICIO, c("fos", "oth", "sup"))

#aggregate countries according to WIOD
ICIO_MIN_va_agg <- rowsum(ICIO_MIN_va, group = countries_ICIOtoWIOD[,2], reorder = FALSE)
# reorder according to WIOD 
ICIO_MIN_va_agg <- ICIO_MIN_va_agg[match(countries_true, rownames(ICIO_MIN_va_agg)),]

# compute fossil ratios
FossilRatiosVA_ICIO <- ICIO_MIN_va_agg/rowSums(ICIO_MIN_va_agg)

# replace 0-ratios by 10e-10 (necessary for TRAS balancing algorithm to converge!)
FossilRatiosVA_ICIO[FossilRatiosVA_ICIO == 0] <- 10e-10
min(FossilRatiosVA_ICIO)


# 4. Z Matrix -----------------------------------------------------

ICIO_Z <- ICIO[1:bottom, 1:bottom]

# aggregate countries according to WIOD
Country_aggregate <- paste0(rep(countries_ICIOtoWIOD[,2], each = 36),"_", substring(rownames(ICIO_Z),first = 5))
ICIO_Z_agg <- rowsum(ICIO_Z, group = Country_aggregate)
ICIO_Z_agg <- t(rowsum(t(ICIO_Z_agg), group = Country_aggregate))
# reorder (not necessary)
#country_sec_true1 <- paste0(rep(countries_true, each = 36),"_",substring(rownames(ICIO_Z_agg),first = 5))
#ICIO_Z_agg <- ICIO_Z_agg[match(country_sec_true1, rownames(ICIO_Z_agg)), match(country_sec_true1, colnames(ICIO_Z_agg))]

# add the NACE U99 (EXT+) sector (included in WIOD but not ICIO) by adding rows/columns of zeros (will be split equally later)
ICIO_Z_agg <- cbind(ICIO_Z_agg, matrix(0, ncol = length(countries_true), nrow = nrow(ICIO_Z_agg), dimnames = list(NULL, paste0(countries_true,"_99"))))
ICIO_Z_agg <- rbind(ICIO_Z_agg, matrix(0, ncol = ncol(ICIO_Z_agg), nrow = length(countries_true), dimnames = list(paste0(countries_true,"_99"), NULL)))

## get matrix to same dimension as WIOD, plus disaggregation of MIN in 3 sub-sectors
# extract ICIO sector codes
ICIO_sect <- unique(substring(rownames(ICIO_Z_agg),first = 5))
# load ICIO to WIOD sector correspondence
ICIO_to_WIOD <- read.xlsx("WIOD_disaggregation/Data/ICIO_to_WIOD.xlsx", colNames = TRUE)
#generate correspondence for every country
country_sec_ICIOtoWIOD <- paste0(rep(countries_true,each=58),"_",rep(ICIO_to_WIOD$ICIO, 44))
# expand ICIO to WIOD dimension, duplicating sectors with lower disaggregation (this is okay because we only take the relative ratios for disaggregation)
ICIO_Z_WIOD <- ICIO_Z_agg[match(country_sec_ICIOtoWIOD, rownames(ICIO_Z_agg)), match(country_sec_ICIOtoWIOD, colnames(ICIO_Z_agg)) ]

# rename to WIOD names
sectors_WIOD_disagg <- as.character(ICIO_to_WIOD$WIOD)
country_sec_WIOD_disagg <- paste0(rep(countries_true, each = 58),"_",rep(sectors_WIOD_disagg, 44))
dimnames(ICIO_Z_WIOD) <- list(country_sec_WIOD_disagg, country_sec_WIOD_disagg)


### 4.1 ratios for Z columns: 

# extract mining columns
ICIO_Z_WIOD_MINcols <- ICIO_Z_WIOD[, grepl("MIN", colnames(ICIO_Z_WIOD))]
# re-aggregate 3 mining sectors in rows
MIN_aggregate_rows <- gsub("_MIN(.*)", "_MIN+", rownames(ICIO_Z_WIOD_MINcols))
ICIO_Z_WIOD_MINcols <- rowsum(ICIO_Z_WIOD_MINcols, group=MIN_aggregate_rows, reorder=F)

# finally, compute fossil ratios
ICIO_Z_WIOD_MINcols_fos <- ICIO_Z_WIOD_MINcols[,seq(1, by=3, length.out=44)]
ICIO_Z_WIOD_MINcols_oth <- ICIO_Z_WIOD_MINcols[,seq(2, by=3, length.out=44)]
ICIO_Z_WIOD_MINcols_sup <- ICIO_Z_WIOD_MINcols[,seq(3, by=3, length.out=44)]

ICIO_Z_WIOD_MINcols_fos_ratio <- ICIO_Z_WIOD_MINcols_fos / (ICIO_Z_WIOD_MINcols_fos + ICIO_Z_WIOD_MINcols_oth + ICIO_Z_WIOD_MINcols_sup)
ICIO_Z_WIOD_MINcols_oth_ratio <- ICIO_Z_WIOD_MINcols_oth / (ICIO_Z_WIOD_MINcols_fos + ICIO_Z_WIOD_MINcols_oth + ICIO_Z_WIOD_MINcols_sup)
ICIO_Z_WIOD_MINcols_sup_ratio <- ICIO_Z_WIOD_MINcols_sup / (ICIO_Z_WIOD_MINcols_fos + ICIO_Z_WIOD_MINcols_oth + ICIO_Z_WIOD_MINcols_sup) #1 - (ICIO_Z_WIOD_MINcols_fos_ratio + ICIO_Z_WIOD_MINcols_oth_ratio); rownames(ICIO_Z_WIOD_MINcols_sup_ratio) <- rownames(ICIO_Z_WIOD_MINcols_sup)

FossilRatiosCols_ICIO <- array(c(ICIO_Z_WIOD_MINcols_fos_ratio, ICIO_Z_WIOD_MINcols_oth_ratio, ICIO_Z_WIOD_MINcols_sup_ratio), dim = c(nrow(ICIO_Z_WIOD_MINcols_fos), ncol(ICIO_Z_WIOD_MINcols_fos), 3))
dimnames(FossilRatiosCols_ICIO) <- list(rownames(ICIO_Z_WIOD_MINcols_fos), countries_true, c("fos", "oth", "sup"))

# replace NaN's by 1/3 (in absence of better information)
FossilRatiosCols_ICIO[is.nan(FossilRatiosCols_ICIO)] <- 1/3
sum(!is.finite(FossilRatiosCols_ICIO))
min(FossilRatiosCols_ICIO)
# replace values < 0 by 0 (if there are any)
FossilRatiosCols_ICIO[FossilRatiosCols_ICIO < 0] <- 0
# check if ratios sum up to 1 across the 3 sub-sectors
check_cols <- apply(FossilRatiosCols_ICIO, c(1,2), sum)
min(check_cols) ; max(check_cols)

# replace 0-ratios by 10e-10 (necessary for TRAS balancing algorithm to converge)
FossilRatiosCols_ICIO[FossilRatiosCols_ICIO == 0] <- 10e-10 
#FossilRatiosCols_ICIO[FossilRatiosCols_ICIO == 1] <- 1 - 10e-10 # not necessary!
min(FossilRatiosCols_ICIO)


### 4.2. ratios for Z rows

# extract mining rows
ICIO_Z_WIOD_MINrows <- ICIO_Z_WIOD[grepl("MIN", colnames(ICIO_Z_WIOD)),]

# compute fossil ratios for rows
ICIO_Z_WIOD_MINrows_fos <- ICIO_Z_WIOD_MINrows[seq(1, by=3, length.out=44),]
ICIO_Z_WIOD_MINrows_oth <- ICIO_Z_WIOD_MINrows[seq(2, by=3, length.out=44),]
ICIO_Z_WIOD_MINrows_sup <- ICIO_Z_WIOD_MINrows[seq(3, by=3, length.out=44),]

ICIO_Z_WIOD_MINrows_fos_ratio <- ICIO_Z_WIOD_MINrows_fos / (ICIO_Z_WIOD_MINrows_fos + ICIO_Z_WIOD_MINrows_oth + ICIO_Z_WIOD_MINrows_sup)
ICIO_Z_WIOD_MINrows_oth_ratio <- ICIO_Z_WIOD_MINrows_oth / (ICIO_Z_WIOD_MINrows_fos + ICIO_Z_WIOD_MINrows_oth + ICIO_Z_WIOD_MINrows_sup)
ICIO_Z_WIOD_MINrows_sup_ratio <- ICIO_Z_WIOD_MINrows_sup / (ICIO_Z_WIOD_MINrows_fos + ICIO_Z_WIOD_MINrows_oth + ICIO_Z_WIOD_MINrows_sup) # 1 - (ICIO_Z_WIOD_MINrows_fos_ratio + ICIO_Z_WIOD_MINrows_oth_ratio); rownames(ICIO_Z_WIOD_MINrows_sup_ratio) <- rownames(ICIO_Z_WIOD_MINrows_sup)

FossilRatiosRows_ICIO <- array(c(ICIO_Z_WIOD_MINrows_fos_ratio, ICIO_Z_WIOD_MINrows_oth_ratio, ICIO_Z_WIOD_MINrows_sup_ratio), dim = c(nrow(ICIO_Z_WIOD_MINrows_fos), ncol(ICIO_Z_WIOD_MINrows_fos), 3))
dimnames(FossilRatiosRows_ICIO) <- list(countries_true, colnames(ICIO_Z_WIOD_MINrows_fos), c("fos", "oth", "sup"))

# replace NaN's by 1/3
FossilRatiosRows_ICIO[is.nan(FossilRatiosRows_ICIO)] <- 1/3
sum(!is.finite(FossilRatiosRows_ICIO))
min(FossilRatiosRows_ICIO)
# replace values < 0 by 0 (if there are any)
FossilRatiosRows_ICIO[FossilRatiosRows_ICIO < 0] <- 0
# check if all ratios sum up to 1
check_rows <- apply(FossilRatiosRows_ICIO, c(1,2), sum)
min(check_rows) ; max(check_rows)

# replace 0-ratios by 10e-10 (necessary for TRAS balancing algorithm to converge)
FossilRatiosRows_ICIO[FossilRatiosRows_ICIO == 0] <- 10e-10
#FossilRatiosCols_ICIO[FossilRatiosCols_ICIO == 1] <- 1 - 10e-10 # not necessary!
min(FossilRatiosRows_ICIO)


### Save all results to an RData file
# save(FossilRatiosCols_ICIO,FossilRatiosRows_ICIO, FossilRatiosOutput_ICIO, FossilRatiosFD_ICIO, FossilRatiosVA_ICIO, file = "FossilRatios2014_ICIO.RData")


   