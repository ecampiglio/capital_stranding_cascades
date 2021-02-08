
###### Disaggregation of the WIOD mining sector and consequent balancing of the table #######

# Note: This script loads the file "FossilRatios2014_ICIO.Rdata", which is itself the result of the script "ICIO_FossilRatios.R"

# Structure of the code
# 1. Disaggregation of WIOD mining sectors using ratios from OECD ICIO
# 2. Balancing using TRAS algorithm

# load the WIOT (World Input-Output Tables)
load("WIOD_disaggregation/Data/WIOT2014_October16_ROW.RData")

# load the fossil ratios of the mining sector from  OECD ICIO
load("WIOD_disaggregation/Data/FossilRatios2014_ICIO.Rdata")

# load the functions written in the library file, including the TRAS balancing function 
source("Main/Cascades_function_library.R")

## define sectors
# FOR REF: NACE default sectors: 
sectors_EU <-  c("A01", "A02", "A03", "B", "C10-12", "C13-15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_32", "C33" , "D", "E36", "E37-39", "F", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I", "J58", "J59_60", "J61", "J62_63", "K64", "K65", "K66", "L", "M69_70", "M71", "M72", "M73", "M74_75", "N77", "N78", "N79", "N80-82", "O", "P", "Q86", "Q87_88", "R90-92", "R93", "S94", "S95", "S96")
# the WIOD aggregation of NACE sectors
sectors_old <- c("A01", "A02", "A03", "B", "C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30", "C31_C32", "C33", "D35", "E36", "E37-E39", "F", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I", "J58", "J59_J60", "J61", "J62_J63", "K64", "K65", "K66", "L68", "M69_M70", "M71", "M72", "M73", "M74_M75", "N", "O84", "P85", "Q", "R_S", "T", "U")
# naming of WIOD sectors: Base version 
sectors_base <- c("AGRagr", "AGRfor", "AGRfis", "MIN+", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+")
# naming of WIOD sectors: final version, where the MIN sector is disaggregated
sectors <- c("AGRagr", "AGRfor", "AGRfis", "MINfos", "MINoth", "MINsup", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+")

## define countries and country-sector vectors
countries <- c("AUS", "AUT", "BEL", "BGR", "BRA", "CAN", "CHE", "CHN", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV", "HUN", "IDN", "IND", "IRL", "ITA", "JPN", "KOR", "LTU", "LUX", "LVA", "MEX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SVK", "SVN", "SWE","TUR", "TWN", "USA", "ROW")
country_sec_base <- paste0(rep(countries, each=length(sectors_base)),"_",rep(sectors_base, length(countries)))
country_sec <- paste0(rep(countries, each=length(sectors)),"_",rep(sectors, length(countries)))

# define indexes for extraction of individual components of the WIOT (last row/column of the transaction matrix)
nrow_ind <- which(wiot[,1]=="II_fob")-1
ncol_ind <- which(colnames(wiot)=="AUS57")-1


# 1. Disaggregation ---------------------------------------------------------------

## Import & prepare data

# extract the transaction matrix Z and total output from the IOT, transform them to numeric values and set names
Z_base <- as.matrix(wiot[1:nrow_ind,6:ncol_ind])
dimnames(Z_base) <- list(country_sec_base, country_sec_base)
output_base <- as.numeric(wiot[nrow(wiot),6:ncol_ind])
names(output_base) <- country_sec_base

# compute final demand and value added vectors as residuals to total output
FD_base <- output_base - rowSums(Z_base)
VA_base <- output_base - colSums(Z_base)

# import capital stock data and transform it to US$
k_raw <- read.csv("WIOD_disaggregation/Data/capital_stocks_wiod.csv",  dec=",", sep=";")
k_loc <- as.numeric(gsub(" ","",k_raw[,5]))
names(k_loc) <- country_sec_base[1:2408]
ex_rate <- read.csv("WIOD_disaggregation/Data/exchange_rate_wiod.csv",  dec=",", sep=";")
ex_rate <- as.numeric(ex_rate[,2])
names(ex_rate) <- countries[-length(countries)]
k_base <- k_loc*rep(ex_rate, each=56)
# add zeros for RoW
k_base <- c(k_base, rep(0,length(sectors_base))); names(k_base) <- country_sec_base 
# transform negative PRT_MANrep capital stock to positive value
k_base["PRT_MANrep"] <- -k_base["PRT_MANrep"]


## disaggregate all parts of the IOT, specifying the correct ratio matrices from ICIO used for disaggregation

# Z
Z <- fossil_dissaggregate_ICIO(Z_mat = Z_base, RatiosRows = FossilRatiosRows_ICIO, RatiosCols = FossilRatiosCols_ICIO) 
# output
output <- fossil_dissaggregate_vect_ICIO(input_vector = output_base, ratio_matrix = FossilRatiosOutput_ICIO) 
# final demand
FD <- fossil_dissaggregate_vect_ICIO(input_vector = FD_base, ratio_matrix = FossilRatiosFD_ICIO) 
# value added
VA <- fossil_dissaggregate_vect_ICIO(input_vector = VA_base, ratio_matrix = FossilRatiosVA_ICIO) 
# capital stock USING OUTPUT RATIOS!
k <- fossil_dissaggregate_vect_ICIO(input_vector = k_base, ratio_matrix = FossilRatiosOutput_ICIO) 


# 2. Balancing ---------------------------------------------------------------

# the IOT is balanced as a whole (including VA and FD) - therefore "glue"  Z, FD and VA back together 
IOT <- cbind(Z, FD)
IOT <- rbind(IOT, "VA" = c(VA,0))

# same for the aggregated base table
IOT_base <- cbind(Z_base, FD_base)
IOT_base <- rbind(IOT_base, "VA" = c(VA_base,0))

# row/column targets for balancing are given by the disaggregated total output vector + an additional element for the sum of VA row / FD column
IOT_rowgoal <- c(output, sum(VA))
IOT_colgoal <- c(output, sum(FD))

# the TRAS balancing algorithm needs aggregation matrices/rules that aggregate the 3 disaggregated mining sub-sectors in the IOT to the original mining sector: matrices P and Q (= t(P)) 
P <- diag(ncol(Z)+1)
sectors_agg <- c("AGRagr", "AGRfor", "AGRfis", "MIN+",  "MIN+",  "MIN+", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+") #sectors_agg <- c("AGRagr", "AGRfor", "AGRfis", "MIN+",  "MIN+", "MANfoo", "MANtex", "MANwoo", "MANpap", "MANpri", "MANref", "MANche", "MANpha", "MANpla", "MANmin", "MANmet", "MANfmp", "MANcom", "MANele", "MANmac", "MANmot", "MANtra", "MANfur", "MANrep", "PWR+", "WATwat", "WATwst", "CNS+", "TRDmot", "TRDwho", "TRDret", "TRAinl", "TRAwat", "TRAair", "TRAwar", "TRApos", "FD+", "COMpub", "COMvid", "COMtel", "COMcom", "FINser", "FINins", "FINaux", "RES+", "PROleg", "PROeng", "PROsci", "PROadv", "PROoth", "ADM+", "PUB+", "EDU+", "HEA+", "ART+", "HOU+", "EXT+")
country_sec_agg <- paste0(rep(countries, each=length(sectors)),"_",rep(sectors_agg, length(countries)))
country_sec_agg <- c(country_sec_agg, "FD/VA") # add an element for the FD
P <- rowsum(P, group = c(country_sec_agg), reorder = F); colnames(P) <- c(country_sec, "FD/VA")
Q <- t(P)

# finally, balance the IOT with the extended TRAS function (which takes out negative values of the matrix and adds them back in the end)
# parameters: "tol" defines the tolerance for the algorithm convergence, "maxiter" the maximum number of iterations
# NOTE: the algorithm should converge after about 380 iterations
IOT_TRAS <- TRAS_extended(IOT=IOT, rowgoal=IOT_rowgoal, colgoal=IOT_colgoal, blockgoal=IOT_base, P= P, Q = Q, tol = 1e-3, maxiter = 1000, verbose = T)
Z <- IOT_TRAS$Z
FD <- IOT_TRAS$FD
VA <- IOT_TRAS$VA

# save results to am R object
# save(Z, FD, VA, output, k, file = "WIOT2014_disaggregated.Rdata")
