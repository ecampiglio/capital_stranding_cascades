# The code below replicates the results presented in: 
# Cahen-Fourot, L., Campiglio, E., Dawkins, E., Godin, A., and Kemp-Benedict, E. (2019) 
# "Capital stranding cascades: The impact of decarbonisation on productive asset utilisation"
# For correspondence, please write to: Emanuele.campiglio@wu.ac.at

# The code is structured in five sections: 
# 1. Introductory statements
# 2. Derivation of the matrix of asset stranding multipliers (S)
# 3. Calculation of the values of capital stocks at risk of stranding
# 4. Definition and plotting of stranding cascade networks
# 5. Write-up of the results

######################################################################################
# 1. Introductory statements
######################################################################################

# Load required libraries
library(openxlsx)
library(igraph)
library(plotrix)
library(RColorBrewer)
library(circlize)
# Load the functions written in the library file
source('files_to_load/capital_stranding_cascades_function_library.R')

# Countries to be analysed
countries<- c("AT", "BE", "CZ", "DE", "EL", "FR", "IT", "SE", "SK", "UK")
# For reference: entire set of countries: countries<- c("AT", "BE", "CZ", "DE", "EL", "FR", "IT", "SE", "SK", "UK")

# List of sectors (it contains all combinations appearing in the ten countries being examined)
sectors<-c(  "A", "A01",    "A02",    "A03",    "B",     "C", "C10-12", "C13-15", "C16-18", "C16",    "C17",    "C18",    "C19",    "C20-21", "C20",    "C21", "C22-23",   "C22",    "C23",  "C24-25",  "C24",    "C25",    "C26",    "C27",    "C28",  "C29-30",  "C29",    "C30",    "C31_32", "C31-33", "C33" ,   "D",   "E",   "E36",    "E37-39", "F",   "G",   "G45",    "G46",    "G47",  "H",  "H49",    "H50",    "H51",    "H52-53", "H52",    "H53",    "I",   "J",  "J58-60", "J58",    "J59_60", "J61",    "J62_63", "K", "K64",    "K65",    "K66",   "L", "M",   "M69_70", "M69-71",  "M71-72", "M71",    "M72",   "M73",  "M73-75",  "M74_75", "N", "N77",    "N78",    "N79",    "N80-82", "O",      "P",   "Q",   "Q86",    "Q87_88", "R", "R90-92", "R93",  "S",  "S94",    "S95",    "S96")
# FOR REF: NACE default sectors: sectors<-c(  "A01",    "A02",    "A03",    "B",      "C10-12", "C13-15", "C16",    "C17",    "C18",    "C19",    "C20",    "C21",    "C22",    "C23",    "C24",    "C25",    "C26",    "C27",    "C28",    "C29",    "C30",    "C31_32", "C33" ,   "D",      "E36",    "E37-39", "F",      "G45",    "G46",    "G47",    "H49",    "H50",    "H51",    "H52",    "H53",    "I",      "J58",    "J59_60", "J61",    "J62_63", "K64",    "K65",    "K66",    "L",   "M69_70", "M71",    "M72",   "M73",    "M74_75", "N77",    "N78",    "N79",    "N80-82", "O",      "P",      "Q86",    "Q87_88", "R90-92", "R93",    "S94",    "S95",    "S96")

# We define the dataframes where to store international results
# Capital intensity (kappa)
int_kappa<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_kappa_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
# Assets stranding mulipliers (total, internal, net, exposure)
int_strand_mult<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_internal_strand<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_net_mult<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_strand_impact<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
# Assets stranding mulipliers rankings (total, net, exposure)
int_strand_mult_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
int_net_mult_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
int_strand_impact_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
# Results for sector B and other sectors if needed (focus_sect to be chosen below)
int_B_strand_mult<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_B_strand_mult_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
int_sect_strand_mult<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors,countries)))
int_sect_strand_mult_rank<-data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(NULL, countries)))
# Capital stocks at risk of stranding due to decarbonisation
int_B_sect_stranding<- data.frame(matrix(0, nrow=length(sectors)+3, ncol=length(countries), dimnames=list(c(sectors,"Tot stranding","Stranding over GDP","Stranding over K"), countries)))
int_B_sect_strand_share<- data.frame(matrix(0, nrow=length(sectors), ncol=length(countries), dimnames=list(sectors, countries)))



# This is the start of the country for loops. It runs the same code for all the countries chosen above. 
count<-1 
for (geo in countries) {

  
######################################################################################
# 2. Matrix of asset stranding multipliers
######################################################################################
  
  # The code below defines the csv files to upload
  k_data_folder<- paste0("country_data/country_",geo,"/",geo,"_cleaned_fixed_assets_2010.csv")
  io_data_folder<-  paste0("country_data/country_",geo,"/",geo,"_cleaned_io_2010.csv")
  
  # Read the csv files for IO and capital data
  k_raw<-read.csv(k_data_folder, row.names = 1, dec=",", sep=",")
  io_raw<-read.csv(io_data_folder, row.names = 1, check.names=FALSE, stringsAsFactors=FALSE, sep=",")
  
  #Defines the inter-industry matrix Z
  nrow_ind<-which(rownames(io_raw)=="T")-1
  ncol_ind<-which(colnames(io_raw)=="T")-1
  Z_dat<-io_raw[1:nrow_ind,1:ncol_ind]
  
  # Extract values for domestic output and total supply
  output_dom_dat<-io_raw["P1",1:ncol_ind]
  supply_tot_dat<-io_raw["TS_BP",1:ncol_ind]
  
  #Calculate GDP as Total Value Added at Basic Prices (B1G, TOTAL) plus Net Taxes on Products (D21X31, TU) (see Eurostat Manual, p.305)
  GDP<-io_raw["B1G","TOTAL"]+io_raw["D21X31","TU"]
  
  # Transform variables in matrices/numeric vectors (otherwise no matrix multiplications possible for Ghosh)
  k<-as.numeric(unlist(k_raw))
  names(k)<-rownames(k_raw)
  k_tot<-sum(k)
  Z<-data.matrix(Z_dat)
  supply_tot<-as.numeric(supply_tot_dat)
  names(supply_tot)<-names(supply_tot_dat)
  output_dom<-as.numeric(output_dom_dat)
  names(output_dom)<-names(output_dom_dat)
  
  # Calculate total uses of B products
  B_tot_uses<-rowSums(Z)["B"]
  
  #Find Ghosh matrix G (and its transpose Gt)
  B<-solve(diag(supply_tot))%*%Z
  G<-solve(diag(nrow(B))-B)
  colnames(G)<-rownames(G)
  Gt<-t(G)
  
  # Calculate sectoral capital intensity
  k_int<-k/output_dom
  
  # Calculate the stranded asset matrix by multiplying the diagonalized k_int vector and the Ghosh transpose
  # Each element in strand_matrix tells the amount of physical assets in sector i due to a unit change in inputs for sector j
  S<-diag(k_int)%*%Gt
  rownames(S)<-rownames(Z)
  
  # Calculate the total stranded assets multiplier (SAM)
  strand_mult<-colSums(S)
  
  # Calculate the extent to which sectors are affected by external impacts
  S_diagoff<-S
  diag(S_diagoff)<-0
  strand_impact<-rowSums(S_diagoff)
  
  # Calculate the external multiplier by subtracting the sectoral internal stranding effect
  diagG<-diag(Gt)
  internal_strand<-diagG*k_int
  net_mult<-strand_mult-internal_strand
  
  # Create matrix with all stranding results
  strand_res<-cbind(strand_mult,internal_strand,net_mult)
  strand_res<-strand_res[rowSums(is.na(strand_res)) != ncol(strand_res), ]
  
  # Extract from S the stranding effects of B on all other sectors
  B_strand_mult<-S[,"B"]
  names(B_strand_mult)<-rownames(S)
  
  # Extract from S the stranding effects of a sector of choice ("focus_sect") on all other sectors
  focus_sect<-if ("C19" %in% rownames(k_raw)){"C19"} else {"C"}
  sect_strand_mult<-S[,focus_sect]
  names(sect_strand_mult)<-rownames(S)
  
  # Rank sectoral multipliers
  k_int_rank<-k_int[order(k_int, decreasing = T)]
  strand_mult_rank<-strand_mult[order(strand_mult, decreasing = T)]
  net_mult_rank<-net_mult[order(net_mult, decreasing=T)]
  B_strand_mult_rank<-B_strand_mult[order(B_strand_mult, decreasing=T)]
  sect_strand_mult_rank<-sect_strand_mult[order(sect_strand_mult, decreasing=T)]
  strand_impact_rank<- strand_impact[order(strand_impact, decreasing=T)]
  
  
######################################################################################
# 3. Capital stocks at risk of stranding
######################################################################################

  # Load files with results from Exiobase database (see "Fossil ratios from Exiobase.R")
  load("files_to_load/fossil.use.ratio.Rda")
  load("files_to_load/fossil.prod.ratio.Rda")
  names(fossil.prod.ratio.int)<-countries
  B_dom_prod<-read.csv("files_to_load/B_domestic_production.csv", row.names = 1)
  
  # Compute domestic production and capital stock of sector B
  fossil_dom_prod<-fossil.prod.ratio.int*B_dom_prod
  fossil_capital_B<-fossil_dom_prod*k_int["B"]
  
  #Extracting specific data for sector B
  B_uses<-Z["B",]
  B_diagGt<-diag(Gt)["B"]
  B_S<-B_strand_mult[!is.na(B_strand_mult)]
  
  # The code below calculates the sectoral capital stocks at risk of stranding due to use of fossil products, and their share over total sectoral capital
  # To each sector is applied the fossil.ratio relative to its NACE level 1 category (no further NACE disaggregation possible in Exiobase)
  # The stranding from fossil use is calculated as the total fossil use in terms of primary inputs times the sectoral stranding multiplier on the B column of matrix S
  B_sect_stranding<-array(0, dim=length(B_uses),dimnames=list(names(B_uses)))
  B_sect_strand_share<-array(0, dim=length(B_uses),dimnames=list(names(B_uses)))
  for (sect in names(B_uses)){
    NACElev1<-substr(sect,1,1)
    f.ratio<-fossil.use.ratio.int.by.sector[NACElev1,geo]
    B_stranding<-(B_tot_uses*f.ratio)/B_diagGt*S[sect,"B"]
    B_sect_stranding[sect]<-B_stranding
    B_sect_strand_share[sect]<-B_stranding/k[sect]
  }
  
  # We correct the stranding of sector B in light of it being the origin of the disruption
  # The entire amount of capital in the B sector used to produce fossil fuels is considered at risk of stranding (fossil_capital_B)
  # The stranding in the remaining capital stock in B (used to extract metals and mineral) is calculated considering a hypothetical internal use of fossil products (Gt["B","B"]-1) 
  B_sect_stranding["B"]<-(1-fossil.prod.ratio.int[geo])*(B_tot_uses*fossil.use.ratio.int.by.sector["B",geo])/B_diagGt*(Gt["B","B"]-1)*k_int["B"]+fossil_capital_B[geo,]
  B_sect_strand_share["B"]<-B_sect_stranding["B"]/k["B"]
  
  # Calculate summary results
  B_tot_stranding<-sum(B_sect_stranding)
  B_stranding_over_GDP<-B_tot_stranding/GDP
  B_stranding_over_totK<-B_tot_stranding/k_tot
  B_sect_stranding<-c(B_sect_stranding,B_tot_stranding,B_stranding_over_GDP,B_stranding_over_totK)
  
  
  
######################################################################################
# 4. Cascade networks
######################################################################################
  
  # The code below draws the circular plot of the S_mfc matrix (diagonal excluded) using the chordDiagram function. col_vector defines a color palette.
  # The S_mfc creates a minimal fully-connected network (see Campiglio et al. 2017, AFD Working Paper)
  # The code below is commented because the drawing of the chord diagram slows down the process. Uncomment if needed.
  # S_mfc<-mfc(S)
  # S_mfc_diagoff<-S_mfc
  # diag(S_mfc_diagoff)<-0
  # qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  # col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  # chordDiagram(t(S_mfc_diagoff), grid.col=col_vector[1:dim(S_mfc)[1]], transparency=0.5)
  
  # Choose the sector of focus in the cascade analysis
  sector<-"B"
  
  # Create a graph from the S matrix
  S.graph <- simplify(graph_from_adjacency_matrix(t(S), mode = "directed", weighted = TRUE))

  # Define the cn (cascade network) graph. 'q' defines the percentile of values to look at. 'depth' defines the desired number of layers.
  cn <- cascade_network(S.graph, sector, q = 0.05 , depth=NA)
  
  
  # The lines below define the graphical properties of the cascade network. Comment/uncomment as needed
  ###########################################
  maxwidth <- 4
  weights <- E(cn)$weight/max(E(cn)$weight)
  E(cn)$width <- 1.5+maxwidth * weights
  
 
  
  #Vertex attributes
  V(cn)$label.color <- "black"
  V(cn)$size<-15
  V(cn)$label.font <- 2
  palette<-colorRampPalette(c("darkgray", "white"))
  pal<-palette(length(unique(V(cn)$layer)))
  V(cn)$color <- pal[V(cn)$layer]
  
  #Edge attributes
  E(cn)$arrow.size <- 0.2
  E(cn)$color <- "gray53"
  E(cn)$label.color <- "black"
  E(cn)$label.cex <- 1
  
  # Add edge labels for the top 5 edges
  for (w in 1:length(E(cn))) {
    E(cn)$label[w]<-if (E(cn)$weight[w] %in% tail(sort(E(cn)$weight),10)) {round(E(cn)$weight[w], 3)} else {NA}
  }

  # Plot the cascade network
  plot.igraph(cn, layout=layout_as_tree(cn,circular=F))
  title(main = geo)
  
######################################################################################
# 5. Write-up of results
######################################################################################  
  
  # Code below to create a ranking table with both sector code and SAM values in the same cell
  strand_mult_rank_codes<-names(strand_mult_rank)
  strand_mult_rank_tot<-vector()
  for (i in 1:length(strand_mult_rank)){
    strand_mult_rank_tot[i]<-paste(strand_mult_rank_codes[i]," (", round(strand_mult_rank[i], digits = 2),")", sep = "")
  }
  net_mult_rank_codes<-names(net_mult_rank)
  net_mult_rank_tot<-vector()
  for (i in 1:length(net_mult_rank)){
    net_mult_rank_tot[i]<-paste(net_mult_rank_codes[i]," (", round(net_mult_rank[i], digits = 2),")", sep = "")
  }
  B_strand_mult_rank_codes<-names(B_strand_mult_rank)
  B_strand_mult_rank_tot<-vector()
  for (i in 1:length(B_strand_mult_rank)){
    B_strand_mult_rank_tot[i]<-paste(B_strand_mult_rank_codes[i]," (", round(B_strand_mult_rank[i], digits = 2),")", sep = "")
  } 
  sect_strand_mult_rank_codes<-names(sect_strand_mult_rank)
  sect_strand_mult_rank_tot<-vector()
  for (i in 1:length(sect_strand_mult_rank)){
    sect_strand_mult_rank_tot[i]<-paste(sect_strand_mult_rank_codes[i]," (", round(sect_strand_mult_rank[i], digits = 2),")", sep = "")
  }
  k_int_rank_codes<-names(k_int_rank)
  k_int_rank_tot<-vector()
  for (i in 1:length(k_int_rank)){
    k_int_rank_tot[i]<-paste(k_int_rank_codes[i]," (", round(k_int_rank[i], digits = 2),")", sep = "")
  }
  strand_impact_rank_codes<-names(strand_impact_rank)
  strand_impact_rank_tot<-vector()
  for (i in 1:length(strand_impact_rank)){
    strand_impact_rank_tot[i]<-paste(strand_impact_rank_codes[i]," (", round(strand_impact_rank[i], digits = 2),")", sep = "")
  }
  
  # Preparatory code to deal with the fact that countries may differ in their last sector (this would interfere with the loop in the next block of code)
  s_fix<-c("S94"=NA,"S95"=NA,"S96"=NA)
  if (names(strand_mult[length(strand_mult)])=="S") {
    strand_mult<-c(strand_mult, s_fix)
    internal_strand<-c(internal_strand, s_fix)
    net_mult<-c(net_mult, s_fix)
    B_strand_mult<-c(B_strand_mult, s_fix)
    B_strand_mult_rank<-c(B_strand_mult_rank, s_fix)
    sect_strand_mult<-c(sect_strand_mult, s_fix)
    sect_strand_mult_rank<-c(sect_strand_mult_rank, s_fix)
    k_int<-c(k_int,s_fix)
    k_int_rank<-c(k_int_rank,s_fix)
    strand_impact<-c(strand_impact,s_fix)
    strand_impact_rank<-c(strand_impact_rank,s_fix)
    B_sect_strand_share<-c(B_sect_strand_share,s_fix)
  }
  
  #The lines below deal with the fact that certain countries do not report certain sectors. NA is inserted in their place.
  for (i in 1:length(sectors)){
    if (sectors[i]!=names(strand_mult)[i]){
      strand_mult<-c(strand_mult[1:i-1],NA,strand_mult[i:length(strand_mult)])
      names(strand_mult)[i]<-sectors[i]
    }
    if (sectors[i]!=names(internal_strand)[i]){
      internal_strand<-c(internal_strand[1:i-1],NA,internal_strand[i:length(internal_strand)])
      names(internal_strand)[i]<-sectors[i]
    }
    if (sectors[i]!=names(net_mult)[i]){
      net_mult<-c(net_mult[1:i-1],NA,net_mult[i:length(net_mult)])
      names(net_mult)[i]<-sectors[i]
    }
    if (sectors[i]!=names(B_strand_mult)[i]){
      B_strand_mult<-c(B_strand_mult[1:i-1],NA,B_strand_mult[i:length(B_strand_mult)])
      names(B_strand_mult)[i]<-sectors[i]
    }
    if (sectors[i]!=names(sect_strand_mult)[i]){
      sect_strand_mult<-c(sect_strand_mult[1:i-1],NA,sect_strand_mult[i:length(sect_strand_mult)])
      names(sect_strand_mult)[i]<-sectors[i]
    }
    if (sectors[i]!=names(k_int)[i]){
      k_int<-c(k_int[1:i-1],NA,k_int[i:length(k_int)])
      names(k_int)[i]<-sectors[i]
    }
    if (sectors[i]!=names(strand_impact)[i]){
      strand_impact<-c(strand_impact[1:i-1],NA,strand_impact[i:length(strand_impact)])
      names(strand_impact)[i]<-sectors[i]
    }
    if (sectors[i]!=names(B_sect_stranding)[i]){
      B_sect_stranding<-c(B_sect_stranding[1:i-1],NA,B_sect_stranding[i:length(B_sect_stranding)])
      names(B_sect_stranding)[i]<-sectors[i]
    }
    if (sectors[i]!=names(B_sect_strand_share)[i]){
      B_sect_strand_share<-c(B_sect_strand_share[1:i-1],NA,B_sect_strand_share[i:length(B_sect_strand_share)])
      names(B_sect_strand_share)[i]<-sectors[i]
    }
  }
  
  # Add NA values to make the length of the ranking vectors equal to the length of the 'sectors' vector (otherwise error in filling result dataframes)
  strand_mult_rank<- c(strand_mult_rank,  rep_len(NA, length(strand_mult)-length(strand_mult_rank)))
  strand_mult_rank_tot<- c(strand_mult_rank_tot,  rep_len(NA, length(strand_mult)-length(strand_mult_rank_tot)))
  net_mult_rank<- c(net_mult_rank,  rep_len(NA, length(net_mult)-length(net_mult_rank)))
  net_mult_rank_tot<- c(net_mult_rank_tot,  rep_len(NA, length(net_mult)-length(net_mult_rank_tot)))
  B_strand_mult_rank_tot<- c(B_strand_mult_rank_tot,  rep_len(NA, length(B_strand_mult)-length(B_strand_mult_rank_tot)))
  sect_strand_mult_rank_tot<- c(sect_strand_mult_rank_tot,  rep_len(NA, length(sect_strand_mult)-length(sect_strand_mult_rank_tot)))
  k_int_rank_tot<- c(k_int_rank_tot,  rep_len(NA, length(k_int)-length(k_int_rank_tot)))
  strand_impact_rank<- c(strand_impact_rank,  rep_len(NA, length(strand_impact)-length(strand_impact_rank)))
  strand_impact_rank_tot<- c(strand_impact_rank_tot,  rep_len(NA, length(strand_impact)-length(strand_impact_rank_tot)))
  
  #assign results to the dataframes summarising international results
  int_strand_mult[count]<-strand_mult 
  int_internal_strand[count]<-internal_strand 
  int_net_mult[count]<-net_mult 
  int_strand_mult_rank[count]<-strand_mult_rank_tot
  int_net_mult_rank[count]<-net_mult_rank_tot
  int_B_strand_mult[count]<-B_strand_mult
  int_B_strand_mult_rank[count]<-B_strand_mult_rank_tot
  int_sect_strand_mult[count]<-sect_strand_mult
  int_sect_strand_mult_rank[count]<-sect_strand_mult_rank_tot
  int_kappa[count]<-k_int
  int_kappa_rank[count]<-k_int_rank_tot
  int_strand_impact[count]<-strand_impact 
  int_strand_impact_rank[count]<-strand_impact_rank_tot
  int_B_sect_stranding[count]<-B_sect_stranding
  int_B_sect_strand_share[count]<-B_sect_strand_share
  
  
  # Move one step further in the country loop
  count<-count+1
}
#This is the end of the country loop


# To export to Excel. Uncomment as needed
write.xlsx(int_strand_mult_rank,"Results/int_strand_mult_rank.xlsx")
write.xlsx(int_net_mult_rank,"Results/int_net_mult_rank.xlsx")
write.xlsx(int_strand_impact_rank,"Results/int_strand_impact_rank.xlsx")
write.xlsx(cbind(rownames(int_B_sect_stranding), int_B_sect_stranding),"Results/int_B_sect_stranding.xlsx")
write.xlsx(cbind(rownames(int_B_sect_strand_share), int_B_sect_strand_share),"Results/int_B_sect_strand_share.xlsx")




