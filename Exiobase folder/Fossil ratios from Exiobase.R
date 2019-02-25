# The code below complements the main code file for: 
# Cahen-Fourot, L., Campiglio, E., Dawkins, E., Godin, A., and Kemp-Benedict, E. (2019)  "Capital stranding cascades: The impact of decarbonisation on productive asset utilisation"
# It uses the Exiobase database to extract fossil production and sectoral use
# Running the code requires the download of EXIOBASE3, available at www.exiobase.eu (not provided on GitHub)

# Load required libraries
library(readxl)
library(openxlsx)

# Countries to be analysed
countries<- c("AT")
countries<- c("AT", "BE", "CZ", "DE", "GR", "FR", "IT", "SE", "SK", "GB")
# Some country codes are not consistent between Eurostat and Exiobase: EL<->GR; UK<->GB. Below Eurostat acronyms
countries_EU<- c("AT", "BE", "CZ", "DE", "EL", "FR", "IT", "SE", "SK", "UK")

# Load Exiobase files: 
# IO.codes reports the entire vector of category values (product.name, product.code, etc.) 
# Z is the inter-industry matrix for 2010 (very large)
load(paste0("pxp/IO.codes.RData"))
year <- 2010
load(paste0("pxp/",year,"_Z.RData"))

# Load spreadsheet where the 200 Exiobase products have been assigned to: 
  # Mining categories: 1. non-mining; 2. (mining) fossils; 3. (mining) non-fossils
  # NACE categories
IO_codes_mining <- read_excel("IO codes for fossil mining.xlsx")

# Add a mining and NACE columns to the IO.code file
IO.codes <- cbind(IO.codes,IO_codes_mining$Mining, IO_codes_mining$NACE)
colnames(IO.codes)[7] <- "mining"
colnames(IO.codes)[8] <- "NACE"

# Create arrays/dataframes to store results
fossil.use.ratio.int<-array(0, dim=length(countries), dimnames=list(countries))
fossil.stranding.int<-array(0, dim=length(countries), dimnames=list(countries))
fossil.use.ratio.int.by.sector<-data.frame(matrix(0, nrow=length(unique(IO_codes_mining$NACE)), ncol=length(countries), dimnames=list(unique(IO_codes_mining$NACE),countries_EU)))
fossil.prod.ratio.int<-array(0,dim=length(countries), dimnames=list(countries))

# This is the beginning of the country loop
count<-1 
for (geo in countries) {
  
# Specify which country we want to analyse and extract its data
# IO.nat.use looks at the country as user (i.e. extracts the country columns)
# IO.nat.prod. looks at the country as producer (i.e. extracts the country rows)
IO.nat.use <- Z[,IO.codes$Index[IO.codes$Country.Code == geo]]
IO.nat.prod <- Z[IO.codes$Index[IO.codes$Country.Code == geo],]

# Define the indexes of the rows we are interested in 
# The .nat version extract the rows over the 200 national products; the other does the same over the whole codes vector (200x49 countries = 9800)
rows.fossil<-IO.codes$Index[IO.codes$mining == "fossils"]
rows.nonfossil <- IO.codes$Index[IO.codes$mining == "non-fossils"]
rows.fossil.nat<-IO_codes_mining$Index[IO_codes_mining$Mining == "fossils"]
rows.nonfossil.nat<-IO_codes_mining$Index[IO_codes_mining$Mining == "non-fossils"]

# The code below calculates the ratio of fossil fuels over the entire B domestic production 
fossil.prod<-IO.nat.prod[rows.fossil.nat,]
nonfossil.prod<-IO.nat.prod[rows.nonfossil.nat,]
fossil.prod.ratio<-sum(fossil.prod)/(sum(fossil.prod)+sum(nonfossil.prod))

# Create a matrix with the products in rows and the intermediate consumption of fossils and non-fossils products in columns
intermediate <- data.frame("index" = 1:200,
                           "product names" = IO.codes$Product.Name[1:200],
                           "sector group" = IO.codes$Sector.Group[1:200],
                           "NACE" =IO.codes$NACE[1:200],
                           "fossil mining" = colSums(IO.nat.use[rows.fossil,]),
                           "non-fossil mining" = colSums(IO.nat.use[rows.nonfossil,]),
                           stringsAsFactors = FALSE)

# Calculate the ratio of fossil fuels over the total use of B products
tot.fossil.use<-sum(intermediate[which(colnames(intermediate)=="fossil.mining")])
tot.non.fossil.use<-sum(intermediate[which(colnames(intermediate)=="non.fossil.mining")])
fossil.use.ratio<-tot.fossil.use/(tot.fossil.use+tot.non.fossil.use)

# Calculate the fossil use ratio at the sectoral level
B.use.by.sector<-aggregate(intermediate[,5:6], by=list(Category=intermediate$NACE), FUN=sum)
fossil.use.ratio.by.sector<-cbind(B.use.by.sector["Category"], B.use.by.sector["fossil.mining"]/(B.use.by.sector["fossil.mining"]+B.use.by.sector["non.fossil.mining"]))

# Store the country results
fossil.use.ratio.int[count]<-fossil.use.ratio
fossil.use.ratio.int.by.sector[count]<-fossil.use.ratio.by.sector[,2]
fossil.prod.ratio.int[count]<-fossil.prod.ratio

#Update count
count<-count+1
}
#End of country loop

# Remove Z as it is very large and sometimes it slows down the process
rm("Z")

#Export data to be used in the main file
save(fossil.use.ratio.int.by.sector,file="fossil.use.ratio.Rda")
save(fossil.prod.ratio.int,file="fossil.prod.ratio.Rda")
