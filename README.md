---
# Capital Stranding Cascades
---

This repository contains the code and material for: Cahen-Fourot, L., Campiglio, E., Godin, A., Kemp-Benedict, E. and Trsek, S. (2021) "Capital stranding cascades: The impact of decarbonisation on productive asset utilisation"

The main code file is "Cascades_script.R". It requires the installation of [R](https://cran.r-project.org/).

To run the main code, two additional files are required:
* A library of functions ("Cascades_function_library.R")
* A version of the World Input-Output Database (WIOD) for the year 2014 (including capital stocks) in which the mining sector has been disaggregated into three separate sectors ("WIOT2014_disaggregated.RData", see [Data](/Data) folder)


The disaggregatation of the WIOD mining sector is conducted in separate preparatory code files (see [R/Data_preparation](/R/Data_preparation))
* The script "ICIO_FossilRatios.R" computes ratios for the splitting of the WIOD mining sector from the OECD Inter-Country Input-Output (ICIO) table. This script requires the sector correspondence sheet "ICIO_to_WIOD.xlsx" as well as the OECD ICIO table for the year 2014. To run the code, please download the ICIO table in csv format from the [OECD website](https://www.oecd.org/sti/ind/inter-country-input-output-tables.htm).
* The script "WIOD_MiningDisaggregation" disaggregates the WIOD mining sector using the ratios from ICIO and balances the disaggregated table by means of a two-step RAS algorithm. It requires the raw WIOD table for the year 2014 ("WIOT2014_October16_ROW.RData"), the WIOD capital stock data for the same year ("capital_stocks_wiod.xlsx") as well as splitting ratios from ICIO ("FossilRatios2014_ICIO.RData").
To run the main code, you do not necessarily need to run the preparotry codes. 


## Results
The [Results](/Results) folder contains several tables and figures contained in the paper. 
![](/Results/figures/html/Cascades_global_sectors_HTML.html)

## Contact
For any questions or comments, please write to: emanuele.campiglio@unibo.it
