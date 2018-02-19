# Data and code for Casas-Cardona et al. 2018. Ethology (In press), doi:10.1111/eth.12729


This repository includes four files files: Two tables in .csv format with the raw data for attacks on the retreived clay models (Attacks.csv) and the RGB scores obtained from photographs of frogs and models (RGBScores.csv), and an R script to replicate the analyses performed with each dataset in the paper (AttackAnalyses.R and ColorAnalyses.R).

The AttackAnalyses script requires the package <i>multcomp</i>, (Hothorn et al. 2008, <i>Biometrical Journal</i>, 50, 346-363), available from CRAN.

To run the scripts simply move the script and data file to the same directory and type:

``` Rscript AttackAnalyses.R``` or 
``` Rscript ColorAnalyses.R```

The first command should output results of statistical analyses to stdout, while the second one will produce a table with RGB-derived PC scores and LM-B scores (ColorationOutputs.csv). 
