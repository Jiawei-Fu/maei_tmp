# MAEI Calculation for Randomized Experiments in Elections

The package contains two functions to calculate maximum aggregate electoral impact for randomized experiments in elections. The maximum aggregate electoral impact (MAEI) is derived in the following article:

Slough, Tara. 2023. "The Ethics of Electoral Experiments: Design-Based Recommendations." Working paper, New York University. Available at https://taraslough.github.io/assets/pdf/eee.pdf.

**MAEI Calculation for Individually-Randomized Experiments**

This function calculates the maximum aggregate electoral impact (MAEI) for individually-randomized experiments following Slough (2023). This function returns the MAEIs under the assumption of no interference between voters (SUTVA). The optional argument psi uses the calculation of MAEI_d to implements the decision rule proposed in the paper.

**MAEI Calculation for Cluster-Randomized Experiments**

This function calculates the maximum aggregate electoral impact (MAEI) for cluster randomized experiments following Slough (2023). This function returns the MAEIs under each of three assumptions about interference between voters. MAEI_d assumes no within-cluster or between-cluster spillovers; MAEI_w assumes within-cluster but no between-cluster spillovers (SUTVA);  and MAEI_bw assumes within-cluster and bounded between-cluster spillovers. The argument psi uses the calculated MAEIs to implement the decision rule proposed in the paper.

To install and use the latest version of the package, use the following code:
```r
install.packages("devtools")
devtools::install_github("Jiawei-Fu/maei_tmp")
library(maei)
```
