% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_maei_ind_rand.R
\name{get_maei_ind_rand}
\alias{get_maei_ind_rand}
\title{MAEI Calculation for Individually-Randomized Experiments}
\usage{
get_maei_ind_rand(
  vr,
  dist,
  nvoters,
  s10,
  s01 = NULL,
  exp_ac0 = 1,
  psi = NULL,
  ...
)
}
\arguments{
\item{vr}{a data frame contains variables: district and the corresponding number of registered voters in that district.}

\item{dist}{a character to specify the column name of the district variable in the data frame vr.}

\item{nvoters}{a character to specify the column number of the number of voters in the data frame vr.}

\item{s10}{a number or vector to denote the individuals exposed to the treatment because it is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1).}

\item{s01}{a number or vector to denote the individuals not exposed to the treatment because is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1). The default value is NULL which corresponds to the case in which a researcher designs and implements an intervention that would otherwise not have occurred. If it is not NULL, it corresponds to the case in which some intervention by a third party is modified to include an experimental component.}

\item{exp_ac0}{a number or vector to denote the expectation of untreated potential outcomes. This parameter is bounded between 0 and 1. The default value is one which will return the most conservative bound.}

\item{psi}{a number or vector specifying “margin to pivotality”, as minimum change in vote share, as a proportion of registered voters, at which a different officeholder would be elected in district. If psi > 2MAEI (Maximal Aggregate Electoral Impact), an experiment could not change the ultimate electoral outcome (the output result will show "PASS" the decision rule); in contrast, if psi < 2MAEI, the experiment could affect the ultimate electoral outcome (the output result will show "FAIL"). The decision rule in Slough (2023) advocates use of the fith percentile of the predictive distribution as the predicted value of psi.}
}
\description{
This function calculates the maximum aggregate electoral impact (MAEI) for individually-randomized experiments following Slough (2023). This function returns the MAEIs under the assumption of no interference between voters (SUTVA). The optional argument psi uses the calculation of MAEI_d to implements the decision rule proposed in the paper.
}
\examples{
### input data
data(rv1)

### specify s10 as a constant number across districts
get_maei_ind_rand(vr = rv1, dist = "d",
nvoters = "n_voters", s10 = 200)

### let s10 be different proportion and specify s01 = 10 (thus case 2)
set.seed(10)
get_maei_ind_rand(vr = rv1, dist = "d",
nvoters = "n_voters", s10 = runif(10), s01 = 10)

### add margin to pivotality say psi = 0.3
get_maei_ind_rand(vr = rv1, dist = "d",
nvoters = "n_voters", s10 = 0.13, s01 = 10, psi = 0.3)

}
\references{
Slough, Tara. 2023. "The Ethics of Electoral Experimentation: Design-Based Recommendations." Working paper. Available at www.taraslough.github.io/assets/pdf/eee.pdf.
}
