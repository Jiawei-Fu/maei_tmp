#' @title MAEI Calculation for Individually-Randomized Experiments
#'
#' @description This function calculates the maximum aggregate electoral impact (MAEI) for individually-randomized experiments following Slough (2023). This function returns the MAEIs under the assumption of no interference between voters (SUTVA). The optional argument psi uses the calculation of MAEI_d to implements the decision rule proposed in the paper.
#'
#' @param vr a data frame contains variables: district and the corresponding number of registered voters in that district.
#' @param dist a character to specify the column name of the district variable in the data frame vr.
#' @param nvoters a character to specify the column number of the number of voters in the data frame vr.
#' @param s10 a number or vector to denote the individuals exposed to the treatment because it is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1).
#' @param s01 a number or vector to denote the individuals not exposed to the treatment because is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1). The default value is NULL which corresponds to the case in which a researcher designs and implements an intervention that would otherwise not have occurred. If it is not NULL, it corresponds to the case in which some intervention by a third party is modified to include an experimental component.
#' @param exp_ac0  a number or vector to denote the expectation of untreated potential outcomes. This parameter is bounded between 0 and 1. The default value is one which will return the most conservative bound.
#'
#' @param psi a number or vector specifying “margin to pivotality”, as minimum change in vote share, as a proportion of registered voters, at which a different officeholder would be elected in district. If psi > 2MAEI (Maximal Aggregate Electoral Impact), an experiment could not change the ultimate electoral outcome (the output result will show "PASS" the decision rule); in contrast, if psi < 2MAEI, the experiment could affect the ultimate electoral outcome (the output result will show "FAIL"). The decision rule in Slough (2023) advocates use of the fith percentile of the predictive distribution as the predicted value of psi.
#'
#' @examples
#' ### input data
#' data(rv1)
#'
#' ### specify s10 as a constant number across districts
#' get_maei_ind_rand(vr = rv1, dist = "d",
#' nvoters = "n_voters", s10 = 200)
#'
#' ### let s10 be different proportion and specify s01 = 10 (thus case 2)
#' set.seed(10)
#' get_maei_ind_rand(vr = rv1, dist = "d",
#' nvoters = "n_voters", s10 = runif(10), s01 = 10)
#'
#' ### add margin to pivotality say psi = 0.3
#' get_maei_ind_rand(vr = rv1, dist = "d",
#' nvoters = "n_voters", s10 = 0.13, s01 = 10, psi = 0.3)
#'
#' @references Slough, Tara. 2023. "The Ethics of Electoral Experimentation: Design-Based Recommendations." Working paper. Available at www.taraslough.github.io/assets/pdf/eee.pdf.
#'
#'
#' @import dplyr
#' @import magrittr
#' @import randomizr
#'
#' @export

get_maei_ind_rand <- function(vr, ## voter_rolls
                              dist, ## specify variable name in the vr
                              nvoters, ## the same
                              s10,  ## number or proportion
                              s01 = NULL,
                              exp_ac0 = 1, # most conservative
                              psi = NULL,
                              ...
){

  ### check the inputs

  if ( !("data.frame" %in% class(vr) ) )
    stop("vr should be the data.frame")

  if (!(length(s10)==1 | length(s10)== nrow(vr)))
    stop("the length of s10 is not consistent with the vr")

  if (!is.null(s01)){
    if(!(length(s01)==1 | length(s01)== nrow(vr)))
      stop("the length of s01 is not consistent with the vr")
  }

  if (!(length(exp_ac0)==1 | length(exp_ac0)== nrow(vr)))
    stop("the length of exp_ac0 is not consistent with the vr")

  if (sum(exp_ac0>1)>0|sum(exp_ac0<0)>0)
    stop("exp_ac0 should be between 0 and 1")

  if (!("character" %in% class(dist)))
    stop("dist should be a character variable that is the same as the column name of the district variable in the vr")

  if (!(class(nvoters) %in% "character"))
    stop("nvoters should be a character variable that is the same as the column name of the number of the voters in the vr")

  if(!is.null(psi)){
    if(sum(psi>1 | psi<0)>=1)
      stop("psi denotes the proportion of registered voters; it should be between 0 and 1")
  }

  ### generate complete data.frame

  if (is.null(s01)) {
    s01 <- 0
    case <- "Case 1"
  }else{
    case <- "Case 2"
  }

  ## check s10,s01 is number or proportion
  if(sum(s10<1 & s10>0)>0){   ### perhaps proportion
    if(sum(s10>1)>0){stop("s10 is mixed with proportion and the number")}
    s10 <- s10 * vr[,nvoters]  ## calculate numbers
  }

  ## check s10,s01 is number or proportion
  if(sum(s01<1 & s01>0)>0){   ### perhaps proportion
    if(sum(s01>1)>0){stop("s01 is mixed with proportion and the number")}
    s01 <- s01 * vr[,nvoters]  ## calculate numbers
  }

  maei = data.frame(vr[,dist],vr[,nvoters],s10, s01, exp_ac0)
  colnames(maei) <- c("district","nvoters","s10","s01","exp_ac0")

  maei1 <- maei %>%
    group_by(district) %>%
    summarise(MAEI_d = max( exp_ac0*(s10+s01)/nvoters,
                            (1-exp_ac0)*(s10+s01)/nvoters )
    )



  ### output

  output <- tibble(Districts = vr[,dist],
                   MAEI_d = maei1$MAEI_d)


  ### psi

  if (!is.null(psi)){

    output <- output %>%
      mutate(psi=psi) %>%
      mutate(result = NA)
    output$result <- ifelse(output$psi>2*output$MAEI_d,"PASS","FAIL")

    output <- output[,-3]

  }


  cat("\n")
  cat("MAEI_ind_randomized: exp_ac0 = ", exp_ac0,"\n")
  cat("Individually randomized: ", case,"\n")

  if(!is.null(psi)){cat("Margin to pivotality： Psi = ", psi,"\n")}
  print(output)

  # for extraction

  output2 <- list()
  output2$MAEI_d <- output[, 2]
  output2$district <- output[, 1]

  if(!is.null(psi)){
    output2$psi <- psi
    output2$result <- output[,3]
  }


  invisible(output2)
}



