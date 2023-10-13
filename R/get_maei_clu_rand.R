#' @title MAEI Calculation for Cluster-Randomized Experiments
#'
#' @description This function calculates the maximum aggregate electoral impact (MAEI) for cluster randomized experiments following Slough (2020). This function returns the MAEIs under each of three assumptions about interference between voters. MAEI_d assumes no within-cluster or between-cluster spillovers; MAEI_w assumes within-cluster but no between-cluster spillovers (SUTVA);  and MAEI_bw assumes within-cluster and bounded between-cluster spillovers. The argument psi uses the calculated MAEIs to implement the decision rule proposed in the paper.
#'
#' @param vr a data frame contains variables: district, cluster (unit of random assignment), and corresponding the number of registered voters in the cluster.
#' @param dist a character to specify the column name of the district variable in the data frame vr.
#' @param nvoters a character to specify the column number of the number of voters in the data frame vr.
#' @param cluster a character to to specify the column number of the cluster variable in the data frame vr.
#' @param Z a vector to denote the cluster-level treatment assignments.
#' @param s10 a number or vector to denote the individuals exposed to the treatment because it is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1).
#' @param s01 a number or vector to denote the individuals not exposed to the treatment because is assigned experimentally. It can be the number of voters or the proportion of the electorate (between 0 and 1). The default value is NULL which corresponds to the case in which a researcher designs and implements an intervention that would otherwise not have occurred. If it is not NULL, it corresponds to the case in which some intervention by a third party is modified to include an experimental component.
#' @param pi a number or vector to to measure researchers’ ex-ante beliefs about the proportion (between 0 and 1) of voters that could respond to treatment (or some manifestation thereof) in clusters where allocation of the intervention is not changed by the experiment.
#' @param exp_ac0  a number or vector to denote the expectation of untreated potential outcome. The default value is 1 which will return the most conservative bound.
#'
#' @param psi a number or vector specifying “margin to pivotality”, as minimum change in vote share, as a proportion of registered voters, at which a different officeholder would be elected in district. If psi > 2MAEI (Maximal Aggregate Electoral Impact), an experiment could not change the ultimate electoral outcome (the output result will show "PASS" the decision rule); in contrast, if psi < 2MAEI, the experiment could affect the ultimate electoral outcome (the output result will show "FAIL").  The decision rule in Slough (2023) advocates use of the fith percentile of the predictive distribution as the predicted value of psi.
#'
#' @examples
#'
#' ### input data
#' data(rv)
#' require(randomizr)
#' set.seed(10)
#' Z = block_ra(blocks = rv$d, prob = 1/3) ### generate treatment assignments
#'
#' ### specify s01 as 30 (thus case 2)
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10 = 0.2, s01 = 30,
#' exp_ac0 = 1)
#'
#' ### s10 can be a vector;
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10 = round(runif(n = 150,min = 25,max = 75)),
#' s01 = 30, pi = 0.2)
#'
#' ### add pi = 0.3 to calculate MADI_bw
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10 = 0.2, s01 = 30)
#'
#'
#' ### let margin to pivotality psi as say 0.8
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10 = 0.4,
#' s01 = 30, pi = 0.1, psi = 0.8)
#'
#' @references Slough, Tara. 2020. "The Ethics of Electoral Experimentation: Design-Based Recommendations." Working paper. Available at www.taraslough.com/assets/pdf/eee.pdf.
#'
#' @import dplyr
#' @import magrittr
#' @import randomizr
#'
#' @export

get_maei_clust_rand <- function(vr, ## voter_rolls
                                dist, ## specify variable name in the vr
                                nvoters, ## the same
                                cluster,  ## the same
                                Z, ## treatment vector
                                s10,  ### now is number
                                s01 = NULL,
                                pi = NULL,
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

  ###
  if(sum(s01>vr[,nvoters])>=1) stop("s01 is larger than the number of voters")
  if(sum(s10>vr[,nvoters])>=1) stop("s10 is larger than the number of voters")

  ### maei_d,maei_w

  maei = data.frame(vr[,dist],vr[,cluster],vr[,nvoters],s10, s01, exp_ac0, Z=Z)
  colnames(maei) <- c("district","clus","nvoters","s10","s01","exp_ac0","Z")


  maei1 <- maei %>%
    group_by(district) %>%
    summarise(MAEI_d = max( exp_ac0 * sum((s10+s01)*Z) / sum(nvoters),
                            (1-exp_ac0) * sum((s10+s01)*Z) / sum(nvoters) ),
              MAEI_w = max( exp_ac0 * sum(nvoters*Z*((s10+s01) > 0)) / sum(nvoters),
                            (1-exp_ac0) * sum(nvoters*Z*((s10+s01) > 0)) / sum(nvoters) )
    )


  ### maei_bw

  if (!is.null(pi)){

    maei_ex = cbind(maei,pi)
    colnames(maei1)[8] <- "pi"

    maei2 <- maei_ex %>%
      group_by(district) %>%
      summarise(MAEI_bw =  max( (exp_ac0 * sum(nvoters*Z*((s10+s01)>0))+exp_ac0 * sum(nvoters*(1-Z)*pi)) / sum(nvoters),
                                ((1-exp_ac0) * sum(nvoters*Z*((s10+s01)>0))+(1-exp_ac0) * sum(nvoters*(1-Z)*pi)) / sum(nvoters) )
      )


    if (!is.null(psi)){
      result_MAEI_bw <- NA
      result_MAEI_bw <- ifelse(psi>2*maei2$MAEI_bw, "PASS", "FAIL")
      }


  }

  ### output

  output2 <- list()

  if (!is.null(psi)){

    result_MAEI_d <- NA
    result_MAEI_d <- ifelse(psi>2*maei1$MAEI_d, "PASS", "FAIL")

    result_MAEI_w <- NA
    result_MAEI_w <- ifelse(psi>2*maei1$MAEI_w, "PASS", "FAIL")

    output <- tibble(Districts = maei1$district,
                     MAEI_d = maei1$MAEI_d,
                     result_MAEI_d = result_MAEI_d,
                     MAEI_w = maei1$MAEI_w,
                     result_MAEI_w = result_MAEI_w)

    output2$MAEI_d <- output[, 2]
    output2$MAEI_w <- output[, 4]
    output2$result_MAEI_d <- output[, 3]
    output2$result_MAEI_w <- output[, 5]

    if (!is.null(pi)) {
      output <- cbind(output, MAEI_bw = maei2$MAEI_bw, result_MAEI_bw = result_MAEI_bw)
      output2$MAEI_bw <- output[, 6]
      output2$result_MAEI_bw <- output[, 7]
    }

    output2$psi <- psi

  } else {

    output <- tibble(Districts = maei1$district,
                     MAEI_d = maei1$MAEI_d,
                     MAEI_w = maei1$MAEI_w)

    output2$MAEI_d <- output[, 2]
    output2$MAEI_w <- output[, 3]

    if (!is.null(pi)) {
      output <- cbind(output, MAEI_bw = maei2$MAEI_bw)
      output2$MAEI_bw <- output[, 4]
    }

  }




  cat("\n")
  cat("MAEI_cluster_randomized: exp_ac0 = ", exp_ac0,"\n")
  cat("Randomized by clusters: ", case,"\n")
  if(!is.null(psi)){cat("Margin to pivotality： Psi = ", psi,"\n")}
  print(output)

  # for extraction


  output2$district <- output[, 1]

  invisible(output2)
}


