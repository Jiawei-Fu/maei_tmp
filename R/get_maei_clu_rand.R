#' @title Title
#'
#' @description Description
#'
#' @param vr a data frame contains variables: district and corresponding the number of voters in the district.
#' @param dist a character to specify the column name of the district variable in the vr.
#' @param nvoters a character to specify the column number of the number of voters in the vr.
#' @param cluster a character to to specify the column number of the cluster variable in the vr.
#' @param Z a vector to denote the treatment assignments.
#' @param s10 a number or vector to denote the individuals exposed to the treatment because it is assigned experimentally. It can be the exact number or proportion (between 0 and 1).
#' @param s01 a number or vector to denote theindividuals not exposed to the treatment because is assigned experimentally. It can be the exact number or proportion (between 0 and 1). Default value is NUll which means it is the case one  in which a researcher designs and implements an intervention that would otherwise not have occurred. If it is not NUll, it means case 2 in which some intervention by an NGO or IG is modified to include an experimental component.
#' @param  a number or vector to to measure researchers’ ex-ante beliefs about the proportion of voters that could respond to treatment (or some manifestation thereof) in clusters where allocation of the intervention is not changed by the experiment.
#' @param exp_ac0  a number or vector to denote the expectation of untreated potential outcome. The default value is one which will return the most conservative bound.
#'
#'
#' @examples
#' \dontrun{
#' data(rv)  # input data
#' require(randomizr)
#' Z = block_ra(blocks = rv$d, prob = 1/3) ### generate treatment assignments
#'
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10=0.2, s01 = 30, pi = 0.3,
#'  exp_ac0 = 1)
#'
#' get_maei_clust_rand (vr = rv, dist = "d",
#' nvoters = "n_voters",
#' cluster = "c", Z = Z,
#' s10=round(runif(n = 150,min = 25,max = 75)),
#' s01 = 30,
#' pi = 0.1)
#'}
#'
#'
#' @references xxx
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
                                Z, ## treatment vector (discuss)
                                s10,  ### now is number
                                s01 = NULL,
                                pi = NULL,
                                exp_ac0 = 1 # most conservative
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

  }

  ### output


  output <- tibble(Districts = maei1$district,
                   MAEI_d = maei1$MAEI_d,
                   MAEI_w = maei1$MAEI_w)

  if (!is.null(pi)) {
    output <- cbind(output,MAEI_bw = maei2$MAEI_bw)
  }

  cat("\n")
  cat("MAEI_cluster_randomized: exp_ac0 = ", exp_ac0,"\n")
  cat("Randomized by clusters: ", case,"\n")
  print(output)

  # for extracttion

  output2 <- list()
  output2$district <- output[, 1]
  output2$MAEI_d <- output[, 2]
  output2$MAEI_w <- output[, 3]


  if (!is.null(pi)) output2$MAEI_bw <- output[, 4]


  invisible(output2)
}

