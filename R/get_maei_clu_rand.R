#' @title Title
#'
#' @description Description
#'
#' @param vr A data frame contains variable district and the number of voters in the district.
#' @param dist character variable to specify variable district
#' @param nvoters character variable to specify the number of voters
#' @param s10 can be number or proportion
#' @param s01 default NUll: in case one
#' @param exp_ac0  default 1 most conservative
#' @param cluster character variable to specify the cluster
#' @param z treatment
#' @param  pi pi
#'
#'
#'
#' @examples
#' \dontrun{
#' data(rv)  # input data
#' get_maei_clust_rand (vr = rv, dist = "d", nvoters = "n_voters", cluster = "c", z = Z,  s10=0.2, s01 = 30, pi = 0.3, exp_ac0 = 1)
#'}
#'
#'
#' @references xxx
#'
#' @import dplyr
#' @import magrittr
#'
#' @export

get_maei_clust_rand <- function(vr, ## voter_rolls
                                dist, ## specify variable name in the vr
                                nvoters, ## the same
                                cluster,  ## the same
                                z, ## treatment vector (discuss)
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


  ### maei_d,maei_w

  maei = data.frame(vr[,dist],vr[,cluster],vr[,nvoters],s10, s01, exp_ac0, z=z)
  colnames(maei) <- c("district","clus","nvoters","s10","s01","exp_ac0","z")


  maei1 <- maei %>%
    group_by(district) %>%
    summarise(MAEI_d = max( exp_ac0 * sum((s10+s01)*z) / sum(nvoters),
                            (1-exp_ac0) * sum((s10+s01)*z) / sum(nvoters) ),
              MAEI_w = max( exp_ac0 * sum(nvoters*z*((s10+s01) > 0)) / sum(nvoters),
                            (1-exp_ac0) * sum(nvoters*z*((s10+s01) > 0)) / sum(nvoters) )
    )


  ### maei_bw

  if (!is.null(pi)){

    maei_ex = cbind(maei,pi)
    colnames(maei1)[8] <- "pi"

    maei2 <- maei_ex %>%
      group_by(district) %>%
      summarise(MAEI_bw =  max( (exp_ac0 * sum(nvoters*z*((s10+s01)>0))+exp_ac0 * sum(nvoters*(1-z)*pi*((s10+s01)==0))) / sum(nvoters),
                                ((1-exp_ac0) * sum(nvoters*z*((s10+s01)>0))+(1-exp_ac0) * sum(nvoters*(1-z)*pi*((s10+s01)==0))) / sum(nvoters) )
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

