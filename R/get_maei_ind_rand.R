#' @title Title
#'
#' @description Description
#'
#' @param vr A data frame contains variable district and the number of voters in the district.
#' @param dist character to specify district variable
#' @param nvoters character to specify the number of voters vairable
#' @param s10 can be number or proportion
#' @param s01 default NUll: in case one
#' @param exp_ac0  default 1 most conservative
#'
#' @examples
#' \dontrun{
#' data(rv1)  # input data
#' get_maei_ind_rand(vr = rv1, dist = "d", nvoters = "n_voters", s10 = 20)
#'}
#' @references xxx
#'
#' @import dplyr
#' @import magrittr
#'
#' @export

get_maei_ind_rand <- function(vr, ## voter_rolls
                              dist, ## specify variable name in the vr
                              nvoters, ## the same
                              s10,  ## number or proportion
                              s01 = NULL,
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


  # output <- matrix(NA, nrow = nrow(vr), ncol = 2)
  #  output[,1] <- vr[,dist]
  # output[,2] <- maei$MAEI_d
  # colnames(output) <- c("Districts","MAEI_d")

  cat("\n")
  cat("MAEI_ind_randomized: exp_ac0 = ", exp_ac0,"\n")
  cat("Individually randomized: ", case,"\n")
  print(output)

  # for extracttion

  output2 <- list()
  output2$MAEI_d <- output[, 2]
  output2$district <- output[, 1]

  invisible(output2)
}


