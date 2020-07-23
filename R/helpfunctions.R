# creates a version and answer variable for each pair

make_versions <- function(df, s, v_order, in_answers, set){

  k = s[[set]]

  out <- df %>%
    dplyr::transmute(V = dplyr::case_when(!is.na(.[[k[1]]]) ~ v_order[1],
                                          !is.na(.[[k[2]]]) ~ v_order[2]),
                     Q = dplyr::case_when(grepl(in_answers[1], df[[k[1]]]) ~ "one",
                                          grepl(in_answers[2], df[[k[1]]]) ~ "both/none",
                                          grepl(in_answers[1], df[[k[2]]]) ~ "one",
                                          grepl(in_answers[2], df[[k[2]]]) ~ "both/none"))
   colnames(out) <- paste0(c("V", "Q"), set)

  out
}


# softmax function

lg  <- function(x) exp(c(x, 0)) / sum(exp(c(x, 0)))

# logistic function.

lgt <- function(x) exp(x) / (1 + exp(x))

# loglikelihood function for univariate CWM

cwm <- function(p0, q, nobs, P){

  -t(nobs) %*% log(P %*% lg(p0))

}


# constructs the misclassification matrix for the logistic model

P_twoway <- function(nobs, q){

  p1  <- sum(nobs[1:2])/sum(nobs) # group with smallest q
  p2  <- sum(nobs[3:4])/sum(nobs) # group with largest q



  matrix(c(q*p1,     (1-q)*p1,
           (1-q)*p1,     q*p1,
           (1-q)*p2,     q*p2,
           q*p2,     (1-q)*p2),
         4, 2,
         dimnames = list(c("1/5 both/none", "1/5 one", "4/5 both/none", "4/5 one"),
                         c("dope", "free")),
         byrow=T)
}








