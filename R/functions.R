#' Contigency tables for analysis with the cross-wise model
#'
#' @description \code{make_tables} Transforms individual responses to questions of the cross-wise
#' model into a contingency table format for analysis with the logistic and loglinear model.
#' @param df a data frame including the subsequent pairs of individual responses to the sensitive
#' items, with the 1st variable pertaining to the subsample with the smallest probability of answering
#' the innocuous question affirmatively.
#' @param v_order a character vector with fractional notation of the probabilities of answering the
#' innocuous question within each subsample. Defaults to \code{c("1/5", "4/5")}.
#' @param in_answers a character vector with two strings  distinguishing between the
#' the reponses, with the first string corresponding to a single
#' affirmative response, and the second to zero or two affirmative responses. Defaults
#' to \code{c("ONE", "TWO")}.
#' @return
#' \item{twoway}{list with the two-way contingency tables for the subsequent items. The contingency tables
#' are a cross-classification of the suffixed variables \code{V}  denoting the probability of an
#' affirmative response to the innocuous question, and the suffixed variables \code{Q} denoting
#' the responses \code{both/none} and \code{one}, with the suffix denoting the sensitive item.}
#' \item{multiway}{a multi-way contingency table of the suffixed variables \code{V} and \code{Q}
#' for all items jointly.}
#' \item{q}{the first element of the vector \code{v_order}. To be ussed in the functions
#' \code{\link{logistic}} and \code{\link{loglinear}}}
#' @examples
#' ## Two-way and multiway tables for all four sensitive items.
#' tables_R2 <- make_tables(R2)
#' @importFrom dplyr filter arrange across transmute case_when %>%
#' @importFrom rlang .data
#' @export

make_tables  <- function(df, v_order = c("1/5", "4/5"), in_answers = c("ONE", "TWO")){

  sets  <- ncol(df)/2

  pairs <- factor(rep(letters[1:sets], each = 2))

  s     <- split(1:ncol(df), pairs)

  d     <- df

  ####################################################
  # twoway contingency table for loglinear analysis#
  ####################################################



  for(i in 1:sets){

    d <- data.frame(d, make_versions(df = df, s = s, v_order = v_order, in_answers = in_answers, set = i))

  }

  d <- d[, -(1:ncol(df))]

  twoway <- list()

  for(i in 1:sets){

    twoway[[paste0("Q",i)]] <- as.data.frame(table(rev(d[s[[i]]])))

  }

  ####################################################
  # multiway contingency table for loglinear analysis#
  ####################################################

   multiway <- as.data.frame(table(d)) %>%
    dplyr::filter(.data$Freq > 0) %>%
    dplyr::arrange(dplyr::across(.cols = dplyr::everything()))

  list(twoway   = twoway,
       multiway = multiway,
       q        = v_order[1])
}




#' Prevalence estimation with the logistic crosswise model
#'
#' @description \code{logistic} Fits logistic models to data collected under the crosswise model.
#' @param tables object created by the function \code{make_tables}.
#' @return
#' \item{estimates}{matrix with theprevalence estimates for each sensitive item.}
#' \item{fit}{list with the observed and the fitted frequencies of the two-way
#' contingency tables.}
#' @examples
#' ## Fit the logistic model to the items in dataset R2.
#' logistic(make_tables(R2))
#' @importFrom dplyr filter
#' @export


logistic <- function(tables){

  fit   <- tables$twoway

  items <- length(fit)

  q     <- sapply(strsplit(tables$q, split = "/"),
                  function(x) as.numeric(x[1]) / as.numeric(x[2]))

  estimates <- matrix(NA, items, 5, dimnames = list(names(tables[[1]]),
                                                    c("phat", "max95", "min95", "X2", "pval")))

  for(i in seq_len(items)){

    nobs <- tables$twoway[[i]]$Freq

    P <- P_twoway(nobs = nobs, q)

    f <- stats::optim(0, cwm, nobs = nobs, q = q, P = P,
              method = "Brent", lower = -10, upper = 10,
              hessian = T)

    phat  <- lgt(f$par)

    se   <- sqrt(solve(f$hessian))

    ci    <- c(min95 = lgt(f$par - 1.96*se),
               max95 = lgt(f$par + 1.96*se))

   Fitted <- sum(nobs) * P %*% lg(f$par)

   LR     <- nobs * log(nobs / Fitted)

   X2     <- 2*sum(nobs * log(nobs / Fitted), na.rm = T)

   fit[[i]] <- cbind(tables$twoway[[i]], Fitted = round(Fitted, 1), LR = round(LR, 2))


   estimates[i, ] <- round(c(phat, ci, round(X2, 1), stats::pchisq(X2, 2, lower.tail = FALSE)), 3)


  }

  print(estimates)

  invisible(list(estimates = estimates, fit = fit))


}


#' Prevalence estimation with the loglinear crosswise model
#'
#' @description \code{loglinear} Fits logistic models to data collected under the crosswise model.
#' @param tables object created by the function \code{make_tables}.
#' @param model model to be fitted. The model is denoted by suffixed variables \code{Q}.
#' @param exclude scalar or numeric denoting the senstive item(s) to be excluded from the analysis
#' @param seed the seed for the start values of the loglinear parameters
#' @return
#' \item{gof}{goodness-of-fit statistics.}
#' \item{fitted}{multi-way contingency table with the observed and fitted frequencies.}
#' \item{probs}{multivariate prevalence estimates.}
#' \item{prev}{univariate prevalence estimates.}
#' \item{pars}{loglinear parameter estimates.}
#' @examples
#' ## Fit the loglinear model [Q3][Q2Q4] to the items in R2, excluding the 1st item.
#' loglinear(tables = make_tables(R2), model = ~ . + Q2:Q3, exclude = 1)
#' @importFrom dplyr filter
#' @export

loglinear <- function(tables, model = ~ ., exclude = NULL, seed = 1){

  if(is.null(exclude)){

    d <- tables$multiway

  }else{

    d <- tables$multiway[, -grep(exclude, names(tables$multiway))]

    d <- as.data.frame(stats::xtabs(Freq ~ ., d)) %>%
      dplyr::filter(.data$Freq > 0) %>%
      dplyr::arrange(dplyr::across(.cols = dplyr::everything()))

  }

  f    <- d$Freq

  n    <- sum(f)

  k    <- (ncol(d) - 1) / 2

  p1   <- sum(f[1:(length(f)/2)]) / sum(f)

  p2   <- 1 - p1

  q    <- sapply(strsplit(tables$q, split = "/"),
                 function(x) as.numeric(x[1]) / as.numeric(x[2]))

  P <- matrix(c(q, 1 - q,
                1 - q, q,
                1 - q, q,
                q, 1 - q), 4, 2)

  P1 <- P2 <- 1



  for(j in seq(1, 2 * k, by = 2)){

    if(d[1, j] == tables$q){

        P1 <- P1 %x% P[1:2, ]
        P2 <- P2 %x% P[3:4, ]

    }else{

      P1 <- P1 %x% P[3:4, ]
      P2 <- P2 %x% P[1:2, ]

    }
  }

  Q <- rbind(p1 * P1, p2 * P2)

  D <- stats::model.matrix(model, d[1:(length(f) / 2), seq(2, 2 * k, by = 2)])

  logl <- function(b){

    beta <- c(-log(sum(exp(D[, -1] %*% b))), b)

    -sum(f * log(Q %*% exp(D %*% beta)))
  }

  set.seed(seed)

  fit <- stats::optim(stats::runif(ncol(D) - 1, -.1, .1), logl, method = "BFGS", hessian = TRUE)

  beta <- c(-log(sum(exp(D[,-1] %*% fit$par))), fit$par)

  vcv <-  tryCatch(solve(fit$hessian),
                   error   = function(e){return("singular")},
                   warning = function(e){return("infinite")})

  if(class(vcv)[1]=="matrix"){

    se   <- c(sum(sqrt(sum(solve(fit$hessian)))), sqrt(diag(solve(fit$hessian))))
    tval <- beta/se
    pval <- 2 * stats::pt(-abs(tval), df = dim(D)[1] - length(beta))

    parameters <- data.frame(beta, se, tval, pval, row.names = colnames(D))


  }else{

    parameters <- data.frame(beta = beta,
                             se   = rep(NA, length(beta)),
                             tval = rep(NA, length(beta)),
                             pval = rep(NA, length(beta)),
                             row.names = colnames(D))

  }


  phat <- exp(D %*% beta)

  nhat <- n * Q %*% phat

  AIC <- 2 * (fit$value + length(beta))

  X2   <- 2 * sum(f * log(f / nhat))

  df   <- nrow(d) - length(beta)

  pval <- 1 - stats::pchisq(X2, df = df)

  univ <- matrix(0, 1, k, dimnames = list("phat", colnames(d)[seq(2, 2*k, by = 2)]))

  for(i in 1:k) univ[, i] <- 1 - sum(phat[D[, i + 1] == 1])

  gof  <-  data.frame(AIC = round(AIC, 1), X2 = round(X2, 1), df = df, pval = round(pval, 3), row.names = "g.o.f")

  univ <- data.frame(univ)

  cat("\n")

  print(data.frame(round(gof, 3), row.names  = "g.o.f."))

  cat("\n")

  print(round(univ, 3))

  cat("\n")

  print(round(parameters, 3))

  invisible(list(gof     = gof,
                 fitted  = data.frame(d, Fitted = round(nhat, 1)),
                 probs   = data.frame(D[, 2:(k+1)], phat =  round(phat, 3), row.names = NULL),
                 prev    = univ,
                 pars    = parameters))
}
