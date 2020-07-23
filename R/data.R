#' R2
#'
#' Data collected via an online forum in 2020 with the cross-wise model with
#' 5-number sequences as randomizer for the innocuous questions. The four
#' sensitive questions involve substance use, and the responses to these
#' questions are collected in pairs of variables, with the 1st and 2nd variables
#' denoting the subsamples with respective probabilities of 1/5 and 4/5
#' of answering the innocuous questionan affirmatively.
#'
#' @format Data frame with 1211 observations and the variables in columns:
#' \describe{
#'   \item{1}{drug tested, subsample with probability 1/5}
#'   \item{2}{drug tested, subsample with probability 4/5}
#'   \item{3}{supplements, subsample with probability 1/5}
#'   \item{4}{supplements, subsample with probability 4/5}
#'   \item{5}{psychoactive drugs, subsample with probability 1/5}
#'   \item{6}{psychoactive drugs, subsample with probability 4/5}
#'   \item{7}{doping, subsample with probability 1/5}
#'   \item{8}{doping, subsample with probability 4/5}
#' }
#'
#' @note The responses are coded "I've only ONE 'YES' answer" and
#' "I've TWO 'YES' or TWO 'NO' answers".
#' @source WADA.
"R2"

#' PanAm
#'
#' Data collected during the PanAm Games of 2019 with the cross-wise model with
#' 15-number sequences as randomizer for the innocuous questions. The three
#' sensitive questions involve substance use, and the responses to these
#' questions are collected in pairs of variables, with the 1st and 2nd variables
#' denoting the subsamples with respective probabilities of 1/5 and 4/5
#' of answering the innocuous questionan affirmatively.
#'
#' @format Data frame with 630 observations and the variables in columns:
#' \describe{
#'   \item{1}{doping tested, subsample with probability 1/5}
#'   \item{2}{doping tested, subsample with probability 4/5}
#'   \item{3}{supplements, subsample with probability 1/5}
#'   \item{4}{supplements, subsample with probability 4/5}
#'   \item{5}{doping, subsample with probability 1/5}
#'   \item{6}{doping, subsample with probability 4/5}
#' }
#' @note The responses are coded "one" and "zero/two".
#' @source WADA.
"PanAm"

#' monkey
#'
#' Data collected on SurveyMonkey with the cross-wise model with
#' 15-number sequences as randomizers for the innocuous questions. The four
#' sensitive questions involve substance use, and the responses to these
#' questions are collected in pairs of variables, with the 1st and 2nd variables
#' denoting the subsamples with respective probabilities of 1/5 and 4/5
#' of answering the innocuous questionan affirmatively.
#'
#' @format Data frame with 1506 observations and the variables in columns:
#' \describe{
#'   \item{1}{doping tested, subsample with probability 1/5}
#'   \item{2}{doping tested, subsample with probability 4/5}
#'   \item{3}{supplements, subsample with probability 1/5}
#'   \item{4}{supplements, subsample with probability 4/5}
#'   \item{5}{psychoactive drugs, subsample with probability 1/5}
#'   \item{6}{psychoactive drugs, subsample with probability 4/5}
#'   \item{7}{doping, subsample with probability 1/5}
#'   \item{8}{doping, subsample with probability 4/5}
#' }
#' @note The data were collected under a condition with statements and a condition with
#' questions. Consequently, different codings of for the responses were used. In the present data
#' these two conditions are taken together, and the responses are coded "I've only ONE 'YES' answer"
#' or "Only ONE statement is true" for a single affirmative response, and
#' "I've TWO 'YES' or TWO 'NO' answers" or "TWO" for zero or two affirmative answers.

#' @source WADA.
"monkey"
