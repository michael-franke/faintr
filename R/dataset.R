#' Politeness data
#'
#' @description This data set is a preprocessed version of the data
#' provided by Winter and Grawunder (2012). It includes measurements
#' of voice pitch of Korean speakers in a 2 x 2 factorial design, with
#' factors \code{gender} (male or female) and \code{context} (informal or polite).
#'
#'
#' @format A data frame with 83 rows and 5 variables:
#' \describe{
#'   \item{subject}{A unique identifier for each participant.}
#'   \item{gender}{An indicator of the participants gender (only binary).}
#'   \item{sentence}{An indicator of the sentence spoken by the participant.}
#'   \item{context}{The main manipulation of whether the context was a polite
#'   or informal setting.}
#'   \item{pitch}{The measured voice pitch in Hz (presumably: average over
#'   the sentence spoken).}
#' }
#'
#' @source
#' Winter, B., Grawunder, S. (2012). The phonetic profile of Korean formality.
#' \emph{Journal of Phonetics}, \emph{40}, 808-815.
#'
#' @examples
#' \dontrun{
#' # fit a linear model using 'brms'
#' # regress pitch as a function of dummy-coded factors 'gender' and 'context'
#' fit <- brms::brm(pitch ~ gender + context, data = politeness)
#'
#' # check encoding scheme used in the model
#' get_cell_definitions(fit)
#'
#' # get draws for male speakers in informal contexts
#' extract_cell_draws(fit, gender == "M" & context == "inf")
#'
#' # compare main effects of gender
#' compare_groups(fit, gender == "F", gender == "M")
#' }
#'
"politeness"
