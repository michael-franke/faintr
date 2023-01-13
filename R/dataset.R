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

#' Mental Chronometry data
#'
#' @description Data from an experiment investigating differences in reaction times
#' across three tasks of increasing complexity.
#'
#' Participants reacted to a square or a circle (variable \code{shape})
#' appearing on the screen by
#'
#' * pressing the space bar whenever they saw any shape (block \code{reaction}).
#' * pressing the space bar whenever they saw a specific shape (block \code{goNoGo}).
#' * pressing a key when they saw one of the shapes and a different key when they
#' saw the other one (block \code{discrimination}).
#'
#' @format A data frame with 2,404 rows and 4 variables:
#' \describe{
#'   \item{submission_id}{A unique identifier for each participant.}
#'   \item{shape}{The shape presented on the screen.}
#'   \item{block}{The task of how to react to a shape.}
#'   \item{RT}{The measured reaction time in milliseconds.}
#' }
#'
#' @examples
#' \dontrun{
#' # fit a linear model using 'brms'
#' # regress reaction time as a function of the block
#' fit <- brms::brm(RT ~ block, data = MC)
#'
#' # check encoding scheme used in the model
#' get_cell_definitions(fit)
#'
#' # get draws for the reaction block
#' extract_cell_draws(fit, block == "reaction")
#'
#' # compare the goNoGo block with the reaction block
#' compare_groups(fit, block == "goNoGo", block == "reaction")
#' }
#'
"MC"

#' Adams' Thesis data
#'
#' @description Data from a replication of Douven and Verbrugge (2010)
#' investigating the relationship between acceptability ratings of conditional
#' sentences and conditional probabilities (variable \code{rating}) across
#' three different types of conditionals (inductive, abductive, deductive).
#'
#' @format A data frame with 4,590 rows and 5 variables:
#' \describe{
#'   \item{submission_id}{A unique identifier for each participant.}
#'   \item{item_nr}{An identifier for the context-statement pair presented
#'   to the participant.}
#'   \item{rating}{The kind of rating the participant has given.}
#'   \item{cond_type}{The type of conditional statement.}
#'   \item{response}{The measured acceptability rating on a 7-point Likert scale.}
#' }
#'
#' @references
#' Douven, I., Verbrugge, S. (2010). The Adams Family. \emph{Cognition},
#' \emph{117}, 302-18. \url{https://doi.org/10.1016/j.cognition.2010.08.015}.
#'
#' @examples
#' \dontrun{
#' # fit an ordinal model using 'brms'
#' # regress response as a function of rating and conditional type (with interaction)
#' fit <- brms::brm(response ~ rating * cond_type,
#'                  data = AT,
#'                  family = cumulative("logit"))
#'
#' # check encoding scheme used in the model
#' get_cell_definitions(fit)
#'
#' # get draws for acceptability averaged over the type of conditional
#' extract_cell_draws(fit, rating == "acceptability")
#'
#' # compare main effects of rating
#' compare_groups(fit, rating == "acceptability", rating == "cond_probability")
#' }
#'
"AT"
