#' Australian Federal Voting Intention Polls
#' 
#' Poll and election results for first preference and two-party-preferred voting intention for the 
#' Australian House of Representatives, 2007 to 2019
#'
#' @rdname ozpolls
#' @name ozpolls
#' @docType data
#' @format A tibble with 5,500+ rows and 10 variables.
#' @source Various polling firms, collected on Wikipedia and scraped by Peter Ellis
NULL

#' Australian Federal Voting Intention Polls for the 2016 Election
#' 
#' Poll and election results for first preference and two-party-preferred voting intention
#'
#' @details Normally you don't need this as all the information in it is in the larger object \code{ozpolls}. 
#' However, the 2016 data is unusual in having information on sample size, margin of error, and method which
#' are not available in \code{ozpolls}.
#' @rdname ozpolls_2016
#' @name ozpolls_2016
#' @docType data
#' @format A tibble with 1,518 rows and 9 variables.
#' @source Various polling firms, collected on Wikipedia and scraped by Peter Ellis
NULL

#' Australian Federal Voting Intention Polls for the 2010 Election
#' 
#' Poll and election results for first preference and two-party-preferred voting intention
#'
#' @details Normally you don't need this as all the information in it is in the larger object \code{ozpolls}.
#' However, the 2010 data was unusual in having separate first preference information for Liberal and National.
#' This has been combined in \code{ozpolls} so the original \code{ozpolls_2010} data frame is made available if wanted.
#' @rdname ozpolls_2010
#' @name ozpolls_2010
#' @docType data
#' @format A tibble with 485 rows and 9 variables.
#' @source Various polling firms, collected on Wikipedia and scraped by Peter Ellis
NULL




