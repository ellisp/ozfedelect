#' Australian Federal Division Electoral Boundaries
#' 
#' Poll and election results for first preference and two-party-preferred voting intention for the 
#' Australian House of Representatives, 2007 to 2019
#'
#' @rdname ced_boundaries
#' @name ced_boundaries
#' @docType data
#' @details a single tibble with the federal division boundaries for 2010, 2013, 2016, and 2019 federal elections.
#' These are called \code{ced_boundaries} to follow the ABS practice of referring to these as Commonwealth
#' Electoral Divisions (AEC does not use the term "Commonwealth election" but "federal election" instead).
#' 
#' These maps have been simplified to 10% of their original size on the ABS website, and cropped to the bounding box
#' that was in use in 2010 (which excludes some far western islands that were in the 2016 and later maps),
#' @format A tibble with simple features geometry
#' @source Australian Bureau of Statistics
NULL