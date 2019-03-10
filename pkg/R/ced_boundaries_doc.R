#' Australian Federal Electoral Division Boundaries
#' 
#' Federal electoral division boundaries 2010 to 2019
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


#' Australian Census by Federal Election Division
#' 
#' Selected variables from the 2016 Census by Federal Election division
#'
#' @rdname ced_data_2016
#' @name ced_data_2016
#' @docType data
#' @details a single tibble with one row per electoral division and one column per variable of interest.
#' @format A tibble
#' @source Australian Bureau of Statistics, via Hugh Parsonage's Census2016.DataPack R package, with minimal 
#' aggregation done by Peter Ellis for convenience.
NULL