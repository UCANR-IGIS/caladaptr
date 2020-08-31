#' @title Area-of-Interest Presets
#' @description Area of interest presets
#' @docType data
#' @usage data(aoipreset_types)
#' @format An character vector with the names of area-of-interest presets
#' @keywords aoipreset
#'
"aoipreset_types"

#' @describeIn aoipreset_types AOI Preset id fields
#' @description The field(s) each AOI Preset provides to identify features
#' @docType data
#' @usage data(aoipreset_idflds)
#' @keywords aoipreset
#' @format Named list with with one element per preset type
"aoipreset_idflds"

#' @describeIn aoipreset_types Values that can be used to identify features
#' @description Values that can be used to identify features in AOI Preset
#' @docType data
#' @keywords aoipreset
#' @usage data(aoipreset_idval)
#' @format Named list of dataframes, with with one element per preset type
"aoipreset_idval"
