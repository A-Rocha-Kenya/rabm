#' Extract all data for one specie at all location
#'
#' @param species_id A character the id of the single specie.
#' @param inclnull
#'
#' @export
#'
#' @return
#'
#' @examples
#'
#' \dontrun{
#'
#' # Extract data for a single specie (variable sunbird) at one location
#'
#' pull_single_specie (
#' species_id = '762',
#' inclnull = 1,
#' )
#'
#' }
#'
#'
pull_single_specie <- function(species_id,
                               inclnull = 0) {
  # Check Arguments of function
  Check <- ArgumentCheck::newArgCheck()
  if (length(species_id) > 1 || is.na(as.numeric(species_id))) {
    ArgumentCheck::addError(
      msg = "'species_id' is incorrectly specified. Please use a single numeric or character of numeric",
      argcheck = Check
    )
  }
  if (!(as.numeric(inclnull) == 1 || as.numeric(inclnull) ==0) ) {
    ArgumentCheck::addError(
      msg = "'inclnull' needs to be 0 or 1",
      argcheck = Check
    )
  }
  ArgumentCheck::finishArgCheck(Check)


  # get number of records to be returned
  readr::read_csv(
    glue::glue(
      "http://api.adu.org.za/sabap2/v2/cards/species/info/{species_id}?format=csv&inclnull={inclnull}"
    ),
    col_types = readr::cols()
  )


}
