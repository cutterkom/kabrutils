#' extract text from xml nodes
#'
#' When scraping data with `rvest` one will typically extract text from nodes.
#' This functions helps doing so. It makes sure, that multiple `p` are parsed into one object.
#' This is important, when parsing different-length items into one dataframe.
#' @param node node
#' @param class css class to parse
#' @export
#' @examples
#' \dontrun{
#' "https://forummuenchen.org/lgbtiq-chronik/" %>%
#'   read_html() %>%
#'   html_nodes(".timeline-item") %>%
#'   extract_text("p")
#' }
extract_text <- function(node, class) {
  text <- map(node, ~ .x %>%
    html_nodes(class) %>%
    html_text())
  return(text)
}

#' Call lobid API
#' @description
#' lobid offers three types of APIs: `resources`, `organisations` and `gnd`. See their respective docs at:
#'
#' * https://lobid.org/resources
#' * https://lobid.org/organisations
#' * https://lobid.org/gnd
#'
#'
#' @param query string
#' @param parameter string, e.g. isbn
#' @param verbose if TRUE, then url printed to console
#' @param as_list default: ``FALSE``. If `TRUE` then json is converted to a R list, otherwise just url
#' @param lobid_api_type lobid offers 3 types of APIs: `resources`, `organisations` and `gnd`; defaults to `resources`
#' @return list
#' @export
#' @examples
#' \dontrun{
#' call_lobid_api(3894090685, query = ., parameter = "isbn")
#' }

call_lobid_api <- function(query, parameter = NULL, verbose = TRUE, as_list = FALSE, lobid_api_type = "resources") {

  # build URL according to lobid documentation
  if (is.null(parameter)) {
    url <- paste0("https://lobid.org/", lobid_api_type, "/search?q=", query, "&format=json")
    url <- URLencode(url)
  } else if (!is.null(parameter)) {
    url <- paste0("https://lobid.org/", lobid_api_type, "/search?q=", parameter, ":", query, "&format=json")
  }

  if (verbose == TRUE) {
    message(paste0("URL: ", url))
  }

  if (as_list == TRUE) {
    json <- jsonlite::fromJSON(url)
    json
  } else {
    url
  }

}


#' Get value of a special field
#'
#' This function fetches the value of fields in a nested json, no matter on which level.
#' Based on the very popular js, JSON command line processor https://stedolan.github.io/jq/
#' @importFrom curl curl
#' @importFrom jqr jq
#' @param input the json url
#' @param input_type `url` when string, `response` when fetched response (e.g. when using the same response for multiple queries)
#' @param jq_syntax jq filter (test here https://jqplay.org/)
#' @export
#' @examples
#' \dontrun{
#' "https://lobid.org/resources/search?q=isbn:3596237785&format=json" %>%
#' get_field_values("gndIdentifier")
#' }
get_field_values <- function(input, input_type = "url", jq_syntax) {
  if(input_type == "url") {
    curl::curl(input) %>% jqr::jq(jq_syntax)
  } else if(input_type == "response") {
    input %>% jqr::jq(jq_syntax)
  } else {
    stop("not implemented")
  }

}
