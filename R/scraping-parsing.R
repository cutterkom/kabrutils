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
#' @param lobid_api_type lobid offers 3 types of APIs: `resources`, `organisations` and `gnd` as well as a direct search for an entity via GND_ID `gnd_id_search`; defaults to `resources`
#' @return list
#' @export
#' @examples
#' \dontrun{
#' call_lobid_api(3894090685, query = ., parameter = "isbn")
#' }

call_lobid_api <- function(query, parameter = NULL, verbose = TRUE, as_list = FALSE, lobid_api_type = "resources") {

  if (lobid_api_type == "gnd_id_search") {
    # calling GND entity json directly via GND_ID
    # https://lobid.org/gnd/api
    url <- paste0("https://lobid.org/gnd/", query, ".json")
  } else {
    # build URL according to lobid documentation
    if (is.null(parameter)) {
      url <- paste0("https://lobid.org/", lobid_api_type, "/search?q=", query, "&format=json")
      url <- URLencode(url)
    } else if (!is.null(parameter)) {
      url <- paste0("https://lobid.org/", lobid_api_type, "/search?q=", parameter, ":", query, "&format=json")
    }
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

#' Transform json to a tidy dataframe
#' Uses `jsonlite::fromJSON` together with some `map`, `enframe` and `unnest_wider` to create a tidy tibble/dataframe.
#' @param json json string
#' @param unnest_type should the dataframe be wide or long?
#' @param keep_name default: `TRUE`; `tibble::enframe()` names resulting cols, decide if `name` column is needed
#' @return dataframe/tibble
#' @export
#' @examples
#' \dontrun{
#' res <- httr::GET("https://lobid.org/gnd/search?q=preferredName:Max%20Spohr&format=json")
#' res <- httr::content(res, as = "text")
#' df_from_json <- transform_json_to_dataframe(res, unnest_type = "wide")
#' }

transform_json_to_dataframe <- function(json, unnest_type = "long", keep_name = TRUE) {

  if (unnest_type == "long") {
    dataframe <- json %>%
      purrr::map(jsonlite::fromJSON) %>%
      tibble::enframe() %>%
      tidyr::unnest_longer(value)
  } else if (unnest_type == "wide") {
    dataframe <- json %>%
      purrr::map(jsonlite::fromJSON) %>%
      tibble::enframe() %>%
      tidyr::unnest_wider(value)
  } else {
    stop("not implemented")
  }

  if(keep_name == FALSE) {
    dataframe <- dataframe %>% dplyr::select(-name)
  }
  dataframe
}
