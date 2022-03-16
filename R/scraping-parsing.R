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
