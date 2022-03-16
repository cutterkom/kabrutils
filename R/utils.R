#' Collapse ID cols to distinct rows
#'
#' Imagine you have a dataframe with two id columns that contain two rows that actually are just one combination.
#' This function distincts them within a `purrr` workflow, see example.
#'
#' @param ... string or numeric vectors, typically id columns
#' @seealso Copied from \url{https://stackoverflow.com/a/62381665/2646974}
#' @examples
#'\dontrun{
#' df <- data.frame(id_1 = c(1,2), id_2 = c(2,1))
#' df %>%
#'   dplyr::mutate(id_1_id_2 = purrr::map2_chr(id_1, id_2, collapse_to_distinct_rows)) %>%
#'   dplyr::distinct(id_1_id_2, .keep_all = TRUE) %>%
#'   dplyr::select(-id_1_id_2)
#'   }

collapse_to_distinct_rows <- function(...){
  c(...) %>%
    sort() %>%
    stringr::str_c(collapse = ".")
}
