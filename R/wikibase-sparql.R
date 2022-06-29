#' Fetch results of a SPARQL query to a tibble
#'
#' @param query SPARQL query as a string
#' @param endpoint SPARQL endpoint of a Wikibase instance
#' @param useragent default: paste("Wikibase", R.version.string)
#' @export
#' @examples
#' \dontrun{
#' addresses_in_munich <- 'SELECT ...'
#' endpoint <- "https://database.factgrid.de/sparql"
#' sparql_tibble <- sparql_to_tibble(query = addresses_in_munich, endpoint = endpoint)
#' }

sparql_to_tibble <- function(query, endpoint, useragent) {
  useragent <- paste("Wikibase", R.version.string)
  res <- SPARQL::SPARQL(endpoint, query, curl_args = list(useragent = useragent))
  tibble::tibble(res$results)
}

#' Add a statement to an item
#'
#' This function helps to add certain statements. A statement consists of a PID-QID combination that is added to a dataframe. The PID will be the column name, QID the row content.
#' It assumes there is a dataframe that has at least three columns: (1) `statements`, (2) `pid`, (3) `qid`.
#' @param data dataframe to add the new column
#' @param available_statements dataframe with all statements that can be added with this method. It assumes there is a dataframe that has at least three columns: (1) `statement`, (2) `pid`, (3) `qid`.
#' @param new_statement string of new statement. Must exist in `available_statements$statement`. If statement is `coordinates`, then a column `longitude` and a column `latitude` is expected.
#' @param verbose show in terminal what was added
#' @param qid_from_row boolean; default `FALSE` - then QID is taken from dataframe `statements`, if `TRUE` id QID value should be taken from another row
#' @param col_for_row_content string; name of column in dataframe `data` that contains the QID values
#' @export
#' @examples
#' statements <- data.frame(statement = c("my_statement"), pid = c("P2"), qid = c("Q1"))
#' data <- data.frame(item = "my item")
#' data %>% add_statement(available_statements = statements, new_statement = "my_statement")

add_statement <- function(data = NULL,
                          available_statements = statements,
                          new_statement = NULL,
                          verbose = TRUE,
                          qid_from_row = FALSE,
                          col_for_row_content = NULL) {

  new_statement_df <- dplyr::filter(available_statements, statement == new_statement)

  if(nrow(new_statement_df) == 0) {
    stop("Your statement can't be found. Please check if it exists in the table `available_statements` or if there's a typo.")
  }

  pid_as_column_name <- rlang::sym(new_statement_df$pid)

  if(qid_from_row == TRUE) {

    if (new_statement == "coordinates") {
      if (!is.null(data$latitude) & !is.null(data$longitude)) {
        latitude <- "latitude"
        longitude <- "longitude"
        data <- data %>% dplyr::mutate(
          !!pid_as_column_name :=
            dplyr::case_when(
              !is.na(.data[[latitude]]) ~

                #paste0(.data[[latitude]], ",", .data[[longitude]]),
                paste0('"@', .data[[latitude]], '/', .data[[longitude]], '"'),
              TRUE ~ NA_character_
            ))
      } else {
        stop("The input data frame `data` needs a column `latitude` and a column `longitude` in order to add coordinates.")
      }
    } else {
      data <- dplyr::mutate(data, !!pid_as_column_name := .data[[col_for_row_content]])
    }

  } else {
    data <- dplyr::mutate(data, !!pid_as_column_name := new_statement_df$qid)
  }

  if (verbose == TRUE) {
    cli::cli_h1('add "{new_statement}" statement')
    cli::cli_bullets(
      c(
        "*" = "PID = {new_statement_df$pid}",
        "*" = "QID = {new_statement_df$qid}"
      )
    )
  }

  return(data)
}


#' Longform for import
#'
#' in form: item - property - value
#' @param data dataset
#' @param start_at column to start longform; input to ``tidyr::pivot_longer(col)``
#' @importFrom tidyselect last_col
#' @export
long_for_quickstatements <- function(data, start_at = 2) {
  data %>%
    tidyr::pivot_longer(cols = start_at:tidyselect::last_col(), names_to = "property", values_to = "value") %>%
    # fix helper with two instances_of:
    dplyr::mutate(property = stringr::str_remove(property, "_.*")) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::distinct()
}
