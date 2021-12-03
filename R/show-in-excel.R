#' Open a dataframe in an Excel file
#'
#' Pipe a dataframe to an xlsx file and save it maybe.
#'
#' @param data dataframe to show
#' @export
#' @examples
#' \dontrun{
#' mtcars %>% filter(am == 1) %>% show_in_excel()
#' }
show_in_excel <- function(data) {
  if (interactive()) { # avoid unwanted excel executions
    tmp <- paste0(tempfile(), ".xlsx")
    write_xlsx(data, tmp)
    file_show(path = tmp)
  }
  data
}
