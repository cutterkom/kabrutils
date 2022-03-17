#' Write a dataframe to an xlsx file
#'
#' adds filter and auto-width
#'
#' beware: deletes existing file with the same name
#'
#' @param data dataframe to export
#' @param filename name of excel file
#' @param sheet sheetname
#' @export
#' @examples
#' \dontrun{
#' write_xlsx(my_data, "name.xlsx", "sheetname")
#' }
#'

write_xlsx <- function(data, filename, sheet) {

  wb <- createWorkbook()

  addWorksheet(wb = wb, sheetName = sheet)
  writeData(wb = wb, sheet = sheet, x = data)
  # freeze first row
  freezePane(wb = wb, sheet = sheet, firstRow = TRUE)
  # auto width depending on content
  setColWidths(wb, sheet = sheet, cols = 1:ncol(data), widths = "auto")
  # add filter
  addFilter(wb, sheet = sheet, rows = 1, cols = 1:ncol(data))
  # delete existing file
  unlink(filename)
  saveWorkbook(wb = wb, file = filename)

}
