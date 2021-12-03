#' Connect to Databases
#'
#' Used to connect to the databases using a configuration file - for unix and windows
#' The credentials are stored in an external file in the home directory `~`.
#' On Unix: Edit the credentials with `file.edit("~/dbconfig.yml")`.
#' On Windows: Edit the credentials with `file.edit("~/.my.cnf")`.
#'
#' **What inside .yml file?**
#' \describe{
#'   \item{default:}{}
#'   \item{factfield_data:}{}
#'   \item{host: 'db_main.factfield.de'}{}
#'   \item{user: '........'}{}
#'   \item{password: '......'}{}
#'   \item{port: 13306}{}
#'   \item{dbname: 'db_main'}{}
#'   }
#' **What inside .cnf file?**
#'
#' Something like that: \url{https://gist.github.com/rhtyd/d59078be4dc88123104e}
#'
#' @param credential_name Credential identification in the configuration file,
#' example `datascience`, `factfield_data` or `pubmed`
#' @param package The package to be used: DBI or dbx
#' @param config_path Path to the credential file,
#' example: `"~/dbconfig.yml"`
#' @param default.file.windows Path to the configuration file in windows
#' example: `"~/.my.cnf"`
#' @export
#' @examples
#' connect_db() # this connect to factfield_data db_main auto using the credential from the yml data.

connect_db <- function(credential_name = "db_local",
                       package = "DBI",
                       config_path = "~/config.yml",
                       default.file.windows = "~/.my.cnf") {
  # get credentials from config file
  config <- config::get(file = config_path, credential_name)
  if (.Platform$OS.type == "unix") { #if unix
    if (package == "DBI") {
      DBI::dbConnect(
        RMySQL::MySQL(),
        host = config$host,
        user = config$user,
        password = config$password,
        port   = config$port,
        dbname = config$dbname
      )
    } else if (package == "dbx") {
      dbx::dbxConnect(
        adapter = "mysql",
        host = config$host,
        user = config$user,
        password = config$password,
        port   = config$port,
        dbname = config$dbname
      )
    }
  } else { # if windows
    DBI::dbConnect(RMySQL::MySQL(),
                   dbname = credential_name,
                   default.file = default.file.windows)
  }
}
