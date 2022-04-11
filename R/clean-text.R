#' Clean string
#'
#' - remove leading and trailing whitespace
#' - change all characters to their lowercase representation
#' - remove all punctuation and numbers
#' - transforming all special characters (ä, ß, ...) to 1) de-ASCII and 2) Latin-ASCII (see `stri_trans_list()`)
#'
#' @param data dataframe
#' @param col string; column name to clean
#' @export
#' @examples
#' df <- data.frame(name = "Fritz Müller-Scherz 2")
#' clean_string(df, "name")
clean_string <- function(data, col) {
  data %>%
    dplyr::mutate(
      # remove all punctuations and digits
      !!col := stringr::str_replace_all(.data[[col]], "[:punct:]|[:digit:]", " "),
      # transform to ascii, keeping german Umlauts
      !!col := stringi::stri_trans_general(.data[[col]], "de-ASCII; Latin-ASCII"),
      # remove whitespace and transform to lowercase
      !!col := trimws(tolower(.data[[col]]))
    )
}


#' Split a human full name in its parts
#'
#' Rules:
#'
#' * lastname = last word
#' * givenname = first word
#' * middlename = everything not first word or last word
#' * initial = first character
#'
#' Beware that there will be a warning `argument is not an atomic vector; coercing `
#' It works nonetheless.
#'
#' In general it might **not** be a good idea to try to split a human name, because it's nearly impossible to do that correctly.
#' Mainly works only for Kartoffel names.
#'
#' @seealso \url{https://shinesolutions.com/2018/01/08/falsehoods-programmers-believe-about-names-with-examples/}
#'
#' @param data dataframe containing a column with a human name
#' @param col string; name of the column containing the name, defaults to `name`
#' @export
#' @return dataframe with 4 new colums: lastname, givenname, middlename, initial
#' @examples
#' df <- data.frame(name = c("Rita Mae Brown", "vorname df dsafasf", "sdf asdfd"))
#' split_human_name(df)
#'
split_human_name <- function(data, col = "name") {
  data %>%
    dplyr::mutate(
      !!col := trimws(.data[[col]]),
      # lastname = last word
      lastname = stringr::word(.data[[col]], start = -1),
      # givenname = first word
      givenname = stringr::word(.data[[col]], start = 1),
      # middlename = everything not first word or last word
      middlename = stringr::word(.data[[col]], start = 2, end = -2),
      middlename = ifelse(middlename == "", NA_character_, middlename),
      # initial = first character
      initial = stringr::str_sub(.data[[col]], start = 1, end = 1)
    )
}

#' Create Fingerprint of a string

#' @description
#' This function creates a fingerprint of a string. This can be used for de-duplication or calculation of string similarity or string distance. It is bases on normalised tokens and implements Open Refine's clustering algorithm, precisly the Fingerprint Key Collision
#' See \href{Open Refine Clustering Documentation}{https://github.com/OpenRefine/OpenRefine/wiki/Clustering-In-Depth}
#'
#' @param string input string
#' @param tokens how to generate tokens? `word` for whitespace-separated tokens, `ngram` for ngrams/shingles
#' @param n The number of characters in each shingle. If `token = "ngram"` a `n` must be provided
#' @return character string
#' @export
#' @examples
#' create_fingerprint("Max Spohr Verlag", token = "word")
#' create_fingerprint("Max Spohr Verlag", token = "ngram", n = 2)

create_fingerprint <- function(string, tokens = "word", n = NULL) {
  # change all characters to their lowercase representation
  string <- tolower(string)
  # remove all punctuation, whitespace, and control characters
  string <- trimws(stringr::str_remove_all(string, "[[:punct:]]|[[:cntrl:]]"))
  # normalize extended western characters to their ASCII representation (for example "gödel" → "godel")
  # deviation from Open Refine: keep German Version -> "gödel" → "goedel"
  string <- stringi::stri_trans_general(string, "de-ASCII; Latin-ASCII")

  if (tokens == "word") {
    # split the string into whitespace-separated tokens
    string <- stringr::str_split(string, "\\s")
  } else if (tokens == "ngram") {
    # split the string into ngrams/shingles
    if (is.null(n) == TRUE) {
      stop("In order to calculate ngrams, you need to provide n as function argument.")
    }
    string <- tokenizers::tokenize_character_shingles(string, n = n)
  } else {
    stop("not implemented")
  }
  # sort the tokens and remove duplicates
  string <- sort(unique(unlist(string)))

  # join the tokens back together
  string <- paste(string, collapse = "")

  return(string)
}
