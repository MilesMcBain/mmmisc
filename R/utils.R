#' How to use a package
#'
#' Open all package vignettes in browser tabs ready for skimming.
#'
#' @param package a package to open vigettes of.
#'
#' @return Vignette data, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' howto("naniar")
#' }
howto <- function(package){
  package <- as.character(substitute(package))
  vignette_df <-
    as.data.frame(browseVignettes(package = package)[[package]])
  if(nrow(vignette_df) > 0){
    lapply(X = paste(vignette_df$Dir, vignette_df$PDF, sep = "/doc/"),
           browseURL)
  }else{
    message("[how_to] No Vignettes for package ", package, "!")
  }
  invisible(vignette_df)
}

#' Open URL for a package from it's description file.
#'
#' The URL is fetched from CRAN, so the package need not be installed to run this function.
#'
#' @param package_name the package to open URL for
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' webdocs(visdat)
#' }
#'
webdocs <- function(package_name){
  package_name <- as.character(substitute(package_name))

  html_table <-
    rvest::html_nodes(
      xml2::read_html(
        url(paste0(
          "https://cran.r-project.org/package=",
          package_name)
          )
        ), "table tr")

  URL_el <-
    grep(x = html_table, pattern = "URL", value = TRUE)

  if(nzchar(URL_el)){

    pack_URL <- haircut::regex_match(
         strsplit(URL_el, ",")[[1]],
         "htt.*(?=\">)"
      )
    lapply(pack_URL, browseURL)
  }
  else{
    message("No website for ", package_name, "!")
  }
}


#' Remove variables with names like this
#'
#' Remove things in the global environment with name like a
#' string you supply. Like is in the regex match sense, so
#' if you make a slip blowing away most of your environment
#' is a possibility.
#'
#' @param pattern a regex pattern to match. Could be a simple
#' prefix or suffix string.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' lm_1 = 1
#' lm_2 = 2
#' glm_1 = 3
#' rm_like("^lm_")
#'
#' > ls()
#' [1] "glm_1"
#' }
rm_like <- function(pattern = character()) {
  rm( list = grep(pattern = pattern,
                  x=ls(envir = globalenv()),
                  value = TRUE),
      envir = globalenv()
  )
}

#' Create a concatenation cross product of chracter vectors
#'
#' For every string in two character vectors create the full
#' outer product of pasting every string in `chars1`` before
#' every string in `chars2`. This version was created by
#' Jono Carrol.
#'
#' @param chars1 A character vector of string prefixes
#' @param chars2 A character vector of string suffixes
#'
#' @return A character vector of all prefix/suffix combos.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' }
#'
cross_paste0 <- function(chars1, chars2){
  kronecker(chars1, chars2, paste0)
}

#' Order levels in factor as per some example string
#'
#' This is the way fct_relevel should work. The warning is silly.
#'
#' @param fctr a factor with levels
#' @param order_chars a character vector with strings matching levels of fctr.
#'
#' @return a version of `fctr` with levels ordered as per `order_chars`
#' @export
order_as_per <- function(fctr, order_chars){
  suppressWarnings(forcats::fct_relevel(fctr, order_chars))
}

#
#' Paste0, Parse, Eval
#'
#' Wraps up the common task for evaluting machine generated code.
#' Be careful with pipes (%>%) as these mess with the
#' evironment chain for eval(). Choose a suitable argument for .env
#' like .GlobalEnv or a binding to sys.frame().
#'
#' @param ... text to parse and evaluate
#' @param env an environment to evaulate parsed expressions in.
#'
#' @return the result of the evaluation.
#' @export
P0PE <- function(..., .env){
  eval(parse(text = paste0(...)), envir = .env)
}

#
#' Map if predicate, otherwise take a value.
#'
#' Like map_if but with the option to specify a value for false cases.
#  E.g. NA, character(0) etc.
#'
#' @param .x a list
#' @param .p a list of logicals or predicate function to evaluate on list
#' @param .f a function
#' @param .e a value to use in results for false predicates.
#' @param ...
#'
#' @return This applied to all elements: if(.p) .f(.x) else .e
#' @export
map_ifelse <- function(.x, .p, .f, .e, ...) {
  if (purrr::is_logical(.p)) {
    stopifnot(length(.p) == length(.x))
    sel <- .p
  } else {
    sel <- purrr::map_lgl(.x, .p, ...)
  }
  .x[sel] <- purrr::map(.x[sel], .f, ...)
  .x[!sel] <- .e
  .x
}

#' Test if a file has an rmd or R extension.
#'
#' @param filename of a file. Can include path.
#'
#' @return A logical value. True if the file ends in
#' .r, .rmd or any mixed-case variant of those.
#' @export
is_R_file <- function(filename){
  regexpr(pattern =  "\\.([Rr]{1}[Mm]{1}[Dd]{1})|([Rr]{1})$",
          text = filename) > 0
}

#' Return matching characters matching an expression.
#'
#' Given a vector of chracter strings, this function returns matches as a vector.
#' This allows a chainable regex filtering style that is readily compatible with `%>%`.
#' All matches from the resulting regexec call are pasted together. This will create
#' weird behaviour if you use capturing groups. Best to use non-capturing `(?:)`.
#'
#' @param text a vector of charcter strings to be matched.
#' @param pattern a regular expression.
#'
#' @return vector of substrings of `text` that matched the `pattern`.
#' @export
#' @examples
#' \dontrun{
#' rblogger_urls %>%
#'   regex_match("(?<=\")http.*(?=\")") %>%
#'   regex_match_except("^https*://") %>%
#'   regex_match("[A-Za-z0-9-.]+(?=/|\"|$)") %>%
#'   datapasta::dpasta()
#  c("dmlc.ml", "lionel-.github.io", "jean9208.github.io", "ryouready.wordpress.com", "rveryday.wordpress.com",
#  "www.talyarkoni.org", "rtricks.wordpress.com")
#' }
regex_match <- function(text, pattern){
  text_matches <- regexec(text = text, pattern = pattern, perl = TRUE)
  match_content <- regmatches(text, text_matches)
  unlist(lapply(match_content, paste0, collapse = ""))
}

#' Return characters not matching a regular expression
#'
#' Given a vector of chracter strings, this function returns portions of the stirng
#' that do not march a pattern, as a vector. This allows a chainable regex
#' filtering style that is readily compatible with `%>%`.
#' All matches from the resulting regexec call are pasted together. This will create
#' weird behaviour if you use capturing groups. Best to use non-capturing `(?:)`.
#'
#' @param text a vector of characer strings to be (un)matched.
#' @param pattern a regular expression.
#'
#' @return vector of substrings of `text` that dit not match the `pattern`.
#' @export
regex_match_except <- function(text, pattern){
  text_matches <- regexec(text = text, pattern = pattern, perl = TRUE)
  match_content <- regmatches(text, text_matches, invert = TRUE)
  unlist(lapply(match_content, paste0, collapse = ""))
}

#' Substitute characters matching a regular expression.
#'
#' Given a vector of character strings, substitute parts of the strings
#' matching `pattern` with `replacement`. `replacement` can be a vector
#' argument, in which case it must be the same length as `text`, and
#' the substiuted `text` is made per `replacement` string.
#'
#' The default mode is to substitute all pattern matches. This can be
#' switched to first match only by setting `sub_all = FALSE`.
#'
#' @param text a vector of character strings to have patterns substituted.
#' @param pattern a string pattern that, if matched in `text` will be substituded with `replacement`.
#' @param replacement either a string or vector of strings to replace matching patterns in `text`.
#' @param sub_all Determined if all matches are substited (default), or the first ocurrence per input string.
#'
#' @return a vector of character strings
#' @export
#'
#' @examples
#' \dontrun{
#' regex_sub(c("ABC", "AXYZ", "DEFA"),"A", "_")
#' regex_sub(c("ABC_", "AXY_", "DEF_"),"_", c(1,2,3)
#' }
#'
regex_sub <- function(text, pattern, replacement, sub_all = TRUE){
  if(sub_all){
    sub_func <- gsub
  } else {
    sub_func <- sub
  }

  if(length(replacement) == length(text)){
    result <-
      mapply(
        FUN = sub_func,
        replacement = replacement,
        x = text,
        MoreArgs = list(
          pattern = pattern,
          perl = TRUE
        )
      )
  } else if(length(replacement) == 1){
    result <- sub_func(pattern,
                       replacement,
                       x = text,
                       perl = TRUE
                       )
  } else {
    stop("regex sub expected replacement argument of
         length ", length(text)," or length 1 got length",
         length(replacement),".")
  }
  result
}

#' Download a folder from Google Drive
#'
#' @param drive_folder a google drive folder path or id
#' @param dl_path a local path to dowload the folder to. Defaults to getwd().
#'
#' @return nothing.
#' @export
#'
#' @examples
#' \dontrun{
#' drive_download_dir(
#' drive_folder = as_id("https://drive.google.com/open?id=0B7688WPR38x2Nk5yYV9ZMVYwWVE"),
#' dl_path = "./models")
#' }
drive_download_dir <- function(drive_folder = NULL, dl_path = getwd() ){

  if(is.null(drive_folder)){
    stop("Expected a Google Drive folder for drive_folder, got NULL." )
  }

  if(!googledrive::is_folder(googledrive::drive_get(id = drive_folder))){
    stop("Expected a Google Drive folder for drive_folder, got an id for something else.")
  }

  drive_items <- googledrive::drive_ls(googledrive::as_id(drive_folder))

  drive_items$mimeType <-
    purrr::map_chr(drive_items$drive_resource, "mimeType")

  folders <- drive_items[drive_items$mimeType == "application/vnd.google-apps.folder",]
  files <- drive_items[drive_items$mimeType != "application/vnd.google-apps.folder",]

  #recurse into folders, updating download path
  purrr::pwalk(.l = list(drive_folder = folders$id, dl_path = file.path(dl_path, folders$name)),
        .f = drive_download_dir)

  #Make the file dl path if it doesn't exist
  if(!dir.exists(dl_path)){
    dir.create(dl_path, recursive = TRUE)
  }
  #download files, to current download path
  purrr::pwalk(.l = list( file = files$id, path = file.path(dl_path, files$name)),
        .f = ~googledrive::drive_download(file = googledrive::as_id(..1), path = ..2))
}


