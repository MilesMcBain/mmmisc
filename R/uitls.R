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
#' how_to("naniar")
#' }
how_to <- function(package){
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


