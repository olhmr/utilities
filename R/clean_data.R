#' Clean Data Frame Names
#'
#' Applies some general formatting and replacements to enforce a naming
#' convention that is clear and easy to read, while also ensuring it's valid in
#' the context of data frame column names.
#'
#' Essentially functions as a wrapper for make.names, making some useful
#' substitutions before passing to make.names for final validation. Inputs are
#' given as either a data frame or a character vector, and the return type will
#' correspond to the input.
#'
#' @param x data frame or character vector
#' @return Cleaned data frame or character vector
#' @export
clean_names <- function(x) {
  # Check input type
  if (is.data.frame(x)) {
    namx <- names(x) # Get names from data frame
  } else {
    namx <- x # Names supplied as vector
  }

  # Clean names vector
  # Spaces to underscores
  namx <- gsub(pattern = " ", replacement = "_", x = namx)
  # Dashes to underscores
  namx <- gsub(pattern = "-", replacement = "_", x = namx)
  # Pluses to underscores
  namx <- gsub(pattern = "+", replacement = "_", x = namx, fixed = TRUE)
  # Ampersands to _and_
  namx <- gsub(pattern = "&", replacement = "_and_", x = namx)
  # Percent symbols to "percent", adding underscore if missing
  namx <- gsub(pattern = "(?<!_)%", replacement = "_percent", x = namx,
               perl = TRUE) # Negative lookbehind
  namx <- gsub(pattern = "%", replacement = "percent", x = namx)
  # Remove multiple underscores, sometimes arising from & replacement
  namx <- gsub(pattern = "_+", replacement = "_", x = namx)
  # Remove trailing underscores
  namx <- gsub(pattern = "_$", replacement = "", x = namx)

  # Run through make.names to catch anything else
  namx <- make.names(namx)

  # Change to lowercase
  namx <- tolower(namx)

  # Return based on input type
  if (is.data.frame(x)) {
    names(x) <- namx
    return(x)
  } else {
    return(namx)
  }

}
