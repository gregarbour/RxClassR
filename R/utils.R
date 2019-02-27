# Generic parser function for various response types.
parse_results <- function(result, result_format = NULL) {
  if(status_code(result) != 200){
    NULL
  } else {
    resContent <- ifelse(is.null(result_format),
                         content(result),
                         content(result, result_format))
    resContent
  }
}

#form_query <- function()
