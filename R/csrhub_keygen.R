#' CSRHub Generate Session ID
#'
#' This function creates a session ID to use the API. It will expire within 24 hours.
#' @param email csrhub email associated with your account
#' @param password csrhub email for associated account
#' @keywords session, id, key
#' @export
#' @examples
#' @name csrhub_keygen

library(curl)
library(jsonlite)
library(magrittr)

csrhub_keygen <- function(email,password) {
  if(is.null(email)){
    message("Missing Login Email")
  }
  else if(is.null(password)) {
    message("Missing Password")
  }
      else{
        key <- POST("http://www.csrhub.com/api/v2/session/",
                    body=paste0("name=",email,"&password=",password))

            if (is.null(content(key, 'parsed')$session_id)){
                message(content(key, 'parsed')$error)
              } else{
                key <- content(key, 'parsed')$session_id
                return(key)
                }
      }
}
