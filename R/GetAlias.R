#' CARHUB Search for Company API Alias
#'
#' The function will recieve the csrhub api alias and return a data frame with searched term
#' for the company you search for. Currently only written to work with names but other
#' searches are possible by ISIN or Ticker.
#' @param company pass a company name or multiple company names for alias match
#' @param key use the key generated with the csrhub_keygen() function
#' @keywords alias, search
#' @export
#' @import dplyr
#' @examples company_names <- c("apple","alphabet")
#' results <- GetAlias(company=company_names,key=key)
#'print(results)
#'search_term    CSRNAME
#'1       apple  Apple-Inc
#'2    alphabet Google-Inc
#'
#' @name GetAlias

GetAlias <- function(company,key){

  #create a handle object

  data <- list()
  h <- curl::new_handle() %>%
    curl::handle_setheaders("Content-Type" = "application/json") %>%
    curl::handle_setform(body = jsonlite::toJSON(iris))

  pool <- curl::new_pool(total_con = 4, host_con = 4)
  # results only available through call back function
  nam_fun <-  function(response){
    names <- list()
    if (response[["status_code"]]==200 & length(jsonlite::fromJSON(rawToChar(response[["content"]])))>0){
      names[["Name"]] <- jsonlite::fromJSON(rawToChar(response[["content"]]))[["alias"]][1]
      names[["url"]] <- response[["url"]]
      data <<- c(data, list(names))

    } else{
      names[["Name"]] <- NA
      names[["url"]] <- response[["url"]]
      data <<- c(data, list(names))
    }
    cat(length(data), " of ", length(company), " ", round(((length(data)/length(company))*100),2), "% done","\n")
  }

  submits <- data.frame("search_term"=company,
                        "url"=paste0("https://www.csrhub.com/api/v2/lookup:",curl::curl_escape(company),"/company?session_id=",key),
                        stringsAsFactors = FALSE)
  # all scheduled requests are performed concurrently
  sapply(submits$url, curl::curl_fetch_multi, done=nam_fun, pool=pool)

  # This actually performs requests
  out <- curl::multi_run(pool = pool)
  print(out)


  alias_df <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T),stringsAsFactors=FALSE)
  colnames(alias_df) <- c("CSRNAME","url")
  result_df <- dplyr::left_join(submits,alias_df,by="url")

  return(result_df[,c("search_term","CSRNAME")])

}
