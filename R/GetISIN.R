#' CSRHUB Search for Company ISIN
#'
#' The function will need to use the csr hub api alias name retrieved by the GetAlias() function.
#'
#'
#' @param alias pass a company name or multiple company names for alias match
#' @param key use the key generated with the csrhub_keygen() function
#' @keywords ISIN, search
#' @export
#' @import dplyr
#' @examples company_names <- c("Apple_Inc","Google-Inc")
#' results <- GetISIN(company=company_names,key=key)
#' print(results)
#' search_term    CSRNAME
#' 1       apple  Apple-Inc
#' 2    alphabet Google-Inc
#'
#' results <- csRhub::GetAlias('US0378331005',key = key)
#' search_term   CSRNAME
#' US0378331005 Apple-Inc
#'
#' results <- csRhub::GetAlias('AAPL',key = key)
#' search_term   CSRNAME
#' AAPL Apple-Inc

#' @name GetISIN


GetISIN <- function(alias,key){
  #create a handle object


  data <- list()
  h <- curl::new_handle() %>%
    curl::handle_setheaders("Content-Type" = "application/json") %>%
    curl::handle_setform(body = jsonlite::toJSON(iris))

  pool <- curl::new_pool(total_con = 4, host_con = 4)
  # results only available through call back function
  isin_fun <-  function(response){
    names <- list()
    if (response[["status_code"]]==200 &
        !is.null(jsonlite::fromJSON(rawToChar(response[["content"]]))[["ISIN"]])){
      names[["ISIN"]] <- jsonlite::fromJSON(rawToChar(response[["content"]]))[["ISIN"]][1]
      names[["url"]] <- response[["url"]]
      data <<- c(data, list(names))

    } else{
      names[["ISIN"]] <- NA
      names[["url"]] <- response[["url"]]
      data <<- c(data, list(names))
    }
    cat(length(data), " of ", length(alias), " ", round(((length(data)/length(alias))*100),2), "% done","\n")
  }

  submits <- data.frame("search_term"=alias,
                        "url"=paste0("https://www.csrhub.com/api/v2/company:",curl::curl_escape(alias),"/ISIN?session_id=",key),
                        stringsAsFactors = FALSE)
  # all scheduled requests are performed concurrently
  sapply(submits$url, curl::curl_fetch_multi, done=isin_fun, pool=pool)

  # This actually performs requests
  out <- curl::multi_run(pool = pool)
  print(out)

  isin_df <- data.frame(matrix(unlist(data), nrow=length(data), byrow=T),stringsAsFactors=FALSE)
  colnames(isin_df) <- c("ISIN","url")
  result_df <- dplyr::left_join(submits,isin_df,by="url")

  return(result_df[,c('search_term',"ISIN")])
}
