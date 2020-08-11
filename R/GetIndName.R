#' CSRHUB Search for Company ISIN
#'
#' The function can be used to determine the csrhub designated industries for
#' a given company. This is used for collecting industry relative metrics.
#'
#'
#' @param alias pass a company name or multiple company names for alias match
#' @param key use the key generated with the csrhub_keygen() function
#' @keywords Industry, search
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


GetIndName <- function(alias,key){
  #create a handle object

  data <- list()
  h <- curl::new_handle() %>%
    curl::handle_setheaders("Content-Type" = "application/json") %>%
    curl::handle_setform(body = jsonlite::toJSON(iris))

  pool <- curl::new_pool(total_con = 4, host_con = 4)

  # results only available through call back function
  cb <-  function(res){
    test <- res
    names <- list()
    if (test[["status_code"]]==200 & length(jsonlite::fromJSON(rawToChar(test[["content"]])))>0){
      names[["Name"]] <- jsonlite::fromJSON(rawToChar(test[["content"]]))[["name"]]
      names[["Ind_Alias"]] <- jsonlite::fromJSON(rawToChar(test[["content"]]))[["alias"]]
      names[["url"]] <- test[["url"]]
      data <<- c(data, list(names))

    }else{
      names[["Name"]] <- NA
      names[["Ind_Alias"]] <- NA
      names[["url"]] <- test[["url"]]
      data <<- c(data, list(names))

    }
    message(paste(length(data), " of ", length(alias), " ", round(((length(data)/length(alias))*100),2), "% done","\n"))
  }

  submits <- data.frame("search_term"=alias,
                        "url"=paste0("https://www.csrhub.com/api/v2/company:",curl::curl_escape(alias),"/Industry?session_id=",key),
                        stringsAsFactors = FALSE)

  # all scheduled requests are performed concurrently

  sapply(submits$url, curl::curl_fetch_multi, done=cb, pool=pool)

  # This actually performs requests

  out <- curl::multi_run(pool = pool)
  print(out)

  ind_nam_df <- bind_rows(data)
  colnames(ind_nam_df) <- c("Name","Ind_Alias","url")
  result_df <- dplyr::left_join(submits,ind_nam_df,by="url")

  return(result_df[c('search_term','Name','Ind_Alias')])

}

