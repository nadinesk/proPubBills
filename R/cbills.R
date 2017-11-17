
#' Gets the bills depending on U.S.Congress, branch, and offsets
#' @param key is the api key
#' @param congress it the number of the U.S. Congress: for example, 114 for the 114th U.S. Congress
#' @param branch is the House or the Senate branch
#' @param type is the type of bills; options are introduced, updated, active, passed, enacted, vetoed
#' @param numFrom is the offset number beginning range: for example, 1 will get the first 20 bills
#' @param numTo is the offset number to: for example, 40 will get bills 40 to 60. Entering numFrom as 1 and numTo as 40 will return bills 1 to 60
#' @return returns a dataframe with all bills and info from the API within the arguments' parameters
#' @examples
#' \dontrun{
#' congress_115_enacted <- getBills(api_key,"115","house","enacted",1,500)
#' congress_114_introduced <- getBills(api_key,"114","senate","introduced",1,100)
#' }
#' @author Nadine Fischoff, \email{nadine.khattak@@gmail.com}
#' @export 
#' @references \url{https://projects.propublica.org/api-docs/congress-api/}
#' @keywords propublica, api, congress, bills
#' @import httr dplyr stringr



getBills <- function(key,congress,branch,type,numFrom, numTo) {

  get_url <- paste('https://api.propublica.org/congress/v1/',congress, '/',branch,'/bills/',type,'.json', sep="" )

  listofdfs <- list()
  x=c()

  first_20 <-  GET(get_url,
                   add_headers(`X-API-Key` = key))

  ft_pr <- content(first_20, 'parsed')
  ft_res1 <- ft_pr$results
  ft_res2 <- ft_res1[[1]]
  ft_res3 <- ft_res2$bills

  ft_res4 <- data.frame(do.call(rbind, ft_res3), stringsAsFactors=FALSE)

  for(i in numFrom:numTo) {

    if ((i %% 20) ==0) {

      url <- paste(get_url,'?offset=', i,sep="")
      r <- GET(url,
               add_headers(`X-API-Key` = key))
      pr <- content(r, 'parsed')
      t1 <- pr$results
      t2 <- t1[[1]]
      t3 <- t2$bills

      listofdfs[[i]] <- t3

      df_name <- paste("df", i, sep="_")
      assign(df_name,listofdfs[[i]])

      df1_name <- paste("df1", i, sep="_")

      assign(df1_name, data.frame(do.call(rbind, listofdfs[[i]]), stringsAsFactors=FALSE))

      x=append(x,df1_name)
    }
  }
  x1 <- do.call(rbind, mget(x))

  x2 <- rbind(ft_res4,x1)
  x3 <- lapply(x2, function(x) ifelse(x=="NULL", NA, x))

  x4 <- lapply(x3, function(x) as.character((unlist(x))))

  x5 <- as.data.frame(do.call(cbind, x4))

  return(x5)
}

