#' cleanLawRawdata
#'
#' A function to clean law raw data into lawinfo
#' @param x law data
#' @keyword lawinfo
#' @importFrom stringr dplyr
#' @export cleanLawRawdata
#' @examples
#' mean(1:3)
#' \dontrun{ mean(1:1e99) }

#' @rdname cleanLawRawdata
#' @export
cleanLawRawdata <- function(...){
  UseMethod("cleanLawRawdata")
}

#' @rdname cleanLawRawdata
#' @export
cleanLawRawdata.disciplinaryCommittee <- function(...){
  tryCatch({
    # 擷取相關人員 ----
    #(公務員懲戒委員會) ----
    #str_locate(char.vec, pattern = "ti") 找特定字串在資料中的位置(第一個)
    #str_locate_all(char.vec, pattern = "ti") 找特定字串在資料中的全部位置
    index1 <- str_locate(gsub_nr, pattern = "主席委員")

    peoplelist <-str_sub(gsub_nr, start = index1[1])

    peoplelist_index <- str_locate_all(peoplelist, pattern = "委員")

    endpoint_index <- str_locate(peoplelist, pattern = "右正本證明")
    #擷取委員長及各委員
    committeeMember= c()
    for (i in 1:nrow(peoplelist_index[[1]])) { #i=1
      start_index = peoplelist_index[[1]][i,][2]+1
      if(i == nrow(peoplelist_index[[1]])){end_index <- endpoint_index[1,][1]-1}else{end_index = peoplelist_index[[1]][i+1,][1]-1}
      committeeMember[i] <- str_sub(peoplelist, start = start_index, end = end_index)
    }
    #擷取書記官
    clerk_index <- str_locate(gsub_nr, pattern = "書記官")
    Clerk <- str_sub(gsub_nr, start = clerk_index[2]+1)
    #彙整相關人員

    #擷取結論
    conclusion_index <- str_locate(gsub_nr, pattern = "據上論結")
    conclusion <-
      str_sub(
        gsub_nr,
        start = str_locate(gsub_nr, pattern = "據上論結")[2] + 2 ,
        end = str_locate(gsub_nr, pattern = "如主文。")[2]
      )
    return(conclusion)
  },error = function(msg) {
    message("utility.rawdata Original error message:")
    message(paste0(msg,"\n"))
    return(NA)
  })
}
