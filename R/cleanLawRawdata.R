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
cleanLawRawdata <- function(law_Rawdata,...){
  UseMethod("cleanLawRawdata")
}

##(公務員懲戒委員會) ----
#' @rdname cleanLawRawdata
#' @export
cleanLawRawdata.disciplinaryCommittee <- function(law_Rawdata,...){
  tryCatch({
    gsub_nr <-
      law_Rawdata$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .)
    #法院
    area <- strsplit(gsub_nr,"　　　　　　　　　　　　　")[[1]][[1]]
    # 擷取相關人員 ----
    index1 <- stringr::str_locate(gsub_nr, pattern = "主席委員")
    peoplelist <-stringr::str_sub(gsub_nr, start = index1[1])

    #彙整相關人員
    peoplelist_index <- stringr::str_locate_all(peoplelist, pattern = "委員")
    endpoint_index <- stringr::str_locate(peoplelist, pattern = "右正本證明")

    #擷取委員長及各委員
    committeeMember= c()
    for (i in 1:nrow(peoplelist_index[[1]])) { #i=1
      start_index = peoplelist_index[[1]][i,][2]+1
      if(i == nrow(peoplelist_index[[1]])){end_index <- endpoint_index[1,][1]-1}else{end_index = peoplelist_index[[1]][i+1,][1]-1}
      committeeMember[i] <- str_sub(peoplelist, start = start_index, end = end_index)
    }
    #擷取書記官
    clerk_index <- str_locate(gsub_nr, pattern = "書記官")
    #人員
    Clerk <- str_sub(gsub_nr, start = clerk_index[2]+1)
    mainjudge <- paste0(jsonlite::toJSON(list("委員長"=str_sub(committeeMember[1], start =2)),auto_unbox = T))
    secondaryjudge <- paste0(jsonlite::toJSON(list("委員"=committeeMember[-1]),auto_unbox = T))

    #擷取結論
    conclusion_index <- str_locate(gsub_nr, pattern = "據上論結")
    conclusion <-
      str_sub(
        gsub_nr,
        start = str_locate(gsub_nr, pattern = "據上論結")[2] + 2 ,
        end = str_locate(gsub_nr, pattern = "如主文。")[2]
      )
    tmplist <- list(
      law_Rawdata[1,]$judgmentid,
      law_Rawdata[1,]$jdate,
      mainjudge,
      secondaryjudge,
      Clerk,
      conclusion,
      area
    )
    utility$sqlDbSendStatement("INSERT INTO `legal`.`legalinfo` (`judgmentid`, `datetime`, `mainjudge`, `secondaryjudge`, `clerk`, `conclusion`, `class`) VALUES (?, ?, ?, ?, ?, ?, ?);",tmplist)
    return("新增成功")
  },error = function(msg) {
    message("utility.rawdata Original error message:")
    message(paste0(msg,"\n"))
    return(NA)
  })
}
