#' @import dplyr
cleanLawRawdata<- list(
    ##(公務員懲戒委員會) ----
    disciplinaryCommittee = function(law_Rawdata,...){
      tryCatch({
        gsub_nr <-
          law_Rawdata$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .)
        #類別
        class <- strsplit(gsub_nr,"　")[[1]][[1]]
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
        if(nchar(Clerk)>6){ Clerk <- "未記載"}
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
          class
        )
        utility$sqlDbSendStatement("INSERT INTO `legal`.`legalinfo` (`judgmentid`, `datetime`, `mainjudge`, `secondaryjudge`, `clerk`, `conclusion`, `class`) VALUES (?, ?, ?, ?, ?, ?, ?);",tmplist)
        return(paste0("id = ",tmplist[1]," 新增成功"))
      },error = function(msg) {
        message("utility.rawdata Original error message:")
        return(paste0("id = ",tmplist[1]," 新增失敗","\n",msg))
      })

    },
    ##(司法院冤獄賠償覆議委員會決定書) ----

    injusticeCompensation=function(law_Rawdata){
      tryCatch({
        gsub_nr <-
          law_Rawdata$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .)

        #類別
        class <- strsplit(gsub_nr,"　")[[1]][[1]]

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
        if(nchar(Clerk)>6){ Clerk <- "未記載"}
        mainjudge <- paste0(jsonlite::toJSON(list("委員長"=str_sub(committeeMember[1], start =1)),auto_unbox = T))
        secondaryjudge <- paste0(jsonlite::toJSON(list("委員"=committeeMember[-1]),auto_unbox = T))

        #擷取結論
        conclusion_index <- str_locate(gsub_nr, pattern = "據上論結")
        if(is.na(conclusion_index[,"start"]) && is.na(conclusion_index[,"end"])){
          conclusion_index <- str_locate(gsub_nr, pattern = "依前開說明")
        }
        conclusion <-
          str_sub(
            gsub_nr,
            start = conclusion_index[,"end"]+2 ,
            end = str_locate(gsub_nr, pattern = "如主文。")[2]
          )
        tmplist <- list(
          law_Rawdata[1,]$judgmentid,
          law_Rawdata[1,]$jdate,
          mainjudge,
          secondaryjudge,
          Clerk,
          conclusion,
          class
        )
        utility$sqlDbSendStatement("INSERT INTO `legal`.`legalinfo` (`judgmentid`, `datetime`, `mainjudge`, `secondaryjudge`, `clerk`, `conclusion`, `class`) VALUES (?, ?, ?, ?, ?, ?, ?);",tmplist)
        return(paste0("id = ",tmplist[1]," 新增成功"))
        },error = function(msg) {
          message("utility.rawdata Original error message:")
          message(paste0(msg,"\n"))
          return(paste0("id = ",tmplist[1]," 新增失敗","\n",msg))
        })
      },
    ##(最高法院民事裁定) ----
    supremeCourtCivil=function(law_Rawdata){
      tryCatch({
        gsub_nr <-
          law_Rawdata$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .)

        #類別
        class <- strsplit(gsub_nr,"　")[[1]][[1]]

        # 擷取相關人員 ----
        index1 <- stringr::str_locate(gsub_nr, pattern = "審判長法官")
        peoplelist <-stringr::str_sub(gsub_nr, start = index1[1])

        #彙整相關人員
        peoplelist_index <- stringr::str_locate_all(peoplelist, pattern = "法官")
        endpoint_index <- stringr::str_locate_all(peoplelist, pattern = "右正本證明")

        #擷取委員長及各委員
        committeeMember= c()
        for (i in 1:nrow(peoplelist_index[[1]])) { #i=5
          start_index = peoplelist_index[[1]][i,][2]+1
          if(i == nrow(peoplelist_index[[1]]))
            {
            end_index <- endpoint_index[[1]][length(endpoint_index[[1]][,1]),][1]-1
            if(nchar(str_sub(peoplelist, start = start_index, end = end_index))<5){
            committeeMember[i] <- str_sub(peoplelist, start = start_index, end = end_index)
            }
            }else{
            end_index = peoplelist_index[[1]][i+1,][1]-1}
          if(nchar(str_sub(peoplelist, start = start_index, end = end_index))<5){
            committeeMember[i] <- str_sub(peoplelist, start = start_index, end = end_index)
          }
        }
        mainjudge=committeeMember[1]
        committeeMember=committeeMember[!committeeMember%in%committeeMember[1]]
        committeeMember=names(table(committeeMember[!is.na(committeeMember)]))
        #擷取書記官
        clerk_index <- str_locate(gsub_nr, pattern = "書記官")

        #人員
        Clerk <- str_sub(gsub_nr, start = clerk_index[2]+1)
        if(nchar(Clerk)>6){ Clerk <- "未記載"}
        mainjudge <- paste0(jsonlite::toJSON(list("審判長法官"=str_sub(mainjudge, start =1)),auto_unbox = T))
        secondaryjudge <- paste0(jsonlite::toJSON(list("法官"=committeeMember[-1]),auto_unbox = T))

        #擷取結論
        conclusion_index <- str_locate(gsub_nr, pattern = "據上論結")
        if(is.na(conclusion_index[,"start"]) && is.na(conclusion_index[,"end"])){
          conclusion_index <- str_locate(gsub_nr, pattern = "依上說明")
        }
        conclusion <-
          str_sub(
            gsub_nr,
            start = conclusion_index[,"end"] + 2 ,
            end = str_locate(gsub_nr, pattern = "如主文。")[2]
          )
        tmplist <- list(
          law_Rawdata[1,]$judgmentid,
          law_Rawdata[1,]$jdate,
          mainjudge,
          secondaryjudge,
          Clerk,
          conclusion,
          class
        )
        utility$sqlDbSendStatement("INSERT INTO `legal`.`legalinfo` (`judgmentid`, `datetime`, `mainjudge`, `secondaryjudge`, `clerk`, `conclusion`, `class`) VALUES (?, ?, ?, ?, ?, ?, ?);",tmplist)
        return(paste0("id = ",tmplist[1]," 新增成功"))
      },error = function(msg) {
        message("utility.rawdata Original error message:")
        message(paste0(msg,"\n"))
        return(paste0("id = ",tmplist[1]," 新增失敗","\n",msg))
      })
    },
    ##(最高法院刑事判決) ----
    supremeCourtCriminalJudgment=function(law_Rawdata){
      tryCatch({
        gsub_nr <-
          law_Rawdata$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .)

        #類別
        class <- strsplit(gsub_nr,"　")[[1]][[1]]

        # 擷取相關人員 ----
        index1 <- stringr::str_locate(gsub_nr, pattern = "審判長法官")
        peoplelist <-stringr::str_sub(gsub_nr, start = index1[1])

        #彙整相關人員
        peoplelist_index <- stringr::str_locate_all(peoplelist, pattern = "法官")
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
        if(nchar(Clerk)>6){ Clerk <- "未記載"}
        mainjudge <- paste0(jsonlite::toJSON(list("審判長法官"=str_sub(committeeMember[1], start =1)),auto_unbox = T))
        secondaryjudge <- paste0(jsonlite::toJSON(list("法官"=committeeMember[-1]),auto_unbox = T))

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
          class
        )
        utility$sqlDbSendStatement("INSERT INTO `legal`.`legalinfo` (`judgmentid`, `datetime`, `mainjudge`, `secondaryjudge`, `clerk`, `conclusion`, `class`) VALUES (?, ?, ?, ?, ?, ?, ?);",tmplist)
        return(paste0("id = ",tmplist[1]," 新增成功"))
      },error = function(msg) {
        message("utility.rawdata Original error message:")
        message(paste0(msg,"\n"))
        return(paste0("id = ",tmplist[1]," 新增失敗","\n",msg))
      })
    }

)
