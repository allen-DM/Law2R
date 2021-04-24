#### 原始資料整理
#' @export
controllerCleanRawLaw <- function(tmpText){
  findClass=function (tmp){
    gsub_nr <-tmp$jfull %>%  gsub("[\n]", "", .) %>%  gsub("[\r]", "", .) %>%  gsub(" ", "", .) %>% strsplit(.,"　")
    gsub_nr <- gsub_nr[[1]][[1]]
    return(gsub_nr)
  }
  tryCatch({
    if(findClass(tmpText)=="公務員懲戒委員會議決書"){
      result=cleanLawRawdata$disciplinaryCommittee(tmpText)
      return(paste0("公務員懲戒委員會議決書 add new data: ",result))

    }else if(findClass(tmpText)=="司法院冤獄賠償覆議委員會決定書"){
      result=cleanLawRawdata$injusticeCompensation(tmpText)
      return(paste0("司法院冤獄賠償覆議委員會決定書 add new data: ",result))

    }else if(findClass(tmpText)=="最高法院民事裁定" || findClass(tmpText)=="最高法院民事判決"){
      result=cleanLawRawdata$supremeCourtCivil(tmpText)
      return(paste0("最高法院民事裁定 add new data: ",result))
    }else if(findClass(tmpText)=="最高法院刑事判決" || findClass(tmpText)=="最高法院刑事裁定" || findClass(tmpText)=="最高法院刑事附帶民事訴訟判決"){
      result=cleanLawRawdata$supremeCourtCivil(tmpText)
      return(paste0("最高法院刑事判決 add new data: ",result))

    }else{
      return(paste0(tmpText$judgmentid,"cant be defind"))
    }
  },# 遇到 error 時的自訂處理函數
  error = function(msg) {
    message("utility.rawdata Original error message:")
    message(paste0(msg,"\n"))
    return(paste0("controllerCleanRawLaw add new data fail: ",msg))
  })
}
