utility <- list(
  dbGetconncet = function(){
    tryCatch({
      config <- config::get(file = "config.yml")
      con <-DBI::dbConnect(RMariaDB::MariaDB(),dbname=config$dbname,
                           host=config$host,
                           user = config$user,
                           password = config$password)

    },# 遇到 error 時的自訂處理函數
    error = function(msg) {
      message("utility.rawdata Original error message:")
      message(paste0(msg,"\n"))
      return(NA)
    })
    return(con)
  },
  dbQuery = function(query){
    tryCatch({
      con <- utility$dbGetconncet()
      result <- DBI::dbGetQuery(con, query)
      DBI::dbDisconnect(con)
    },# 遇到 error 時的自訂處理函數
    error = function(msg) {
      DBI::dbDisconnect(con)
      message("utility.dbQuery error message:")
      message(paste0(msg,"\n"))
      return(NA)
    })
    return(result)
  }
)
