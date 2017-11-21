#' sort_data
#'
#' @param none
#' @return
devtools::use_package("dplyr")
devtools::use_package("stringr")
devtools::use_package("readxl")
devtools::use_package("csvread")
devtools::use_package("compiler")

#' @importFrom dplyr spread
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom stringr str_replace_all
#' @importFrom readxl read_xlsx
#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom compiler cmpfun
#' @export
sort_data=function ()
{
  A = cmpfun(function() {
    setwd("C:/Program Files/R")
    table_tf = readline(prompt = "Is there the table you need at workspace?(1.yes 2.no):")
    if (table_tf == 2) {
      file <- list.files()
      print(file)
      file_no = readline(prompt = "order number of file:")
      file_no = as.numeric(file_no)
      print(file[file_no])
      excel_tf = readline(prompt = "1.excel file 2.csv file:")
      if (excel_tf == 1) {
        sheet_no = readline(prompt = "sheet number:")
        sheet_no = as.numeric(sheet_no)
        test = as.data.frame(read_xlsx(file[file_no],
                                       sheet = sheet_no))
        test <- test
      }
      else if (excel_tf == 2) {
        coltypes = map.coltypes(file[file_no], header = T)
        test = csvread(file[file_no], coltypes = coltypes,
                       header = T)
        test <- test
      }
    }
    print(names(test))
    start = readline(prompt = "start column(ex>1):")
    start <- as.numeric(start)
    last = readline(prompt = "last column(ex>2):")
    last <- as.numeric(last)
    # print("1. 蹂듭닔\\xec쓳\\xeb떟媛\\u0080\\xeb뒫 ','濡\\x9c 援щ텇")
    # print("2. pivot湲곕뒫 \\xec궗\\xec슜\\xec떆 pivot 湲곗\\xa4\\u0080\\xec뿴 媛\\u0080\\xec옣 癒쇱\\xa0\\u0080 湲곗엯")
    content = readline(prompt = "content column(if you want columns more than one,seperate by ',') :")
    con = str_count(content, ",")
    try(test[, as.numeric(start)] <- as.numeric(as.character(test[,
                                                                  as.numeric(start)])))
    try(test[, as.numeric(last)] <- as.numeric(as.character(test[,
                                                                 as.numeric(last)])))
    eval(parse(text = paste0("test=test[,c(", start, ",",
                             last, ",", content, ")]")))
    test <- test
    sub = readline(prompt = "A standard unit of distance?(ex>0.01,1000s):")
    sub <- as.numeric(sub)
    spread = readline(prompt = "if you want pivot table?(1.yes 2.no):")
    spread = as.numeric(spread)
    col_count = con + 1
    i = 1
    for (i in 1:length(test[, 1])) {
      len = ((test[i, 2] - test[i, 1])/sub) + 1
      if (i != 1) {
        sort1 = sort
      }
      evalText = paste0("cbind(data.frame('range'=test[i,1]+sub*((1:len)-1)),")
      count = 1
      for (count in 1:col_count) {
        evalText1 = evalText
        if (count != col_count) {
          evalText = paste0(evalText1, "data.frame(content",
                            count, "=test[i,", count + 2, "]),")
        }
        else if (count == col_count) {
          evalText = paste0(evalText1, "data.frame(content",
                            count, "=test[i,", count + 2, "]))")
        }
      }
      assign("sort", eval(parse(text = evalText)))
      if (i != 1) {
        sort = rbind(sort1, sort)
      }
      if (spread == 1 & i == length(test[, 1])) {
        print(sort)
        # names(sort) = c("dist", "content1", "content2")
        sort = sort %>% group_by(content1, range) %>%
          mutate(ind = row_number()) %>% spread(content1,
                                                content2)
      }
      sort=sort[,names(sort)!='ind']
      sort <- sort
      print(paste0(i, "/", length(test[, 1])))
    }
    View(sort)
    save = readline(prompt = "Do you want save the table to csv file?(1.yes 2.no)")
    save = as.numeric(save)
    time = Sys.time()
    time = str_replace_all(time, "KST", "")
    time = str_replace(time, ":", "h")
    time = str_replace(time, ":", "m")
    time = paste0(time, "s")
    if (save == 1) {
      write.csv(sort, paste0("data_sort_", time, ".csv"),
                row.names = FALSE)
    }
  })
  A()
}
