#' Sort_data
#'
#' @param none
#' @return
devtools::use_package("dplyr")
devtools::use_package("stringr")
devtools::use_package("readxl")
devtools::use_package("csvread")
devtools::use_package("compiler")
devtools::use_package("tidyr")

#' @importFrom stringr str_replace_all
#' @importFrom readxl read_xlsx
#' @importFrom csvread map.coltypes
#' @importFrom csvread csvread
#' @importFrom compiler cmpfun
#' @importFrom tidyr spread
#' @export

sort_data=function(){
  A=cmpfun(
    function(){
      setwd("C:/Program Files/R")

      table_tf=readline(prompt="workspace에 table이 있습니까?(1.yes 2.no):")
      if(table_tf==2){
        file<-list.files()
        print(file)
        file_no=readline(prompt="몇번째 파일입니까?:")
        file_no=as.numeric(file_no)
        print(file[file_no])
        excel_tf=readline(prompt="1.엑셀파일 2.csv파일:")
        if(excel_tf==1){
          sheet_no=readline(prompt="sheet number:")
          sheet_no=as.numeric(sheet_no)
          test=as.data.frame(read_xlsx(file[file_no],sheet=sheet_no))
          # test1=as.data.frame(test)
          test<<-test1
        }else if(excel_tf==2){
          coltypes=map.coltypes(file[file_no],header=T)

          test=csvread(file[file_no],coltypes=coltypes,header=T)
          test<<-test
        }
      }

      print(names(test))
      start=readline(prompt="시점 column(ex>1):")
      start<<-as.numeric(start)
      last=readline(prompt="종점 column(ex>2):")
      last<<-as.numeric(last)
      print("1. 복수응답가능 ','로 구분")
      print("2. pivot기능 사용시 pivot 기준열 가장 먼저 기입")
      content=readline(prompt="content column :")
      try(test[,as.numeric(start)]<<-as.numeric(as.character(test[,as.numeric(start)])))
      try(test[,as.numeric(last)]<<-as.numeric(as.character(test[,as.numeric(last)])))
      eval(parse(text=paste0("test=test[,c(",start,",",last,",",content,")]")))
      test<<-test
      sub=readline(prompt="단위기준:")
      sub<-as.numeric(sub)
      spread=readline(prompt="pivot 적용여부(1.적용 2.생략):")
      spread=as.numeric(spread)

      col_count=length(con)

      i=1;for(i in 1:length(test[,1])){

        len=((test[i,2]-test[i,1])/sub)+1

        if(i!=1) sort1=sort

        evalText=paste0("cbind(data.frame('range'=test[i,1]+sub*((1:len)-1)),")

        count=1;for(count in 1:col_count){
          evalText1=evalText
          if(count!=col_count){
            evalText=paste0(evalText1,
                            "data.frame(content",count,"=test[i,",count+2,"]),"
            )
          }else if(count==col_count){
            evalText=paste0(evalText1,
                            "data.frame(content",count,"=test[i,",count+2,"]))"
            )
          }#else if
        }#for(count)

        assign("sort",eval(parse(text=evalText)))

        if(i!=1) sort=rbind(sort1,sort)

        if(spread==1&i==length(test[,1])) sort=spread(sort,content1,content2)

        sort<<-sort
        print(
          paste0(
            i,"/",length(test[,1])
          )
        )

      }#for(i)
      View(sort)
      save=readline(prompt="저장하시겠습니까?(1.yes 2.no)")
      save=as.numeric(save)
      time=Sys.time();
      time=str_replace_all(time,"KST","")
      time=str_replace(time,":","시")
      time=str_replace(time,":","분")
      time=paste0(time,"초")

      if(save==1){
        write.csv(sort, paste0("data_sort_",time,".csv"),row.names=FALSE)
      }
    }#function
  )#cmpfun
  A()
}
