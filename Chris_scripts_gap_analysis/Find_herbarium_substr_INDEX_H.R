  
  INDEX_QUERY_NAME<-function(csv_file,data_dir){
    require(RCurl);require(Hmisc)
    
  query<-read.csv(csv_file,header=T)
  code=query$institute_id
  query$IHQ<-NA
  code[which(code=="")]<-NA
  # i=2477
  for(i in 1:length(code)){
    if(is.na(code[[i]])){
      cat(code[[i]]," is empty","\n")
      c2<-"NO_DATA"
      } else {
    cat("proccesing ", as.character(code[[i]]),"\n")
    cat("proccesing ", i, "of ",length(code)," ",round((i/length(code)*100),2)," %","\n")
  
    a<-readLines(paste0("http://sweetgum.nybg.org/science/ih/herbarium_list.php?col_NamOrganisationAcronym=",toupper(code[[i]])))
  mypattern ="./herbarium_details.php?"
  datalines = grep(mypattern,a,value=TRUE)
  
  if(length(datalines)!=0){
  c2<-unlist(strsplit(datalines,paste0(">",toupper(code[[i]]),"</a></td><td>")))
  c2<-c2[[2]]
  c2<-unlist(strsplit(c2,"</td><td>"))
  c2<-c2[[1]]
      }else{
  c2<-"NO_DATA"
    }
  }
    query[i,"IHQ"]<-c2
  }
results<-cbind(query$ID_DUMMY,query$institute_id,query$IHQ)
colnames(results)<-c("ID_DUMMY","institute_id","IHQ")

  write.table(results,paste0(data_dir,"/","IHQ_QUERY2.txt"),row.names = F,quote = F,sep="\t",fileEncoding="UTF-8")
  
  }
  
  csv_file<-"E:/Dropbox/Dropbox/WIEW/CSV_TAB_23.csv"
  data_dir<-"E:/Dropbox/Dropbox/WIEW"
  
  
  x<-INDEX_QUERY_NAME(csv_file,data_dir)
