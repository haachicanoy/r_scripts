



GRIN_NAREA_GET<-function(spname,code_path,out_dir){
require(RCurl);require(httr);library(XML);require(rvest);require(stringi);require(htmltools)
 
  
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }  
  
cat("READING CSV FILE WITH GRIN CODES ","\n")
  
CODES<-read.csv(paste0(code_path,"/","CODES.csv"),sep="|",header=T)

sp=data.frame(spname);colnames(sp)<-"Taxon"
sp$Taxon<-sub("_"," ",sp$Taxon)

sp2<-merge(sp,CODES,by="Taxon")

if(nrow(sp2)>0){
 
url<-as.character(sp2$Webpage)


cat("CONNECTING TO GRIN DATABASE","\n")

con <- curl::curl(url)
open(con)
out <- readLines(con,skipNul=T)

close(con)
 #html<-htmlTreeParse("https://npgsweb.ars-grin.gov/gringlobal/taxonomydetail.aspx?id=460141",useInternalNodes=T)

#pattern1 = "<h1><a href='taxon/abouttaxonomy.aspx?chapter=distrib' target='_blank'>Distributional Range:</a></h1>"
 pattern1="        <h1>Native:</h1>"
 #pattern2="        <h1><a href='taxon/abouttaxonomy.aspx?chapter=liter' target='_blank'>References:</a></h1>"
 pattern2= "<div id=\"ctl00_cphBody_pnlReferences\">"
 #pattern2="            </ul></li>"
 
 
 f1<-which(out==pattern1,arr.ind=TRUE)
 
 if(is.integer0(f1)){
   cat("OMMITING (NO NATIVE AREA INFORMATION FOR) ",as.character(sp),"\n")
 }else{
   
f2<-which(out==pattern2,arr.ind=TRUE)

out2<-out[f1:f2]
out2<-sub("                    <b>","",out2);out2<-sub("</b>;","",out2);out2<-sub("</b>","",out2)

pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
plain.text <- gsub(pattern, "\\1", out2)
plain.text <- gsub(pattern, "\\1", out2)
plain.text <- trimws(plain.text,which = c("both", "left", "right"))
plain<-as.data.frame(plain.text)
plain[plain == ''] <- NA
plain[plain == 'Native:'] <- NA
# 
plain<-plain[1:(nrow(plain)-1),];plain<-as.data.frame(plain)
#sub(";","",plain)

narea<-plain[!(is.na(plain))]
 narea<-sub(":","",narea)
 narea<-sub(";","",narea)
 fin<-as.data.frame(unlist(narea))
 cat("WRITTING CSV FILE FOR ",as.character(sp),"\n")
 
 write.csv(fin,paste0(out_dir,"/",spname,"_NAREA_",Sys.Date(),".csv"))
 }
 cat("DONE!",as.character(sp),"\n")
 
}else{
  cat("OMMITING (SPECIES IS NOT IN GRIN...CHECK!) ",as.character(sp),"\n")
  
  }
}
 ##############
 
 out_dir<-"D:/"
 code_path<-"D:/GRIN_NA"
 #spname<-"Daucus_pusillus"
 spname<-"Manihot"
 
 GRIN_NAREA_GET(spname,code_path,out_dir)
   