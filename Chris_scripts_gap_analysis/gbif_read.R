require(ff);require(ffbase);require(xlsx);require(geosphere);require(foreach)

### Defining functions
'%ni%' <- Negate('%in%')
#
cbind.ffdf2 <- function(...){
  argl <- list(...)
  if(length(argl) == 1L){
    return(argl[[1]])
  }else{
    physicalList = NULL
    for(i in 1:length(argl)){
      if(class(argl[[i]]) == "data.frame"){
        physicalList = c(physicalList, physical(as.ffdf(argl[[i]])))
      }else{
        physicalList = c(physicalList, physical(argl[[i]]))
      }
      
    }
    mergeCall <- do.call("ffdf", physicalList)
    return(mergeCall)
  }
}

options(ffmaxbytes = min(getOption("ffmaxbytes"),.Machine$integer.max * 12))

###Calling directories
dir <- "//dapadfs/Workspace_cluster_9/gap_analysis_landraces/Input_data/datasets_20180416"
orDir <- paste0(dir, "/", "compressed"); if(!file.exists(orDir)){dir.create(orDir)}
out_Dir <- paste0(dir, "/", "outcomes"); if(!file.exists(out_Dir)){dir.create(out_Dir)}
inDir <- paste0(dir, "/", "inputs"); if(!file.exists(inDir)){dir.create(inDir)}
auxDir <- paste0(dir, "/", "auxiliar"); if(!file.exists(auxDir)){dir.create(auxDir)}

###Reading GBIF file

gbif_dataset <- read.csv.ffdf(x=NULL,
                              file=paste0(inDir,"/","occurrence.txt"),
                              encoding="UTF-8",
                              sep = "\t",
                               VERBOSE = TRUE,
                               na.strings="",
                               first.rows = 10000,
                               next.rows = 5000,
                              # FUN= "read.csv",
                              quote="",
                              #quote="\"",
                              header=T,
                             colClasses=rep("factor",235)
                              )#,
                              #colClasses=rep("factor",235))

###Getting original institution codes.
inst_codes <- as.character(as.data.frame(as.ffdf(unique(na.omit(gbif_dataset$institutionCode)))))
inst_codes <- as.data.frame(inst_codes)
colnames(inst_codes) <-"institutionCode"
inst_ins <- as.character(as.data.frame(as.ffdf(unique(na.omit(gbif_dataset$institutionID)))))
# write.xlsx(inst_codes,
#            paste0(out_Dir,"/","institutionCode.xls"),
#            sheetName = "institutionCode",
#            col.names=TRUE,
#            row.names=FALSE,
#               )

############
###subsetting

gbif_dataset1 <- gbif_dataset

##removing records without coordinates.
idx <- ffbase::ffwhich(x=gbif_dataset1, expr = !is.na(decimalLatitude))
sub <- as.ffdf(gbif_dataset1[idx,][,])
rm(idx);gc()
#extracting unique issue codes
is_codes <- as.data.frame.ffdf(as.character(unique(na.omit(sub$issue))))

##removing records with coordinate issues.
idx <- ffbase::ffwhich(x=sub, expr =  issue %in%  c("COORDINATE_OUT_OF_RANGE",
                                                   "COUNTRY_COORDINATE_MISMATCH",
                                                   "ZERO_COORDINATE",
                                                   "COORDINATE_INVALID",
                                                   "COORDINATE_INVALID;COUNTRY_INVALID",
                                                   "COORDINATE_INVALID;RECORDED_DATE_INVALID;BASIS_OF_RECORD_INVALID",
                                                   "ZERO_COORDINATE;GEODETIC_DATUM_ASSUMED_WGS84;COUNTRY_INVALID",
                                                   "ZERO_COORDINATE;GEODETIC_DATUM_ASSUMED_WGS84",
                                                   "ZERO_COORDINATE;GEODETIC_DATUM_ASSUMED_WGS84;COUNTRY_COORDINATE_MISMATCH;TAXON_MATCH_HIGHERRANK",
                                                   "COORDINATE_INVALID;ELEVATION_MIN_MAX_SWAPPED"
                                                   )
)

sub <- as.ffdf(sub[-c(as.numeric(idx[])),][,])
#dim(sub)
rm(idx);gc()
##removing records duplicate records.
idx <- ffbase::ffwhich(x=sub, expr =  institutionCode %in%  
                                                 c("Bioversity-ECPGR",
                                                   "Bioversity-SINGER",
                                                   "Bioversity",
                                                   "CIAT",
                                                   "USDA_NPGS",
                                                   "USDA_NPGS_GRIN",
                                                   "USA005",
                                                   "USA020",
                                                   "USA022",
                                                   "USA971"
                                                   
                                                   
    )
)

sub <- as.ffdf(sub[-c(as.numeric(idx[])),][,])
rm(idx);gc()
#getting institution codes
is_codes <- as.data.frame.ffdf(as.character(unique(na.omit(sub$institutionCode))))

###new information to be added

new_info <- as.ffdf(as.data.frame(matrix(nrow=dim(sub)[1], ncol = 3)))
colnames(new_info) <- c("status","is_selected","action")
#new_info <- as.ffdf(new_info)
# plot(
# as.numeric(as.character(sub[]$decimalLongitude)),
# as.numeric(as.character(sub[]$decimalLatitude))
# )


#Merge two ff objects
sub <- cbind.ffdf2(sub,new_info)

colnames(sub)[((ncol(sub)-ncol(new_info)+1):ncol(sub))] <- colnames(new_info)

#Gathering GBIF with coordinates records

sub2 <- sub[,c("gbifID","decimalLongitude","decimalLatitude")][]

sub2[,1] <- as.numeric(as.character(gsub(pattern= '\"',"",x=sub2[,1])))
sub2[,2] <- as.numeric(as.character(gsub(pattern= '\"',"",x=sub2[,2])))
sub2[,3] <- as.numeric(as.character(gsub(pattern= '\"',"",x=sub2[,3])))
sub2  <- as.ffdf(sub2)
###################################################################################
rm(gbif_dataset);gc()
###################################################################################

###Reading cwr file

cwr_dataset <- read.csv.ffdf(x=NULL,
                              file=paste0(auxDir,"/","Phaseolus_vulgaris.csv"),
                              encoding="UTF-8",
                              sep = "|",
                              VERBOSE = TRUE,
                              na.strings= c("","\\N"),
                              first.rows = 10000,
                              next.rows = 5000,
                              # FUN= "read.csv",
                              quote="",
                             # quote="\"",
                              header=T,
                              colClasses=rep("factor",96)
)#,
cwr_dataset1 <- cwr_dataset
idx <- ffbase::ffwhich(x=cwr_dataset1, expr = source %in% c("\"G\""))
cwr_dataset1 <- as.ffdf(cwr_dataset1[idx,][,])
#cwr_dataset1 
#colClasses=rep("factor",235))

So_list <- as.character(as.data.frame(as.ffdf(unique(na.omit(cwr_dataset1$source)))))

##removing records without coordinates.
#idx <- ffbase::ffwhich(x=cwr_dataset1, expr = !is.na(lat_deg))
#cwr_dataset1 <- as.ffdf(cwr_dataset1[idx,][,])
rm(idx);gc()
coords <- cwr_dataset1[,c("id","final_lon","final_lat")][]
###################################################################################
rm(idx,new_info);gc()
###################################################################################
#Extracting coordinates

coords <- coords[which(!is.na(coords[,2])),]

coords[,1] <- as.numeric(as.character(gsub(pattern= '\"',"",x=coords[,1])))
coords[,2] <- as.numeric(as.character(gsub(pattern= '\"',"",x=coords[,2])))
coords[,3] <- as.numeric(as.character(gsub(pattern= '\"',"",x=coords[,3])))
coords  <- as.ffdf(coords)

#hist(coords[,2])
rm(cwr_dataset,cwr_dataset1)
#gsub(pattern= '\"',"",x=coords[,1])

###################################################################################
# Evaluating GBIF coordinates against Crop wild relatives database coordinates
###################################################################################

#for(j in 1:nrow(sub2)){
  
  
  
#distmin_coords <- #function(j){
distmin_coords <- lapply(1:nrow(sub),function(j){
 require(geosphere);require(ff)
  cat("                           ","\n")
  cat("###########################","\n")
  cat("                           ","\n")
  cat("Proccesing... Row: ",j," | ",round((j/nrow(sub2)*100),3),"%","\n")
  cat("                           ","\n") 
  
  
  dist_c <- min(geosphere::distm(x=cbind(sub2$decimalLongitude[j],sub2$decimalLatitude[j]),
                     y=cbind(coords$final_lon[],coords$final_lat[]),
                     fun=distHaversine)/1000,
                na.rm=T
                )
# # cat("landrace row: ",j," | ","cwr row: ",i,"\n")
# # cat("                           ","\n")
#   
#   x <- distm(c(sub2$decimalLongitude[j],sub2$decimalLatitude[j]),
#                c(coords$final_lon[i],coords$final_lat[i]))/1000
# return(x)
# 
# })

cat("                           ","\n")
if(dist_c <= 1){
  x <-"DELETE"
} else {
  x <-"KEEP"
}
cat("                           ","\n")
cat("###########################","\n")
cat("                           ","\n")

return(x)
#}#;rm(j)
})


distmin_coords2 <- as.ff(factor(unlist(distmin_coords)))
sub$is_selected <- distmin_coords2


###################################################################################
# Evaluating GBIF coordinates using Institude Code name table curated
###################################################################################

ins_code_curated <- read.csv(paste0(out_Dir,"/","gbif_institution_code2.csv"),header=T,sep="|")

inst_to_check <- sub[,c("gbifID","institutionCode","status")][]
inst_to_check$status <- as.character(inst_to_check$status)
#inst_to_check  <- as.ffdf(inst_to_check)



#lapply(1:nrow(sub),function(i){
for(i in 1:nrow(ins_code_curated)){
  cat("institutionCode: ",i," | ",round((i/nrow(ins_code_curated)*100),2)," %","\n")
  inst_to_check$status[which(  as.character(inst_to_check$institutionCode) ==
                               as.character(ins_code_curated$institutionCode_original[[i]])
                         )] <- as.character(ins_code_curated$status[[i]])
  cat("                 ","\n")
}

sub$status <- as.ff(factor(inst_to_check$status));rm(inst_to_check)

###################################################################################
# Final Evaluation  for GBIF coordinates using Institude Code name and coordinate distance to CWR
###################################################################################

action_list <- sub[,c("gbifID","institutionCode","status","is_selected","action")][]


#for( i in 1:nrow(action_list)){
  
  action_list$action[which(action_list$status=="H" &
                           action_list$is_selected=="KEEP" 
                             )] <- 1
  
  action_list$action[which(action_list$status=="G" &
                             action_list$is_selected=="KEEP" 
  )] <- 1
  
  
  action_list$action[which(action_list$status=="CHECK" &
                             action_list$is_selected=="KEEP" 
  )] <- 2
  
  action_list$action[which(action_list$status=="CHECK" &
                             action_list$is_selected=="DELETE" 
  )] <- 0
  
  action_list$action[which(action_list$status=="H" &
                             action_list$is_selected=="DELETE" 
  )] <- 0
  
  action_list$action[which(action_list$status=="G" &
                             action_list$is_selected=="DELETE" 
  )] <- 0
  
  
  action_list$action[which(is.na(action_list$status) &
                           is.na(action_list$is_selected)
                           )] <- 2
  
  
#}
  sub$action <- as.ff(factor(action_list$action));rm(action_list)

###Saving dataset cleaned!

write.table.ffdf(sub,paste0(out_Dir,"/","gbif_cleaned.csv"),sep="\t",na="",quote=F)




















# #idx <- subset.ffdf(x=gbif_dataset1, subset = gbif_dataset1$institutionCode %in% c("FB-UMSNH"))
# idx <- ffbase::ffwhich(x=gbif_dataset1, expr = institutionCode %in%
# c("YPM")
# )
# sub <- gbif_dataset1[idx,][,]
# 
# basisOfRecord <- as.character(as.data.frame(as.ffdf(unique(na.omit(gbif_dataset$basisOfRecord)))))
# basisOfRecord <- as.data.frame(basisOfRecord)
# colnames(basisOfRecord) <-"institutionCode"
# 
# #"PRESERVED_SPECIMEN"
# 
# #pr <- subset.ffdf(x=gbif_dataset1, subset = basisOfRecord %in% c("OBSERVATION"))
# 
# #as.character(unique(na.omit(pr$institutionCode)))
# 
# 
# pr_codes <- (as.character(unique(na.omit(sub$basisOfRecord))))
# 
# 
#   # as.character(as.data.frame(as.ffdf(unique(na.omit(pr$institutionCode)))))
# pr_codes <- as.data.frame(pr_codes)
# colnames(pr_codes) <-"basisOfRecord"
# pr_codes
# sub

