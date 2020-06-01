#setwd("S:/ClinEng_Users/CEDAR/PROJECTS and Reports/CED106 All Wales PROMS/11 Data/DW_MAPPING_V1/PROMS_SHINY_APP_v1/data/") ##remove

CreatQueryTable <- function(DWlookup.file,FormsInfo.file,FormLabels.file)
{
  # map ftid to proms tools and DW views
  dwlookup <- read.csv(DWlookup.file,check.names = FALSE,stringsAsFactors = FALSE)
  dwlookup <- unique(dwlookup[,c("FTID","PROM_TOOL","DW_views")])
  
  # - separate generic proms tools
  idx <- which(dwlookup$PROM_TOOL=="GENERIC")
  knew <- c()
  for(i in 1:length(idx))
  {
    k <- dwlookup[idx[i],]
    k <- rbind(k,k,k)
    k$PROM_TOOL <- c("EQ5D-5L","About You","WPAI")
    knew <- rbind(knew,k)
  }
  dwlookup <- rbind(dwlookup[-idx,],knew)
  
  # map ftid to pathways
  colflag <- rep("NULL",64)
  colflag[c(2,56)] <- NA
  formsinfo <- read.csv(FormsInfo.file,check.names=FALSE,stringsAsFactors=FALSE,colClasses=colflag)
  formsinfo <- unique(formsinfo)
  
  # map ftid to labels
  formlabels <- read.csv(FormLabels.file,check.names=FALSE,stringsAsFactors = FALSE)
  
  
  # merge
  QueryTable <- merge(formlabels,dwlookup,by="FTID")
  QueryTable <- merge(QueryTable,formsinfo,by="FTID")
  QueryTable <- QueryTable[order(QueryTable$FTID,QueryTable$Pathway,QueryTable$PROM_TOOL,QueryTable$DW_views),]
  return(QueryTable)
}

#qt <- CreatQueryTable("GeraldFile_200313.csv","FormsInfo_200313.csv","FormLabels_200313.csv") ##remove
