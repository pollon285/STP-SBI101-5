### REQUIRED SUB-FUNCTIONS (1-4) ###

#########################################
################## (1) ##################
#function to map the FTIDs onto PROMS TOOLS/DW VIEWS/PATHWAYS/+additional info for the specific FTID
getmap_ftid <- function(mapcombos,formnames,anchor,anchortype)
{
  # get anchor combos
  combos <- mapcombos[which(mapcombos[[anchor]]==anchortype),]
  ptools <- unique(combos$PROM_TOOL)
  
  # create table
  ktable <- data.frame("PROM_TOOL"=ptools,"DW_views"=rep(NA,length(ptools)),"PATHWAY"=rep(NA,length(ptools)))
  for(i in 1:length(ptools))
  {
    dwviews <- unique(combos[which(combos$PROM_TOOL==ptools[i]),"DW_views"])
    dwviews <- paste(dwviews,sep="", collapse="<br>") #this is to have multiple line in the same dataframe cell
    ktable[i,"DW_views"] <- dwviews
    pathways <- unique(combos[which(combos$PROM_TOOL==ptools[i]),"PATHWAY"])
    pathways <- paste(pathways,sep="", collapse="<br>")
    ktable[i,"PATHWAY"] <- pathways
  }
  
  ktableOUT <<-ktable
  
  # get additional info
  idx <- which(formnames$FTID==anchortype)
  CollectionMethod <- formnames$`Collection method`[idx]
  Form_Type <- formnames$Form_Title[idx]
  Tool <- formnames$Tool[idx]
  Plabel_eng <- formnames$`Patient-friendly label (English)`[idx]
  Plabel_cym <- formnames$`Patient-friendly label (Welsh)`[idx]
  Clabel <- formnames$`Clinician-friendly label`[idx]
  
  AddInfoFTID <- data.frame("Collection method"=CollectionMethod,
                             "Form Type"=Form_Type,
                             "Tool"=Tool,
                             "Plabel_eng"=Plabel_eng,
                             "Plabel_cym"=Plabel_cym,
                             "Clabel"=Clabel)

  AddInfoFTID <- AddInfoFTID[!duplicated(AddInfoFTID),] #remove duplicate rows
 
  list_tables_FTID <<- list(ktable,AddInfoFTID)
 
  
}

#########################################
################## (2) ##################
#function to map the PROM TOOLS onto FTIDs/DW VIEWS/PATHWAYS/+additional PROMS TOOLS present in the specific FTID
getmap_PT <- function(mapcombos,formnames,anchor,anchortype)
{
  # get anchor combos
  combos <- mapcombos[which(mapcombos[[anchor]]==anchortype),]
  
  ftid <- as.integer(unique(combos$FTID)) #FTID number is kept an integer
  
  # create table
  ktable <- data.frame("FTID"=ftid,"DW_views"=rep(NA,length(ftid)),"PATHWAY"=rep(NA,length(ftid)),"EXTRA_PROM_TOOLS"=rep(NA,length(ftid)))
  for(i in 1:length(ftid))
  {
    dwviews <- unique(combos[which(combos$FTID==ftid[i]),"DW_views"])
    dwviews <- paste(dwviews,sep="", collapse="<br>")
    ktable[i,"DW_views"] <- dwviews
    pathways <- unique(combos[which(combos$FTID==ftid[i]),"PATHWAY"])
    pathways <- paste(pathways,sep="", collapse="<br>")
    ktable[i,"PATHWAY"] <- pathways
    extra_pt <- unique(mapcombos[which(mapcombos$FTID==ftid[i]),"PROM_TOOL"])
    extra_pt<-extra_pt[!extra_pt %in% anchortype]
    extra_pt <- paste(extra_pt,sep="", collapse="<br>") #this is to have multiple line in the same dataframe cell
    ktable[i,"EXTRA_PROM_TOOLS"] <- extra_pt
  }
  ktableOUT <<-ktable
  
}



#########################################
################## (3) ##################
#function to map the PATHWAY onto DW VIEWS/FTIDs/PROM TOOLS
getmap_pathway <- function(mapcombos,formnames,anchor,anchortype)
{
  # get anchor combos
  combos <- mapcombos[which(mapcombos[[anchor]]==anchortype),]
  
  dwviews <- unique(combos$DW_views)
  
  # create table
  ktable <- data.frame("DW_views"=dwviews,"FTID"=rep(NA,length(dwviews)),"PROM_TOOL"=rep(NA,length(dwviews)))
  for(i in 1:length(dwviews))
  {
    ftid <- unique(combos[which(combos$DW_views==dwviews[i]),"FTID"])
    ftid <- paste(ftid,sep="", collapse="<br>")
    ktable[i,"FTID"] <- ftid
    ptools <- unique(combos[which(combos$DW_views==dwviews[i]),"PROM_TOOL"])
    ptools <- paste(ptools,sep="", collapse="<br>")
    ktable[i,"PROM_TOOL"] <- ptools
    
  }
  
  ktableOUT <<-ktable
  
}


#########################################
################## (4) ##################
#function to map the DW VIEWS onto PATHWAYS/FTIDs/PROM TOOLS
getmap_dwviews <- function(mapcombos,formnames,anchor,anchortype)
{
  # get anchor combos
  combos <- mapcombos[which(mapcombos[[anchor]]==anchortype),]
  
  pathways <- unique(combos$PATHWAY)
  
  # create table
  ktable <- data.frame("PATHWAY"=pathways,"FTID"=rep(NA,length(pathways)),"PROM_TOOL"=rep(NA,length(pathways)))
  for(i in 1:length(pathways))
  {
    ftid <- unique(combos[which(combos$PATHWAY==pathways[i]),"FTID"])
    ftid <- paste(ftid,sep="", collapse="<br>")
    ktable[i,"FTID"] <- ftid
    ptools <- unique(combos[which(combos$PATHWAY==pathways[i]),"PROM_TOOL"])
    ptools <- paste(ptools,sep="", collapse="<br>")
    ktable[i,"PROM_TOOL"] <- ptools
    
  }
  
  ktableOUT <<-ktable
  
}

#########################################
################## (0) ##################

#master mapping function calling mapping subfunctions 1-4
getmap <- function(mapcombos,formnames,anchor,anchortype)
{
  #specify the subfunction on the basis of the anchor
  if (anchor=="FTID"){
    getmap_ftid(mapcombos,formnames,anchor,anchortype)
    return(list_tables_FTID)
  }
  
  else {
    if (anchor=="PROM_TOOL"){
      getmap_PT(mapcombos,formnames,anchor,anchortype)
    }
  
    else if (anchor=="PATHWAY"){
      getmap_pathway(mapcombos,formnames,anchor,anchortype)
    }
  
    else if (anchor=="DW_views"){
      getmap_dwviews(mapcombos,formnames,anchor,anchortype)
    }
    
    return(ktableOUT)
  }
  
}




