# Martin Dugas, Julian Varghese 2016
# license: GPL

.ODMMetaData2R <- function( ODMfile, Form_OID, IG_OID )
{
   # Parse ODM-File
   ODM = xmlRoot(xmlTreeParse(ODMfile),useInternalNodes=TRUE)
   MD <- ODM[["Study"]][["MetaDataVersion"]]
   if (is.null(MD)) { 
      cat("odmdata <- data.frame()\n")
      return("No MetaData available\n")
      break 
   }

   ODMDescription <- xmlAttrs(ODM)["Description"]
   StudyOID <- xmlAttrs(ODM[["Study"]])["OID"]
   StudyName <- xmlValue(ODM[["Study"]][["GlobalVariables"]][["StudyName"]])
   StudyDescription <- xmlValue(ODM[["Study"]][["GlobalVariables"]][["StudyDescription"]])
   StudyDescription <- gsub("\n"," ",StudyDescription)
   StudyDescription <- gsub("\t"," ",StudyDescription)
   StudyDescription <- gsub(";",",",StudyDescription)
   Sponsor <- xmlValue(ODM[["Study"]][["GlobalVariables"]][["ProtocolName"]])
   FirstName <- xmlValue(ODM[["AdminData"]][["User"]][["FirstName"]])
   LastName <- xmlValue(ODM[["AdminData"]][["User"]][["LastName"]])
   Organization <- xmlValue(ODM[["AdminData"]][["User"]][["Organization"]])
   
   Condition <- xmlAttrs(MD[["StudyEventDef"]])["Name"]
   FormDefNodes <- MD[names(xmlChildren(MD)) == "FormDef"]
   ItemGroupDefNodes <- MD[names(xmlChildren(MD)) == "ItemGroupDef"]
   ItemDefNodes <- MD[names(xmlChildren(MD)) == "ItemDef"]
   CodeListNodes <- MD[names(xmlChildren(MD)) == "CodeList"]
      
   ivec  <- vector()
   igvec <- vector()
   ignvec <- vector()
   ign2vec <- vector()
   ign3vec <- vector()
   tvec  <- vector()
   t2vec  <- vector()
   t3vec  <- vector()
   typevec <- vector()
   cvec  <- vector()
   ainfo <- list()
   IGAlias <- list()

   # data.frame
   cat("odmdata <- data.frame( \n")
   for (i in 1: length(FormDefNodes) )
   {
      FoNode <- FormDefNodes[[i]]
      FoOID <- xmlAttrs(FoNode)["OID"]
      if (Form_OID != "" && Form_OID != FoOID) next
      
      FormOID <- gsub(".","_",FoOID, fixed=T)

      # Form: FormOID
      IGRefNodes <- FoNode[names(xmlChildren(FoNode))=="ItemGroupRef"]
      newForm <- T
      for (k in 1: length(IGRefNodes))
      {
         IGRefNode <- IGRefNodes[[k]]
         ItemGroupOID <- xmlAttrs(IGRefNode)["ItemGroupOID"]
         # ItemGroup layer
         for (j in 1: length(ItemGroupDefNodes) )
         {
            IGDefNode <- ItemGroupDefNodes[[j]]
            IGDefNodeOID  <- xmlAttrs(IGDefNode)["OID"]
            if (IG_OID != "" && IG_OID != IGDefNodeOID) next
            
            IGDefNodeName <- xmlAttrs(IGDefNode)["Name"]
            IGDefNodeName2 <- IGDefNodeName
            IGDefNodeName3 <- IGDefNodeName
            tmp <- xmlValue(IGDefNode[["Description"]][["TranslatedText"]])
            if (!is.na(tmp))
            {
               IGDefNodeName2 <- tmp
               if ( length(xmlChildren(IGDefNode[["Description"]]))> 1 ) IGDefNodeName3 <- xmlValue(IGDefNode[["Description"]][2][["TranslatedText"]])
            }
            # Alias for itemgroup                
            AliasNodes <- IGDefNode[tolower(names(xmlChildren(IGDefNode)))=="alias"]
            if (length(AliasNodes)>0 ) for (nn in 1: length(AliasNodes) )
            {
               IGAlias <- c(IGAlias, 
                            list( c( IGDefNodeOID, 
                                     xmlAttrs(AliasNodes[[nn]])["Context"],
                                     xmlAttrs(AliasNodes[[nn]])["Name"]
                                   )
                                )
                           )
            }				
            # Check where ItemGroup references
            if(ItemGroupOID == xmlAttrs(IGDefNode)["OID"] )
            {
               IRefNodes <- IGDefNode[names(xmlChildren(IGDefNode))=="ItemRef"]
               for (m in 1: length(IRefNodes) )
               {
                  ItemOID <- xmlAttrs(IRefNodes[[m]])["ItemOID"]
                  # Item layer
                  for (n in 1: length(ItemDefNodes) )
                  {
                     IDNode <- ItemDefNodes[[n]]
                     # Check where Item references
                     if (ItemOID == xmlAttrs(IDNode)["OID"])
                     {
                        # item Alias
                        AliasNodes <- IDNode[tolower(names(xmlChildren(IDNode)))=="alias"]
                        if (length(AliasNodes)>0 ) for (nn in 1: length(AliasNodes) )
                        {
                           ainfo <- c(ainfo, 
                                      list( c( ItemOID, 
                                               xmlAttrs(AliasNodes[[nn]])["Context"],
                                               xmlAttrs(AliasNodes[[nn]])["Name"]
                                             )
                                          )
                                      )
                        }
                        ItemOID <- gsub("-","_",ItemOID, fixed=T)  # problem with - in itemOID
                        Itemtext <- xmlAttrs(IDNode)["Name"]
                        Itemtext <- gsub("'","_",Itemtext)
                        Itemtext2 <- Itemtext
                        tmp <- xmlValue(IDNode[["Question"]][["TranslatedText"]])
                        if (!is.na(tmp))
                        {
                           Itemtext2 <- tmp
                           Itemtext3 <- Itemtext2
                           if ( length(xmlChildren(IDNode[["Question"]]))> 1 ) Itemtext3 <- xmlValue(IDNode[["Question"]][2][["TranslatedText"]])
                        } 
                        DataType <- xmlAttrs(IDNode)["DataType"]
                        Rtype <- "character()" # default
                        if (DataType == "integer") { Rtype <- "integer()" }
                        if (DataType == "float")   { Rtype <- "numeric()" }
                        if (DataType == "boolean") { Rtype <- "logical()" }
                        if (DataType == "date")    { Rtype <- "character()" }
                        if (DataType == "time")    { Rtype <- "character()" }
                        # if (newForm == T) { newForm <- F } else cat(",\r")
                        if (length(ivec) > 0) { cat(",\n") }
                        cat(ItemOID, "=", Rtype, sep="")
                        #
                        ivec    <- append(ivec,ItemOID)
                        igvec   <- append(igvec,IGDefNodeOID)
                        ignvec  <- append(ignvec,IGDefNodeName)
                        ign2vec  <- append(ign2vec,IGDefNodeName2)
                        ign3vec  <- append(ign3vec,IGDefNodeName3)
                        tvec    <- append(tvec, Itemtext)
                        t2vec   <- append(t2vec,Itemtext2)
                        t3vec   <- append(t3vec,Itemtext3)
                        typevec <- append(typevec,DataType)
                        # codelists - ItemOID
                        tmp <- IDNode[["CodeListRef"]]
                        CLOID <- ""
                        if (!is.null(tmp)) { CLOID <- xmlAttrs(tmp)["CodeListOID"] }
                        cvec <- append(cvec,CLOID)
                     }
                  }
               }
            }
         }
      }
   }
   cat(",\nstringsAsFactors = F)\n\n")
   # study header
   cat("attr(odmdata, \"StudyOID\")         <- \"", StudyOID, "\"\n", sep="")
   cat("attr(odmdata, \"Sponsor\")          <- \"", Sponsor, "\"\n", sep="")
   cat("attr(odmdata, \"Condition\")        <- \"", Condition, "\"\n", sep="")
   cat("attr(odmdata, \"StudyName\")        <- \"", StudyName, "\"\n", sep="")
   cat("attr(odmdata, \"StudyDescription\") <- \"", StudyDescription, "\"\n", sep="")
   cat("attr(odmdata, \"Form_OID\")         <- \"", Form_OID, "\"\n", sep="")
   cat("attr(odmdata, \"IG_OID\")           <- \"", IG_OID, "\"\n", sep="")
   cat("attr(odmdata, \"FirstName\")        <- \"", FirstName, "\"\n", sep="")
   cat("attr(odmdata, \"LastName\")         <- \"", LastName, "\"\n", sep="")
   cat("attr(odmdata, \"Organization\")     <- \"", Organization, "\"\n\n", sep="")

   # names
   cat("attr(odmdata, \"varlabels\") <- c(\n\"")
   cat(paste(tvec, sep="", collapse="\",\n\""))
   cat("\")\n")
   cat("attr(odmdata, \"varlabels_en\") <- c(\n\"")
      cat(paste(t2vec, sep="", collapse="\",\n\""))
      cat("\")\n")
      cat("attr(odmdata, \"varlabels_de\") <- c(\n\"")
      cat(paste(t3vec, sep="", collapse="\",\n\""))
      cat("\")\n")
      cat("attr(odmdata, \"DataTypes\") <- c(\n\"")
      cat(paste(typevec, sep="", collapse="\",\n\""))
      cat("\")\n")
      # itemgroups
      cat("attr(odmdata, \"itemgroups\") <- c(\"")
      cat(paste(igvec, sep="", collapse="\",\""))
      cat("\")\n")
      cat("attr(odmdata, \"itemgroups_unique\") <- c(\"")
      cat(paste(unique(igvec), sep="", collapse="\",\""))
      cat("\")\n")
      cat("attr(odmdata, \"itemgroupnames_unique\") <- c(\"")
      cat(paste(unique(ignvec), sep="", collapse="\",\""))
      cat("\")\n\n")
      cat("attr(odmdata, \"itemgroupnames_unique_en\") <- c(\"")
      cat(paste(unique(ign2vec), sep="", collapse="\",\""))
      cat("\")\n\n")
      cat("attr(odmdata, \"itemgroupnames_unique_de\") <- c(\"")
      cat(paste(unique(ign3vec), sep="", collapse="\",\""))
      cat("\")\n\n")
      cat("attr(odmdata, \"IGAlias\") <- c(\"")
      cat(paste(unlist(IGAlias),collapse="\",\""))
      cat("\")\n\n")
	
   # VALUE LABELS
   if (length(cvec) > 0) for (i in 1: length(cvec))
   {
      if (cvec[i] == "") next
      for(o in 1: length(CodeListNodes) ) 
      {
         CodeListNode <- CodeListNodes[[o]]
         CLOID <- xmlAttrs(CodeListNode)["OID"]
         if (cvec[i] == CLOID)
         {
            CLitems <- CodeListNode[names(xmlChildren(CodeListNode))=="CodeListItem"]
            # levels
            cat(paste("odmdata$", ivec[i], " <- factor(levels=c(", sep=""))
            if (length(CLitems) > 0) for (p in 1: length(CLitems) ) 
            {
               CLitem <- CLitems[[p]]
               CodeValue <- xmlAttrs(CLitem)["CodedValue"]
               CodeValue <- gsub("'","_",CodeValue, fixed=T)
               CLItemText <- xmlValue(CLitem[["Decode"]][["TranslatedText"]])
               CLItemText <- gsub("'","_",CLItemText, fixed=T)
               # cat(CodeValue, " '", CLItemText, "' ", sep="")
               if (p > 1) { cat(",") }
               cat(paste("\"",CodeValue, "\"", sep=""))
            }
            cat("))\n")
            #
            # labels
            cat(paste("attr(odmdata$", ivec[i], ",\"labels\") <- c(", sep=""))
            if (length(CLitems) > 0) for (p in 1: length(CLitems) ) 
            {
               CLitem <- CLitems[[p]]
               CLItemText <- xmlValue(CLitem[["Decode"]][["TranslatedText"]])
               CLItemText <- gsub("'","_",CLItemText, fixed=T)
               if (p > 1) { cat(",") }
               cat(paste("\"",CLItemText, "\"", sep=""))
            }
            cat(")\n")
            cat(paste("attr(odmdata$", ivec[i], ",\"labels2\") <- c(", sep=""))
            if (length(CLitems) > 0) for (p in 1: length(CLitems) ) 
            {
               CLitem <- CLitems[[p]]
               CLItemText <- xmlValue(CLitem[["Decode"]][2][["TranslatedText"]])
               if (is.null(CLItemText)) CLItemText <- ""
               CLItemText <- gsub("'","_",CLItemText, fixed=T)
               if (p > 1) { cat(",") }
               cat(paste("\"",CLItemText, "\"", sep=""))
            }
            cat(")\n")
            #
            # UMLS-codes
            contexts <- vector()
            if (length(CLitems) > 0) for (p in 1: length(CLitems) ) 
            {
               CLitem <- CLitems[[p]]
               AlNodes <- CLitem[tolower(names(xmlChildren(CLitem)))=="alias"]
               if (length(AlNodes)>0 ) for (nn in 1: length(AlNodes) )
               {
                  contexts <- append(contexts, xmlAttrs(AlNodes[[nn]])["Context"] )
               }
            }
            contexts <- unique(contexts)
            if (length(contexts) > 0)
            {
               cat(paste("attr(odmdata$", ivec[i], ",\"Alias_Contexts\") <- c(\"", sep=""))
               cat(paste(contexts, collapse="\",\""))
               cat("\")\n")
               for (p in 1: length(contexts) ) 
               {
                  cnames <- rep("",length(CLitems))
                  if (length(CLitems) > 0) for (pp in 1: length(CLitems) ) 
                  {
                     CLitem <- CLitems[[pp]]
                     AlNodes <- CLitem[tolower(names(xmlChildren(CLitem)))=="alias"]
                     if (length(AlNodes)>0 ) 
                     {
                        for (nn in 1: length(AlNodes) )
                        {
                           if (contexts[p] == xmlAttrs(AlNodes[[nn]])["Context"] )
                           {
                              if (cnames[pp] =="")
                              {
                                 cnames[pp] <- xmlAttrs(AlNodes[[nn]])["Name"]
                              }
                              else
                              {
                                 cnames[pp] <- paste(cnames[pp], xmlAttrs(AlNodes[[nn]])["Name"], sep=" ")
                              }
                           }  
                        }
                     }
                  }
                  cat(paste("attr(odmdata$", ivec[i], ",\"",contexts[p],"\") <- c(\"", sep=""))
                  cat(paste(cnames, collapse="\",\""))
                  cat("\")\n")
               }
            }
         }
      }
   }
   # Output Alias / Item
   tmp <- vector()
   if (length(ainfo) > 0)
   {
      cat("attr(odmdata, \"Alias_Items\") <- matrix(ncol=3,byrow=T, data=c(\n")
      for (i in 1:length(ainfo) )
      {
         tmp[i] <- paste("\"", ainfo[[i]][1], "\",\"", ainfo[[i]][2], "\",\"", ainfo[[i]][3], "\"", sep="") 
      }
      cat( paste(tmp, sep="", collapse=",\n") )
      cat(" ))\n")
   }
   #
   return(paste0("MetaData: ", length(ivec), " items\n"))
}

.ODMClinicalData2R <- function(ODMfile, Form_OID, IG_OID)
{
  subjects <- vector()
  ODM = xmlRoot(xmlTreeParse(ODMfile),useInternalNodes=TRUE)
  ClinDatNodes <- ODM[names(xmlChildren(ODM))=="ClinicalData"]
  if (length(ClinDatNodes)==0) { 
     cat("clindata <- data.frame()\n")
     return ("No ClinicalData\n")
     break 
  }

  cat("clindata <- data.frame(StudyOID=character(),\nMetaDataVersionOID=character(),\nSubjectKey=character(),
      \nLocationOID=character(),\nStudyEventOID=character(),\nStudyEventRepeatKey=character(),
      \nFormOID=character(),\nFormRepeatKey=character(),\nItemGroupOID=character(),
      \nItemGroupRepeatKey=character(),\nItemOID=character(), \nValue=character(), 
      stringsAsFactors = F)\n")
  
  valueCounter<-1 #add line item value by item value
  
  # Go through all Clinical Data Nodes
  for (i1 in 1: length(ClinDatNodes) )
  {
    ClinDatNode <- ClinDatNodes[[i1]]
    StudyOID <- xmlAttrs(ClinDatNode)["StudyOID"]
    MetaDataVersionOID <- xmlAttrs(ClinDatNode)["MetaDataVersionOID"]
    
    SubjectDataNodes <-ClinDatNode[names(xmlChildren(ClinDatNode))=="SubjectData"]
    
    # Go through all SubjectDataNodes
    if (length(SubjectDataNodes)>0) for (i2 in 1: length(SubjectDataNodes) )
    {
      SubjectDataNode=SubjectDataNodes[[i2]]
      SubjectKey <- xmlAttrs(SubjectDataNode)["SubjectKey"]
      subjects <- c(subjects, SubjectKey)
      
      SiteRefNodes <-SubjectDataNode[names(xmlChildren(SubjectDataNode))=="SiteRef"]
      SiteRefNode <- NULL
      LocationOID <- NULL
      if (length(SiteRefNodes)>0){
        SiteRefNode <- SiteRefNodes[[1]]
        LocationOID <- xmlAttrs(SiteRefNode)["LocationOID"]
      }


      
      # Go through all StudyEventDataNodes (if no StudyEventDataNodes available look for FormDataNodes)
      StudyEventDataNodes <- SubjectDataNode[names(xmlChildren(SubjectDataNode))=="StudyEventData"]
      for (i3 in 1: length(StudyEventDataNodes) )
      {
        StudyEventOID<-NULL
        StudyEventRepeatKey <- NULL
        if (length(StudyEventDataNodes)>0){
          StudyEventDataNode <- StudyEventDataNodes[[i3]]
          StudyEventOID <- xmlAttrs(StudyEventDataNode)["StudyEventOID"]
          StudyEventRepeatKey <- xmlAttrs(StudyEventDataNode)["StudyEventRepeatKey"]
          # Go through all FormDataNodes
          FormDataNodes <- StudyEventDataNode[names(xmlChildren(StudyEventDataNode))=="FormData"]
        }
        # if no StudyEventDataNodes available look for FormDataNodes
        else{
          FormDataNodes <- SubjectDataNode[names(xmlChildren(SubjectDataNode))=="FormData"]
        }
        for (i4 in 1: length(FormDataNodes) )
        {
          if (length(FormDataNodes)==0) break
          FormDataNode <- FormDataNodes[[i4]] 
          FormOID<- xmlAttrs(FormDataNode)["FormOID"]
          if (Form_OID != "" && Form_OID != FormOID) next

          FormRepeatKey <- xmlAttrs(FormDataNode)["FormRepeatKey"]
     
          # Go through all ItemGroupData Nodes
          ItemGroupDataNodes <- FormDataNode[names(xmlChildren(FormDataNode))=="ItemGroupData"]
          for (i5 in 1: length(ItemGroupDataNodes) )
          {
            if (length(ItemGroupDataNodes)==0) break
            ItemGroupDataNode <- ItemGroupDataNodes[[i5]]
            ItemGroupOID <- xmlAttrs(ItemGroupDataNode)["ItemGroupOID"]
            if (IG_OID != "" && IG_OID != ItemGroupOID) next

            
            ItemGroupRepeatKey <- xmlAttrs(ItemGroupDataNode)["ItemGroupRepeatKey"]

            
            # Go through all ItemData Nodes
            ItemDataNodes <- ItemGroupDataNode[names(xmlChildren(ItemGroupDataNode))=="ItemData"]
            for (i6 in 1: length(ItemDataNodes) )
            {
              if (length(ItemDataNodes)==0) break
              ItemGroupDataNodes
              ItemDataNode <- ItemDataNodes[[i6]]
              ItemOID <- xmlAttrs(ItemDataNode)["ItemOID"]
              cat("clindata[", valueCounter,",\"ItemOID\"]<-","\"",ItemOID,"\"","\n",sep="") # Add ItemOID
              
              Value <- xmlAttrs(ItemDataNode)["Value"]
              cat("clindata[", valueCounter,",\"Value\"]<-","\"",Value,"\"","\n",sep="") # Add Item value
              
              # Insert Values of Upper Nodes
              cat("clindata[", valueCounter,",\"StudyOID\"]<-","\"",StudyOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"MetaDataVersionOID\"]<-","\"",MetaDataVersionOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"SubjectKey\"]<-","\"",SubjectKey,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"LocationOID\"]<-","\"",LocationOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"StudyEventOID\"]<-","\"",StudyEventOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"StudyEventRepeatKey\"]<-","\"",StudyEventRepeatKey,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"FormOID\"]<-","\"",FormOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"FormRepeatKey\"]<-","\"",FormRepeatKey,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"ItemGroupOID\"]<-","\"",ItemGroupOID,"\"","\n",sep="")
              cat("clindata[", valueCounter,",\"ItemGroupRepeatKey\"]<-","\"",ItemGroupRepeatKey,"\"","\n",sep="")
              
              # Start new line, and insert old value for upper nodes
              valueCounter <- valueCounter+1

            }
          }
        }
      } 
    }
  }
  
  # return value: info text
  first_subject <- ""
  last_subject <- ""
  if (length(subjects)>0)
  {
     first_subject <- subjects[1]
     last_subject <- subjects[length(subjects)]
  }
  subjects <- unique(subjects)
  subject_text <- ""
  if (length(subjects) == 1) subject_text <- paste0(" (", first_subject,")")
  if (length(subjects)  > 1) subject_text <- paste0(" (", first_subject," .. ", last_subject, ")")
  return(paste0("ClinicalData: ",length(subjects), " subjects", subject_text, ", ", valueCounter-1, " values\n" ))
}

ODM2R <- function( ODMfile="", Form_OID="", IG_OID="")
{
   cat("ODM2R\n")
   if (ODMfile == "") ODMfile <- file.choose()
   cat(paste0("ODMfile=",ODMfile,"\n"))
   Rfile <- paste(ODMfile,".R",sep="")   
   cat(paste0("Rfile=",Rfile,"\n"))
   sink(Rfile)

   resultMetaData <- .ODMMetaData2R( ODMfile, Form_OID, IG_OID )

   resultClinicalData <- .ODMClinicalData2R( ODMfile, Form_OID, IG_OID )

   sink()
   cat(paste0(resultMetaData, resultClinicalData))
}