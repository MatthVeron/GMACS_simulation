library(gtools)


#==============================================================================
 
# Function for finding locations in tables
FindLoc <- function(x,string,col,Error=T)
{
  Loc <- which(x[,col]==string) 
  if (length(Loc)==0)
  {
    if (Error==TRUE) 
    { cat("Warning: could not find ",string," STOPPING"); AAA; }
    else
      Loc <- -1
  }  
  return(Loc)  
}  

# ===================================================================================================

# Function for finding locations in tables
FindLoc2 <- function(x,string1,string2,col1,col2)
{
  Loc <- which(x[,col1]==string1 & x[,col2]==string2) 
  if (length(Loc)==0) {cat("Warning: could not find",string1,"STOPPING"); AAA; }
  return(Loc)  
}  

# ==================================================================================
# ==================================================================================

ReadFiles <- function(Folder)
 {
  FileName <- paste(Folder,"gmacs.dat",sep="")
  GmacsFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(GmacsFile); GmacsFile[Use] <- ""
  DatFileName <- GmacsFile[1,1]
  CtlFileName <- GmacsFile[2,1]
  PrjFileName <- GmacsFile[3,1]
  FileName <- paste(Folder,DatFileName,sep="")
  DatFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(DatFile); DatFile[Use] <- ""
  FileName <- paste(Folder,CtlFileName,sep="")
  CtlFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(CtlFile); CtlFile[Use] <- ""
  FileName <- paste(Folder,PrjFileName,sep="")
  PrjFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(PrjFile); PrjFile[Use] <- ""
  FileName <- paste(Folder,"Gmacs.par",sep="")
  ParFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(ParFile); ParFile[Use] <- ""
  FileName <- paste(Folder,"SimData.out",sep="")
  SimFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(SimFile); SimFile[Use] <- ""
  
  # Extract relevant lines
  Index1 <- FindLoc2 (DatFile,"catch","frames",4,6)+1
  NumCatFrame <- as.numeric(DatFile[Index1,1])
  NumCatRows <- as.numeric(DatFile[Index1+2,1:NumCatFrame])
  # Extract relevant lines
  Index2 <- FindLoc2(DatFile,"relative","indicies",4,6)+1
  NumCpueFrame <- as.numeric(DatFile[Index2,1])
  NumCpueRows <- as.numeric(DatFile[Index2+4,1])
  print(NumCpueFrame)
  print(NumCpueRows)
  # Extract relevant lines
  Index3 <- FindLoc2(DatFile,"length","frequency",4,5)+1
  NumLengthFrame <- as.numeric(DatFile[Index3,1])
  NumLengthRows <- as.numeric(DatFile[Index3+2,1:NumLengthFrame])
  NumLengthBins <- as.numeric(DatFile[Index3+4,1:NumLengthFrame])
  Index4 <- FindLoc2(DatFile,"Growth","data",2,3)
  
  Remove1 <- NULL
  for (II in (Index1+3):(Index2-2))
  {
   if (substr(DatFile[II,1],1,1)=="#") Remove1 <- c(Remove1,II)  
  }

  Remove2 <- NULL
  for (II in (Index2+5):(Index3-2))
   {
    if (substr(DatFile[II,1],1,1)=="#") Remove2 <- c(Remove2,II) 
   }
  
  Remove3 <- NULL
  for (II in (Index3+5):(Index4-1))
   {
    if (substr(DatFile[II,1],1,1)=="#") Remove3 <- c(Remove3,II) 
   }
  
  # Remove comment lines
  DatFile <- DatFile[-c(Remove1,Remove2,Remove3),]
  
  
  Outs <- NULL
  Outs$DatFileName <- DatFileName
  Outs$CtlFileName <- CtlFileName
  Outs$PrjFileName <- PrjFileName
  Outs$OrigFolder <- Folder
  Outs$NumCatFrame <- NumCatFrame
  Outs$NumCatRows <- NumCatRows
  Outs$NumCpueFrame <- NumCpueFrame
  Outs$NumCpueRows <- NumCpueRows
  Outs$NumLengthFrame <- NumLengthFrame
  Outs$NumLengthRows <- NumLengthRows
  Outs$NumLengthBins <- NumLengthBins
  print(str(Outs))
  Outs$GmacsFile <- GmacsFile
  Outs$DatFile <- DatFile
  Outs$CtlFile <- CtlFile
  Outs$PrjFile <- PrjFile
  Outs$ParFile <- ParFile
  Outs$SimFile <- SimFile
  return(Outs)
  
 }  

# ==================================================================================================================

WriteFiles <- function(Folder,Results,RunUsingPar=F,NoEst=F,Isim=0,Nsim=10000,RunFile="Run.bat")
 {
  GmacsFileName <- paste("Gmacs",Isim,".dat",sep="")
  DataFileName <- paste("GmacsI",Isim,".dat",sep="")
  CtlFileName <- paste("GmacsI",Isim,".ctl",sep="")
  PrjFileName <- paste("GmacsI",Isim,".proj",sep="")
  RunSpec <- Results$GmacsFile
  RunSpec[1,1] <- DataFileName
  RunSpec[2,1] <- CtlFileName
  RunSpec[3,1] <- PrjFileName
  
  FileName <- paste(Folder,GmacsFileName,sep="")
  write.table(RunSpec,FileName,col.names=F,row.names=F,quote=F)
  FileName <- paste(Folder,DataFileName,sep="")
  write.table(Results$DatFile,FileName,col.names=F,row.names=F,quote=F)
  
  if (NoEst==T)
   {
    Index1 <- FindLoc2 (Results$CtlFile,"Maximum","function",3,6)
    Results$CtlFile[Index1,1] <- 1
   }
  
  FileName <- paste(Folder,CtlFileName,sep="")
  write.table(Results$CtlFile,FileName,col.names=F,row.names=F,quote=F)
  FileName <- paste(Folder,PrjFileName,sep="")
  write.table(Results$PrjFile,FileName,col.names=F,row.names=F,quote=F)
  file.copy(from=paste0(Results$OrigFolder,"gmacs.exe"),to=paste0(Folder,"gmacs.exe"))
  if (RunUsingPar==T)
   {
    FileName <- paste(Folder,"gmacs.pin",sep="")
    write.table(Results$ParFile,FileName,col.names=F,row.names=F,quote=F)
   }
  
  write(paste0("Copy ",GmacsFileName," Gmacs.dat"),RunFile,append=T)
  write(paste0("Gmacs -est >a.a"),RunFile,append=T)
  write(paste0("Copy Gmacsall.out Gmacsall",Isim,".out"),RunFile,append=T)
  
  # Clean up
  if (Isim==Nsim)
   {
    write("@Del a.a",RunFile,append=T)
    write("@Del fmin.log",RunFile,append=T)
    write("@Del gmacs.dat",RunFile,append=T)
    #write("@Del gmacs.pin",RunFile,append=T)
    write("@Del gmacs.exe",RunFile,append=T)
    write("@Del gmacs.log",RunFile,append=T)
    write("@Del gmacs.rep",RunFile,append=T)
    write("@Del gmacs_in.ctl",RunFile,append=T)
    write("@Del gmacs_in.dat",RunFile,append=T)
    write("@Del gmacs_in.prj",RunFile,append=T)
    write("@Del gmacs_files_in.dat",RunFile,append=T)
    write("@Del gmacsall.out",RunFile,append=T)
    write("@Del simdata.out",RunFile,append=T)
    write("@Del CheckFile.rep",RunFile,append=T)
    write("@Del McOut.rep",RunFile,append=T)
    write("@Del McOutRec.rep",RunFile,append=T)
    write("@Del McOutDIAG.rep",RunFile,append=T)
    write("@Del McOutPROJ.rep",RunFile,append=T)
    write("@Del McOutREF.rep",RunFile,append=T)
    write("@Del McOutSSB.rep",RunFile,append=T)
    write("@Del Personal.rep",RunFile,append=T)
    
    if (NoEst==F)
     {
      write("@del gmacs.b01",RunFile,append=T)
      write("@del gmacs.b02",RunFile,append=T)
      write("@del gmacs.b03",RunFile,append=T)
      write("@del gmacs.b04",RunFile,append=T)
      write("@del gmacs.b05",RunFile,append=T)
      write("@del gmacs.b06",RunFile,append=T)
      write("@del gmacs.b07",RunFile,append=T)
      write("@del gmacs.b08",RunFile,append=T)
      write("@del gmacs.b09",RunFile,append=T)
      write("@del gmacs.bar",RunFile,append=T)
      write("@del gmacs.p01",RunFile,append=T)
      write("@del gmacs.p02",RunFile,append=T)
      write("@del gmacs.p03",RunFile,append=T)
      write("@del gmacs.p04",RunFile,append=T)
      write("@del gmacs.p05",RunFile,append=T)
      write("@del gmacs.p06",RunFile,append=T)
      write("@del gmacs.p07",RunFile,append=T)
      write("@del gmacs.p08",RunFile,append=T)
      write("@del gmacs.p09",RunFile,append=T)
      write("@del gmacs.par",RunFile,append=T)
      write("@del gmacs.r01",RunFile,append=T)
      write("@del gmacs.r02",RunFile,append=T)
      write("@del gmacs.r03",RunFile,append=T)
      write("@del gmacs.r04",RunFile,append=T)
      write("@del gmacs.r05",RunFile,append=T)
      write("@del gmacs.r06",RunFile,append=T)
      write("@del gmacs.r07",RunFile,append=T)
      write("@del gmacs.r08",RunFile,append=T)
      write("@del gmacs.r09",RunFile,append=T)
      write("@del gradient.1",RunFile,append=T)
      write("@del gradient.2",RunFile,append=T)
      write("@del gradient.3",RunFile,append=T)
      write("@del gradient.4",RunFile,append=T)
      write("@del gradient.5",RunFile,append=T)
      write("@del gradient.6",RunFile,append=T)
      write("@del gradient.7",RunFile,append=T)
      write("@del gradient.8",RunFile,append=T)
      write("@del gradient.9",RunFile,append=T)
      write("@del gradient.dat",RunFile,append=T)
    } 
    
    
   }
    
}

# ==================================================================================================================

GenData <- function(Results,Determ=F)
{
 # Catch generation  
 Index1 <- FindLoc(Results$SimFile,"pre_catch",1)+1
 #print(Index1)
  
 Index2 <- FindLoc2 (Results$DatFile,"catch","frames",4,6)+3
 for (Iframe in 1:Results$NumCatFrame)
  {  
   for (Iyear in 1:Results$NumCatRows[Iframe])
    {
     Index2 <- Index2 + 1
     ExtCatch <- as.numeric(Results$SimFile[Index1+Iframe-1,Iyear])
     CV <- as.numeric(Results$DatFile[Index2,6])
     OldCatch <- as.numeric(Results$DatFile[Index2,5])
     GenCatch <- ExtCatch
     if (Determ==F) GenCatch <- GenCatch*exp(rnorm(1,0,1)*CV-CV^2/2.0)
    if (OldCatch > 0) Results$DatFile[Index2,5] <- GenCatch
    }
  } 
 
 # Cpue generation
 Index1 <- FindLoc(Results$SimFile,"pre_cpue",1)+1
 #print(Index1)
 
 Index2 <- FindLoc2 (Results$DatFile,"Number","index",2,7)+1
 #print(Index2)
 for (II in 1:Results$NumCpueRows)
  {  
   Index2 <- Index2 + 1
   ExtCpue <- as.numeric(Results$SimFile[Index1,II])
   CV <- as.numeric(Results$DatFile[Index2,8])
   OldCpue <- as.numeric(Results$DatFile[Index2,7])
   #cat(II,Index1,Index2,OldCpue,ExtCpue,"\n")
   GenCpue <- ExtCpue
   if (Determ==F) GenCpue <- GenCpue*exp(rnorm(1,0,1)*CV-CV^2/2.0)
   if (OldCpue > 0) Results$DatFile[Index2,7] <- GenCpue
  } 
 
 # Length data
 Index6a <- FindLoc(Results$SimFile,"Size_data_summary1",1)+2
 #print(Index6a)
 Index6b <- FindLoc(Results$SimFile,"Size_data_summary2",1)-2
 #print(Index6b)
 Gen <- NULL
 for (Index in Index6a:Index6b)
  {
   VecA <- rep(0,8+40)
   Ipnt <- 11; Ilast <- -1
   Neff <- Results$SimFile[Index,10]
   for (Jpnt in Ipnt:200)
    {
     if (Results$SimFile[Index,Jpnt]=="" & Ilast==-1) Ilast <- Jpnt-1
   }
   Nlen <- (Ilast-Ipnt+1)/2
   Vec1 <- as.numeric(c(Results$SimFile[Index,1:7],Results$SimFile[Index,10]))
   Vec2 <- as.numeric(Results$SimFile[Index,10+Nlen+1:Nlen])
   #Vec2a <- as.numeric(rmultinom(1,as.numeric(Results$SimFile[Index,10]),prob=Vec2))
  Vec2a <- as.numeric(rdirichlet(1,alpha=as.numeric(Results$SimFile[Index,10])*Vec2/sum(Vec2)))
   if (Determ==T) Vec <- c(Vec1,Vec2)
   if (Determ==F) Vec <- c(Vec1,Vec2a)
   VecA[1:length(Vec)] <- Vec 
   Gen <- rbind(Gen,VecA)
  }
 #print(str(Gen))
 
 Index4 <- FindLoc(Results$SimFile,"Size_data_summary2",1)+1
 #print(Index4)
 Index5 <- FindLoc2 (Results$DatFile,"length","frequency",4,5)+5
 #print(Index5)
 Iout <- 0
 for (Iframe in 1:Results$NumLengthFrame)
  for (Irow in 1:Results$NumLengthRows[Iframe])
   {
    Index4 <- Index4 + 1; Index5 <- Index5 + 1; Iout <- Iout + 1
    OK <- -1
    for (Iline in 1:length(Gen[,1]))
     {
      Found <- T
      if (Results$DatFile[Index5,2]==3)
       {
        for (JJ in 1:5) if (Gen[Iline,JJ]!=Results$DatFile[Index5,JJ]) Found <- F
       }
      else
       {
        for (JJ in 1:3) if (Gen[Iline,JJ]!=Results$DatFile[Index5,JJ]) Found <- F
       }
      if (Found==T) OK <- Iline
     }  
    #if (Iout<=20) cat(as.numeric(c(Index5,OK,Gen[OK,1:7])),"\n")
    #if (Iout==20) AAA
    # Males or females
    if (Results$DatFile[Index5,4]==1 || (Results$DatFile[Index5,3]==1 & Results$DatFile[Index5,8]>0))
     {
      for (JJ in 1:Results$NumLengthBins[Iframe])
       Results$DatFile[Index5,8+JJ] <- Gen[OK,8+JJ]
     }  
    else
     {
      for (JJ in 1:Results$NumLengthBins[Iframe])
        Results$DatFile[Index5,8+JJ] <- Gen[OK,8+20+JJ]
     }
    
   }
  
 return(Results)  
}
# ==================================================================================================================

GenTrue <- function(Isim,Orig,Nrec,SigmaR)
{
  file.copy(from=paste0(Orig$OrigFolder,"gmacs.exe"),to=paste0(TrueFolder,"gmacs.exe"))
  
  Index <- FindLoc(Orig$ParFile,"rec_dev_est:",2)
  Par2 <- Orig$ParFile
  for (Iyr in 1:Nrec) Par2[Index+1,Iyr] <- as.numeric(Orig$ParFile[Index+1,Iyr])+rnorm(1,0,SigmaR)
  FileName <- paste(TrueFolder,"gmacs2.pin",sep="")
  write.table(Par2,FileName,col.names=F,row.names=F,quote=F)
  FileName <- paste(TrueFolder,"gmacs.dat",sep="")
  write.table(Orig$GmacsFile,FileName,col.names=F,row.names=F,quote=F)
  FileName <- paste(TrueFolder,Orig$DatFileName,sep="")
  write.table(Orig$DatFile,FileName,col.names=F,row.names=F,quote=F)
  Index1 <- FindLoc2 (Orig$CtlFile,"Maximum","function",3,6)
  Orig$CtlFile[Index1,1] <- 1
  FileName <- paste(TrueFolder,Orig$CtlFileName,sep="")
  write.table(Orig$CtlFile,FileName,col.names=F,row.names=F,quote=F)
  FileName <- paste(TrueFolder,Orig$PrjFileName,sep="")
  write.table(Orig$PrjFile,FileName,col.names=F,row.names=F,quote=F)
  setwd(TrueFolder)
  TheCall <- paste("gmacs.exe -apin gmacs2.pin -est",sep="")
  AA <- system(TheCall,intern=T)
  file.copy(from=paste0(TrueFolder,"gmacsall.out"),to=paste0(RunFolder,"gmacsAllT",Isim,".out"))

  FileName <- paste(TrueFolder,"SimData.out",sep="")
  SimFile <- read.table(FileName,col.names=seq(from=1,to=200),stringsAsFactors=F,fill=T,comment.char = "%")
  Use <- is.na(SimFile); SimFile[Use] <- ""
  
  # Reset and return
  setwd(BaseDir)
  return(SimFile)
  
}

# ==================================================================================================================

set.seed(781)
BaseDir <-  "C:\\Research\\gmacs\\Unified\\"
setwd(BaseDir)
Nsim <- 2
OrigFolder <- "C:\\Research\\gmacs\\Unified\\Build\\BBRKC\\"
TrueFolder <- "C:\\Research\\gmacs\\Unified\\Build\\True\\"
RunFolder <- "C:\\Research\\gmacs\\Unified\\Build\\Runs\\"
Orig <- ReadFiles(OrigFolder)
RunFile <- paste(RunFolder,"Run.bat",sep="")
write("",RunFile)
for (Isim in 1:Nsim)
 {  
  cat("Generating simulation ",Isim," of ",Nsim,"\n")
  Orig2Sim <- GenTrue(Isim,Orig,Nrec=46,SigmaR=0.6)
  Orig$SimFile <- Orig2Sim
  
  Results <- Orig
  Results <- GenData(Results,Determ=F)
  WriteFiles(RunFolder,Results,RunUsingPar=T,NoEst=F,Isim=Isim,Nsim=Nsim,RunFile=RunFile)
 }



