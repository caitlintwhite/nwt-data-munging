#clihomog.R.- Homogenization functions for the Climatol package.
#Author: Jose A. Guijarro. Licence: GPL >= 3.0
#(Most comments in Spanish)

climatol.version <- 'cr 4.0z'
#- cerrar.- Cerrar los archivos de salida.
cerrar <- function() {
  graphics.off()
  while (sink.number()>0) sink() #cerrar bitÃ¡cora(s)
}

#- xls2csv.- Read all excel files of a folder and write data in a csv file.
#' Read all excel files of a folder and write data in a csv file
#'
#' @param tmpdir temporal directory containing the files to read
#' @param archdir directory where to archive files after processing
#' @param var destination name of the variable
#' @param ext extension of the files. Either 'xlsx' (the default) or 'xls'
#' @param datcols data columns to be written to the output file
#' @param codesep character string separating the code from the rest of the file name ('_' by default)
#' @param dec='.': character to use as decimal point in the output file
#' @param sep=',': character separating data in the output file
#' @return This function writes output files without returning any value
#' @details Excel files must have one header line at the top. If they contain more, supplementary header lines should have at least one empty cell in the columns of date and data to be read.
#' @examples
#'
#' #Set temporal directories:
#' dir1 <- tempdir(); dir2 <- paste0(dir1,'-2'); dir.create(dir2)
#' dir1 <- tempdir(); dir2 <- tempdir()
#' wd0 <- setwd(dir1)
#' #Download example Excel files to dir1:
#' download.file('http://climatol.eu/pub/S034-Somewhere.xlsx','S034-Somewhere.xlsx')
#' download.file('http://climatol.eu/pub/S113-Green_Valley.xlsx','S113-Green_Valley.xlsx')
#' download.file('http://climatol.eu/pub/S154-Lost_city.xlsx','S154-Lost_city.xlsx')
#' #Now apply the function:
#' xls2csv(dir1,dir2,'TMax',datcols=c(1:3,5),codesep='-')
xls2csv <- function(tmpdir, archdir, var, ext='xlsx', datcols=1:4, codesep='_',
                    dec='.', sep=',') {
  if(tmpdir==archdir) stop('Please, set archdir different from tmpdir')
  library(readxl) #load library to read excel files
  fdat <- sprintf('all_%s_data.csv',var) #name of the data output file
  fsta <- sprintf('all_%s_stations.csv',var) #name of the stations output file
  Fs <- file(fdat,'a') #open data output file
  Ft <- file(fsta,'a') #open stations output file
  fich <- dir(tmpdir,sprintf('*.%s',ext)) #files in the tmpdir
  nf <- 0 #counter of processed files
  for(f in fich) { #for every file
    cat(f,'\n')
    code <-  strsplit(f,codesep)[[1]][1] #station code
    z <- try(d <- as.data.frame(readxl::read_excel(sprintf('%s/%s',tmpdir,f)))[,datcols])
    if (class(z)=="try-error") next
    nalines <- apply(is.na(d),1,sum)>0 #lines with one or more missing data
    d <- d[!nalines,]
    d <- sapply (d,as.numeric)
    d <- data.frame(code,d)
    write.table(d,Fs,dec=dec,sep=sep,row.names=FALSE,col.names=FALSE)
    write(code,Ft)
    file.rename(sprintf('%s/%s',tmpdir,f),sprintf('%s/%s',archdir,f))
    nf <- nf+1 #update no. of processed files
  }
  close(Fs)
  if(nf>0) {
    cat(sprintf('Data from %d %s/*.%s files have been saved into %s\n',nf,tmpdir,ext,fdat))
    cat(sprintf('Station codes list has been saved into %s\n',fsta))
    cat(sprintf('(Original files have been moved to the %s directory.)\n',archdir))
  } else cat(sprintf('No %s/*.%s files found!\n',tmpdir,ext))
}

#- csv2climatol.- Convert data in a single CSV file to CLIMATOL input format.
#Station codes, names and coordinates can go in a separate CSV file.
#Only prepared for daily or monthly values.
csv2climatol <- function(csvfile, datacol=6:8, stnfile=csvfile, stncol=1:5,
                         varcli, anyi=NA, anyf=NA, mindat=NA, sep=',', dec='.', na.strings='NA',
                         dateformat='%Y-%m-%d', cf=1, ndec=1, header=TRUE) {
  #csvfile: name of the CSV file containing the data
  #datacol: column(s) holding station codes, dates and data. If 4 (5) values
  #  are provided, dates are expected to appear as year, month (and days) in
  #  separate columns. Otherwise, dates will be provided as character strings
  #  (see parameter dateformat below)
  #stnfile: name of the CSV file containing station codes, names and
  #  coordinates (if these data are not in the csvfile)
  #stncol: columns holding longitudes, latitudes, elevations and station
  #  identifiers and names. At least station identifiers must be present in
  #  tablefile. Put a zero for every inexistent column. Example when only
  #  station identifiers are present, in column 1: stncol=c(0,0,0,1,0)
  #varcli: acronym of the climatic variable under study
  #anyi: first year to study
  #anyf: last year to study
  #mindat: minimum required number of data per station (by default, 60 monthly
  #  values or 1000 daily values)
  #sep: data separator (',' by default: Comma Separated Values)
  #dec: decimal point ('.' by default)
  #na.strings: strings coding missing data ('NA' by default)
  #dateformat: format of dates (if not in separate columns. Default '%Y-%m-%d')
  #cf: conversion factor to apply if data units need to be changed
  #ndec: no. of decimals to round to
  #header: TRUE by default, set to FALSE if csvfile has not a header
  #NOTE that if stnfile is different than csvfile, then sep, dec, na.strings and
  # header will be applied to both files.
  #----------------- Operation: -----------------------------------
  #read input table:
  d <- read.csv(csvfile,sep=sep,dec=dec,header=header,na.strings=na.strings,as.is=TRUE)
  #find out no. of stations and dates range:
  if(stnfile!=csvfile) stn <- read.csv(stnfile,sep=sep,dec=dec,header=header,na.strings=na.strings,as.is=TRUE)[,stncol]
  else stn <- unique(d[,stncol])
  stid <- stn[,4] #station codes
  dupl <- duplicated(stid)
  if(sum(dupl)>0) { #remove duplicated codes:
    zz <- unique(stn[dupl,4])
    cat('Codes with different names or coordinates:\n')
    print(stn[stn[,4]%in%zz,])
    cat('Only one version of names and coordinates will be kept!\n')
    moda <- function(x) names(which.max(table(x))) #mode function
    for(qz in zz) {
      kz <- which(stn[,4]==qz)
      for(j in 1:5) stn[kz,j] <- moda(stn[kz,j])
    }
    stn <- unique(stn); stid <- stn[,4] #updated station list and codes
  }
  ne <- length(stid) #no. of stations
  ldc <- length(datacol); jd <- datacol[ldc] #data column
  if(length(datacol)==3) fech <- as.Date(d[,datacol[2]],dateformat)
  else if(length(datacol)==4) fech <- as.Date(sprintf('%d-%02d-01',d[,datacol[2]],d[,datacol[3]]))
  else  fech <- as.Date(sprintf('%d-%02d-%02d',d[,datacol[2]],d[,datacol[3]],d[,datacol[4]]))
  xstep <- min(diff(sort(unique(fech)))) #minimum time interval
  if(xstep==1) { nm <- 0; if(is.na(mindat)) mindat <- 1000 } #daily values
  else { nm <- 12; if(is.na(mindat)) mindat <- 60 } #monthly values
  nas <- which(is.na(fech))
  z <- as.integer(strftime(range(fech,na.rm=TRUE),'%Y'))
  if(is.na(anyi)) anyi <- z[1] #initial year of data
  if(is.na(anyf)) anyf <- z[2] #final year of data
  #target dates vector:
  if(nm==0) x <- seq(as.Date(sprintf('%d-01-01',anyi)),as.Date(sprintf('%d-12-31',anyf)),1)
  else x <- seq(as.Date(sprintf('%d-01-01',anyi)),as.Date(sprintf('%d-12-01',anyf)),'1 month')
  nd <- length(x) #number of dates (=data per station)
  #initialize data matrix:
  dat <- matrix(NA,nd,ne)
  #populate data matrix:
  cat(sprintf('\nCreating %s input files for Climatol from %s ...\n',varcli,csvfile))
  for(i in 1:ne) { #for every station
    cat(sprintf(' %s',stid[i]))
    sel <- d[,datacol[1]]==stid[i] #select lines of current station
    ds <- d[sel,jd] #data 
    fe <- fech[sel] #dates
    kd <- match(fe,x) #match data dates with the dates vector
    #avoid "NAs are not allowed in subscripted assignments" error:
    z <- is.na(kd); if(sum(z>0)) { ds <- ds[!z]; kd <- kd[!z] }
    dat[kd,i] <- round(ds*cf,ndec)
  }
  cat('\n')
  #remove stations without mindat data:
  ndat <- apply(!is.na(dat),2,sum)
  sel <- ndat < mindat
  if(sum(sel)==ne) stop('Not enough data in any station. No files created!')
  if(sum(sel)>0) { dat <- dat[,!sel]; stn <- stn[!sel,] }
  #write data file:
  fich <- sprintf('%s_%s-%s.dat',varcli,anyi,anyf)
  write(dat,fich,ncolumns=ifelse(nm==0,10,12))
  cat('\nData saved to file',fich,':\n')
  print(summary(as.vector(dat)))
  #write stations file:
  fich <- sprintf('%s_%s-%s.est',varcli,anyi,anyf)
  stn[,1:3] <- sapply(stn[,1:3],as.numeric) #avoid coordinates as characters
  write.table(stn,fich,row.names=FALSE,col.names=FALSE)
  if(length(stncol==5)) {
    cat('\nStation coordinates and names saved to file',fich,':\n')
    names(stn) <- c('X (lon)','Y (lat)','Z (elev)','Code','Name')
    print(summary(stn))
  } else {
    cat('\nStation data saved to file',fich,'\n')
    cat('It should have columns: X (lon), Y (lat), Z (elev), Code, Name\n')
    cat('Please, edit the file to add the missing items in that order.\n\n')
  }
  if(length(nas)>0) {
    cat('Data lines with wrong dates (these data have been ignored):\n')
    print(d[nas,]); cat('\n')
  }
}

#- daily2climatol.- Convert daily data files to Climatol input format.
#' Convert daily data files to Climatol input format
#' 
#' @param stfile file with file names and station coordinates, codes and names
#' @param stcol columns in stfile holding file names, longitudes, latitudes,
#'  elevations and station codes and names. (Defaults to 1:6. Use 0 for codes
#'  and/or names columns if they are missing, and numeric values will be
#'  assigned.)
#' @param datcol columns in data files holding year,month,day,value (default to 1:4) 
#' @param varcli acronym of the climatic variable under study
#' @param anyi first year to study (defaults to the first year available in data)
#' @param anyf last year to study (defaults to the last year available in data)
#' @param mindat minimum required number of data per station
#' @param sep data separator ('' by default, meaning any white space)
#' @param dec decimal point ('.' by default)
#' @param na.strings strings coding missing data ('NA' by default)
#' @param header TRUE by default, set to FALSE if files do not have a header
#' @param skip no. of lines to skip at the top of data files (0 by default)
#' @return This function writes output files without returning any value
#' @examples
#' 
#' data(climatol_data) #load examples data
#' 
#' wd <- tempdir(); wd0 <- setwd(wd) #set a temporal directory
#' 
#' write.csv(climatol_data$WY003[,2:5],'S01.csv',row.names=FALSE)
#' 
#' write.csv(climatol_data$WY018[,2:5],'S02.csv',row.names=FALSE)
#' 
#' write.csv(climatol_data$WY020[,2:5],'S03.csv',row.names=FALSE)
#' 
#' df <- data.frame(c('S01.csv','S02.csv','S03.csv'),climatol_data$estTx.c)
#' 
#' names(df) <- c('File name',names(climatol_data$estTx.c))
#' 
#' write.csv(df,'stations.csv',row.names=FALSE)
#' 
#' #Now run the example:
#' 
#' daily2climatol('stations.csv',datcol=1:4,varcli='Temp',na.strings='-99.9')
#' 
#' setwd(wd0) #Return to user's working directory
#' 
#' #Input and output files of the example are in directory:
#' print(wd)
daily2climatol <- function(stfile, stcol=1:6, datcol=1:4, varcli='VRB',
                           anyi=NA, anyf=NA, mindat=365, sep=',', dec='.', na.strings='NA',
                           header=TRUE, skip=0) {
  #read stations file:
  st <- read.table(stfile,as.is=TRUE,sep=sep,dec=dec,header=header)
  ne <- nrow(st) #no. of stations
  if(is.na(anyi) | is.na(anyf)) { #check the time period of the data:
    cat('\nChecking the period covered by the data...\n')
    inidate <- as.Date('3000-12-31'); enddate <- as.Date('0001-01-01')
    for(i in 1:ne) { #for every station
      cat('',i)
      d <- read.table(st[i,stcol[1]],sep=sep,dec=dec,header=header,na.strings=na.strings)
      dates <- as.Date(sprintf('%d-%02d-%02d',d[,datcol[1]],d[,datcol[2]],d[,datcol[3]]))
      rdates <- range(dates,na.rm=TRUE) #range of dates in the file
      nadates <- is.na(dates)
      if(sum(nadates)>0) {
        cat('Abnormal dates found in file',st[i,stcol[1]],':\n')
        print(d[nadates,])
      }
      dates[is.na(d[,datcol[4]])] <- NA #remove dates without data
      rdates <- range(dates,na.rm=TRUE) #range of dates with data
      if(rdates[1]<inidate) inidate <- rdates[1]
      if(rdates[2]>enddate) enddate <- rdates[2]
    }
    cat('\n')
  } else {
    if(anyf<anyi) stop('Set initial year (anyi) lower or equal than final year (anyf)')
    inidate <- as.Date(sprintf('%d-01-01',anyi))
    enddate <- as.Date(sprintf('%d-12-31',anyf))
  }
  dates <- seq(inidate,enddate,by='1 day') #vector of dates
  nd <- length(dates) #number of dates (=data per station)
  cat(sprintf('%d days between %s and %s\n',nd,inidate,enddate))
  dat <- matrix(NA,nd,ne)
  #populate data matrix:
  cat('\nCreating',varcli,'input files for Climatol from daily files...:\n')
  for(i in 1:ne) { #for every station
    cat(sprintf('%3d %s\n',i,st[i,stcol[1]]))
    d <- read.table(st[i,stcol[1]],sep=sep,dec=dec,header=header,na.strings=na.strings)
    ddates <- as.Date(sprintf('%d-%02d-%02d',d[,datcol[1]],d[,datcol[2]],d[,datcol[3]]))
    kd <- match(ddates,dates) #match data dates with the dates vector
    #avoid "NAs are not allowed in subscripted assignments" error:
    if(sum(is.na(kd))>0) { d <- d[!is.na(kd),]; kd <- kd[!is.na(kd)] }
    ddat <- d[,datcol[4]]
    dat[kd,i] <- ddat
  }
  # dat[dat==mis] <- NA #use R missing data code
  #remove stations without mindat data:
  ndat <- apply(!is.na(dat),2,sum)
  sel <- ndat < mindat
  if(sum(sel)>0) { 
    cat('\nStations with less than',mindat,'data: ',which(sel),'\n')
    if(sum(sel)==ne) stop('No station has enough data!')
    dat <- dat[,!sel]; st <- st[!sel,]; ne <- nrow(st) }
  #write data file:
  anyi <- format(inidate,'%Y'); anyf <- format(enddate,'%Y')
  fich <- sprintf('%s_%s-%s.dat',varcli,anyi,anyf)
  write(dat,fich,ncolumns=10)
  cat('\nData saved to file',fich,':\n')
  print(summary(as.vector(dat)))
  #write stations file:
  nc <- ncol(st)
  #assign numeric codes and names if not provided:
  if(stcol[5]==0)  cod <- as.character(1:ne) else cod <- st[,stcol[5]]
  if(stcol[6]==0)  nam <- as.character(1:ne) else nam <- st[,stcol[6]]
  st <- cbind(st[,stcol[2:4]],cod,nam)
  fich <- sprintf('%s_%s-%s.est',varcli,anyi,anyf)
  write.table(st,fich,row.names=FALSE,col.names=FALSE)
  cat('\nStation coordinates and names saved to file',fich,':\n')
  names(st) <- c('X (lon)','Y (lat)','Z (elev)','Code','Name')
  print(summary(st))
}

#- rclimdex2climatol.- Convert DAILY data from RClimDex to CLIMATOL.
rclimdex2climatol <- function(stfile, sep='\t', kvar, varcli='',
                              chrcod=c(6,10), anyi=NA, anyf=NA, mis=-99.9, mindat=365) {
  #stfile: file with the data file names and station coordinates (HOMER format:
  #   'dataFile latDeg latMin latSec lonDeg lonMin lonSec elev stationName')
  #sep: column separator (tab by default)
  #kvar: RClimDex variable to extract: 1(RR), 2(TX), 3(TN)
  #chrcod: initial and final characters of data file names to use as codes
  #anyi: initial year to study (defaults to the first year available in data)
  #anyf: final year to study (defaults to the last year available in data)
  #mindat: minimum number of data per station
  if(varcli=='') varcli=c('RR','TX','TN')[kvar] #acronym of the variable
  cat('\nCreating',varcli,'Climatol input files from RClimDex files...:\n\n')
  st <- read.table(stfile,sep=sep,as.is=TRUE,header=TRUE) #stations
  ne <- nrow(st)
  if(is.na(anyi) | is.na(anyf)) { #check the time period of the data:
    inidate <- as.Date('3000-12-31'); enddate <- as.Date('0001-01-01')
    for(i in 1:ne) { #for every station
      d <- read.table(st[i,1])
      dates <- as.Date(sprintf('%d-%02d-%02d',d[,1],d[,2],d[,3]))
      rdates <- range(dates,na.rm=TRUE) #range of dates with data
      nadates <- is.na(dates)
      if(sum(nadates)>0) {
        cat('Abnormal dates found in file',st[i,1],':\n')
        print(d[nadates,])
      }
      if(rdates[1]<inidate) inidate <- rdates[1]
      if(rdates[2]>enddate) enddate <- rdates[2]
    }
  }else {
    if(anyf<anyi) stop('Set initial year (anyi) lower or equal than final year (anyf)')
    inidate <- as.Date(sprintf('%d-01-01',anyi))
    enddate <- as.Date(sprintf('%d-12-31',anyf))
  }
  dates <- seq(inidate,enddate,by='1 day') #vector of dates
  nd <- length(dates) #number of dates (=data per station)
  dat <- matrix(NA,nd,ne)
  #populate data matrix:
  for(i in 1:ne) { #for every station
    cat(st[i,1],'\n')
    d <- read.table(st[i,1]) #data
    ddates <- as.Date(sprintf('%d-%02d-%02d',d[,1],d[,2],d[,3]))
    kd <- match(ddates,dates) #match data dates with the dates vector
    #avoid "NAs are not allowed in subscripted assignments" error:
    if(sum(is.na(kd))>0) { d <- d[!is.na(kd),]; kd <- kd[!is.na(kd)] }
    ddat <- d[,kvar+3]
    dat[kd,i] <- ddat
  }
  dat[dat==mis] <- NA #use R missing data code
  #remove stations without mindat data:
  ndat <- apply(!is.na(dat),2,sum)
  sel <- ndat < mindat
  if(sum(sel)>0) { dat <- dat[,!sel]; st <- st[!sel,] }
  #write data file:
  anyi <- format(inidate,'%Y'); anyf <- format(enddate,'%Y')
  fich <- sprintf('%s_%s-%s.dat',varcli,anyi,anyf)
  write(dat,fich,ncolumns=10)
  cat('\nData from',format(inidate),'to',format(enddate),'saved to file',fich,'\n')
  #find longest period without concurrent missing data in all stations:
  avd=apply(!is.na(dat),1,sum)>0
  if(sum(!avd)>0) {
    rle=rle(avd)
    maxrle=which.max(rle$lengths)
    ki=diffinv(rle$lengths)[maxrle]+1
    kf=diffinv(rle$lengths)[maxrle+1]
    cat('The longest period without concurrent missing data in all stations\n')
    cat('  goes from',format(dates[ki]),'to',format(dates[kf]),'\n')
  }
  #write stations file:
  neg <- st[,5]<0; st[neg,5] <- -st[neg,5]
  X <- round(st[,5]+st[,6]/60.+st[,7]/3600.,6)
  X[neg] <- -X[neg]
  neg <- st[,2]<0; st[neg,2] <- -st[neg,2]
  Y <- round(st[,2]+st[,3]/60.+st[,4]/3600.,6)
  Y[neg] <- -Y[neg]
  cod <- substr(st[,1],chrcod[1],chrcod[2])
  if(ncol(st)>8) df <- data.frame(X,Y,st[,8],cod,st[,9])
  else df <- data.frame(X,Y,st[,8],cod,cod)
  fich <- sprintf('%s_%s-%s.est',varcli,anyi,anyf)
  write.table(df,fich,row.names=FALSE,col.names=FALSE)
  cat('Station coordinates and names saved to file',fich,'\n\n')
}

#- climatol2rclimdex.- Convert DAILY data from Climatol to RClimDex.
#Read homogenized data for the three daily variables, choose the reconstructions
#from the last homogeneous sub-period, and write them in an RClimDex file.
# varRR, varTX, varTN.- Name of the variables in the climatol files. If some
# variable is not available, name it as ''.
# yiRR, yfRR.- Initial and final years for the RR variable.
# yiTX, yfTX, yiTN, yfTN.- Initial and final years for the TX and TN variables.
# (By default they are the same for the three variables; otherwise, the output
# file will contain data for the common period only).
climatol2rclimdex <- function(varRR,varTX,varTN,yiRR,yfRR,yiTX=yiRR,
                              yfTX=yfRR,yiTN=yiRR,yfTN=yfRR,prefix='hoclm',dir=NA,na='-99.9',
                              nm=NA, dah=NA, nei=NA, est.c=NA) {
  anyi <- max(c(yiRR,yiTX,yiTN)) #initial year of the output
  anyf <- min(c(yfRR,yfTX,yfTN)) #final year of the output
  if(!is.na(dir)) if(!dir.exists(dir)) dir.create(dir) #output directory
  fech <- seq(as.Date(sprintf('%s-01-01',anyi)),as.Date(sprintf('%s-12-31',anyf)),by='1 day')
  ndd <- length(fech) #no. of daily data per station
  avl <- rep(FALSE,3) #availability flags
  cod <- NULL
  #-------- read results for the three daily variables (if available):
  #precipitation:
  if(varRR != '') {
    load(sprintf('%s_%d-%d.rda',varRR,yiRR,yfRR))
    if(nm>0) stop(sprintf('Data in %s_%d-%d.rda does not seem to be DAILY!',
                          varRR,yiRR,yfRR))
    #select series from last homogeneous fragments:
    fe <- seq(as.Date(sprintf('%s-01-01',yiRR)),as.Date(sprintf('%s-12-31',yfRR)),by='1 day')
    self <- match(fech,fe) #selected days
    dRR <- dah[self,1:nei] #selected data
    sRR <- unsufix(est.c[1:nei,4]) #selected stations
    if(length(sRR)>0) { avl[1] <- TRUE; cod <- sRR }
  }
  #maximum temperatures:
  if(varTX != '') {
    load(sprintf('%s_%d-%d.rda',varTX,yiTX,yfTX))
    if(nm>0) stop(sprintf('Data in %s_%d-%d.rda does not seem to be DAILY!',
                          varTX,yiTX,yfTX))
    #select series from last homogeneous fragments:
    fe <- seq(as.Date(sprintf('%s-01-01',yiTX)),as.Date(sprintf('%s-12-31',yfTX)),by='1 day')
    self <- match(fech,fe) #selected days
    dTX <- dah[self,1:nei] #selected data
    sTX <- unsufix(est.c[1:nei,4]) #selected stations
    if(length(sTX)>0) { 
      avl[2] <- TRUE
      if(is.null(cod)) cod <- sTX else cod <- intersect(cod,sTX)
    }
  }
  #minimum temperatures:
  if(varTN != '') {
    load(sprintf('%s_%d-%d.rda',varTN,yiTN,yfTN))
    if(nm>0) stop(sprintf('Data in %s_%d-%d.rda does not seem to be DAILY!',
                          varTN,yiTN,yfTN))
    #select series from last homogeneous fragments:
    fe <- seq(as.Date(sprintf('%s-01-01',yiTN)),as.Date(sprintf('%s-12-31',yfTN)),by='1 day')
    self <- match(fech,fe) #selected days
    dTN <- dah[self,1:nei] #selected data
    sTN <- unsufix(est.c[1:nei,4]) #selected stations
    if(length(sTN)>0) { 
      avl[3] <- TRUE
      if(is.null(cod)) cod <- sTN else cod <- intersect(cod,sTN)
    }
  }
  #-------- sort common station codes for the available variables:
  cod <- sort(cod)
  #-------- write RClimDex files (one per station):
  ne <- length(cod) #no. of stations
  cat('\nCreating',ne,'RClimDex files from Climatol homogenizations...:\n\n')
  for(i in 1:ne) { #for every station
    if(is.na(dir)) stfile <- sprintf('%s%s.txt',prefix,cod[i])
    else stfile <- sprintf('%s/%s%s.txt',dir,prefix,cod[i])
    cat(' ',stfile)
    dat <- matrix(NA,ndd,3)
    if(avl[1]) dat[,1] <- dRR[,which(sRR==cod[i])]
    if(avl[2]) dat[,2] <- dTX[,which(sTX==cod[i])]
    if(avl[3]) dat[,3] <- dTN[,which(sTN==cod[i])]
    df <- data.frame(format(fech,'%Y'),format(fech,'%m'),format(fech,'%d'),dat)
    write.table(df,stfile,sep='\t',quote=FALSE,row.names=FALSE,
                col.names=FALSE,na=na)
  }
  cat('\n')
}

#- sef2climatol.- Convert SEF DAILY data files to CLIMATOL input files.
#SEF stands for Station Exchange Format. Visit:
#           https://datarescue.climate.copernicus.eu/node/80
#Missing elevations will be assigned value 99
sef2climatol <- function(dr,Vbl,varcli=Vbl,ndec=1,mindat=NA) {
  #dr: directory containing the SEF files
  #Vbl: name of the variable in the SEF files
  #period: periodicity of the data: day (default), month, '3 hour', etc.
  #varcli: name of the variable in the Climatol destination files
  #ndec: number of decimals to save
  #mindat: minimum required number of data per station
  z <- try(source('csv2climatol.R'))
  if(class(z)=="try-error") stop('File csv2climatol.R not found')
  Fs <- file('SEFauxi.csv','w') #open auxiliary file
  for(fich in dir(dr)) { #for every file in directory dr
    cat(fich,'\n')
    Fe <- file(sprintf('%s/%s',dr,fich),'r') #open for reading
    li <- readLines(Fe,1)
    if(substr(li,1,3)!='SEF') { cat(':  Not a SEF file'); next }
    li <- readLines(Fe,1)
    cod <- unlist(strsplit(li,'\t'))[2] #station code
    li <- readLines(Fe,1)
    nom <- unlist(strsplit(li,'\t'))[2] #station name
    li <- readLines(Fe,1)
    Y <- as.numeric(unlist(strsplit(li,'\t'))[2]) #Y (longitude)
    li <- readLines(Fe,1)
    X <- as.numeric(unlist(strsplit(li,'\t'))[2]) #X (latitude)
    li <- readLines(Fe,1)
    Z <- unlist(strsplit(li,'\t'))[2]
    if(is.na(Z)) Z <- 99 else  Z <- as.numeric(Z) #Z (elevation)
    li <- readLines(Fe,3)
    vrb <- unlist(strsplit(li[3],'\t'))[2] #Vbl
    if(vrb!=Vbl) { cat(':  Not variable',Vbl); next }
    li <- readLines(Fe,4)
    d <- read.table(Fe,sep='\t') #data table
    nas <- is.na(d[,3])
    if(sum(nas)>0) d[nas,3] <- '01' #monthly values
    write(sprintf('%f,%f,%f,"%s","%s","%s",%s,%f',X,Y,Z,cod,nom,cod,
                  sprintf('%s-%s-%s',d[,1],d[,2],d[,3]),d[,7]),Fs)
    close(Fe)
  }
  close(Fs)
  cat('\n')
  csv2climatol('SEFauxi.csv',varcli=varcli,ndec=ndec,mindat=mindat,header=FALSE)
}

#- dahgrid.- GeneraciÃ³n de grids de datos homogeneizados.
dahgrid <- function(varcli, anyi, anyf, anyip=anyi, anyfp=anyf, grid,
                    mh=FALSE, std=NA, ini=NA, obsonly=TRUE, nmax=Inf, idp=2.0) {
  #anyip: aÃ±o inicial de referencia (para el cÃ¡lculo de las anomalÃ­as)
  #anyfp: aÃ±o final de referencia
  #grid: grid a interpolar, de clase SpatialPixel
  #mh: Si TRUE, leer datos mensuales de la homogeneizaciÃ³n diaria (*-mh_*.dat)
  #std: Incluido en el fichero *.rda, pero si mh=TRUE ese fichero no se lee,
  #     y entonces conviene especificarlo. (=3 por defecto)
  #ini: Incluido en el fichero *.rda, pero si mh=TRUE ese fichero no se lee,
  #     y entonces puede especificarse aquÃ­. (Por defecto, 1 de enero de anyi)).
  #obsonly: do not interpolate missing data estimated by homogen().
  #nmax: maximum number of nearest stations to use (all by default).
  #idp: power of the inverse distance weights (2 by default).
  if(!requireNamespace("sp", quietly=TRUE)
     | !requireNamespace("gstat", quietly=TRUE)
     | !requireNamespace("raster", quietly=TRUE)
     | !requireNamespace("ncdf4", quietly=TRUE)
  ) stop('This function requires packages sp, gstat, raster and ncdf4.\nPlease, install the lacking packages an re-run the function')
  if(anyip<anyi) stop("Asked initial reference year before first year of data!")
  if(anyfp>anyf) stop("Asked final reference year beyond last year of data!")
  #- lectura de los datos originales y homogeneizados
  if(!mh) {
    fbas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #raÃ­z nombres de fichero
    load(sprintf('%s.rda',fbas))
  } else { #leer los datos mensuales *-mh_*.dat (generados por dd2m)
    fbas <- sprintf('%s-mh_%d-%d',varcli,anyi,anyf) #raÃ­z nombres de fichero
    est.c <- read.table(sprintf('%s.est',fbas),colClasses=c('numeric','numeric','numeric','character','character','numeric','numeric','numeric','numeric'))
    ne <- nrow(est.c)
    dah <- scan(sprintf('%s.dat',fbas))
    nd <- length(dah)/ne
    est.b <- read.table(sprintf('%s-m_%d-%d.est',varcli,anyi,anyf),colClasses=c("numeric","numeric","numeric","character","character"))
    nei <- nrow(est.b)
    if(obsonly) { #leer datos originales y asegurar el orden correcto:
      dat <- scan(sprintf('%s-m_%d-%d.dat',varcli,anyi,anyf))
      if(!identical(est.b[1:nei,4],est.c[1:nei,4])) {
        dim(dat) <- c(nd,nei)
        dat <- dat[,match(est.c[1:nei,4],est.b[1:nei,4])]
      }
    }
    if(is.na(std)) std <- 3 #valor por defecto si mh=TRUE
    nm <- 12 #mh se usa solo con valores mensuales
  }
  if(length(dim(dah))>2) { #conversiÃ³n a 2 dimensiones:
    dim(dah) <- c(nd,ne); dim(dat) <- c(nd,nei)
  }
  #- seleccionar las series de los fragmentos mÃ¡s largos
  codo <- unsufix(est.c$Code) #original station codes of all series
  sel <- tapply(est.c$pod,codo,which.max)
  ksel <- rep(NA,nei)
  for(k in 1:nei) ksel[k] <- which(est.c[,7]==k)[sel[k]]
  #- retener solo los datos homogeneizados (dah) de las subseries mÃ¡s largas
  dah <- dah[,ksel]
  #- calcular sus medias y desviaciones tÃ­picas en el periodo escogido
  if(anyip==anyi & anyfp==anyf) { ki <- 1; kf <- nd } #pos. inicial y final
  else { ki <- (anyip-anyi)*nm+1; kf=ki+(anyfp-anyip+1)*nm-1 }
  m <- apply(dah[ki:kf,],2,mean)
  if(std>2) s <- apply(dah[ki:kf,],2,sd)
  #- grabarlas, con sus coordenadas, para su uso con GIS
  if(std<3) {
    df <- data.frame(est.c[ksel,1:4],m)
    names(df) <- c('X','Y','Z','Code','Means')
  } else {
    df <- data.frame(est.c[ksel,1:4],m,s)
    names(df) <- c('X','Y','Z','Code','Means','Std.Dev.')
  }
  fmeans <- sprintf('%s_%d-%d_msd.csv',varcli,anyip,anyfp)
  write.csv(df,fmeans,row.names=FALSE)
  #- normalizar las series
  switch(std,
         daz <- scale(dah,center=m,scale=FALSE), #std=1
         if(min(m)<1) { z <- which(m > 1)
         daz <- dah
         daz[,z] <- scale(dah[,z],center=FALSE,scale=m[z]) }
         else daz <- scale(dah,center=FALSE,scale=m),
         daz <- scale(dah,center=m,scale=s) #std=3 (default)
  )
  #- if(obsonly), blanquear los datos ausentes en las series originales
  if(obsonly) daz[is.na(dat)] <- NA
  rg <- range(daz,na.rm=TRUE) #rango de valores
  #- interpolar las medias (y desv. tÃ­picas), y grabarlas en NetCDF
  df <- data.frame(est.c[ksel,1:2],m)
  names(df) <- c('x','y','z')
  sp::coordinates(df) <- ~x+y
  m <- gstat::idw(z~1,df,grid,nmax=nmax,idp=idp,debug.level=0) #medias interp.
  dimLon <- ncdf4::ncdim_def(name='lon', units='degrees_east', vals=unique(grid@coords[,1]))
  dimLat <- ncdf4::ncdim_def(name='lat', units='degrees_north', vals=rev(unique(grid@coords[,2])))
  varCli.m <- ncdf4::ncvar_def(name=sprintf('%s.m',varcli), units='', dim=list(dimLon,
                                                                               dimLat), missval=NA, longname=sprintf('%s %d-%d means',varcli,anyip,anyfp))
  listvar <- list(varCli.m)
  nc <- ncdf4::nc_create(sprintf('%s_m.nc',fbas), listvar) #abrir el fichero netcdf
  zz <- raster::rasterFromXYZ(m)
  ncdf4::ncvar_put(nc,varCli.m,zz@data@values)
  ncdf4::nc_close(nc)
  if(std>2) {
    df <- data.frame(est.c[ksel,1:2],s)
    names(df) <- c('x','y','z')
    sp::coordinates(df) <- ~x+y
    s <- gstat::idw(z~1,df,grid,nmax=nmax,idp=idp,debug.level=0) #d.tÃ­p.interp.
    varCli.s <- ncdf4::ncvar_def(name=sprintf('%s.s',varcli), units='',
                                 dim=list(dimLon, dimLat), missval=NA,
                                 longname=sprintf('%s %d-%d std. deviations',varcli,anyip,anyfp))
    listvar <- list(varCli.s)
    nc <- ncdf4::nc_create(sprintf('%s_s.nc',fbas), listvar) #abrir el fichero netcdf
    zz=raster::rasterFromXYZ(s)
    ncdf4::ncvar_put(nc,varCli.s,zz@data@values)
    ncdf4::nc_close(nc)
  }
  #- === crear un netcdf con los grids interpolados en cada paso de tiempo
  if(is.na(ini)) ini <- sprintf('%d-01-01',anyi) #fecha inicial por defecto
  if(nm>0) x <- seq(as.Date(ini),length.out=nd,by=sprintf('%d months',12/nm))
  else x <- seq(as.Date(ini),length.out=nd,by='1 day')
  dimTime <- ncdf4::ncdim_def(name='Date', units='days since 1970-01-01',
                              vals=as.numeric(x), calendar='standard')
  varCli <- ncdf4::ncvar_def(name=varcli, units='', dim=list(dimLon, dimLat,
                                                             dimTime), missval=NA)
  listvar <- list(varCli)
  nc <- ncdf4::nc_create(sprintf('%s.nc',fbas), listvar) #abrir el fichero netcdf
  #- para cada paso de tiempo:
  cat(sprintf('Interpolating %d grids...:      ',nd))
  kz <- max(10,round(nd/100))
  for(k in 1:nd) {
    if(!k%%kz) cat('\b\b\b\b\b',sprintf('%2s %%',round(k*100/nd)))
    #- interpolar (IDW) la variable estandarizada a los puntos del grid
    df <- data.frame(est.c[ksel,1:2],daz[k,])
    if(obsonly) df <- df[!is.na(df[,3]),]
    names(df) <- c('x','y','z')
    sp::coordinates(df) <- ~x+y
    z <- gstat::idw(z~1,df,grid,nmax=nmax,idp=idp,debug.level=0) #d.tÃ­p.interp.
    #pasar de SpatialPointsDataFrame a RasterLayer:
    zz=raster::rasterFromXYZ(z)
    #- grabar los valores en el netcdf
    ncdf4::ncvar_put(nc,varCli,zz@data@values,start=c(1,1,k),count=c(-1,-1,1))
  }
  cat(' (done)\n\n')
  #- cerrar el netcdf y terminar
  ncdf4::nc_close(nc)
  cat(sprintf('Normalized grids (%f to %f) saved to file %s.nc',rg[1],rg[2],fbas),'\n')
  cat('Means')
  if(std>2) cat(' and standard deviations')
  cat(' (of the whole series) saved to files\n')
  cat(sprintf('%s_m.nc',fbas))
  if(std>2) cat(',',sprintf('%s_s.nc',fbas))
  cat(' and',fmeans,'\n\n')
}

#- dahstat.- EstadÃ­sticas de datos homogeneizados.
#' Extract homogenized series or calculate various statistics from them
#'
#' @param  varcli acronym of the homogenized climatic variable
#' @param anyi first year of the homogenized series
#' @param anyf last year of the homogenized series
#' @param anyip first year for the statistical calculation
#' @param anyfp last year for the statistical calculation
#' @param stat statistic to calculate (one of "me"(means), "mdn"(medians), "max"(maxima), "min"(minima), "std"(standard deviations), "q"(quantiles), "tnd"(OLS trends and their p-values), "series"(none, just save homogenized series into a CSV file, plus another file with flags)
#' @param ndc no. of decimals (defaults to that used in the homogenization)
#' @param vala annual value: 0(none), 1(sum), 2(mean), 3(maximum), 4(minimum)
#' @param valm monthly value: 1(sum), 2(mean), 3(maximum), 4(minimum)
#' @param cod list of station codes to use (all by default)
#' @param prob probability to calculate quantiles (0.5 by default)
#' @param all if TRUE, all reconstructed series will be used. The default is FALSE, hence using only the series reconstructed from the last homogeneuos subperiod
#' @param long if TRUE (the default is FALSE), only series reconstructed from the longest homogeneuos subperiod will be used
#' @param lerr if TRUE (the default is FALSE), only series with the lowest RMSE will be used
#' @param relref set to TRUE to use also any added reliable reference series
#' @param pernyr no. of years on which to express trend units (10 by default)
#' @param estcol columns of est.c to include in the output tables (defaults to c(1,2,4): coordinates and station codes)
#' @param sep column separator symbol (',' by default)
#' @param dec decimal separator ('.' by default)
#' @examples
#' @dontrun
#' #after having run the example of the homogen() function, on the same
#' #working directory you can run:
#'
#' dahstat('Ptest',1951,2010) #get monthly precipitation means
#' dahstat('Ptest',1951,2010,stat='tnd') #get monthly OLS trends
#'
#' #look at the results in the temporary working directory
#' #then return to your former working directory
dahstat <- function(varcli, anyi, anyf, anyip=anyi, anyfp=anyf, stat="me",
                    ndc=NA, vala=2, valm=vala, cod=NULL, prob=.5, all=FALSE, long=FALSE,
                    lerr=FALSE, relref=FALSE, pernyr=10, estcol=c(1,2,4), sep=',', dec='.') {
  #- inicializaciones
  if(valm==0) valm <- 2 #valm es necesario para los agregados mensuales
  na <- anyf-anyi+1 #no. de aÃ±os
  if(anyi>anyf) stop ('First year of data greater than the last year!')
  if(anyip<anyi) stop("Asked initial year before first year of data!")
  if(anyfp>anyf) stop("Asked final year beyond last year of data!")
  #funciÃ³n elegida para el cÃ¡lculo de los valores mensuales:
  fun <- c("mean","median","max","min","sd","quantile")[which(c("me","mdn",
                                                                "max","min","std","q","tnd")==stat)]
  lmcoef <- function(y,x) coef(summary(lm(y~x)))[2,c(1,4)] #coef. regresiÃ³n
  #- si no se reconoce la opciÃ³n stat, terminar aquÃ­
  if(!length(fun) & stat!='series') {
    cat('stat should be one of "mean","median","max","min","sd","quantile"\n')
    stop(sprintf("Option stat='%s' not recognized!",stat))
  }
  if(stat=='q') {
    if(length(prob)>1) stop('Please, provide a unique probability to calculate quantiles')
    if(prob<0 | prob>1) stop('prob must be a value between 0 and 1')
  }
  mes3 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct",
            "Nov","Dec")
  #- leer los datos de entrada
  load(sprintf('%s_%d-%d.rda',varcli,anyi,anyf))
  codo <- unsufix(est.c$Code) #original station codes of all series
  estvar <- names(est.c)
  if(nm==1 | stat=='series') vala <- 0 #valor anual innecesario
  else {
    if(nm==0) {
      z <- seq(as.Date(sprintf('%s-01-01',anyi)),as.Date(sprintf('%s-12-31',anyf)),1)
      if(length(z)!=nd) stop('Statistics need complete years to be calculated')
    }
    if(vala<1 | vala>4) vala <- 2 #valor medio en caso de vala errÃ³neo
    funa <- c("sum","mean","max","min")[vala] #funciÃ³n para el valor anual
  }
  #- calcular la posiciÃ³n de los datos del periodo solicitado:
  if(anyip!=anyi | anyfp!=anyf) {
    yy <- as.integer(strftime(x,'%Y'))
    xk <- min(which(yy==anyip)):max(which(yy==anyfp))
  } else xk <- 1:nd
  #- seleccionar estaciones solicitadas:
  esel <- rep(TRUE,ne)
  if(!is.null(cod)) {
    ksel <- which(codo %in% cod) #estaciones solicitadas
    esel[-ksel] <- FALSE
  } else cod <- est.c[1:nei,4]
  if(!all & ne>nei) esel[(nei+1):ne] <- FALSE #solo Ãºltimos fragmentos
  else if(long | lerr) {
    lsel <- rep(TRUE,length(esel)) #inicializar vector
    for(ko in 1:nei) { #para cada estaciÃ³n original
      kest <- which(codo==est.c$Code[ko]) #series de la misma estaciÃ³n ko
      if(length(kest)>1) { #si hay mÃ¡s de un fragmento...
        if(long) ksel <- which.max(est.c$pod[kest]) #mayor % de datos originales
        else ksel <- which.min(est.c$rmse[kest]) #menor error cuadrÃ¡tico medio
        lsel[kest[-ksel]] <- FALSE #selecciÃ“n deseada
      }
    }
    esel <- esel & lsel
  }
  #eliminar las estaciones de referencia confiables:
  if(!relref) esel <- esel & substr(est.c$Code,1,1)!='*'
  if(sum(esel)==0) stop("No station selected! (No output)")
  codo <- codo[esel] #original station codes
  est.c <- est.c[esel,] #estaciones seleccionadas
  #- seleccionar datos del periodo y las estaciones solicitados
  if(length(dim(dah))==3) { dim(dah) <- c(nd,ne); dim(dat) <- c(nd,nei) }
  dah <- dah[xk,esel]; dat <- dat[xk,esel[1:nei]] #datos seleccionados
  #actualizar parÃ¡metros:
  x <- x[xk]; ne <- sum(esel); nei <- length(cod); nd <- length(x)
  na <- anyfp-anyip+1 #no. de aÃ±os
  iest <- match(codo,cod) #Ã­ndice de estaciones originales
  #- if(stat=="series"), listar las series y sus flags en formato CSV
  if(stat=='series') { #series secuenciales, en dos ficheros (datos y flags):
    #comparaciÃ³n datos homogeneizados con originales (no usar '=='!):
    df <- abs(dah-dat[,iest]) < 1e-9
    df <- as.numeric(df) #TRUE=1, FALSE=0
    df[df==0] <- 2 #datos distintos a los originales
    df[df==1] <- 0 #datos iguales a los originales
    df[is.na(df)] <- 1 #datos rellenados (originales ausentes)
    dim(df) <- dim(dah)
    #nombre de los ficheros de salida:
    ard <- sprintf('%s_%d-%d_series.csv',varcli,anyi,anyf)
    arf <- sprintf('%s_%d-%d_flags.csv',varcli,anyi,anyf)
    dah <- data.frame(cbind(format(x),dah[,order(iest)]))
    df  <- data.frame(cbind(format(x),df[,order(iest)]))
    colnames(dah) <- colnames(df) <- c('Date',est.c[order(iest),4])
    write.table(dah,ard,row.names=FALSE,quote=FALSE,sep=',',dec='.')
    write.table(df,arf,row.names=FALSE,quote=FALSE,sep=',',dec='.')
    cat(sprintf('Homogenized values written to %s,\nwith flags in %s:\n',ard,arf))
    cat('  0: Observed data\n')
    cat('  1: Missing data (filled in)\n')
    cat('  2: Corrected data\n')
    return(invisible())
  }
  #- if(nm==0) calculate monthly aggregates:
  if(nm==0) {
    cat('Computing monthly aggregates... ')
    me <- strftime(x,"%m"); anyo <- strftime(x,"%Y")
    dm <- matrix(NA,na*12,ne) #datos mensuales
    funm <- c("sum","mean","max","min")[valm] #funciÃ³n para el valor mensual
    for(ie in 1:ne) {
      z <- aggregate(dah[,ie],list(me,anyo),funm)
      dm[,ie] <- round(z[,3],ndec)
    }
    cat('Done.\n')
    nm=12; dah <- dm
  }
  dim(dah) <- c(nm,na,ne)
  #- if(vala), calcular los valores anuales
  if(vala) { #calcular los valores anuales
    aval <- as.vector(apply(dah,2:3,funa))
    dim(dah) <- c(nm,na*ne)
    dah <- rbind(dah,aval)
    nm <- nm+1
    dim(dah) <- c(nm,na,ne)
  }
  #dimensionar valores a calcular:
  val <- matrix(NA,ne,nm)
  #- if(stat=="tnd"), calcular las tendencias
  if(stat=="tnd") {
    if(!is.na(ndc)) ndec <- ndc else ndec <- ndec+1 #aÃ±adir un decimal
    pval <- val #matriz para almacenar los p-valores
    if(ne==1) {
      z <- apply(dah,1,lmcoef,x=anyip:anyfp)
      val <- t(as.data.frame(round(z[1,]*pernyr,ndec)))
      pval <- t(as.data.frame(round(z[2,],3)))
    } else {
      z <- apply(dah,c(3,1),lmcoef,x=anyip:anyfp)
      val <- round(z[1,,]*pernyr,ndec)
      pval <- round(z[2,,],3)
    }
  }
  #- else, aplicar la funciÃ³n deseada
  else {
    for(i in 1:ne) {
      if(nm==1) {
        if(stat=="q") val[i,] <- round(eval(call(fun,dah,prob)),ndec)
        else val[i,] <- round(eval(call(fun,dah[,,i])),ndec)
      }
      else { #datos mensuales:
        if(stat=="q") val[i,] <- round(apply(dah[,,i],1,fun,prob),ndec)
        else val[i,] <- round(apply(dah[,,i],1,fun),ndec)
      }
    }
  }
  #- imprimir mensaje con los ficheros generados
  if(stat=="me") cat("Mean")
  else if(stat=="mdn") cat("Median")
  else if(stat=="max") cat("Maximum")
  else if(stat=="min") cat("Minimum")
  else if(stat=="std") cat("Standard deviation")
  else if(stat=="q") cat(prob,"prob. quantile")
  else if(stat=="tnd") cat("Trend")
  cat(" values of ",varcli," (",anyip,"-",anyfp,")",sep="")
  if(stat=="tnd") cat(", expressed in units per ",pernyr," years,",sep="")
  dahs <- data.frame(cbind(est.c[,estcol],val))
  if(nm==12) ndf <- c(estvar[estcol],mes3)
  else if(nm==13) ndf <- c(estvar[estcol],mes3,"Annual")
  else if(nm<2) ndf <- c(estvar[estcol],"Value")
  else ndf <- c(estvar[estcol],1:nm)
  names(dahs) <- ndf
  #- grabar los valores en los ficheros
  #fichero de salida:
  if(stat=="q") ars <- sprintf('%s_%d-%d_%s%d.csv',varcli,anyip,anyfp,stat,round(100*prob))
  else ars <- sprintf('%s_%d-%d_%s.csv',varcli,anyip,anyfp,stat)
  write.table(dahs[order(est.c[,4]),],ars,row.names=FALSE,sep=',',dec='.')
  cat("\n  written to",ars,"\n")
  if(stat=="tnd") { #grabar los p-valores
    dahs2 <- data.frame(cbind(est.c[estcol],pval))
    names(dahs2) <- ndf
    ars <- sprintf('%s_%d-%d_pval.csv',varcli,anyip,anyfp)
    write.table(dahs2[order(est.c[,4]),],ars,row.names=FALSE,sep=',',dec='.')
    cat("P-values written to",ars,"\n")
  }
}

#- datsplit.- Split data files in [overlapping] "rectangular" areas.
datsplit <- function(varcli, anyi, anyf, xc, yc, xo=0, yo=0,
                     maponly=FALSE, minst=5, deg=TRUE) {
  #varcli: Climate variable under study (acronym)
  #anyi, anyf: First and last years of data.
  #xc, yc: Vectors of X and Y axis coordinates setting splitting meridians and parallels.
  #xo, yo: Overlapping width in the East-West and North-South directions. 0 by default (no overlapping).
  #maponly: If TRUE, only create a map of the resulting areas.
  #minst: Minimum number of stations in an area (5 by default).
  #deg: Coordinates in degrees? (TRUE by default).
  f.bas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #base name
  est.c <- read.table(sprintf('%s.est',f.bas),colClasses=c("numeric","numeric","numeric","character","character"))
  ne <- nrow(est.c); na <- anyf-anyi+1
  dat <- scan(sprintf('%s.dat',f.bas))
  nd <- length(dat)/ne
  dim(dat) <- c(nd,ne)
  z <- nd/na
  if(z>=1) nm <- ceiling(z)
  if(nm > 12) nm <- 0 #datos diarios
  #if not given, set buffer margins as 10% of the mean sub-areas width:
  nxc <- length(xc) #no. of x cut borders
  nyc <- length(yc) #no. of y cut borders
  #--- save a map of available stations and split areas:
  f.map <- sprintf('%s-map.pdf',f.bas)
  main=paste('Split areas of the',ne,'available',varcli,'stations')
  pdf(f.map,bg='white')
  if(deg) {
    asp=1/(cos(mean(range(est.c[,2]))*pi/180)) #aspect ratio
    plot(est.c[,1:2],pch='+',col=hsv(.6,.7,1),asp=asp,xlab="Longitude (deg)",ylab="Latitude (deg)",main=main)
    try(maps::map('world',add=TRUE))
  } else plot(est.c[,1:2],pch='+',asp=1,xlab="X",ylab="Y",main=main)
  grid(col=gray(.4))
  abline(h=yc,col=2); abline(v=xc,col=2)
  abline(h=yc+yo,col=3); abline(h=yc-yo,col=3)
  abline(v=xc+xo,col=3); abline(v=xc-xo,col=3)
  mtext(paste('xc=',paste(xc,collapse=','),'   xo=',xo,sep=''),3)
  mtext(paste('yc=',paste(yc,collapse=','),'   yo=',yo,sep=''),4)
  graphics.off()
  if(maponly) {
    cat('Split areas and station map saved as',f.map,'\n')
    cat('No further action required\n')
    return(invisible())
  }
  #--- split data into specified rectangular areas:
  Fs <- file(sprintf('%s_%d-%d.split',varcli,anyi,anyf),'w')
  noa <- 0 #no. of overlapping areas
  cat('Areas:')
  for(i in 1:(nyc-1)) { #for every y interval
    for(j in 1:(nxc-1)) { #for every x interval
      sel <- est.c[,1]>=xc[j]-xo & est.c[,1]<xc[j+1]+xo & est.c[,2]>=yc[i]-yo & est.c[,2]<yc[i+1]+yo
      if(sum(sel)<minst) next #not enough stations: skip area
      noa <- noa + 1 #increase no. of areas
      cat(' ',noa)
      basef <- sprintf('%s-%d_%d-%d',varcli,noa,anyi,anyf) #base file name
      write.table(est.c[sel,],sprintf('%s.est',basef),row.names=FALSE,
                  col.names=FALSE)
      write(dat[,sel],sprintf('%s.dat',basef),ncolumns=max(c(10,nm),na.rm=TRUE))
      write(sprintf('%f %f %f %f',xc[j],xc[j+1],yc[i],yc[i+1]),Fs)
    }
  }
  cat('\n')
  close(Fs)
}

#- datsubset.- Subset data by selecting a subperiod and/or less missing data.
datsubset <- function(varcli,anyi,anyf,anyis=anyi,anyfs=anyf,minny=NA,ini=NA) {
  #anyis, anyfs= first and last year for data subsetting.
  #ninny= Minimum number of years with data to subset.
  #initial date (if it does not begin in January 1st)
  if(anyis==anyi & anyfs==anyf & is.na(minny)) stop('No subsetting required!\n')
  if(anyis<anyi) stop("Asked initial selected year before first year of data!")
  if(anyfs>anyf) stop("Asked final selected year beyond last year of data!")
  na <- anyf-anyi+1 #no. of years in original files
  nas <- anyfs-anyis+1 #no. of years in selected subperiod
  fbas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #raÃ­z nombres de fichero
  fbas2 <- sprintf('%s_%d-%d',varcli,anyis,anyfs) #raÃ­z nombres ficheros salida
  est.c <- read.table(sprintf('%s.est',fbas),colClasses=c("numeric","numeric","numeric","character","character"))
  ne <- nrow(est.c) #no. de estaciones
  dat <- scan(sprintf('%s.dat',fbas))
  numdat <- length(dat) #no. de datos leÃ­dos
  nd <- numdat/ne #no. de datos por estaciÃ³n
  dim(dat) <- c(nd,ne) #conversiÃ³n de vector a matriz
  #calcular no. de datos por aÃ±o y estaciÃ³n:
  nm <- nd/na
  if(nm>=1) nm <- ceiling(nm)
  if(nm > 12) nm <- 0 #datos diarios
  else if(!nm%in%c(1,2,3,4,6,12)) {
    cat('Calcultated no. of data per year/station:',nm,'\n')
    cat('but it should be one of 1, 2, 3, 4, 6 or 12.\n')
    stop('Unconsistent no. of data per year/station.')
  }
  #generar vector temporal (x):
  if(nm>0) tinc <- sprintf('%d months',12/nm) else tinc <- '1 day'
  if(is.na(ini)) x <- seq(as.Date(sprintf('%d-01-01',anyi)),length.out=nd,by=tinc)
  else x <- seq(as.Date(ini),length.out=nd,by=tinc)
  if(fbas==fbas2) { #renombrar ficheros de entrada para no pisarlos:
    file.rename(sprintf('%s.dat',fbas),sprintf('%s-ori.dat',fbas))
    file.rename(sprintf('%s.est',fbas),sprintf('%s-ori.est',fbas))
    cat(sprintf('Original files renamed to %s-ori.dat and %s-ori.est\n',fbas,fbas))
  }
  if(nas < na) { #subset a subperiod of data
    xa <- strftime(x,"%Y") #aÃ±os de cada dato
    sel <- xa>=anyis & xa<=anyfs
    dat <- dat[sel,]
  }
  if(!is.na(minny)) { #subset data with no. of years with data >= minny
    nad <- apply(!is.na(dat),2,sum) #no. of available data per station
    if(nm>0) nyd <- nad/nm else nyd <- floor(nad/365.25)#no. of years w data
    sel <- nyd >= minny; nes <- sum(sel) #no. of selected stations
    if(nes==0) stop ('No series meet the requirements!')
    if(nes < ne) {
      dat <- dat[,sel]
      est.c <- est.c[sel,]
    }
  }
  #write output files:
  if(nm>0) ncl <- nm else ncl <- 10
  write(dat,sprintf('%s.dat',fbas2),ncolumns=ncl)
  write.table(est.c,sprintf('%s.est',fbas2),col.names=FALSE,row.names=FALSE)
  cat(sprintf('Subset data written to files %s.dat and %s.est\n',fbas2,fbas2))
}

#- db2dat.- Get data from a database and build input files *.dat and *.est for
#the homogen() function. (ODBC must be intalled and properly configured.)
# ----------------------------------------------------------------------
#Example for a database called "climate", with user "USER" and password "PASS":
# R  #start R (version 3 or higher)
# library(RODBC)
# ch <- odbcConnect("climate",uid="USER",pwd="PASS") #connect to database
# db2dat('HRel',1961,2015,10,FALSE,ch,'%Y-%m-%d','monthly_relhum','Station',
# 'Date','Value','stations','Station','Name','Longitude','Latitude','Elevation')
# odbcClose(ch) #close connection to mcheng
# ----------------------------------------------------------------------
# This example will compile monthly average relative humidity for the period
# 1961-2015 excluding series with less than 10 years of data (120 monthly data)
# in files HRel_1961-2015.dat and HRel_1961-2015.est, which you can
# homogenize later with the Climatol R package with, e.g.:
# library(climatol)
# homogen('HRel',1961,2015,vmin=0,vmax=100)
# -------------------------------------------------------------------
db2dat <- function(varcli,anyi,anyf,minny=5,daily=TRUE,ch,
                   dformat='%Y-%m-%d',vtable,vcode,vdate,vval,stable,scode,sname,sx,sy,sz) {
  #varcli: Achronym of the climatic variable under study
  #anyi:   Fist year of the study period
  #anyf:   Last year of the study period
  #minny:  Minimum number of years with data in the series to study
  #ch:     Name of the ODBC conexion to the database
  #dformat:Format of dates in the database
  #vtable: Name of the table containing our climatic variable
  #vcode:  Name of the variable containing station codes in the database
  #vdate:  Name of the variable containing dates in the database
  #vval:   Name of the climatic variable in the database
  #stable: Name of the table containing station information (metadata)
  #scode:  Name of the variable containing station codes
  #sname:  Name of the variable containing station names
  #sx:     Name of the variable containing longitudes (degrees with decimals!)
  #sy:     Name of the variable containing latitudes (degrees with decimals!)
  #sz:     Name of the variable containing elevations (meters)
  #- initializations
  na <- anyf-anyi+1 #no. of years
  if(na<=0) stop('Last year must be greater than the first year')
  fini <- sprintf('%d-01-01',anyi)
  if(daily) {
    x <- seq(as.Date(sprintf('%d-01-01',anyi)),as.Date(sprintf('%d-12-31',anyf)),by='1 day')
    ndmin <- round(minny*365.25) #min. no. of daily data
    ffin <- sprintf('%d-12-31',anyf)
  } else {
    x <- seq(as.Date(sprintf('%d-01-01',anyi)),as.Date(sprintf('%d-12-01',anyf)),by='1 month')
    ndmin <- minny*12 #min. no. of monthly data
    ffin <- sprintf('%d-12-01',anyf)
  }
  nd <- length(x) #no. of data per station
  #- read station names and coordinates
  cat('Getting station names and coordinates...\n')
  ds <- RODBC::sqlQuery(ch,sprintf("SELECT %s, %s, %s, %s, %s FROM %s", sx,sy,sz,scode,sname,stable,scode))
  ds[,1:2] <- round(ds[,1:2],5) #round coordinates to 5 decimals
  ds[,3] <- round(ds[,3],1) #round elevations to 1 decimal
  ds[,4] <- as.character(ds[,4]) #force codes as character strings
  ds[,5] <- as.character(ds[,5]) #force names as character strings
  ds <- ds[order(ds[,4]),] #order stations by code
  ns <- nrow(ds); ndat <- rep(0,nd)
  #- open data and stations files
  dfile <- sprintf('%s_%d-%d.dat',varcli,anyi,anyf)
  efile <- sprintf('%s_%d-%d.est',varcli,anyi,anyf)
  Fd <- file(dfile,'w')
  Fe <- file(efile,'w')
  #- get data from the ODBC connection, station by station
  cat('Getting data for every station...\n')
  ne <- 0
  for(i in 1:ns) { #for every station
    cat(unlist(ds[i,]),'\n')
    if(sum(is.na(ds[i,]))>0) {
      cat('Warning: Incomplete metadata (station skipped)\n')
      next
    }
    dd <- RODBC::sqlQuery(ch,sprintf("SELECT %s,%s FROM %s WHERE %s >= '%s' AND %s <= '%s' AND %s = '%s'",vdate,vval,vtable,vdate,fini,vdate,ffin,vcode,ds[i,4]))
    if(is.null(dim(dd))) next #no data for the variable at this station
    if(sum(!is.na(dd[,2])) < ndmin) next #not enough data
    dd[,1] <- as.Date(dd[,1],format=dformat,tz='') #force vdate to class Date
    k <- match(dd[,1],x) #match data time steps
    if(sum(is.na(k))>0) {
      cat('Warning: Station skipped because some or all of its dates do not match the expected values\n')
      next
    }
    dat <- rep(NA,nd) #initialize data vector
    dat[k] <- dd[,2] #assign data
    write(dat,Fd,ncolumns=ifelse(daily,10,12)) #write into data file
    write.table(ds[i,],Fe,row.names=FALSE,col.names=FALSE) #write metadata
    ne <- ne + 1 #count no. of saved series
    ndat <- ndat + !is.na(dat) #count no. of data at every time step
  }
  #close files:
  close(Fe); close(Fd)
  cat(sprintf('\nFiles %s and %s successfully generated.',dfile,efile))
  #check data availability along time:
  if(min(ndat)==0) {
    ks <- which(ndat==0)
    cat(sprintf(' BUT:\nNo data available in any station for %s',ifelse(daily,'day','month')))
    if(length(ks)>1) cat('s:\n') else cat(' ')
    print(x[ks])
    cat(sprintf('Add stations or shorten the study period to avoid %s without data\n',ifelse(daily,'days','months')))
  } else cat('\n')
}

#- dd2m.- CÃ¡lculo de valores mensuales a partir de datos diarios o subdiarios.
dd2m <- function(varcli, anyi, anyf, anyip=anyi, anyfp=anyf, ndec=1, suf=NA,
                 valm=2, namax=10, na.strings="NA", homog=FALSE, fech=NULL, ini=NA) {
  #suf: sufijo opcional a aÃ±adir al nombre de la variable para leer los datos.
  #valm: Valor mensual (1=suma, 2=media, 3=mÃ¡ximo, 4=mÃ­nimo, 5=desv. tÃ­pica)
  #namax: MÃ¡ximo no. permitido de datos diarios ausentes originalmente
  #homog: Usar datos ya homogeneizados? (poner homog=TRUE)
  #fech: Vector temporal. (Por defecto se calcularÃ¡ automÃ¡ticamente). 
  #  Necesario si los datos son subdiarios o a intervalos irregulares.
  #ini: Fecha inicial. Si es NA se supone que es el 1 de enero de anyi
  fbas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #raÃ­z nombres de fichero
  if(is.na(suf)) fntr <- fbas #raÃ­z nombres ficheros de entrada
  else fntr <- sprintf('%s-%s_%d-%d',varcli,suf,anyi,anyf)
  if(homog) {
    load(sprintf('%s.rda',fntr)) #leer datos homogeneizados y originales
    fech <- x #vector temporal de los datos homogeneizados
  } else {
    dah <- scan(sprintf('%s.dat',fntr),na.strings=na.strings) #datos originales
    est.c <- read.table(sprintf('%s.est',fntr),colClasses=c("numeric","numeric","numeric","character","character")) #coord. estaciones
    ne <- nrow(est.c) #no. de estaciones originales
    nd <- length(dah)/ne #no. de datos por estaciÃ³n
    dim(dah) <- c(nd,ne)
    na <- anyf-anyi+1 #no. de aÃ±os
    nm <- nd/na #no. de "meses" (no. de datos por aÃ±o y estaciÃ³n)
    if(nm < 180) stop(sprintf("These data does not seem daily (%d items per year)",round(nm)))
  }
  #generar vector temporal x si no se ha suministrado:
  if(is.null(fech)) {
    if(is.na(ini)) ini <- sprintf('%d-01-01',anyi) #fecha inicial por defecto
    fech <- as.Date(0:(nd-1),origin=ini) #fechas
  } else ini <- fech[1]
  me <- strftime(fech,"%m")
  anyo <- strftime(fech,"%Y")
  fun <- c("sum","mean","max","min","sd")[valm] #funciÃ³n para el valor mensual
  na <- anyfp-anyip+1 #no. de aÃ±os
  dm <- matrix(NA,na*12,ne) #datos mensuales
  for(ie in 1:ne) { #para cada estaciÃ³n
    cat(' ',ie)
    z <- aggregate(dah[,ie],list(me,anyo),fun,na.rm=TRUE) #valores mensuales
    z[,3] <- round(z[,3],ndec) #redondear
    z2 <- aggregate(is.na(dah[,ie]),list(me,anyo),sum) #no. de datos ausentes
    #conservar solo el periodo deseado:
    zp <- z[,2]>=anyip & z[,2]<=anyfp
    z <- z[zp,]; z2 <- z2[zp,] 
    zz <- z2[,3] <= namax #meses con suficientes datos
    if(length(zz) < 12*na) stop(sprintf('There are %d months with data in your series, but the period\n%d-%d contains %d months. dd2m() expects series spanning whole years.',length(zz),anyip,anyfp,12*na))
    dm[zz,ie] <- z[zz,3] #asignar los datos mensuales a la matriz general
  }
  dm[is.nan(dm)] <- NA #si no hay datos, poner NA
  #grabar los datos mensuales:
  if(homog) {
    fichsal <- sprintf("%s-mh_%d-%d.dat",varcli,anyip,anyfp)
    fichest <- sprintf("%s-mh_%d-%d.est",varcli,anyip,anyfp)
  } else {
    fichsal <- sprintf("%s-m_%d-%d.dat",varcli,anyip,anyfp)
    fichest <- sprintf("%s-m_%d-%d.est",varcli,anyip,anyfp)
  }
  write(round(dm,ndec),fichsal,ncolumns=12)
  write.table(est.c,fichest,row.names=FALSE,col.names=FALSE)
  cat("\n\nMonthly",fun,"values saved to file",fichsal,"\n")
  if(namax>0 & !homog) cat('  (Months with more than',namax,'missing original daily data\n  have also been set to missing)\n\n')
}

#- fix.sunshine.- Check maximum daily sunshine hours and prune any excess.
#'
#' @param  varcli acronym of the homogenized climatic variable
#' @param anyi first year of the homogenized series
#' @param anyf last year of the homogenized series
#' @examples
#'
#' data(climatol_data) #load examples data
#'
#' wd <- tempdir(); wd0 <- setwd(wd) #set a temporal directory
#'
#' 
#' #Now run the examples:
#' 
#' fix.sunshine(<<<
#'
#' setwd(wd0) #Return to user's working directory
#'
#' #Input and output files of the example are in directory:
#' print(wd)
fix.sunshine <- function(varcli, anyi, anyf) {
  #- leer los datos de entrada
  frda <- sprintf('%s_%d-%d.rda',varcli,anyi,anyf) #file to load/save
  obj2save <- load(frda)
  if(nm!=0) stop('This function only applies to DAILY sunshine series')
  #- funciones auxiliares para el cÃ¡lculo de la insolaciÃ³n mÃ¡xima teÃ³rica
  insolteor <- function(lat,fech) { #insolaciÃ³n mÃ¡xima teÃ³rica
    nf <- length(fech) #no. de fechas deseadas
    it <- rep(NA,nf) #vector de insolaciones teÃ³ricas
    latr <- lat * 0.01745329 #lat en radianes (0.01745329 = 2 * pi / 360)
    for(k in 1:nf) {
      dj <- as.numeric(strftime(fech[k],'%j'))
      dec <- declin(dj)
      c <- -tan(latr) * tan(dec)
      if(c <= -1.) it[k] = 24.
      else if(c >= 1.) it[k] = 0.
      else {
        b <- 1.570796 - atan(c / sqrt(-c*c+1.))
        r <- b * 24 / pi
        if(r > 12) d <- r else d <- 24-r
        it[k] <- r + 2 * d * 0.004627778 + .05 # 0.004627778 = .833 / 180
      }
    }
    return(it)
  }
  declin <- function(djul) { #declinaciÃ³n solar
    #AproximaciÃ³n de http://solardat.uoregon.edu/SolarRadiationBasics.html (118):
    # declin = 23.45 * pi / 180 * sin(2 * pi * (284 + n) / 365) =
    return(0.4092797 * sin(4.888834 + 0.01721421 * djul))
  }
  #- recortar los valores que excedan el mÃ¡ximo teÃ³rico
  sink('fix.sunshine.txt',split=TRUE)
  cat('Checking sunshine durations of',frda,'and fixing any excess...\n')
  fixed <- FALSE; rmargin <- 1/10^ndec/2 #flag and rounding margin
  dec=declin(as.numeric(strftime(x,'%j')))
  for(j in 1:ne) {
    cat('------',est.c[j,4],est.c[j,5],'\n')
    c <- -tan(est.c[1,2]*0.01745329) * tan(dec)
    r <- (1.570796 - atan(c / sqrt(-c*c+1.)))*24/pi
    d <- r; z <- r>12; d[z] <- 24-d[z]
    it <- round(r + 2 * d * 0.004627778 + .05, ndec) #maximum possible value
    for(i in 1:nd) if(dah[i,j]>it[i]) {
      cat(est.c[j,4],format(x[i]),dah[i,j],'->',it[i],'\n')
      dah[i,j] <- it[i]; fixed <- TRUE
    }
  }
  if(fixed) {
    frda0 <- sprintf('%s.bak',frda)
    file.rename(frda,frda0)
    cat('Original file',frda,'renamed to',frda0,'\n')
    cat('Writing the new',frda,'file...\n')
    save(list=obj2save, file=frda)
    cat('List of fixed values saved to fix.sunshine.txt\n')
  } else cat('(No value has been modified)\n')
  sink()
}

#- homogen.- homogeneizaciÃ³n automÃ¡tica de un conjunto de series de datos.
homogen <- function(varcli, anyi, anyf, test='snht', nm=NA, nref=c(10,10,4),
                    std=3, swa=NA, ndec=1, dz.max=5, dz.min=-dz.max, cumc=NA, wd=c(0,0,100),
                    inht=25, sts=5, tol=.02, maxdif=NA, maxite=999, force=FALSE, wz=.001, trf=0,
                    mindat=NA, gp=3, x=NULL, ini=NA, na.strings="NA", vmin=NA, vmax=NA,
                    hc.method='ward.D2', nclust=300, cutlev=NA, grdcol=grey(.4), mapcol=grey(.4),
                    expl=FALSE, metad=FALSE, sufbrk='m', tinc=NA, tz='UTC', cex=1.2, uni=NA,
                    raway=TRUE, verb=TRUE, logf=TRUE, snht1=NA, snht2=NA) {
  #varcli: variable climÃ¡tica (acrÃ³nimo usado)
  #anyi: aÃ±o inicial
  #anyf: aÃ±o final
  #test: break detection test to apply. One of 'snht' (the default) or 'cuct'
  #nm: nÃºmero de meses. (Si no se fija, se calcula por el no. de datos)
  #nref: no. (mÃ¡ximo) de estaciones de referencia en cada fase
  #std: tipo de normalizaciÃ³n. 1 (restar la media), 2 (dividir por la media) o
  # 3 (restar la media y dividir por la desviaciÃ³n tÃ­pica; opciÃ³n por defecto).
  #swa: Semi-Window Amplitude (no. of data; defaults to 5*nm or 365 days).
  #ndec: no. de decimales requeridos en los resultados (1 por defecto)
  #dz.max: lÃ­mite superior de tolerancia de anomalÃ­as (si se dan dos valores,
  #  se rechazaran los del valor superior, pero se listarÃ¡n todos)
  #dz.min: lÃ­mite inferior de tolerancia de anomalÃ­as (-dz.max por defecto)
  #cumc: accumulation code 
  #wd: (Weight distance, km); distancia a la que el peso se reduce a la mitad.
  #    wd=0: todas las estaciones de referencia pesan lo mismo.
  #inht: Umbral(es) del test de homogeneidad. (0 to skip the stage)
  #sts: Series tail size (defaults to 5) 
  #tol: factor de tolerancia para cortes simultÃ¡neos
  #maxdif: maximum data difference from previous iteration (ndec/2 by default).
  #maxite: maximum number of iterations to compute means (999 by default).
  #force: forzar cortes aun con una sola referencia? (FALSE por defecto).
  #wz: factor de escala de z. El valor por defecto es apropiado si z se da en m
  #   y x,y en km. TambiÃ©n sirve para sobreponderar z, o para hallar las
  #   distancias Ãºnicamente en el plano horizontal (wz=0).
  #trf: Transformar los datos? (0:no transformar; 1:log(x+1); >1:raÃ­z trf)
  #mindat: MÃ­nimo no. de datos para fragentar las series
  #gp: ParÃ¡metro de grÃ¡ficos. 0=ninguno; 1=anomalÃ­as globales e histogramas;
  #    2=id+grÃ¡ficos mensuales de diagnÃ³stico; 3=id+grÃ¡ficos de medias anuales
  #    mÃ³viles y correcciones; 4=id., pero con sumas anuales mÃ³viles
  #x: Vector temporal. (Por defecto se calcularÃ¡ automÃ¡ticamente). 
  #ini: fecha inicial (para datos diarios, en formato 'AAAA-MM-DD').
  #na.strings: strings marking missing data (NA by default).
  #vmin, vmax: rango de valores permitidos en la variable climÃ¡tica.
  #hc.method: method for the hierarchical clustering ('ward.D2' by default).
  #nclust: no. mÃ¡ximo de estaciones a usar en el anÃ¡lisis de agrupamiento
  #cutlev: level to cut dendrogram to define clusters (automatic by default).
  #grdcol: color de las retÃ­culas de los grÃ¡ficos.
  #mapcol: color del mapa de fondo.
  #expl: Pasada exploratoria? (FALSE por defecto).
  #metad: Usar metadatos? En ese caso se fragmentarÃ¡n las series en los lugares
  #   indicados en *brk.csv y solo se rellenarÃ¡n lagunas. (FALSE por defecto).
  #sufbrk: sufijo a aÃ±adir al nombre de la variable para leer los metadatos.
  #   ('m' por defecto, para leer breaks detectados a escala mensual, pero
  #   poner sufbrk='' si los datos originales ya eran mensuales).
  #tinc: time increment between data. Not set by default, but must be defined
  #   for subdaily data, with units 'hours', 'mins' or 'secs'.
  #   E.g.: tinc='1 hours'. (Do not forget the last 's' in the units).
  #tz: Time zone. Only relevant for subdaily data. ('UTC' by default.)
  #cex: Character expansion factor para etiquetas y textos en los grÃ¡ficos.
  #uni: Units to put in some axis labels. (None by default.)
  #raway: set reanalysis far away to favor observed series (TRUE by default.)
  #verb: Ver mensajes del proceso por pantalla (ademÃ¡s de en el fichero *.txt).
  #logf: Guardar los mensajes en un fichero *.txt? (TRUE por defecto).
  #snht1, snht2: Provisionally kept for backwards compatibility
  #------------------------------------------------------------------
  #backwards compatibility:
  if(!is.na(snht1)) inht <- snht1
  if(!is.na(snht2)) inht <- c(inht,snht2)
  if(!is.na(snht1)|!is.na(snht2)) cat('Please, note that parameters snht1 and snht2 are deprecated.\nUse inht in future applications of Climatol V.4 or higher.\n')
  #- inicializaciones
  dlt <- 0; class(dlt) <- "integer" #flag of deleted data
  #funciones auxiliares:
  datmed.mean <- function(x) mean(datmed[x])
  datmed.sd <- function(x) sd(datmed[x])
  #chosen inhomogeneity test:
  if(test=='cuct') { inhtest <- 'CucT'; test <- 'cuct' } #apply Cucconi
  else { inhtest <- 'SNHT'; test <- 'snht' } #apply SNHT
  #en caso de error, cerrar archivos de salida:
  options(error=cerrar)
  std <- as.integer(std) #std must be integer between 1 and 3:
  if(std<1) { std <- 1; cat('std lower than 1 has been forced to 1\n') }
  if(std>3) { std <- 3; cat('std greater than 3 has been forced to 3\n') }
  #establecer maxdif en funciÃ³n de la precisiÃ³n elegida:
  if(is.na(maxdif)) maxdif=10^(-ndec)/2 #0.05 para un decimal
  verde <- hsv(.33,1,.6) #color muy usado
  #skip detection stages if metad==TRUE or in exploratory mode:
  if(expl | metad) inht <- c(0,0)
  #dz.min ha de ser negativo!:
  z=dz.min>0; if(sum(z)>0) dz.min[z] <- -dz.min[z]
  if(is.na(vmin) & std==2) vmin <- 0 #vmin=0 si std==2
  #- abrir fichero de bitÃ¡cora y escribir cabecera
  archlog <- paste(varcli,"_",anyi,"-",anyf,".txt",sep="")
  if(logf) sink(archlog,split=verb)
  cat("\nHOMOGEN() APPLICATION OUTPUT  (From R's contributed package 'climatol' ",climatol.version,")\n",sep='')
  cat("\n=========== Homogenization of ",varcli,", ",anyi,"-",anyf,". (",
      date(),")\n",sep="")
  time1 <- Sys.time() #tiempo al inicio del proceso
  cat("\nParameters:")
  arg <- names(formals()) #lista de los argumentos de la funciÃ³n
  nargs <- length(arg) #no. de argumentos
  for(i in 1:nargs) {
    if(arg[i]=='x') next #no imprimir el vector de fechas!
    cat(" ",arg[i],"=",sep="")
    cat(eval(as.symbol(arg[i])))
    if(i<nargs) cat(',')
  }
  cat("\n\n")
  #completar parÃ¡metros multivalor:
  k <- length(inht); if(k<2) inht <- c(inht,inht) #mismo umbral en las pasadas
  k <- length(wd); if(k<3) wd <- c(rep(0,3-k),wd)
  k <- length(nref); if(k<3) nref <- c(nref,rep(nref[k],3-k))
  if(expl) nref[3] <- nref[1] #keep nr. of references in exploratory mode
  #data anomaly tolerance (warning and delete):
  dz.maxw <- min(dz.max); dz.maxd <- max(dz.max)
  dz.minw <- max(dz.min); dz.mind <- min(dz.min)
  if(length(dz.max)==1) dz.maxw <- NA
  if(length(dz.min)==1) dz.minw <- NA
  #etiquetas mensuales (de tres letras): 
  mes3 <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  #- lectura inicial de datos
  fbas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #raÃ­z nombres de fichero 
  fiche <- sprintf('%s.est',fbas) #nombre del fichero de estaciones
  #leer coordenadas y nombres de las estaciones:
  est.c <- read.table(fiche,colClasses=c("numeric","numeric","numeric","character","character"))
  names(est.c) <- c('X','Y','Z','Code','Name')
  ne <- nrow(est.c) #no. de estaciones
  #si los cÃ³digos continenen guiones, cambiarlos a guiones bajos:
  z <- gsub('-','_',est.c[,4])
  zn <- sum(z != est.c[,4])
  if(zn>0) {
    cat('In',zn,'codes containing the "-" character, they have been changed to "_"\n\n')
    est.c[,4] <- z
  }
  #comprobar si hay cÃ³digos duplicados:
  z <- duplicated(est.c[,4])
  if(sum(z)>0) {
    zz <- unique(est.c[z,4])
    cat('Duplicated codes detected:\n')
    print(est.c[est.c[,4]%in%zz,4:5])
    stop('The station file *.est must contain unique codes.')
  }
  #comprobar si las coordenadas estÃ¡n en grados:
  if(max(abs(est.c[,1]))>180 | max(abs(est.c[,2]))>90) {
    deg <- FALSE
    if(mean(est.c[,1])>10000 | mean(est.c[,2])>10000) { #from m to km:
      est.c[,1] <- est.c[,1] / 1000.
      est.c[,2] <- est.c[,2] / 1000.
    }
  } else deg <- TRUE
  fichd <- sprintf('%s.dat',fbas) #nombre del fichero de datos
  dat <- scan(fichd,na.strings=na.strings) #lectura de los datos
  numdat <- length(dat) #no. de datos leÃ­dos
  nd <- numdat/ne #no. de datos por estaciÃ³n
  if(nd-floor(nd)>1e-16) {
    cat(ne,"stations read from",fiche,"\n")
    cat(numdat,"data read from",fichd,"\n")
    stop("The length of data is not a multiple of the number of stations!")
  } else cat(sprintf('Data matrix: %d data x %d stations\n',nd,ne))
  dim(dat) <- c(nd,ne) #conversiÃ³n de vector a matriz
  if(!is.na(cumc)) { #manage data coded as accumulated to the next:
    gp <- 0 #do not create graphics if using cumc
    cuml <- apply(dat==cumc,2,which) #list of accumulated terms
    if(length(cuml)==0) {
      cat('\nThere are no data coded as cumc =',cumc,'. Nothing done.\n')
      cerrar(); return(invisible())
    }
    cumt <- function(z) { #first (cumA) and last (cumB) accumulated terms
      zdif <- diff(z)>1
      cumA <- z[c(TRUE,zdif)]
      cumB <- z[c(zdif,TRUE)]
      return(list(cumA,cumB))
    }
    z <- lapply(cuml,cumt)
    cumA <- sapply(z, function(x) x[1]) #first terms of accumulated runs
    cumB <- sapply(z, function(x) x[2]) #last terms of accumulated runs
    dat[dat==cumc] <- NA #delete accumulated data
    cumv <- vector('list',ne) #list of accumulation values:
    for(j in 1:ne) {
      cumv[[j]] <- dat[cumB[[j]]+1,j] #save accumulation values
      dat[cumB[[j]]+1,j] <- NA #delete them
      cumB[[j]] <- cumB[[j]]+1 #include them in the accumulated runs
    }
  }
  na <- anyf-anyi+1 #no. de aÃ±os
  nsy <- rep(0,na)   #no. de saltos por aÃ±o
  if(!is.null(x)) nm <- 0
  else if(is.na(nm)) { #calcular no. de datos por aÃ±o y estaciÃ³n:
    z <- nd/na
    if(z>=1) nm <- floor(z)
    if(nm > 366) nm <- -1 #datos subdiarios 
    else if(nm > 12) nm <- 0 #datos diarios
    else if(!nm%in%c(1,2,3,4,6,12)) {
      cat(sprintf('Calculated no. of data per year and station: nm=%f\n',z))
      stop('Complete years of monthly or seasonal data are required to avoid\n  missing data in the results. Please complete your series to have\n  a rounded number of data. (nm can be one of 1,2,3,4,6,12).')
    }
  }
  #paso de tiempo kinc, en dÃ­as:
  if(!is.na(tinc)) {
    z <- unlist(strsplit(tinc,' +'))
    kinc <- as.numeric(as.difftime(as.numeric(z[1]),units=z[2])/24)
  } else kinc <- 1
  #comprobar si los aÃ±os estÃ¡n completos:
  if(nm>0 & nd%%nm==0) acomp <- TRUE else acomp <- FALSE
  if(is.na(swa)) { #valores de swa por defecto
    if(nm>0) swa <- 5*nm
    else if(nm==0) swa <- 365 #one year (in days)
    else swa <- 365/kinc #one year (in tinc periods)
  } else if(swa>=nd) swa <- ceiling(nd/4) #evitar semiventana demasiado grande
  #- generar vector temporal x si no se ha suministrado:
  if(is.null(x)) {
    if(is.na(ini)) ini <- sprintf('%d-01-01',anyi) #fecha inicial por defecto
    if(nm>0) x <- seq(as.Date(ini),length.out=nd,by=sprintf('%d months',12/nm))
    else if(!is.na(tinc)) x <- seq(as.POSIXct(ini,tz=tz),length.out=nd,by=tinc)
    else x <- seq(as.Date(ini),length.out=nd,by='1 day')
  } else ini <- x[1]
  #establecer valor de mindat, si no se especificÃ³:
  if(is.na(mindat)) { 
    if(nm<=0) mindat <- 91/kinc #tres meses
    else mindat <- max(5,nm)
  }
  
  #comprobar si hay valores fuera del rango permitido
  if(!is.na(vmin)) { #hay valores inferiores al mÃ­nimo permitido?
    n <- sum(dat<vmin,na.rm=TRUE)
    if(n) {
      dat[dat<vmin] <- NA #borrado de valores errÃ³neos
      cat(n,'data lower than',vmin,'have been deleted\n')
    }
  }
  if(!is.na(vmax)) { #hay valores superiores al mÃ¡ximo permitido?
    n <- sum(dat>vmax,na.rm=TRUE)
    if(n) {
      dat[dat>vmax] <- NA #borrado de valores errÃ³neos
      cat(n,'data greater than',vmax,'have been deleted\n')
    }
  }
  #comprobar si hay series sin ningÃºn dato:
  ksd <- which(apply(!is.na(dat),2,sum) == 0)
  if(length(ksd)>0) {
    cat("There are series with no data!!!:",'\n')
    print(est.c[ksd,])
    stop("Please, remove these series from the input files and run homogen() again")
  }
  #comprobar si hay series con demasiados pocos datos:
  ksd <- which(apply(!is.na(dat),2,sum) < mindat)
  if(length(ksd)>0) {
    cat("There are series with too few data (less than",mindat,'):\n')
    print(est.c[ksd,])
    cat("Warning: Break-points cannot be corrected in these series",'\n')
  }
  
  #- si hay algÃºn tÃ©rmino sin datos, emitir aviso y terminar
  numdat <- apply(!is.na(dat),1,sum) #no. de datos de cada tÃ©rmino
  if(!min(numdat)) {
    fich=sprintf('%s_%d-%d-availability.pdf',varcli,anyi,anyf)
    pdf(fich,bg='white')
    plot(x,numdat,type='l',xlab='Time',ylab='Number of data',
         main=sprintf('Number of %s data along time',varcli))
    z <- class(x)
    if(z[1]=='Date' | z[1]=='POSIXct') {
      grid(NA,NULL,col=grey(.4))
      if(z[1]=='Date') abline(v=axis.Date(1,x),lty=3,col=grey(.4))
      else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x),lty=3,col=grey(.4))
    } else grid(col=grey(.4))
    graphics.off()
    zz <- which(numdat==0); z <- range(zz)
    cat('\n',sum(numdat==0),' time steps between terms ',z[1],' (',format(x[z[1]]),') and ',z[2],' (',format(x[z[2]]),')\n  have missing data in all stations!\n',sep='')
    if(length(zz)<=100) print(format(x[which(numdat==0)]))
    cat(sprintf('(See the figures in %s)',fich),'\n')
    sink()
    stop("Cannot continue.\n(Shorten the study period or add series with data in the void terms.)\n\n")
  }
  #------------- Fin del control de calidad inicial -------------------
  dat.o <- dat #copia de los datos originales
  if(nd<100) lw=3 #anchura de las barras de anomalÃ­as
  else if(nd<300) lw=2
  else lw=1
  nei <- ne  #no. inicial de estaciones
  est.i <- est.c #datos iniciales de las estaciones
  nsp <- rep(0,nei)  #no. de cortes de cada estaciÃ³n original
  iest <- 1:ne   #Ã­ndice seÃ±alando la serie original de cada subserie
  outan <- matrix(NA,nd,ne) #anomalÃ­as de los outliers
  #- if(gp>0), generar grÃ¡ficos iniciales
  if(gp>0) {
    #activar salida grÃ¡fica a documento pdf, con rÃ³tulo inicial:
    pdfname <- sprintf('%s_%d-%d.pdf',varcli,anyi,anyf)
    pdf(pdfname,title=pdfname,bg='white')
    old.par <- par(no.readonly=TRUE)
    plot(-1:1,-1:1,type="n",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
    text(0,0.4,sprintf("CLIMATOL %s",climatol.version),cex=4)
    text(0,-0.45,paste("Homogenization\ngraphic output of\n",varcli,"\n",anyi,"-",anyf,sep=""),cex=3)
    par(cex=cex)
    #datos disponibles en cada serie:
    z <- class(x)
    if(nm<0) cat("Per station data availability graphic skipped for subdaily data\n   (might be too heavy).\n")
    else { #per station data availability:
      if(sum(is.na(dat))==0) col=4 else col=c('white',4)
      image(!is.na(dat),col=col,xaxt='n',yaxt='n',useRaster=TRUE,
            xlab='Time',ylab='Series',main=paste(varcli,'data availability'))
      yy=as.integer(strftime(x,'%Y')); if(length(unique(yy))<3) yy <- 1:nd
      lb=pretty(yy); nt=length(lb)
      if(lb[nt] > max(yy)) { nt=nt-1; lb=lb[1:nt] }
      if(lb[1] < min(yy)) { lb=lb[2:nt]; nt=nt-1 }
      kp=integer(); for(k in 1:nt) kp=c(kp,min(which(yy==lb[k])))
      at=((1:nd-1)/(nd-1))[kp]
      axis(1,at,labels=lb,xlab='Time')
      abline(v=at,lty=3,col=grdcol)
      lb=pretty(1:ne); nt=length(lb)
      if(lb[nt] > ne) { nt=nt-1; lb=lb[1:nt] }
      if(lb[1] < 1) { lb=lb[2:nt]; nt=nt-1 }
      at=((1:ne-1)/(ne-1))[lb]
      axis(2,at,labels=lb,las=1)
      abline(h=at,lty=3,col=grdcol)
    }
    #no. de datos de cada tÃ©rmino:
    numdat <- apply(!is.na(dat),1,sum) 
    plot(x,numdat,type="l",las=1,col=4,ylab="Number of data",xlab='Time',
         ylim=c(0,ne),main=paste("Number of",varcli,"data in all stations"))
    if(z[1]=='Date' | z[1]=='POSIXct') {
      grid(NA,NULL,col=grdcol)
      if(z[1]=='Date') abline(v=axis.Date(1,x),lty=3,col=grdcol)
      else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x),lty=3,col=grdcol)
    } else grid(col=grdcol)
    abline(h=5,lty=2,col=verde)
    abline(h=3,lty=2,col="red")
    #si hay algÃºn tÃ©rmino sin datos no podremos continuar: 
    if(!min(numdat)) {
      cat("At least one term has missing data in all stations! (See the PDF graph)\n")
    }
    #boxplots de los datos de cada estaciÃ³n:
    if(nm>1 & acomp & ne<=nclust) { #boxplots para cada nm (mensuales, etc)
      dim(dat) <- c(nm,na,ne) #dimensiones provisionales
      for(me in 1:nm) { #para cada mes
        z <- data.frame(dat[me,,])
        names(z) <- 1:ne
        #etiqueta del mes (si nm!=12, poner solo el nÃºmero):
        if(nm==12) labm <- mes3[me] else labm <- me
        labm <- paste(" (",labm,")",sep="")
        boxplot(z,xlab="Stations",ylab=ifelse(is.na(uni),"Values",uni),main=paste(varcli,labm,sep=""),col="wheat",border=hsv(.7,1,.9))
        grid(col=grdcol)
        abline(h=0)
      }
      dim(dat) <- c(nd,ne) #restablecer dimensiones de trabajo
    } else { #boxplots con todos los datos de cada estaciÃ³n
      z <- data.frame(dat)
      names(z) <- 1:ne
      if(ne<=60) boxplot(z,xlab="Stations",ylab=ifelse(is.na(uni),
                                                       "Values",uni),main=varcli,col="wheat",border=hsv(.7,1,.9))
      else { #particionar el no. de estaciones en grupos de 50:
        zg <- seq(0,ne,50); ng <- length(zg)
        #que el Ãºltimo grupo tenga mÃ¡s de 10 estaciones:
        if(ne>zg[ng]) { if(ne-zg[ng]<=10) zg[ng] <- ne
        else { zg <- c(zg,ne); ng <- ng + 1 }
        }
        for(i in 2:ng) boxplot(z[,(zg[i-1]+1):zg[i]],xlab='Stations',
                               ylab=ifelse(is.na(uni),"Values",uni),
                               main=sprintf('%s (stations %d to %d)',varcli,zg[i-1]+1,zg[i]),
                               col="wheat",border=hsv(.7,1,.9))
      }
      grid(col=grdcol)
      abline(h=0)
    }
  }
  #- Transformar los datos? :
  if(trf>=1) {
    if(min(dat,na.rm=TRUE)<0) stop('Your data has negative values: cannot apply transformations!')
    if(trf==1) dat <- log1p(dat) else dat <- dat^(1/trf)
    if(is.na(vmin) | vmin<0) vmin <- 0 #evitar valores negativos
    if(maxdif>0.01) maxdif <- 0.01 #rebajar maxdif
  }
  if(gp>0) { #continuamos con los grÃ¡ficos iniciales
    #histograma de todos los datos (distribuciÃ³n quasi-normal?)
    if(trf) main="Histogram of all (transformed) data"
    else main="Histogram of all data"
    zh <- hist(dat,plot=FALSE)
    zx <- zh$breaks
    zy <- zh$counts; zy[zy==0] <- NA
    barplot(zy,log='y',space=0,ylim=c(.9,max(zy,na.rm=TRUE)*2),xlab=varcli,
            ylab='Frecuency',main=main,col=hsv(.4,1,.8),names.arg=zh$mids)
    #correlograma de series diferenciadas a nivel mensual (r <-> distancia)
    #(si hay mÃ¡s de nclust estaciones, solo de una muestra aleatoria de nclust)
    if(ne>nclust) { splc <- sample(1:ne,nclust); nec <- nclust }
    else { splc <- 1:ne; nec <- ne }
    est.d <- matrix(NA,nec,nec) #matriz de distancias
    for(i in 1:(nec-1)) {
      for(j in (i+1):nec) {
        dx <- est.c[splc[i],1]-est.c[splc[j],1]
        dy <- est.c[splc[i],2]-est.c[splc[j],2]
        if(deg) {  #convertir grados a km
          dx <- dx*111*cos((est.c[splc[i],2]+est.c[splc[j],2])*pi/360)
          dy <- dy*111
        }
        dz <- (est.c[splc[i],3]-est.c[splc[j],3])*wz
        d2 <- dx*dx+dy*dy+dz*dz #distancia cuadrÃ¡tica
        est.d[i,j] <- sqrt(d2) #distancia
        est.d[j,i] <- est.d[i,j]  #matriz simÃ©trica
      }
    }
    data <- dat[,splc] #copia de los datos
    if(nm>1 & acomp) { #calcular las series diferenciadas por meses
      dim(data) <- c(nm,na,nec) #dimensionamos por meses
      difd <- apply(data,c(1,3),diff)
      dim(difd) <- c(nd-nm,nec) #redimensionar
    }
    else difd <- diff(data) #series diferenciadas globalmente
    corm <- cor(difd,use="p") #matriz de correlaciones
    #cambiar |r|==1 (debidos a estaciones con solo 2 datos en comÃºn, ademÃ¡s
    #de la diagonal) por la correlaciÃ³n media (para evitar NAs en la matriz):
    corm[abs(corm)==1] <- mean(corm,na.rm=TRUE)
    if(ne>2) {  #dendrograma de las estaciones:
      if(ne>nclust) main <- sprintf('Correlogram of %d sampled %s series\n(First differences)',nclust,varcli)
      else main <- sprintf('Correlogram of first difference %s series',varcli)
      if(trf) main <- paste(main,'(transformed)')
      xd <- as.vector(est.d); y <- as.vector(corm)
      xmin <- floor(min(c(0,xd),na.rm=TRUE)); xmax <- ceiling(max(xd,na.rm=TRUE))
      ymin <- floor(min(c(0,y),na.rm=TRUE)); ymax <- ceiling(max(y,na.rm=TRUE))
      xbin <- seq(xmin,xmax,length=100)
      ybin <- seq(ymin,ymax,length=100)
      freq <- as.data.frame(table(findInterval(xd,xbin),findInterval(y,ybin)))
      freq[,1] <- as.integer(as.character(freq[,1]))
      freq[,2] <- as.integer(as.character(freq[,2]))
      freq2D <- matrix(0,100,100)
      freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]
      freq2D[freq2D==0] <- NA
      col=rev(rainbow(16,start=0,end=2/3))
      nz=max(freq2D,na.rm=TRUE); if(nz<16) col=col[1:nz]
      image(xbin,ybin,freq2D,main=main,useRaster=TRUE,xlab="Distance (km)",
            ylab="Correlation coefficient",col=col)
      grid(col=gray(.3)); abline(h=0,col=2)
      #dendrograma de las estaciones:
      dism <- dist(corm) #matriz de disimilaridad
      #si hay NAs en la matriz de disimilaridad, asignarles distancia 1:
      kna=which(is.na(dism)); if(sum(kna)>0) dism[kna] <- 1
      hc <- hclust(dism,hc.method)
      if(ne>nclust) main <- paste("Dendrogram of",nclust,"sampled stations")
      else main <- "Dendrogram of station clusters"
      plot(hc,xlab="Stations",sub="",ylab="Dissimilarity",main=main)
      #clasificaciÃ³n de las estaciones hasta en un mÃ¡ximo de 9 grupos:
      if(is.na(cutlev)) cutlev <- mean(hc$height)+sd(hc$height)
      repeat {
        ct <- cutree(hc,h=cutlev)
        nc <- length(levels(factor(ct)))
        if(nc<10) break
        cutlev <- cutlev + .1
      }
      if(nc>1) {
        abline(h=cutlev,col="red",lty=2)
        if(ne<=nclust) { #list station clusters
          cat('\n-------------------------------------------\n')
          cat(sprintf('Stations in the %d clusters:\n\n',nc))
          print(split(splc,ct))
          cat('---------------------------------------------\n')
        }
      }
      #mapa de las estaciones:
      if(require(mapdata)) {
        if(nc==1) { col="blue"; main=paste(varcli,"station locations") }
        else {
          col=rainbow(nc,1,.55)[ct]
          main=paste(varcli," station locations (",nc," clusters)",sep="")
        }
        #compute map limits with a 5% margin over station coordinates:
        zxr <- range(est.c[,1]); zyr <- range(est.c[,2])
        zxm <- diff(zxr)*.05; zym <- diff(zyr)*.05
        if(zxm==0 | zym==0) stop('X and/or Y coordinates are all the same!\nAllow some variation in the *.est input file')
        xlim=c(min(est.c[,1])-zxm, max(est.c[,1])+zxm)
        ylim=c(min(est.c[,2])-zym, max(est.c[,2])+zym)
        #now draw the map
        if(deg) {
          if(diff(zyr)<5.) hires <- TRUE else hires <- FALSE #resolution of maps
          #..................................
          #Error in get(dbname) : object 'worldHiresMapEnv' not found
          #     if(requireNamespace("maps",quietly=TRUE)) mapok<-TRUE else mapok<-FALSE
          #     if(hires & requireNamespace("mapdata",quietly=TRUE))
          #       maphr <- TRUE else maphr <- FALSE
          #Workaround: put these packages in "Depends"
          #..................................
          #   }
          # }
          #..........................................
          #Error in get(dbname) : object 'worldHiresMapEnv' not found
          #Error in get(dbname) : object 'worldMapEnv' not found
          #     if(maphr) z<-try(maps::map('worldHires',col=mapcol,xlim=xlim,ylim=ylim))
          #     else if(mapok) z<-try(maps::map('world',col=mapcol,xlim=xlim,ylim=ylim))
          #..........................................
          #Workaround: Put these packages in 'Depends' to load them explicitely:
          if(hires) z<-try(map('worldHires',col=mapcol,xlim=xlim,ylim=ylim),TRUE)
          else z <- try(map('world',col=mapcol,xlim=xlim,ylim=ylim),TRUE)
          #..........................................
          if (class(z)=="try-error") plot(0,0,xlim=xlim,ylim=ylim,xlab='',ylab='',asp=1/(cos(mean(zyr)*pi/180)),main=main,type='n')
          #       else { maps::map.axes(); mtext(main,3,line=1,cex=1.7) }
          else { map.axes(); mtext(main,3,line=1,cex=1.7) }
        } else plot(0,0,xlim=xlim,ylim=ylim,xlab="X (km)",ylab="Y (km)",asp=1,main=main,type='n')
        if(ne>99) { #dibujar sÃ­mbolos si hay mÃ¡s de 99 estaciones
          #dibujar primero en negro las estaciones que no estÃ¡n en la muestra:
          points(est.c[-splc,1:2],pch='+',col=2,cex=.5)
          #y ahora las estaciones de la muestra, con sus colores:
          points(est.c[splc,1:2],col=col,pch=ct)
        } else { #hasta nclust estaciones, poner el nÃºmero
          text(est.c[,1:2],labels=1:ne,col=col)
        }
        grid(col=gray(.4))
      }
    }
    rm(data,difd) #borrar objetos temporales
  }
  #- if(gp==1), terminar (solo se deseaban los grÃ¡ficos iniciales)
  if(gp==1) {
    par(old.par)
    cat("\nOnly the initial exploratory graphics were demanded.\nSee them in ",varcli,"_",anyi,"-",anyf,".pdf\n",sep="")
    cerrar()
    if(exists('ct')) return(invisible(list(corm=corm,ct=ct)))
    else return()
  }
  #  Proceso de homogeneizaciÃ³n en tres etapas:
  #  1) cortes por test en ventanas mÃ³viles
  #  2) cortes por test en toda la serie
  #  3) relleno de lagunas
  #- abrir archivos de outliers y breaks
  Fout <- file(sprintf('%s_out.csv',fbas),'w')
  write('"Code","Date","Observed","Suggested","Anomaly (std.devs.)","Deleted"',Fout)
  if(!metad) {
    Fbrk <- file(sprintf('%s_brk.csv',fbas),'w')
    write(sprintf('"Code","Date","%s"',inhtest),Fbrk)
  }
  #- cÃ¡lculo de las matrices de distancias y rangos de proximidad
  cat("Computing inter-station distances ...")
  refhom <- substr(est.c[,4],1,1)=='*' #referencias homogÃ©neas
  est.d <- matrix(0,ne,ne) #matriz de distancias
  for(i in 1:(ne-1)) {
    cat(" ",i)
    for(j in (i+1):ne) {
      dx <- est.c[i,1]-est.c[j,1]
      dy <- est.c[i,2]-est.c[j,2]
      if(deg) {  #convertir grados a km
        dx <- dx*111*cos((est.c[i,2]+est.c[j,2])*pi/360)
        dy <- dy*111
      }
      dz <- (est.c[i,3]-est.c[j,3])*wz
      d2 <- dx*dx+dy*dy+dz*dz #distancia cuadrÃ¡tica
      est.d[i,j] <- sqrt(d2) #distancia
      #alejar las series de reanÃ¡lisis para priorizar las observadas:
      if(raway & xor(refhom[i],refhom[j])) est.d[i,j] <- est.d[i,j]+1000
      est.d[j,i] <- est.d[i,j]  #matriz simÃ©trica
    }
  }
  cat("\n")
  est.p <- t(apply(est.d,1,order)) #matriz de rangos de proximidad
  #- Estima inicial de medias y desv. tÃ­picas
  datmed <- apply(dat,1,mean,na.rm=TRUE) #serie media global
  refmed <- mean(datmed) #media global de referencia
  dat.m <- apply(dat,2,mean,na.rm=TRUE) #medias de partida
  if(std==2 & min(dat.m)<2) {
    std <- 1
    cat('------------------ WARNING -------------------------------------\n')
    cat('There are series with very low means. This is a problem if std=2.\n')
    cat('Therefore std has been changed to 1. If you want to apply std=2,\n')
    cat('please multiply your data by a factor to avoid means lower than 2\n')
    cat('and run homogen() again.\n')
    cat('----------------------------------------------------------------\n')
  }
  if(std==3) {
    refstd <- sd(datmed) #desv. tÃ­pica global de referencia
    dat.s <- apply(dat,2,sd,na.rm=TRUE) #desv. tÃ­p. de partida
  }
  switch(std,
         dat.m <- dat.m + refmed - apply(!is.na(dat),2,datmed.mean),
         dat.m <- dat.m * refmed / apply(!is.na(dat),2,datmed.mean),
         {dat.m <- dat.m + refmed - apply(!is.na(dat),2,datmed.mean)
         dat.s <- dat.s + refstd - apply(!is.na(dat),2,datmed.sd)},
         dat.m <- dat.m + refmed - apply(!is.na(dat),2,datmed.mean)
  )
  #- metad==TRUE? Leer *_brk.csv y cortar las series por donde indica
  if(metad) {
    cat('\nSplitting the series following the metadata file...:\n')
    if(sufbrk=='') fichbrk <- sprintf('%s_%d-%d_brk.csv',varcli,anyi,anyf)
    else if(nchar(sufbrk)>3) fichbrk <- sprintf('%s_%d-%d_brk.csv',sufbrk,anyi,anyf)
    else fichbrk <- sprintf('%s-%s_%d-%d_brk.csv',varcli,sufbrk,anyi,anyf)
    brk <- read.csv(fichbrk,colClasses=c("character","character","character"))
    if(!is.na(tinc)) brk[,2] <- as.POSIXct(brk[,2],tz=tz)
    else brk[,2] <- as.Date(brk[,2])
    nbrk <- nrow(brk); nn <- 0
    if(nbrk<1) cat('No break-points in the metadata file.\n')
    else {
      for(kb in 1:nbrk) { #para cada break:
        i <- match(brk[kb,1],est.c[,4]) #estaciÃ³n a cortar
        if(is.na(i)) {
          cat(sprintf('\nCode %s not found in station list; break skipped',brk[kb,1]))
          next
        }
        kp <- match(brk[kb,2],x) #posiciÃ³n de corte
        #si x no contiene todas las fechas puede no haber concordancia:
        if(is.na(kp)) kp <- which.max(x>brk[kb,2])
        if(is.na(tinc)) cat(sprintf('\n%s(%d) breaks at %s',est.c[i,4],i,format(x[kp])))
        else cat(sprintf('\n%s(%d) breaks at %s',est.c[i,4],i,format(x[kp],tz=tz,usetz=TRUE)))
        if(sum(!is.na(dat[1:(kp-1),i])) < mindat) {
          dat[1:(kp-1),i] <- NA
          cat(" Fragment with less than",mindat,"data DELETED\n")
        } else if(sum(!is.na(dat[kp:nd,i])) < mindat) {
          dat[kp:nd,i] <- NA
          cat(" Fragment with less than",mindat,"data DELETED\n")
        } else {
          nn <- nn+1 #incrementamos el no. de nuevas series
          iest <- c(iest,iest[i]) #aÃ±adir Ã­ndice a la serie original
          nsp[iest[i]] <- nsp[iest[i]]+1 #y tambiÃ©n su no. de saltos
          if(nm>0) { #contar no. de saltos por aÃ±o
            z <- 1 + floor((kp-1)/nm) #tÃ©rmino anual del salto
            nsy[z] <- nsy[z] + 1 #no. de saltos por aÃ±o
          }
          dat <- cbind(dat,rep(NA,nd)) #nueva columna de datos
          #pasar los datos anteriores al corte a la nueva serie:
          dat[1:(kp-1),ne+nn] <- dat[1:(kp-1),i]
          dat[1:(kp-1),i] <- NA #borrar los datos pasados a la nueva serie
          #copiar las coordenadas y poner sufijo a indicativo y nombre:
          
          z <- data.frame(est.i[iest[i],1:3],paste(est.i[iest[i],4],"-",1+nsp[iest[i]],sep=""),paste(est.i[iest[i],5],"-",1+nsp[iest[i]],sep=""))
          names(z) <- names(est.i)
          est.c <- rbind(est.c,z)
          #asignar mismas medias (y desv. tÃ­picas?) al nuevo fragmento:
          switch(std,
                 { dat.m[i] <- mean(dat[,i],na.rm=TRUE) + refmed - mean(datmed[!is.na(dat[,i])])
                 dat.m <- c(dat.m, mean(dat[,ne+nn],na.rm=TRUE)+refmed-mean(datmed[!is.na(dat[,ne+nn])])) },
                 { dat.m[i] <- mean(dat[,i],na.rm=TRUE) * refmed / mean(datmed[!is.na(dat[,i])])
                 dat.m <- c(dat.m, mean(dat[,ne+nn],na.rm=TRUE)*refmed/mean(datmed[!is.na(dat[,ne+nn])])) },
                 { dat.m[i] <- mean(dat[,i],na.rm=TRUE) + refmed - mean(datmed[!is.na(dat[,i])])
                 dat.m <- c(dat.m, mean(dat[,ne+nn],na.rm=TRUE)+refmed-mean(datmed[!is.na(dat[,ne+nn])]))
                 dat.s[i] <- sd(dat[,i],na.rm=TRUE) + refstd - sd(datmed[!is.na(dat[,i])])
                 dat.s <- c(dat.s, sd(dat[,ne+nn],na.rm=TRUE)+refstd-sd(datmed[!is.na(dat[,ne+nn])])) },
                 { dat.m[i] <- mean(dat[,i],na.rm=TRUE) + refmed - mean(datmed[!is.na(dat[,i])])
                 dat.m <- c(dat.m, mean(dat[,ne+nn],na.rm=TRUE)+refmed-mean(datmed[!is.na(dat[,ne+nn])])) }
          )
        }
      }
      cat("\n\nUpdate number of series: ",ne,"+",nn,"= ")
      ne <- ne + nn  #actualizar el no. de estaciones
      cat(ne,"\n\n")
      refhom <- substr(est.c[,4],1,1)=='*' #actualizar referencias homogÃ©neas
    }
    inht <- c(0,0) #pasar directamente a relleno de lagunas
  }
  #- for (ks in 1:3) #(test en ventanas, test total, y relleno de lagunas)
  for (ks in 1:3) { #para cada etapa:
    if(ks<3) {
      inh <- inht[ks] #umbral de inht para la etapa ks
      if(inh==0) next #saltar la pasada si inh==0
    }
    cat("\n\n========== STAGE",ks)
    switch(ks,
           cat(sprintf(" (%s on overlapping temporal windows) ===========\n\n",inhtest)),
           cat(sprintf(" (%s on the whole series) =======================\n\n",inhtest)),
           cat(" (Final calculation of all missing data) ==========\n\n")
    )
    #- calcular la matriz de pesos? (depende de la pasada)
    if(ks==1) zz <- TRUE else if(wd[ks]!=wd[ks-1]) zz <- TRUE else zz <- FALSE
    if(zz) {
      est.w <- matrix(1,nei,nei) #matriz de pesos
      if(wd[ks]>0) { #pesos diferentes de 1
        cat("Computing inter-station weights...")
        wd2 <- wd[ks]*wd[ks]
        for(i in 1:(nei-1)) {
          for(j in (i+1):nei) {
            est.w[i,j] <- wd2/(wd2+est.d[i,j]*est.d[i,j])
            est.w[j,i] <- est.w[i,j]  #matriz simÃ©trica
          }
        }
        cat(' (done)\n\n')
      }
    }
    #- if(gp>0), pintar rÃ³tulo separador de niveles
    if(gp>0) {
      par(old.par)
      plot(-1:1,-1:1,type="n",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
      text(0,0.4,paste("Stage",ks),cex=4)
      if(ks==1) text(0,-0.3,sprintf("Binary splits on %d term\nstepped windows with\nstd=%d, %s>%d\nand wd=%d km",round(swa),std,inhtest,round(inh),round(wd[ks])),cex=3)
      else if(ks==2) text(0,-0.3,sprintf("Binary splits on\nwhole series with\nstd=%d, %s>%d\nand wd=%d km",std,inhtest,round(inh),round(wd[ks])),cex=3)
      else text(0,-0.3,sprintf("Final anomalies of the\nhomogenized series with\nwd = %d km and nref = %d",round(wd[ks]),nref[ks]),cex=2.5)
      par(cex=cex)
    }
    #valores dependientes de la pasada:
    if(ks==3) aref <- TRUE else aref <- FALSE
    nrefk <- nref[ks]
    #- repetir hasta que no se corte ninguna serie
    repeat {
      #---------- CÃ¡lculo de anomalÃ­as y eliminaciÃ³n de datos anÃ³malos:
      #- inicializar matrices dat.z|e|c oneref anom sanom mindist nrefs used
      dat.z <- matrix(NA,nd,ne) #datos observados (estandarizados)
      dat.e <- matrix(NA,nd,ne) #datos estimados (estandarizados)
      dat.c <- matrix(NA,nd,ne) #datos calculados (estimados, sin estand.)
      oneref <- matrix(FALSE,nd,ne) #solo 1 referencia?
      anom <- matrix(NA,nd,ne) #anomalÃ­as
      sanom <- matrix(NA,nd,ne) #anomalÃ­as estandarizadas
      mindist <- matrix(NA,nd,ne) #distancias mÃ­nimas
      nrefs <- matrix(NA,nd,ne) #no. de referencias
      used <- matrix(FALSE,ne,ne) #flags de estaciones usadas
      #copia de trabajo de los datos:
      dat.d <- dat
      dat.na <- is.na(dat.d) #Ã­ndice de datos ausentes
      #- si hay algÃºn tÃ©rmino sin ningÃºn dato, avisar y terminar
      numdat <- apply(!is.na(dat.d),1,sum)
      nmin=min(numdat)
      if(nmin==0) {
        cat("\nThere are terms with NO DATA!:\n")
        for(j in which(numdat==0)) cat(format(x[j]),"\n")
        stop("Cannot continue! Shorten the study period, add series with data in the empty terms, or be more tolerant to outliers.")
      }
      #usar las medias y desviaciones tÃ­picas anteriores si existen:
      if(exists('dat.m0')) {
        dat.m <- dat.m0
        if(std==3) dat.s <- dat.s0
      }
      #- estandarizar los datos dat.d (obtener dat.z)
      switch(std,
             for(ke in 1:ne) dat.z[,ke] <- dat.d[,ke]-dat.m[ke], #std=1
             for(ke in 1:ne) dat.z[,ke] <- dat.d[,ke]/dat.m[ke], #std=2
             for(ke in 1:ne) dat.z[,ke]<-(dat.d[,ke]-dat.m[ke])/dat.s[ke],#std=3
             dat.z <- dat.d
      )
      #- ite=0 y repetir hasta estabilizar los datos estimados
      #proceso iterativo de estima de las medias de cada serie:  
      ite <- 0
      cat("\nCalculation of missing data with outlier removal\n")
      cat('(Suggested data replacements are provisional)\n')
      if(length(dat)>10000000) cat('This process may take a very long time (many days)\n')
      else if(length(dat)>1000000) cat('This process may take a long time (many hours)\n')
      if(is.na(cumc)) {
        if(ks==3 & !expl) cat("\nThe following lines will have one of these formats:\n")
        cat("  Station(rank) Date: Observed -> Suggested (Anomaly, in std. devs.)\n")
        if(ks==3) cat("  Iteration Max_data_difference (Station_code)\n")
      }
      repeat {
        ite <- ite+1
        #- ite+=1 y obtener las series estimadas (dat.e|c) con las vecinas
        #  actualizando used, nrefs y mindist:
        for(i in 1:ne) { #para cada estaciÃ³n
          if(!iest[i]) next #estaciÃ³n borrada 
          if(refhom[i]) next #saltar estaciones confiables
          ik <- iest[i] #Ã­ndice de referencia estaciÃ³n inicial
          for(j in 1:nd) { #para cada dato
            se <- 0
            sw <- 0
            nr <- 0
            for(ir in 1:nei) { #para cada estaciÃ³n (posible referencia)
              kr <- est.p[ik,ir]
              krf <- which(iest==kr) #fragmentos de la referencia
              k <- which(!dat.na[j,krf]) #cuÃ¡l tiene dato observado?
              if(length(k)!=1) next #ningÃºn fragmento con dato
              k <- krf[k] #Ã­ndice del fragmento con dato
              if(i==k) next #es la misma estaciÃ³n
              nr <- nr+1 #no. de referencias
              used[i,k] <- TRUE #marca de estaciÃ³n usada
              #distancia mÃ­nima (distancia al dato mÃ¡s prÃ³ximo):
              if(nr==1) mindist[j,i] <- max(est.d[ik,kr],1)
              w <- est.w[ik,kr]
              se <- se + w * dat.z[j,k]
              sw <- sw + w
              if(nr>=nrefk) break #si no. mÃ¡x. de referencias, terminar
            }
            if(!nr) { #sin referencia!
              dat.e[j,i] <- dat.z[j,i] #conservar el dato original
              nrefs[j,i] <- NA
            } else {
              nrefs[j,i] <- nr
              #si solo hay una referencia, marcar para no corregir la serie:
              if(nr==1 & !is.na(oneref[j,i])) oneref[j,i] <- TRUE
              #no permitir datos negativos si std=2 (precipitaciÃ³n, etc):
              if(std==2 & se<0) se <- 0
              dat.e[j,i] <- se / sw #dato estimado (estandarizado)
            }
          }
        }
        #si hay NaN, convertirlos en NA. (Sucede a veces con std=2):
        n <- sum(is.nan(dat.e))
        if(n>0) {
          cat(n,"NaN's in dat.e ! (changing them to NA's...)\n")
          dat.e[is.nan(dat.e)] <- NA
        }
        #valores calculados por desestandarizaciÃ³n de dat.e:
        switch(std,
               for(ke in 1:ne) dat.c[,ke] <- dat.e[,ke]+dat.m[ke],     #std=1
               for(ke in 1:ne) dat.c[,ke] <- dat.e[,ke]*dat.m[ke],     #std=2
               for(ke in 1:ne) dat.c[,ke]<-dat.e[,ke]*dat.s[ke]+dat.m[ke],#std=3
               dat.c <- dat.e
        )
        #si habÃ­a acumulaciones de datos, repartirlas, grabarlas en el *.dat
        #y finalizar:
        if(!is.na(cumc)) {
          for(ke in 1:ne) { #para cada estaciÃ³n
            cumvl <- length(cumv[[ke]]) #no. de acumulaciones
            if(cumvl==0) next #estaciÃ³n sin acumulaciones
            for(j in 1:cumvl) {
              #un dato ausente intercalado? juntar las acumulaciones:
              if(is.na(cumv[[ke]][j])) {
                if(j<cumvl & cumA[[ke]][j+1]==cumB[[ke]][j]+1)
                  cumA[[ke]][j+1] <- cumA[[ke]][j]
                next
              }
              sumc <- sum(dat.c[cumA[[ke]][j]:cumB[[ke]][j],ke])
              #si la suma de valores calculados es cero, asignar ceros
              #y el valor acumulado asignarlo al Ãºltimo tÃ©rmino:
              if(sumc==0.0) { 
                dat[cumA[[ke]][j]:cumB[[ke]][j],ke] <- 0
                dat[cumB[[ke]][j],ke] <- cumv[[ke]][j]
                next #seguir con la siguiente acumulaciÃ³n
              }
              prop <- cumv[[ke]][j]/sumc #proporciÃ³n acumulado/suma de estimas
              zc <- dat.c[cumA[[ke]][j]:cumB[[ke]][j],ke] * prop
              zk <- which.max(zc) #posiciÃ³n del mÃ¡ximo valor calculado
              zc <- round(zc,ndec)
              zd <- cumv[[ke]][j] - sum(zc) #diferencia por redondeo
              #aÃ±adir la diferencia al valor mÃ¡s alto:
              if(zd!=0.0) zc[zk] <- zc[zk]+zd
              dat[cumA[[ke]][j]:cumB[[ke]][j],ke] <- zc
            }
          }
          #grabar un nuevos ficheros de entrada y finalizar:
          fcum <- sprintf('%s-cum_%d-%d',varcli,anyi,anyf) #raÃ­z fichero acum.
          file.rename(sprintf('%s.dat',fbas),sprintf('%s.dat',fcum))
          write(dat,sprintf('%s.dat',fbas))
          file.rename(sprintf('%s.est',fbas),sprintf('%s.est',fcum))
          write.table(est.c,sprintf('%s.est',fbas),row.names=FALSE,col.names=FALSE)
          cat('\nAccumulated values have been distributed among the previous days and written\n')
          cat('as new input files. Original input files have been renamed to\n')
          cat(sprintf('%s.dat and %s.est\n\n',fcum,fcum))
          cerrar(); return(invisible()) #terminar
        }
        #- cÃ¡lculo de anomalÃ­as (anom, sanom) y eliminaciÃ³n de outliers
        anom <- dat.z-dat.e #anomalÃ­as
        anom[dat.na] <- NA  #no arrastrar anomalÃ­as de datos rellenados
        #estandarizar las anomalÃ­as:
        anomm <- apply(anom,2,mean,na.rm=TRUE) #anomalÃ­as medias
        anoms <- apply(anom,2,sd,na.rm=TRUE) #desv. tÃ­picas de las anomalÃ­as
        sanom <- scale(anom,center=anomm,scale=anoms)
        if(!expl & dz.maxd>.1) { #eliminar outliers:
          elim <- sanom < dz.mind | sanom > dz.maxd #datos a eliminar
          elim[is.na(elim)] <- FALSE #eliminar los molestos NA
          elim[,refhom] <- FALSE #no modificar las series confiables
          nelim <- sum(elim) #no. de datos a eliminar
          if(nelim>0) { #eliminar los datos originales anÃ³malos
            #listado de los datos a eliminar:
            for(i in 1:ne) {
              for(j in 1:nd) if(elim[j,i] & !is.na(oneref[j,i])) {
                outan[j,iest[i]] <- sanom[j,i] #guardar la anomalÃ­a del outlier
                do <- dat.d[j,i] #dato original
                dc <- dat.c[j,i] #dato calculado
                if(trf==1) { do <- expm1(do); dc <- expm1(dc) }
                else if(trf>1) { do <- do^trf; dc <- dc^trf }
                cat(sprintf('%s(%d) %s',est.c[i,4],i,format(x[j])))
                cat(": ",do," -> ",round(dc,ndec)," (",round(sanom[j,i],2),
                    ")",sep="")
                #no eliminar si solo tenÃ­an una referencia!:
                if(oneref[j,i] & !force & nrefk>1) {
                  cat(" Only 1 reference! (Unchanged)")
                  elim[j,i] <- FALSE
                }
                else { #escribir en Fout con flag 1
                  write(c(est.c[iest[i],4],format(x[j]),round(do,ndec),
                          round(dc,ndec),round(sanom[j,i],2),1),Fout,ncolumns=6,sep=',')
                }
                cat("\n")
              }
            }
            dat[elim] <- NA #eliminaciÃ³n de los datos anÃ³malos
            dat.na[elim] <- TRUE #actualizaciÃ³n Ã­ndice de datos ausentes
          }
          else if(!aref) cat('(No detected outliers)\n')
        }
        #list suspect values? :
        if(ks==3 & (!is.na(dz.maxw) | !is.na(dz.minw))) {
          #suspect values but not big outliers:
          susp <- (sanom < dz.minw & sanom >= dz.mind) | 
            (sanom > dz.maxw & sanom <= dz.maxw)
          susp[is.na(susp)] <- FALSE #eliminar los molestos NA
          susp[,refhom] <- FALSE #obviar las series confiables
          nsusp <- sum(susp) #no. de datos sospechosos
          if(nsusp>0) { #listar los datos originales sospechosos:
            for(i in 1:ne) {
              for(j in 1:nd) if(susp[j,i] & !is.na(oneref[j,i])) {
                do <- dat.d[j,i] #dato original
                dc <- dat.c[j,i] #dato calculado
                if(trf==1) { do <- expm1(do); dc <- expm1(dc) }
                else if(trf>1) { do <- do^trf; dc <- dc^trf }
                #no listarlo como sospechoso si solo tenÃ­a una referencia!:
                if(oneref[j,i] & !force & nrefk>1) susp[j,i] <- FALSE
                else { #escribir en Fout con flag 0
                  write(c(est.c[iest[i],4],format(x[j]),round(do,ndec),
                          round(dc,ndec),round(sanom[j,i],2),0),Fout,ncolumns=6,sep=',')
                }
              }
            }
          }
        }
        #- relleno de las lagunas de datos
        dat.d[dat.na] <- dat.c[dat.na] 
        if(ite>1) {
          maxddif <- max(abs(dat.d-dat.d0),na.rm=TRUE) #mÃ¡x. dat. dif.
          kmaxdif <- which.max(abs(dat.d-dat.d0)) #posiciÃ³n mÃ¡x. dat. dif.
          kmaxest <- ceiling(kmaxdif/nd) #estaciÃ³n mÃ¡x. dat. dif.
          maxsdif <- (dat.d-dat.d0)[kmaxdif %% nd,kmaxest]
        }
        dat.d0 <- dat.d #copia de los datos
        #- actualizar dat.m|s|z
        dat.m <- apply(dat.d,2,mean,na.rm=TRUE)
        if(std==3) dat.s <- apply(dat.d,2,sd,na.rm=TRUE)
        switch(std,
               dat.z <- scale(dat.d,center=dat.m,scale=FALSE),     #std=1
               dat.z <- scale(dat.d,center=FALSE,scale=dat.m),     #std=2
               dat.z <- scale(dat.d,center=dat.m,scale=dat.s),     #std=3
               dat.z <- dat.d
        )
        #- if(!aref) break (no afinar los datos ausentes hasta el final)
        #  porque no parece necesario y alarga el tiempo de proceso 
        if(!aref) break
        #- if(ite>1), si los datos ya no varÃ­an o dejan de converger, break
        if(ite>1) {
          cat(ite,' ',round(maxsdif,ndec+2)," (",est.c[kmaxest,4],")\n",sep="")
          if(maxddif<=maxdif | ite==maxite) {
            if(ite==maxite) cat("\nAverage calculation skipped after",ite,"iterations\n")
            else cat("Prescribed convergence reached\n\n")
            break
          }
          #(maxddif>maxddif0 se puede dar eventualmente en las primeras
          # iteraciones cuando se eliminan datos anÃ³malos, de ahÃ­ ite>10)
          if(ite>10) {
            if(maxddif>maxddif0) {
              cat("Data convergence broken\n\n")
              break
            }
          }
          maxddif0 <- maxddif #guardar mÃ¡x. dat. dif. para siguiente iteraciÃ³n
        }
      }
      #- if(aref==TRUE), repetir relleno de lagunas con autocorrecciÃ³n
      if(aref==TRUE) { # de las series fragmentadas:
        cat('Last series readjustment (please, be patient...)\n')
        #- obtener las series estimadas (dat.e, dat.c) con las vecinas
        #  y actualizar used[ne,ne], nrefs[nd,ne] y mindist[ne,ne]:
        for(i in 1:ne) { #para cada estaciÃ³n
          if(!iest[i]) next #estaciÃ³n borrada
          ik <- iest[i] #Ã­ndice de referencia estaciÃ³n inicial
          for(j in 1:nd) { #para cada dato
            se <- 0
            sw <- 0
            nr <- 0
            for(ir in 1:nei) { #para cada estaciÃ³n (posible referencia)
              kr <- est.p[ik,ir]
              krf <- which(iest==kr) #fragmentos de la referencia
              k <- which(!dat.na[j,krf]) #cuÃ¡l tiene dato observado?
              if(length(k)!=1) next #ningÃºn fragmento con dato
              k <- krf[k] #Ã­ndice del fragmento con dato
              if(i==k) next #es la misma estaciÃ³n
              nr <- nr+1 #no. de referencias
              used[i,k] <- TRUE #marca de estaciÃ³n usada
              #distancia mÃ­nima (distancia al dato mÃ¡s prÃ³ximo):
              if(nr==1) mindist[j,i] <- max(est.d[ik,kr],1)
              w <- est.w[ik,kr]
              se <- se + w * dat.z[j,k]
              sw <- sw + w
              #si no. mÃ¡x. de referencias, o autoreferencia, terminar:
              if(nr>=nrefk | (aref & ir==1)) break
            }
            if(!nr) { #sin referencia!
              dat.e[j,i] <- dat.z[j,i] #conservar el dato original
              nrefs[j,i] <- NA
            } else {
              nrefs[j,i] <- nr
              #si solo hay una referencia, marcar para no corregir la serie:
              if(nr==1 & !is.na(oneref[j,i])) oneref[j,i] <- TRUE
              #no permitir datos negativos si std=2 (precipitaciÃ³n, etc):
              if(std==2 & se<0) se <- 0
              dat.e[j,i] <- se / sw #dato estimado (estandarizado)
            }
          }
        }
        #si hay NaN, convertirlos en NA. (Sucede a veces con std=2):
        n <- sum(is.nan(dat.e))
        if(n>0) {
          cat(n,"NaN's in dat.e ! (changing them to NA's...)\n")
          dat.e[is.nan(dat.e)] <- NA
        }
        #valores calculados por desestandarizaciÃ³n de dat.e:
        switch(std,
               for(ke in 1:ne) dat.c[,ke] <- dat.e[,ke]+dat.m[ke],     #std=1
               for(ke in 1:ne) dat.c[,ke] <- dat.e[,ke]*dat.m[ke],     #std=2
               for(ke in 1:ne) dat.c[,ke]<-dat.e[,ke]*dat.s[ke]+dat.m[ke],#std=3
               dat.c <- dat.e
        )
        #- relleno de los datos ausentes
        dat.d[dat.na] <- dat.c[dat.na]
        if(!is.na(vmax)) dat.d[dat.d > vmax] <- vmax
        if(!is.na(vmin)) dat.d[dat.d < vmin] <- vmin
        #- actualizar dat.m|s|z
        dat.m <- apply(dat.d,2,mean,na.rm=TRUE)
        if(std==3) dat.s <- apply(dat.d,2,sd,na.rm=TRUE)
        switch(std,
               dat.z <- scale(dat.d,center=dat.m,scale=FALSE),     #std=1
               dat.z <- scale(dat.d,center=FALSE,scale=dat.m),     #std=2
               dat.z <- scale(dat.d,center=dat.m,scale=dat.s),     #std=3
               dat.z <- dat.d
        )
      }
      #- calcular los valores finales de las anomalÃ­as (anom, sanom)
      anom <- dat.z-dat.e #anomalÃ­as
      anom[dat.na] <- NA  #no arrastrar anomalÃ­as de datos rellenados!
      anomm <- apply(anom,2,mean,na.rm=TRUE) #anomalÃ­as medias
      anoms <- apply(anom,2,sd,na.rm=TRUE) #desv. tÃ­picas de las anomalÃ­as
      sanom <- scale(anom,center=anomm,scale=anoms) #anomalÃ­as estandarizadas
      #- ----------- AnÃ¡lisis de saltos en la media (binary split):
      #- if(ks>2) break (en la Ãºltima etapa, solo relleno final de lagunas)
      if(ks>2) break
      #analizar los saltos en la media de las series, cortÃ¡ndolas cuando
      #el mÃ¡ximo tVx supere el umbral (inh):
      nn <- 0 #inic. no. de nuevas estaciones
      #     tVx <- rep(NA,ne) #mÃ¡ximos valores del shift test (por estaciÃ³n)
      #     kpx <- rep(NA,ne) #posiciones de los mÃ¡ximos tV (por estaciÃ³n)
      splt <- rep(0,ne)  #tV con que se cortaron las estaciones
      modif <- FALSE #inicializaciÃ³n modificaciÃ³n series
      cat("\nPerforming shift analysis on the",ne,"series...\n")
      y <- sanom; y[is.na(nrefs)] <- NA #eliminar anomalÃ­as sin referencia
      y[,refhom] <- NA #eliminar series confiables
      if(ks==1) tkx <- apply(y,2,wtest,swa,sts,test)
      else tkx <- apply(y,2,test,sts)
      tVx <- tkx[1,]; kpx<- as.integer(tkx[2,])
      #- cortar las series cuyo tVx supere el umbral, de mayor a menor
      #  siempre que no se hayan usado series reciÃ©n cortadas con tVx similar
      #mÃ¡ximo tVx de todas las estaciones:
      if(sum(!is.na(tVx))==0) tVxx <- 0 else tVxx <- max(tVx,na.rm=TRUE)
      while(tVxx > inh) {
        i <- which.max(tVx) #estaciÃ³n con el mÃ¡ximo tVx
        #si i usÃ³ referencias cortadas con un tVx demasiado grande, iniciar
        #una nueva iteraciÃ³n:
        if(max(splt[used[i,]])>tVxx*(1+tol*min(nr,sum(used[i,])))) break
        kp <- kpx[i] #posiciÃ³n del tVx en la estaciÃ³n i
        if(oneref[kp,i] & !force & nrefk>1) { #no cortar con una sola referencia
          tVx[i] <- -1 #pasar el tVx de esta estaciÃ³n a -1
          tVxx <- max(tVx,na.rm=TRUE) #mÃ¡ximo tVx de las estaciones restantes
          next
        }
        cat(sprintf('\n%s(%d) breaks at %s (%.1f)',est.c[i,4],i,
                    format(x[kp]),tVx[i]))
        write(sprintf('%s,%s,%.1f',est.c[iest[i],4],format(x[kp]),tVx[i]),
              Fbrk,ncolumns=3)
        #grÃ¡fico de anomalÃ­as con la posiciÃ³n del corte:
        if(gp>1) {
          y <- sanom[,i] #vector de anomalÃ­as de la estaciÃ³n
          ylab="Standardized anomalies (observed - computed)"
          tit <- sprintf('%s   %d (%s)\n%s',varcli,i,est.c[i,4],est.c[i,5])
          plot(x,y,type="h",lwd=lw,ylim=c(-5,5),main=tit,xlab='Time',ylab=ylab,col=hsv(.7,1,.9))
          z <- class(x)
          if(z[1]=='Date' | z[1]=='POSIXct') {
            grid(NA,NULL,col=grdcol)
            if(z[1]=='Date') abline(v=axis.Date(1,x),lty=3,col=grdcol)
            else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x),lty=3,col=grdcol)
          } else grid(col=grdcol)
          abline(-3,0,lty=3,col=grdcol); abline(-5,0,lty=3,col=grdcol)
          lines(x,log10(nrefs[,i])-5,col='orange2')
          lines(x,log10(mindist[,i])-5,col=verde)
          mtext(" 1",4,las=1,adj=0,at=-5,col=verde)
          mtext(" 10",4,las=1,adj=0,at=-4,col=verde)
          mtext(" 100",4,las=1,adj=0,at=-3,col=verde)
          mtext("min.d.",4,las=1,adj=0,at=-5.4,col=verde)
          mtext(" (km)",4,las=1,adj=0,at=-2,col=verde)
          mtext("n.ref.",4,las=1,adj=0,at=-5.8,col='orange2')
          lines(rep(x[kp],2),c(-5,4.8),col="red",lty=2) #marca del corte
          text(x[kp],5,floor(tVxx))
        }
        #contar no. de saltos por aÃ±o
        z <- as.integer(strftime(x[kp],"%Y"))-anyi+1 #tÃ©rmino anual del salto
        nsy[z] <- nsy[z] + 1 #no. de saltos por aÃ±o
        #dividir la serie por el punto de corte:
        nd1 <- sum(!is.na(dat[1:(kp-1),i])) #no. de datos del fragmento 1
        nd2 <- sum(!is.na(dat[kp:nd,i])) #no. de datos del fragmento 2
        if(nd1 < mindat & nd2 < mindat) stop(sprintf('\nBoth fragments have less than %d data. Please, remove this series\nfrom the input files or increase the mindat parameter.',mindat))
        del <- 0 #flag of deleted fragments
        if(std==2) { #controlar que las medias no sean < 1:
          md1 <- mean(dat[1:(kp-1),i],na.rm=TRUE)
          md2 <- mean(dat[kp:nd,i],na.rm=TRUE)
        } else md1 <- md2 <- NA
        if(nd1 < mindat) {
          dat[1:(kp-1),i] <- NA; del <- del + 1
          cat(" Fragment with less than",mindat,"data DELETED")
        } else if(std==2 & md1<1) {
          dat[1:(kp-1),i] <- NA; del <- del + 1
          cat(" Fragment with mean < 1 DELETED")
        }
        if(nd2 < mindat) {
          dat[kp:nd,i] <- NA; del <- del + 1
          cat(" Fragment with less than",mindat,"data DELETED")
        } else if(std==2 & md2<1) {
          dat[kp:nd,i] <- NA; del <- del + 1
          cat(" Fragment with mean < 1 DELETED")
        }
        if(del==0) {
          nn <- nn+1 #incrementamos el no. de nuevas series
          iest <- c(iest,iest[i]) #aÃ±adir Ã­ndice a la serie original
          nsp[iest[i]] <- nsp[iest[i]]+1 #y tambiÃ©n su no. de saltos
          dat <- cbind(dat,rep(NA,nd)) #nueva columna de datos
          #pasar los datos anteriores al corte a la nueva serie:
          dat[1:(kp-1),ne+nn] <- dat[1:(kp-1),i]
          dat[1:(kp-1),i] <- NA #borrar los datos pasados a la nueva serie
          #copiar las coordenadas y poner sufijo a indicativo y nombre:
          #(Usamos la lista original de estaciones, por si se borra alguna)
          z <- data.frame(est.i[iest[i],1:3],paste(est.i[iest[i],4],"-",1+nsp[iest[i]],sep=""),paste(est.i[iest[i],5],"-",1+nsp[iest[i]],sep=""))
          names(z) <- names(est.i)
          est.c <- rbind(est.c,z)
          #asignar mismas medias (y desv. tÃ­picas?) al nuevo fragmento:
          dat.m <- c(dat.m,dat.m[i])
          if(std==3) dat.s <- c(dat.s,dat.s[i])
        }
        #actualizar tVx y banderas para continuar el bucle:
        modif <- TRUE #marcar si se han modificado series
        splt[i] <- tVx[i] #tV de corte de la estaciÃ³n i
        tVx[i] <- 0 #anular el tVx de esta estaciÃ³n
        tVxx <- max(tVx,na.rm=TRUE) #mÃ¡ximo tVx de las estaciones restantes
      }
      if(nn) {
        cat("\n\nUpdate number of series: ",ne,"+",nn,"= ")
        ne <- ne+nn  #actualizar el no. de estaciones
        cat(ne,"\n")
        refhom <- c(refhom,rep(FALSE,nn)) #actualizar vector de refhom
      }
      #- sin nuevos cortes? histogramas de tVx y breaks
      if(!nn & !modif) {
        if(gp>1) {
          #histograma de tVx globales (sin 0's, que no son reales):
          z <- tVx[!is.na(tVx) & tVx>0]
          main <- sprintf("Histogram of maximum %s (Stage %d)",inhtest,ks)
          if(sum(!is.na(z))) hist(z,breaks=20,xlab=inhtest,col="purple",main=main)
          if(ks==2 | inh<1) {
            #histograma de no. de cortes por estaciÃ³n
            #(no tiene en cuenta los #de fragmentos demasiado cortos,
            #que sÃ­ figuran en la lista final de breaks):
            hist(nsp,breaks=0:max(9,max(nsp)+1)-.5,col="orange2",xlab="Number of splits",ylab="Number of stations",main="Number of splits per station")
            #frecuencias de fragmentaciÃ³n por aÃ±os:
            w <- min(5,ceiling(400/na)) #anchura de las barras
            plot(anyi:anyf,nsy,type="h",lwd=w,col=2,ylim=c(0,max(10,max(nsy))),xlab="Years",ylab="Number of splits",main="Number of splits per year")
            grid(col=grdcol)
          }
        }
        #lista de posibles cortes que solo tienen una referencia:
        z <- which(tVx<0)
        if(length(z)>0) {
          cat('Series that could break but had only one reference:\n')
          print(est.c[z,4])
        }
        break #salir del bucle para ir al siguiente nivel
      }
    }
  }
  #------------ Fin de las tres fases de la homogeneizaciÃ³n ---------
  #RMSE de los datos calculados:
  if(trf==1) z <- expm1(dat.c)
  else if(trf>1) z <- dat.c^trf
  else z <- dat.c
  zo <- dat.o[,iest]; zo[dat.na] <- NA
  rmse <- apply((z-zo)^2,2,function(x) sqrt(mean(x,na.rm=TRUE)))
  #- grÃ¡ficos de anomalÃ­as de las series homogeneizadas
  #  (con tVx mÃ¡ximos, ordenados por series originales):
  tVx <- rep(NA,ne) #(guardaremos los tVx finales por ventanas)
  inhx <- rep(NA,ne) #(guardaremos los tVx finales de las series completas)
  for(io in 1:nei) { #para cada serie original
    wi <- which(iest==io) #estaciones derivadas de la estaciÃ³n io
    lwi <- length(wi)
    if(!lwi) next #(estaciÃ³n totalmente borrada!) 
    for(i in wi) { #para cada serie derivada de la original
      y <- sanom[,i] #anomalÃ­as estandarizadas de la estaciÃ³n
      if(gp>1) {
        ylab="Standardized anomalies (observed - computed)"
        tit <- sprintf('%s   %d (%s)\n%s',varcli,i,est.c[i,4],est.c[i,5])
        plot(x,y,type="h",lwd=lw,ylim=c(-5,5),main=tit,xlab='Time',ylab=ylab,col=hsv(.7,1,.9))
        z <- class(x)
        if(z[1]=='Date' | z[1]=='POSIXct') {
          grid(NA,NULL,col=grdcol)
          if(z[1]=='Date') abline(v=axis.Date(1,x),lty=3,col=grdcol)
          else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x),lty=3,col=grdcol)
        } else grid(col=grdcol)
        abline(-3,0,lty=3,col=grdcol); abline(-5,0,lty=3,col=grdcol)
        lines(x,log10(nrefs[,i])-5,col='orange2')
        lines(x,log10(mindist[,i])-5,col=verde)
        mtext(" 1",4,las=1,adj=0,at=-5,col=verde)
        mtext(" 10",4,las=1,adj=0,at=-4,col=verde)
        mtext(" 100",4,las=1,adj=0,at=-3,col=verde)
        mtext("min.d.",4,las=1,adj=0,at=-5.4,col=verde)
        mtext(" (km)",4,las=1,adj=0,at=-2,col=verde)
        mtext("n.ref.",4,las=1,adj=0,at=-5.8,col='orange2')
      }
      #aplicar wtest y marcar su tV mÃ¡ximo (si >=1):
      st <- wtest(y,swa,sts,test); tVx[i] <- st[1]; zz <- floor(st[1])
      if(zz) {
        kp <- as.integer(st[2])
        if(gp>1) {
          lines(rep(x[kp],2),c(-5,4.8),col=verde,lty=2) #marca mÃ¡ximo wtest
          text(x[kp],5,zz,col=verde) #valor
        }
      }
      #aplicar test y marcar su mÃ¡ximo:
      st <- eval(call(test,y,sts))
      inhx[i] <- round(st[1],1); zz <- floor(st[1])
      if(!is.na(zz) & zz & gp>1) {
        kp <- round(st[2])
        lines(rep(x[kp],2),c(-5,4.8),lty=4) #marca tVx (mÃ¡ximo test)
        text(x[kp],-5.2,zz) #valor
      }
    }
  }
  #datos homogeneizados:
  if(trf==1) dah <- expm1(dat.d) #deshacer transformaciÃ³n logarÃ­tmica
  else if(trf>1) dah <- dat.d^trf #deshacer transformaciÃ³n raÃ­z
  else dah <- dat.d
  dah <- round(dah,ndec) #redondear con el no. de decimales deseado
  #- grÃ¡ficos de las series homogeneizadas y sus correcciones
  if(gp>2) {
    par(old.par)
    plot(-1:1,-1:1,type="n",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
    text(0,0.4,"Final graphics",cex=3.5)
    text(0,-0.3,"Adjusted series and\napplied corrections",cex=2.5)
    if(nm>0) xlab <- "Years" else xlab <- "Dates"
    layout(matrix(1:2,2,1,byrow=TRUE))
    par(las=1,cex=.8*cex)
    #filtros para obtener valores anuales:
    ndpy <- round(nd/na) #no. of data per year
    if(nd<=120 | nd<ndpy*3) { fltr=1; ylabd <- "Data" }#pocos datos? no filtrar
    else {
      fltr <- rep(1,ndpy)
      if(gp>3) ylabd <- "Running annual totals"
      else {
        ylabd <- "Running annual means"
        fltr <- fltr/ndpy
      }
    }
    if(!is.na(uni)) ylabd <- sprintf('%s (%s)',ylabd,uni)
    for(i in 1:nei) { #para cada estaciÃ³n original
      wi <- which(iest==i) #estaciones derivadas de la estaciÃ³n i
      lwi <- length(wi)
      if(!lwi) next #(estaciÃ³n totalmente borrada!) 
      if(lwi>1) vi <- TRUE else vi <- FALSE
      #filtros para valores anuales 
      tit <- sprintf('%s   %d (%s)\n%s',varcli,i,est.c[i,4],est.c[i,5])
      yo <- as.vector(dat.o[,i]) #datos originales
      y <- dah[,wi] #datos homogeneizados
      par(mar=c(0,4,4,2),xaxt="n")
      yf <- stats::filter(y,fltr) #filter to subset
      ylim <- c(floor(min(yf,na.rm=TRUE)),ceiling(max(yf,na.rm=TRUE)))
      #(no usamos matplot porque no maneja bien las fechas en el eje X)
      plot(x,stats::filter(yo,fltr),type="n",ylim=ylim,ylab=ylabd,main=tit) #filter to subset
      matlines(x,yf,lty=1,col=2:20)
      lines(x,stats::filter(yo,fltr)) #repasar la lÃ­nea de datos originales, filter to subset
      par(xaxt="s")
      z <- class(x)
      if(z[1]=='Date' | z[1]=='POSIXct') {
        grid(NA,NULL,col=grdcol)
        if(z[1]=='Date') abline(v=axis.Date(1,x,tick=FALSE,labels=FALSE),
                                lty=3,col=grdcol)
        else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x,tick=FALSE,
                                                       labels=FALSE),lty=3,col=grdcol)
      } else grid(col=grdcol)
      par(mar=c(5,4,0,2),xaxt="s")
      #correcciones:
      if(std==2) {
        yo[yo==0] <- NA; y[y==0] <- NA
        yd <- y/yo; ylab <- "Correction factors"
        if(!vi) ylim <- c(0,2)
      } else {
        yd <- y-yo; ylab <- "Correction terms"
        if(!is.na(uni)) ylab <- sprintf('%s (%s)',ylab,uni)
        if(!vi) ylim <- c(-1,1)
      }
      if(vi) {
        ylim <- c(floor(min(yd,na.rm=TRUE)),ceiling(max(yd,na.rm=TRUE)))
        plot(x,yd[,1],type="n",ylim=ylim,ylab=ylab,xlab='Time')
      } else {
        if(trf) ylim <- c(floor(min(yd,na.rm=TRUE)),ceiling(max(yd,na.rm=TRUE)))
        plot(x,yd,type="n",ylim=ylim,ylab=ylab,xlab='Time')
      }
      matlines(x,yd,type="l",lty=1,col=2:20)
      z <- class(x)
      if(z[1]=='Date' | z[1]=='POSIXct') {
        grid(NA,NULL,col=grdcol)
        if(z[1]=='Date') abline(v=axis.Date(1,x),lty=3,col=grdcol)
        else if(z[1]=='POSIXct') abline(v=axis.POSIXct(1,x),lty=3,col=grdcol)
      } else grid(col=grdcol)
    }
    par(old.par); par(cex=cex)
  }
  if(sum(inht)>0) cat("\n======== End of the homogenization process, after ")
  else cat("\n======== End of the missing data infilling process, after ")
  cat(format(round(Sys.time()-time1,2)),'\n')
  cat("\n----------- Final calculations:\n")
  #prueba inhtest de cada estaciÃ³n
  if(inhtest=='SNHT') cat("\nSNHT: Standard normal homogeneity test (on anomaly series)\n")
  else cat("\nCucT: Cucconi test (on anomaly series)\n")
  print(summary(round(inhx,1)))
  #errores tÃ­picos de las estimas (sin estandarizar):
  cat("\nRMSE: Root mean squared error of the estimated data\n")
  zz <- summary(rmse)
  print(zz)
  sedec <- max(1,2-ceiling(log10(zz[4]))) #no. de decimales de RMSE
  rmse <- round(rmse,sedec) #redondear los RMSE
  pod <- floor(100*(nd-apply(dat.na,2,sum))/nd) #porcentaje de datos originales
  cat("\nPOD: Percentage of original data\n")
  print(summary(pod))
  #- imprimir resumen de resultados
  cat("\n")
  df <- data.frame(SNHT=inhx,RMSE=rmse,POD=pod,Code=est.c[,4],Name=est.c[,5])
  names(df)[1] <- inhtest
  print(df,right=FALSE)
  cat(sprintf('\nFrequency distribution tails of residual anomalies and %s:\n\n',inhtest))
  cat("Left tail of standardized anomalies:\n")
  print(round(quantile(sanom,probs=c(.001,.002,.005,.01,.02,.05,.1),na.rm=TRUE),1))
  cat("Right tail of standardized anomalies:\n")
  print(round(quantile(sanom,probs=c(.9,.95,.98,.99,.995,.998,.999),na.rm=TRUE),1))
  cat(sprintf('Right tail of %s on windows of %d terms with up to %d references:\n',inhtest,2*swa,nref[3]))
  print(round(quantile(tVx,probs=c(.9,.95,.98,.99,.995,.998,.999),na.rm=TRUE),1))
  cat(sprintf('Right tail of %s with up to %d references:\n',inhtest,nref[3]))
  print(round(quantile(inhx,probs=c(.9,.95,.98,.99,.995,.998,.999),na.rm=TRUE),1))
  #aÃ±adir nuevas columnas a la tabla de estaciones (porcentaje de datos
  #originales, inhx y RMSE):
  est.c <- cbind(est.c,pod,inhx,rmse)
  #redondear los datos de entrada para los grÃ¡ficos y los resultados finales
  dat <- round(dat,ndec)
  #- if(gp>1), Ãºltimos grÃ¡ficos (hist. de anomalÃ­as y snht; calidad/singular.)
  if(gp>1) {
    #histograma de las anomalÃ­as (las de los outliers, en rojo):
    main <- "Histogram of standardized anomalies"
    z <- hist(c(sanom,outan),plot=FALSE)
    zx <- z$breaks
    zy <- z$counts; zy[zy==0] <- NA
    barplot(zy,log='y',space=0,ylim=c(.9,max(zy,na.rm=TRUE)*2),ylab='Frequency',col='green',main=main,xlab='Anomalies (standard deviations)')
    axis(1,1:length(zx)-1,labels=as.character(zx),las=2)
    if(sum(!is.na(outan))) { #repintar las frec. de outan en rojo
      zy <- hist(outan,breaks=zx,plot=FALSE)$counts; zy[zy==0] <- NA
      barplot(zy,log='y',space=0,ylim=c(.9,max(zy,na.rm=TRUE)*2),col=hsv(0,.75),add=TRUE)
    }
    #histograma de tVx por ventanas (sin 0's, que no son reales):
    z <- tVx[!is.na(tVx) & tVx>0]
    main <- sprintf("Histogram of maximum windowed %s",inhtest)
    if(sum(!is.na(z))) hist(z,breaks=20,xlab=inhtest,col=verde,main=main)
    #histograma de tVx en series completas:
    z <- inhx; main <- sprintf("Histogram of maximum global %s",inhtest)
    if(sum(!is.na(z))) hist(z,breaks=20,xlab=inhtest,col="purple",main=main)
    #grÃ¡fico de calidad/singularidad:
    if(is.na(uni)) xlab <- 'RMSE' else xlab <- sprintf('RMSE (%s)',uni)
    plot(rmse,inhx,type="n",xlim=c(0,max(1,max(rmse,na.rm=TRUE))),ylim=c(0,max(50,max(inhx,na.rm=TRUE))),xlab=xlab,ylab=inhtest,main="Station's quality/singularity")
    grid(col=grdcol)
    text(rmse,inhx,col=hsv(.7,1,.9))
  }
  if(gp>0) { par(old.par); graphics.off() } #cerrar la salida grÃ¡fica
  #- grabar los resultados en un fichero rda
  kelim <- rev(which(iest==0)) #estaciones eliminadas (en orden inverso)
  nelim <- length(kelim) #no. de estaciones eliminadas
  if(nelim>0) { #si se eliminaron estaciones:
    #ajustar los Ã­ndices de estaciÃ³n original:
    for(ke in 1:nelim) iest[iest>kelim[ke]] <- iest[iest>kelim[ke]] - 1
    dat <- dat.o[,iest[1:nei]>0] #datos originales sin series eliminadas
    nei <- sum(iest[1:nei]>0) #no. de estaciones originales no eliminadas
    dah <- dah[,iest>0] #datos homogeneizados sin series eliminadas
    ne <- sum(iest>0) #no. de estaciones homogeneizadas no eliminadas
    est.c <- est.c[iest>0,] #lista de estaciones homogeneizadas no eliminadas
  }
  else dat <- dat.o
  if(nm>1 & acomp) {
    dim(dat) <- c(nm,na,nei)
    dim(dah) <- c(nm,na,ne)
  }
  names(est.c) <- c('X','Y','Z','Code','Name','pod',test,'rmse')
  rownames(est.c) <- 1:ne
  if(gp>0 & exists('ct')) save(dat,dah,nd,ndec,uni,est.c,corm,ct,nei,ne,nm,std,x,ini, file=sprintf('%s.rda',fbas),version=2)
  else save(dat,dah,nd,ndec,uni,est.c,nei,ne,nm,std,x,ini, file=sprintf('%s.rda',fbas),version=2)
  #ordenar archivos de outliers y breaks:
  if(!metad) { 
    close(Fbrk)
    brk <- read.csv(sprintf('%s_brk.csv',fbas),colClasses=c("character","character","numeric"))
    brk <- brk[order(brk[,1],brk[,2]),]
    write.csv(brk,sprintf('%s_brk.csv',fbas),row.names=FALSE)
  }
  close(Fout)
  out <- read.csv(sprintf('%s_out.csv',fbas),colClasses=c("character","character","numeric","numeric","numeric","numeric"),check.names=FALSE)
  #remove duplicates keeping the last rows:
  out <- out[as.integer(row.names(unique(out[,1:2],fromLast=TRUE))),]
  out <- out[order(out[,1],out[,2]),] #ordenar por estaciones y fechas
  write.csv(out,sprintf('%s_out.csv',fbas),row.names=FALSE)
  cat("\n----------- Generated output files: -------------------------\n\n")
  cat(sprintf('%s.txt :  This text output',fbas),'\n')
  cat(sprintf('%s_out.csv :  List of corrected outliers',fbas),'\n')
  cat(sprintf('%s_brk.csv :  List of corrected breaks',fbas),'\n')
  if(gp>0) cat(sprintf('%s.pdf :  Diagnostic graphics',fbas),'\n')
  cat(sprintf('%s.rda :  Homogenization results.',fbas))
  cat(' Postprocess with (examples):\n')
  cat(sprintf('   dahstat(\'%s\',%d,%d) #get averages in file %s-me.csv',varcli,anyi,anyf,fbas),'\n')
  cat(sprintf('   dahstat(\'%s\',%d,%d,stat=\'tnd\') #get OLS trends and their p-values',varcli,anyi,anyf),'\n')
  cat(sprintf('   dahgrid(\'%s\',%d,%d,grid=YOURGRID) #get homogenized grids',varcli,anyi,anyf),'\n')
  cat('   ... (See other options in the package documentation)\n\n')
  while (sink.number()>0) sink() #cerrar bitÃ¡cora(s)
}

#- outrename.- Append a suffix to the output files, to avoid overwrites.
outrename <- function(varcli, anyi, anyf, suffix, restore=FALSE) {
  #if restore=TRUE, the suffix will be removed! 
  fbn <- sprintf('%s_%d-%d',varcli,anyi,anyf) #original file base name
  #destination file base name:
  fbn2 <- sprintf('%s-%s_%d-%d',varcli,suffix,anyi,anyf)
  for(ext in c(".txt",".pdf")) {
    if(restore) file.rename(paste(fbn2,ext,sep=""),paste(fbn,ext,sep=""))
    else file.rename(paste(fbn,ext,sep=""),paste(fbn2,ext,sep=""))
  }
  if(restore) {
    name <- sprintf('%s.rda',fbn2)
    if(file.exists(name)) file.rename(name,sprintf('%s.rda',fbn))
    name <- sprintf('%s_out.csv',fbn2)
    if(file.exists(name)) file.rename(name,sprintf('%s_out.csv',fbn))
    name <- sprintf('%s_brk.csv',fbn2)
    if(file.exists(name)) file.rename(name,sprintf('%s_brk.csv',fbn))
  } else {
    name <- sprintf('%s.rda',fbn)
    if(file.exists(name)) file.rename(name,sprintf('%s.rda',fbn2))
    name <- sprintf('%s_out.csv',fbn)
    if(file.exists(name)) file.rename(name,sprintf('%s_out.csv',fbn2))
    name <- sprintf('%s_brk.csv',fbn)
    if(file.exists(name)) file.rename(name,sprintf('%s_brk.csv',fbn2))
  }
  return(invisible())
}

#- rdamerge.- Merge *.rda results from split areas into a single *.rda
rdamerge <- function(varcli,anyi,anyf) {
  fbas <- sprintf('%s_%d-%d',varcli,anyi,anyf) #base file name
  na <- anyf-anyi+1 #no. of years
  xy <- read.table(sprintf('%s.split',fbas))
  ns <- nrow(xy) #no. of split areas
  #initializations:
  tnei <- tne <- 0 #total no. of initial and final stations
  Fo <- file(sprintf('%s.auxo',fbas),'w') #original series
  Fh <- file(sprintf('%s.auxh',fbas),'w') #homogenized series (last)
  Ff <- file(sprintf('%s.auxf',fbas),'w') #homogenized series (former)
  Fe <- file(sprintf('%s.auxe',fbas),'w') #stations file (last)
  Fa <- file(sprintf('%s.auxa',fbas),'w') #stations file (former)
  for(i in 1:ns) {
    fn <- sprintf('%s-%d_%d-%d.rda',varcli,i,anyi,anyf) #file name
    cat(fn)
    if(!file.exists(fn)) { cat(' Not found!\n'); next }
    load(fn)
    #selection of series inside limits:
    sel <- est.c[,1]>=xy[i,1] & est.c[,1]<xy[i,2] & est.c[,2]>=xy[i,3] & est.c[,2]<xy[i,4]
    z <- dim(dat); if(length(z)>2) dim(dat) <- c(z[1]*z[2],z[3])
    z <- dim(dah); if(length(z)>2) dim(dah) <- c(z[1]*z[2],z[3])
    write(dat[,sel[1:nei]],Fo,ncolumns=ifelse(nm==12,12,10))
    zsel <- sel; zsel[(nei+1):ne] <- FALSE #last fragments only
    write(dah[,zsel],Fh,ncolumns=ifelse(nm==12,12,10))
    write.table(est.c[zsel,],Fe,row.names=FALSE,col.names=FALSE)
    zsel <- sel; zsel[1:nei] <- FALSE #former fragments only
    write(dah[,zsel],Ff,ncolumns=ifelse(nm==12,12,10))
    write.table(est.c[zsel,],Fa,row.names=FALSE,col.names=FALSE)
    tnei <- tnei+sum(sel[1:nei]); tne <- tne+sum(sel)
    cat('\n')
  }
  close(Fo); close(Fh); close(Ff); close(Fa)
  #read accummulated files and save in the unique *.rda file:
  nei <- tnei; ne <- tne
  dat <- scan(sprintf('%s.auxo',fbas),quiet=TRUE)
  if(nm==12) dim(dat) <- c(12,na,nei) else dim(dat) <- c(nd,nei)
  dah <- scan(sprintf('%s.auxh',fbas),quiet=TRUE)
  dah <- c(dah,scan(sprintf('%s.auxf',fbas),quiet=TRUE))
  if(nm==12) dim(dah) <- c(12,na,ne) else dim(dah) <- c(nd,ne)
  est.c <- read.table(sprintf('%s.auxa',fbas),as.is=TRUE) #read former fragm.
  write.table(est.c,Fe,row.names=FALSE,col.names=FALSE) #append to last fragm.
  close(Fe)
  est.c <- read.table(sprintf('%s.auxe',fbas),as.is=TRUE) #read whole st. list
  names(est.c) <- c('X','Y','Z','Code','Name','pod','ope','snht','rmse')
  save(dat,dah,nd,ndec,uni,est.c,nei,ne,nm,std,x,ini,nsp, file=sprintf('%s.rda',fbas),version=2)
  cat(sprintf('Above results have been merged into %s.rda\n',fbas))
}

#- unsufix.- Remove numeric sufixes from the station codes.
unsufix <- function(str) sapply(strsplit(str,'-'), function(x) x[1])

#- snht.- Maximum Standard Normal Homogeneity Test (allowing missing data).
snht <- function(y,mints=3,allT=FALSE) {
  #mints: minimum tail size (no. mÃ­nimo de tÃ©rminos en los extremos de la serie)
  #allT: return all T values? (By default, only the maximum and its position)
  yav <- which(!is.na(y)) #available data
  x <- y[yav] #series without missing data
  n <- length(x)
  if(n<mints*2) return(c(0,0)) #insuficientes datos
  if(sd(x)==0) return(c(0,0)) #serie constante
  T <- rep(NA,n)
  # z <- scale(x) #algo mÃ¡s lento que:
  z <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
  for(i in mints:(n-mints)+1) { #(obviar las colas de menos de mints tÃ©rminos)
    if(is.na(x[i])) next
    n1 <- sum(!is.na(x[1:(i-1)])) #no. de tÃ©rminos de la muestra 1
    n2 <- sum(!is.na(x[i:n])) #no. de tÃ©rminos de la muestra 2
    if(n1<mints | n2<mints) next #al menos una muestra es demasiado pequeÃ±a
    z1 <- mean(z[1:(i-1)],na.rm=TRUE)
    z2 <- mean(z[i:n],na.rm=TRUE)
    T[i] <- n1*z1*z1 + n2*z2*z2
  }
  if(allT) return(T) else return(c(max(T,na.rm=TRUE),yav[which.max(T)]))
}

#- wtest.- Test para ventanas solapadas de 2*nt tÃ©rminos vÃ¡lidos.
wtest <- function(x,nt=48,sts=3,test) {
  ntt <- length(x) #no. total de tÃ©rminos de la serie
  ntv <- sum(!is.na(x)) #no. de tÃ©rminos vÃ¡lidos de la serie
  if(2*nt>ntv) return(c(0,0)) #no hay suficientes datos vÃ¡lidos para la prueba
  tV <- 0 #inicializaciÃ³n del tV mÃ¡ximo a devolver
  pk <- 0 #inicializaciÃ³n de la posiciÃ³n a devolver
  #inicializaciÃ³n de los lÃ­mites muestrales (a1-b1, a2-b2):
  k <- 1; while(k<ntt & is.na(x[k])) k <- k+1; a1 <- k
  n<-1; while(n<nt & k<ntt) { k <- k+1; if(!is.na(x[k])) n <- n+1; }
  b1 <- k
  k <- k+1; while(k<ntt & is.na(x[k])) k <- k+1; a2 <- k
  n<-1; while(n<nt & k<ntt) { k <- k+1; if(!is.na(x[k])) n <- n+1; }
  b2 <- k
  #aplicaciÃ³n de test a las ventanas solapadas:
  repeat {
    st <- eval(call(test,x[a1:b2],sts))
    stx <- st[1]
    if(!is.na(tV) & stx>tV) { tV <- stx; pk <- round(st[2])+a1-1 }
    if(b2==ntt) return(c(tV,pk))
    #desfasar las ventanas hacia adelante:
    a1 <- a2; b1 <- b2
    k <- b2+1; while(k<ntt & is.na(x[k])) k <- k+1
    if(is.na(x[k])) return(c(tV,pk)) else a2 <- k
    n<-1; while(n<nt & k<ntt) { k <- k+1; if(!is.na(x[k])) n <- n+1; }
    b2 <- k
  }
}

#- cuct.- Maximum Cucconi test in a series (allowing missing data and zeros).
cuct <- function(y,mints=3) {
  #mints: minimum tail size (no. mÃ­nimo de tÃ©rminos en los extremos de la serie)
  yav <- which(!is.na(y)) #available data
  x <- y[yav] #series without missing data
  n <- length(x) #longitud de la serie
  if(n<mints*2) return(c(NA,NA)) #insuficientes datos
  Tx <- 0; kx <- 0 #inicializar valor mÃ¡ximo del test y su posiciÃ³n
  rkx <- rank(x,ties.method="first") #rangos de la serie
  rkz <- n+1-rkx #contra-rangos de la serie
  #(usamos as.numeric() para evitar "enteros excedidos" en series largas:)
  rkx <- as.numeric(rkx)*rkx; rkz <- as.numeric(rkz)*rkz #cuadrados de ambos
  nz1 <- n+1; nz2 <- 2*n+1; nz8 <- 8*n+11 #variables auxiliares
  r <- 2*(as.numeric(n)*n-4) / (nz2*nz8) - 1
  #calcular el test a lo largo de la serie:
  for(i in mints:(n-mints)+1) { #(obviar las colas de menos de mints tÃ©rminos)
    n1 <- i-1; n2 <- n-n1 #no. de tÃ©rminos de las muestras
    S1 <- sum(rkx[i:n]); S2 <- sum(rkz[i:n]) #sumatorios
    den <- sqrt(n1*n2*nz1*nz2*nz8/5) #denominador
    U <- (6*S1-n2*nz1*nz2)/den
    V <- (6*S2-n2*nz1*nz2)/den
    T <- (U*U+V*V-2*r*U*V) / (2*(1-r*r)) #valor del test
    if(T>Tx) { Tx <- T; kx <- i }
  }
  return(c(Tx,yav[kx]))
}