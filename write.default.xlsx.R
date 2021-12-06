# Written by Mark Lammers; Institute for Evolution and Biodiversity, University of Muenster; marklammers6@gmail.com
# (c) 2020. Released under the terms of the GNU General Public License v3.

# Load packages
# if(!require('')){install.packages('')}
# library('')

# Load data

# Reformat data

# Define function
checksumfolder<-paste(wd.base,'Common_scripts/checksums/',sep='')
write.default.xlsx<-function(dataframe,
                             filepath=NA,
                             filename=paste(today,'debugging','xlsx',sep='.'),
                             colwidths=c(),
                             extra.header.info=NA,
                             checksum.folder=checksumfolder, #can be NA to not write checksums
                             quiet=F
){
  if(!quiet){
    print('This function expects any dataframe with a limited number of columns')
    print('Header is compulsory')
  }
  
  # Check input
  #file name
  if(is.na(filepath)){
    filepath<-filename
    file.name<-rev(unlist(strsplit(filename, split = '/', fixed = T)))[1]
  } else {
    file.name<-filename
  }
  
  #column widths
  #TODO #test what happens if no input is given presently
  #from experience I'd say that if not enough column widths are given, the extra columns are dropped
  
  #extra header info for the subtitle
  if(!is.na(extra.header.info[1])){
    if(length(extra.header.info)>1){
      stop('ERROR: extra.header.info can only be a single string')
    } else {
      subtitle<-paste(extra.header.info, '  |   Last processed:',Sys.time())
    }
  } else {
    subtitle<-paste('Last processed:',Sys.time())
  }
  
  # A full script to write a formatted xlsx file
  # See http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r
  # and http://www.sthda.com/english/wiki/r2excel-read-write-and-format-easily-excel-files-using-r-software
  # Create a new workbook for outputs. Possible values for type are : "xls" and "xlsx"
  wb<-createWorkbook(type="xlsx")
  
  # Define some cell styles
  #++++++++++++++++++++
  
  # Title and sub title styles
  TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16,
                                     #color="blue",
                                     isBold=TRUE, underline=1)
  SUB_TITLE_STYLE <- CellStyle(wb) +
    Font(wb,  heightInPoints=11,
         isItalic=TRUE, isBold=FALSE)
  
  # Styles for the data table row/column names
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK"))
  
  # Create a new sheet in the workbook
  sheet <- createSheet(wb, sheetName = file.name)
  
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to
  #contain the title
  # - title : the text to use as title
  # - titleStyle : style object to use for title
  xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
    rows <-createRow(sheet,rowIndex=rowIndex)
    sheetTitle <-createCell(rows, colIndex=1)
    setCellValue(sheetTitle[[1,1]], title)
    setCellStyle(sheetTitle[[1,1]], titleStyle)
  }
  # Add title
  xlsx.addTitle(sheet, rowIndex=1,
                title=file.name,
                titleStyle = TITLE_STYLE)
  # Add sub title
  xlsx.addTitle(sheet, rowIndex=2,
                title=subtitle,
                titleStyle = SUB_TITLE_STYLE)
  # Add a table
  addDataFrame(dataframe, sheet, startRow=3, startColumn=1, row.names = FALSE,
               colnamesStyle = TABLE_COLNAMES_STYLE,showNA = F,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  
  # Change column width
  widths<-colwidths
  if(length(widths) < ncol(dataframe)){ #if not enough column widths are given
    widths<-c(widths,rep(4,times = ncol(dataframe) - length(widths))) #add as many small columns as there are missing
  }
  for (i in (c(1:ncol(dataframe)))){
    setColumnWidth(sheet, colIndex=i, colWidth=widths[i])
  }
  
  # Save the workbook to a file
  saveWorkbook(wb = wb, file = filepath)
  
  if(!quiet){
    print(paste('File is saved as', filepath))
  }
  
  if(!is.na(checksum.folder)){
    if(!quiet){
      print(paste("And the file's md5sum is saved in",checksum.folder))
    }
    #Store an md5 checksum of the just-written file
    checksum<-tools::md5sum(filepath)
    writeLines(text = checksum, con = paste(checksum.folder,file.name,'.md5sum',sep=''))
  }
}
