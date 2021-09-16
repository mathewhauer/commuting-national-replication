## This will unzip the levee files.

zipF <-list.files(path = paste0("./R/DATA-RAW/LEVEES"), recursive=TRUE,  full.names = TRUE)

for(this.file in zipF){
  files <-str_split(str_split(this.file[1], "/")[[1]][5], ".zip")[[1]][1]
  target <- paste0("./R/DATA-RAW/LEVEES/LEVEES/",files)
  unzip(this.file, exdir = target)
  target2 <- paste0(target,"/LeveedArea.zip" )
  unzip(target2, exdir = target)
}



