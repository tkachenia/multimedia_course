setwd("c:/TkacheniaAV/MyDisc/Git/multimedia_course/MIW/miw03")
setwd("D:/MyDisk/Git/multimedia_course/MIW/miw03")
Sys.setlocale("LC_CTYPE", "russian")
source("../base.R")

getFields <- function() {
   fields <- list()  
   fields["bfType"]          <- makeField(c("BM", "BA", "CI", "CP", "IC", "PT"))
   fields["bfSize"]          <- NA
   fields["bfReserved1"]     <- makeField(0, bytesCount = 2)
   fields["bfReserved2"]     <- makeField(0, bytesCount = 2)
   fields["bfOffBits"]       <- makeField(54)
   fields["biSize"]          <- makeField(40)
   fields["biWidth"]         <- NA
   fields["biHeight"]        <- NA
   fields["biPlanes"]        <- makeField(1, bytesCount = 2)
   fields["biBitCount"]      <- makeField(3*c(8, 16, 24, 32), bytesCount = 2)
   fields["biCompression"]   <- makeField(0)
   fields["biSizeImage"]     <- NA
   fields["biXPelsPerMeter"] <- makeField(0)
   fields["biYPelsPerMeter"] <- makeField(0)
   fields["biClrUsed"]       <- makeField(0)
   fields["biClrImportant"]  <- makeField(0)
   
   tags <- as.vector(c("bfType", "bfSize", "bfReserved1", "bfReserved2", "bfOffBits",
                       "biSize", "biWidth", "biHeight", "biPlanes", "biBitCount", "biCompression", "biSizeImage",
                          "biXPelsPerMeter", "biYPelsPerMeter", "biClrUsed", "biClrImportant"))
   
   list(field = fields, tag = tags)
}

getErrorIdx <- function(fields, train = F) {
   idxError <- sample(1:(length(fields$tag) - 4), 1)
   if (!train) {
      # 0  - No error
      idxError <- sample(c(0, which(fields$tag %in% c("bfSize", "biSizeImage"))), 1)
   }
   
   idxError
}

getValue <- function(field, idx = 1) {
   item <- field[idx]
   name <- names(item)
   
   as.integer(name)
}

getSampleIdx <- function(fields, idxError) {
   idxbfType     <- sample(1:length(fields$field["bfType"][[1]]), 1)
   idxbiBitCount <- sample(1:length(fields$field["biBitCount"][[1]]), 1)

   if (getValue(fields$field["biBitCount"][[1]], idxbiBitCount) == 3*32 && any(idxError == which(fields$tag %in% c("bfSize", "biSizeImage")))) {
      idxbiBitCount <- 3
   }
   
   list(bfType = idxbfType, biBitCount = idxbiBitCount)
}

getFieldsUpdate <- function(fields, idx, idxError) {
   fields$field["bfType"] <- list(fields$field["bfType"][[1]][idx$bfType])
   fields$field["biBitCount"]  <- list(fields$field["biBitCount"][[1]][idx$biBitCount])
   
   while(T) {
      biWidth <- sample(1:1024, 1)
      rowSize <- ((getValue(fields$field["biBitCount"][[1]])/8)*biWidth)/4
      if (!any(idxError == which(fields$tag %in% c("bfSize", "biSizeImage")))) break
      if (floor(rowSize) != ceiling(rowSize)) break
   }
   biHeight    <- sample(-768:768, 1)
   if (biHeight == 0) biHeight <- 1
   biSizeImage <- 4*ceiling(rowSize)*abs(biHeight)
   bfSize      <- biSizeImage + 14 + getValue(fields$field["biSize"][[1]])

   fields$field["bfSize"]      <- makeField(bfSize)
   fields$field["biWidth"]     <- makeField(biWidth)
   fields$field["biHeight"]    <- makeField(biHeight)
   fields$field["biSizeImage"] <- makeField(biSizeImage)

   fields
}

getFieldsError <- function (fields, idxError, prefix = "", train = F) {
   if (idxError == 0) return(fields)
   
   if (!train) {
      biSizeImage <- (getValue(fields$field["biBitCount"][[1]])/8)*biWidth*biHeight
      bfSize      <- biSizeImage + 14 + getValue(fields$field["biSize"][[1]])
      fields$field[fields$tag[idxError]][[1]][[1]] <- (if (fields$tag[idxError] == "biSizeImage") biSizeImage else bfSize)
   } else {
      error <- sample(1:254, 1)
      strHex <- fields$field[fields$tag[idxError]][[1]][[1]]
      firstHexValue <- substring(strHex, nchar(prefix) + 1, nchar(prefix) + 2)
      firstValue <- (strtoi(firstHexValue, base = 16) + error) %% 255
      newFirstHexValue <- hexInt(firstValue, bytesCount = 1, prefix = prefix)
      substring(fields$field[fields$tag[idxError]][[1]][[1]], nchar(prefix) + 1, nchar(prefix) + 2) <- newFirstHexValue
   }

   fields
}

getData <- function(fields, value_count_in_line = 16, prefix = "") {
   data <- ""
   for (i in 1:length(fields$tag)) {
      data <- paste(data, fields$field[fields$tag[i]])
   }
   data <- substring(data, 2)
   data <- substring(data, seq(1, nchar(data), value_count_in_line*(nchar(prefix) + 2 + 1)),
                           c(seq(value_count_in_line*(nchar(prefix) + 2 + 1) - 1, nchar(data), value_count_in_line*(nchar(prefix) + 2 + 1)), nchar(data)))
   data
}

miw3.make <- function(count, train = F) {
   fields <- getFields()
   text <- ""
   year <- format(Sys.Date(), "%y")
   if (train) year <- "trn"
   for ( i in 1:( (count%/%2) + count %% 2) ) {
      error_1  <- getErrorIdx(fields, train)
      idx_1    <- getSampleIdx(fields, error_1)
      fields_1 <- getFieldsUpdate(fields, idx_1, error_1)
      fields_1 <- getFieldsError(fields_1, error_1, train = train)
      data_1   <- getData(fields_1)
      
      error_2  <- getErrorIdx(fields, train)
      idx_2    <- getSampleIdx(fields, error_2)
      fields_2 <- getFieldsUpdate(fields, idx_2, error_2)
      fields_2 <- getFieldsError(fields_2, error_2, train = train)
      data_2   <- getData(fields_2)

      text <- paste0(text,
                     "ФИО\t", "\t", "\t",
                     "ФИО\t", "\t", "\t", "\n")
      text <- paste0(text,
                     paste(year, 2*i - 1, sep="_"), "\t", "Группа\t", paste(year, 2*i - 1, sep="_"), "\t",
                     paste(year, 2*i, sep="_"), "\t", "Группа\t", paste(year, 2*i, sep="_"), "\t", "\n")
      text <- paste0(text,
                     "Данные\t", data_1[1], "\t", "\t",
                     "Данные\t", data_2[1], "\t", "\t", "\n")
      for (i in (2:length(data_1))) {
         text <- paste0(text,
                        "\t", data_1[i], "\t", "\t",
                        "\t", data_2[i], "\t", "\t", "\n")
      }
      text <- paste0(text,
                     "bfSize:\t", "\t", getValue(fields_1$field["bfSize"][[1]]), "\t",
                     "bfSize:\t", "\t", getValue(fields_2$field["bfSize"][[1]]), "\t", "\n")
      text <- paste0(text,
                     "biBitCount:\t", "\t", getValue(fields_1$field["biBitCount"][[1]]), "\t",
                     "biBitCount:\t", "\t", getValue(fields_2$field["biBitCount"][[1]]), "\n")
      text <- paste0(text,
                     "Методан. верны\t", "\t", (if (error_1 != 0) "Нет," else "Да"), "\t",
                     "Методан. верны\t", "\t", (if (error_2 != 0) "Нет," else "Да"), "\t", "\n")
      text <- paste0(text,
                     "да/нет (почему)?\t", "\t", fields_1$tag[error_1], "\t",
                     "да/нет (почему)?\t", "\t", fields_2$tag[error_2], "\t", "\n")

   }
   
   file <- file("miw3.txt", "w", encoding = "UTF-8")
   write(text, file)
   close(file)
   
   NULL
}

miw3.make(40, T)
