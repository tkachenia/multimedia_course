# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# local_path <- "D:/Ubuntu/Share/Курс_Мультимедийные системы и среды"
# setwd(paste0(local_path, "/multimedia_course/MIW/miw03"))

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
   fields["biBitCount"]      <- makeField(24, bytesCount = 2)
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
   tagError <- c("bfType", "bfSize", "bfReserved1", "bfReserved2", "bfOffBits",
                 "biSize", "biPlanes", "biBitCount", "biSizeImage")
   idxError <- sample(which(fields$tag %in% tagError), 1)
   if (!train) {
      # 0  - No error
      idxError <- sample(c(0, which(fields$tag %in% c("biSizeImage"))), 1)
   }
   
   idxError
}

getValue <- function(field, idx = 1) {
   item <- field[[1]][idx]
   name <- names(item)
   
   as.integer(name)
}

# If field has set of possible values, sample single one from the set
getSampleIdx <- function(fields, idxError) {
   # bfType field only
   idxbfType     <- sample(1:length(fields$field["bfType"][[1]]), 1)
   
   list(bfType = idxbfType)
}

getFieldsUpdate <- function(fields, idx, idxError) {
   # get single value from a set (bfType field only)
   fields$field["bfType"] <- list(fields$field["bfType"][[1]][idx$bfType])
   
   rowSize <- NA
   padding <- NA
   
   biWidth <- NA
   while(T) {
      biWidth <- sample(1:1024, 1)
      rowSize <- (getValue(fields$field["biBitCount"]) %/% 8) * biWidth
      padding = 4 - (rowSize %% 4)
      if (padding %in% c(1, 2, 3)) break
   }
   biHeight    <- sample(-1024:-1, 1)

   biSizeImage <- (rowSize + padding)*abs(biHeight)
   bfSize      <- getValue(fields$field["bfOffBits"]) + biSizeImage

   fields$field["bfSize"]      <- makeField(bfSize)
   fields$field["biWidth"]     <- makeField(biWidth)
   fields$field["biHeight"]    <- makeField(biHeight)
   fields$field["biSizeImage"] <- makeField(biSizeImage)

   fields
}

getFieldsError <- function (fields, idxError, prefix = "", train = F) {
   if (idxError == 0) return(fields)
   
   fieldName  <- fields$tag[idxError]
   if (fields$tag[idxError] %in% c("bfType")) {
      fieldErrorValue <- sample(c("MB", "AB", "II", "PC", "CC", "TP"), 1)
      fields$field[fieldName] <- makeField(fieldErrorValue)
   } else {
      error <- sample(1:254, 1)
      fieldValue <- getValue(fields$field[fieldName])
      fieldErrorValue <- fieldValue + error
      
      if (fields$tag[idxError] %in% c("bfReserved1", "bfReserved2", "biPlanes", "biBitCount")) {
         fields$field[fieldName] <- makeField(fieldErrorValue, bytesCount = 2)
      } else {
         fields$field[fieldName] <- makeField(fieldErrorValue, bytesCount = 4)
      }
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

miw3.make <- function(count, train = F, exam = F) {
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

      if (exam) {
         text <- paste0(text, "Вариант:\t", paste(year, 2*i - 1, sep="_"), "\n")
         text <- paste0(text, "Данные:\n")
         for (j in (1:length(data_1))) {
            text <- paste0(text, data_1[j], "\n")
         }
         text <- paste0(text, "bfSize:\nabs(biHeight):\nМетодан. верны: да/нет (почему)?\n")
         text <- paste0(text, "Ответ:\t", getValue(fields_1$field["bfSize"]), ", ",
                                          abs(getValue(fields_1$field["biHeight"])), ", ",
                                          (if (error_1 != 0) "Нет" else "Да"), " (", fields_1$tag[error_1], ")\n")
         
         text <- paste0(text, "Вариант:\t", paste(year, 2*i, sep="_"), "\n")
         text <- paste0(text, "Данные:\n")
         for (j in (1:length(data_2))) {
            text <- paste0(text, data_2[j], "\n")
         }
         text <- paste0(text, "bfSize:\nabs(biHeight):\nМетодан. верны: да/нет (почему)?\n")
         text <- paste0(text, "Ответ:\t", getValue(fields_2$field["bfSize"]), ", ",
                                          abs(getValue(fields_2$field["biHeight"])), ", ",
                                          (if (error_2 != 0) "Нет" else "Да"), " (", fields_2$tag[error_2], ")\n")
      } else {
         text <- paste0(text,
                        "ФИО\t", "\t", "\t",
                        "ФИО\t", "\t", "\t", "\n")
         text <- paste0(text,
                        paste(year, 2*i - 1, sep="_"), "\t", "Группа\t", paste(year, 2*i - 1, sep="_"), "\t",
                        paste(year, 2*i, sep="_"), "\t", "Группа\t", paste(year, 2*i, sep="_"), "\t", "\n")
         text <- paste0(text,
                        "Данные\t", data_1[1], "\t", "\t",
                        "Данные\t", data_2[1], "\t", "\t", "\n")
         for (j in (2:length(data_1))) {
            text <- paste0(text,
                           "\t", data_1[j], "\t", "\t",
                           "\t", data_2[j], "\t", "\t", "\n")
         }
         text <- paste0(text,
                        "bfSize:\t", "\t", getValue(fields_1$field["bfSize"]), "\t",
                        "bfSize:\t", "\t", getValue(fields_2$field["bfSize"]), "\t", "\n")
         text <- paste0(text,
                        "abs(biHeight):\t", "\t", abs(getValue(fields_1$field["biHeight"])), "\t",
                        "abs(biHeight):\t", "\t", abs(getValue(fields_2$field["biHeight"])), "\n")
         text <- paste0(text,
                        "Методан. верны\t", "\t", (if (error_1 != 0) "Нет," else "Да"), "\t",
                        "Методан. верны\t", "\t", (if (error_2 != 0) "Нет," else "Да"), "\t", "\n")
         text <- paste0(text,
                        "да/нет (почему)?\t", "\t", fields_1$tag[error_1], "\t",
                        "да/нет (почему)?\t", "\t", fields_2$tag[error_2], "\t", "\n")
      }

   }
   
   file <- file("miw3.txt", "w", encoding = "UTF-8")
   write(text, file)
   close(file)
   
   NULL
}

set.seed(20211117)
miw3.make(60, train = F, exam = F)
