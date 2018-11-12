setwd("D:/MyDisk/Git/multimedia_course/MIW/miw01")
Sys.setlocale("LC_CTYPE", "russian")
source("../base.R")

getLetter <- function(hex_eng, eng_idx, hex_rus, rus_idx, i) {
     letter <- ""
     if (i == 1) { # english
          letter <- names(hex_eng[eng_idx])
     } else {# russion
          letter <- names(hex_rus[rus_idx[i - 1]])
     }
     letter
}

getHexString <- function(hex_eng, eng_idx, hex_rus, rus_idx, idx) {
     hexString <- ""
     for (i in 1:length(idx)) {
          if (idx[i] == 1) { # english
               hexString <- paste(hexString, hex_eng[eng_idx], sep = "\t")
          } else { # russion
               hexString <- paste(hexString, hex_rus[rus_idx[idx[i] - 1]], sep = "\t")
          }
     }
     substring(hexString, 2)
}

ksr1.make <- function(count) {
     eng <- "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
     rus <- "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
     
     hex_eng <- hexAbc(eng, prefix = "")
     hex_rus <- hexAbc(rus, prefix = "")
     
     text <- ""
     year <- format(Sys.Date(), "%y")
     for ( i in 1:( (count%/%2) + count %% 2) ) {
          eng_idx_1 <- sample(1:length(hex_eng), 1, replace=FALSE)
          rus_idx_1 <- sample(1:length(hex_rus), 2, replace=FALSE)
          idx_1 <- sample(1:3, 3, replace = FALSE) # 1 - english, 2 and 3 - russian

          eng_idx_2 <- sample(1:length(hex_eng), 1, replace=FALSE)
          rus_idx_2 <- sample(1:length(hex_rus), 2, replace=FALSE)
          idx_2 <- sample(1:3, 3, replace = FALSE) # 1 - english, 2 and 3 - russian

          text <- paste0(text,
                         "ФИО\t", paste0(rep("\t", 5), collapse = ""), paste(year, 2*i - 1, sep="_"), "\t\t",
                         "ФИО\t", paste0(rep("\t", 5), collapse = ""), paste(year, 2*i, sep="_"), "\n")
          text <- paste0(text,
                         "Группа\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[1]), "\t\t",
                         "Группа\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[1]), "\n")
          text <- paste0(text,
                         paste(year, 2*i - 1, sep="_"), "\t", getHexString(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1), "\t", getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[2]), "\t\t",
                         paste(year, 2*i, sep="_"), "\t", getHexString(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2), "\t", getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[2]), "\n")
          text <- paste0(text,
                         "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[3]), "\t\t",
                         "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[3]), "\n")
     }
     
     file <- file("miw1.txt", "w", encoding = "UTF-8")
     write(text, file)
     close(file)
     
     NULL
}

ksr1.make(36)