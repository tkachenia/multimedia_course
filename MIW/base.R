eng <- "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
rus <- "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

hexAbc <- function(str_abc, prefix = "0x") {
     
  abc <- strsplit(str_abc, "")[[1]]
  abc_utf8 <- enc2utf8(abc)
  
  hex <- sapply(abc_utf8, charToRaw, simplify = TRUE)
  hex_str <- paste0(prefix, toupper(hex))
  
  utf8 <- sapply(hex_str, as.character)
  dim(utf8) <- dim(hex)
  
  if (!is.null(dim(utf8)))
    utf8 <- apply(utf8, 2, paste, collapse = "\t")
  names(utf8) <- abc
  
  utf8
}

hexStr <- function(str, prefix = "0x") {
     str_utf8 <- enc2utf8(str)
     hex <- charToRaw(str_utf8)
     utf8 <- paste0(prefix, toupper(hex))
     paste(utf8, collapse = "\t")
}