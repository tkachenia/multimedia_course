eng <- "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
rus <- "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

hexStr <- function(str) {
  abc <- strsplit(str, "")[[1]]
  abc_utf8 <- enc2utf8(abc)
  
  hex <- sapply(abc_utf8, charToRaw, simplify = TRUE)
  hex_str <- paste0("0x", toupper(hex))
  
  utf8 <- sapply(hex_str, as.character)
  dim(utf8) <- dim(hex)
  
  if (!is.null(dim(utf8)))
    apply(utf8, 2, paste, collapse = "\t")
  else
    utf8
}

hex_eng <- hexStr(eng)
hex_rus <- hexStr(rus)
  
eng_idx <- sample(1:length(hex_eng), 1, replace=FALSE)
rus_idx <- sample(1:length(hex_rus), 2, replace=FALSE)

# 1 - english
# 2 and 3 - russian
idx <- sample(1:3, 3, replace = FALSE)



  hex <- charToRaw(abc)
  hex <- charToRaw(abc)
  hex <- 
  Encoding(hex) <- "bytes"
  bytes
  
  z <- strsplit(hex, "")
  z[[1]][2]
  utf8ToInt(z[[1]][1])
  as.hexmode(z[[1]][2])
  q <- strsplit(cat(hex), "\\x")
}

x <- y <- z <- "\u9b3c"
Encoding(y) <- "bytes"
Encoding(z) <- "latin1"
print(x); print(y); print(z)
bytes(x); bytes(y); bytes(z)
bits(x); bits(y); bits(z)
bytes(x)

a <- engHexStr(eng)

eng_list <- as.vector(strsplit(eng, "")[[1]], mode = "character")
rus_list <- as.vector(strsplit(rus, "")[[1]], mode = "character")

eng_list_utf8 <- enc2utf8(eng_list)
rus_list_utf8 <- enc2utf8(rus_list)

Encoding(eng_list_utf8) <- "bytes"
Encoding(rus_list_utf8) <- "bytes"

hex_eng <- sapply(eng_list_utf8 , utf8ToInt)
hex_rus <- sapply(rus_list_utf8 , utf8ToInt)



getHexStr <- function(num) {
  hex <- as.hexmode(num)
  len <- nchar(hex)
  print(len)
  print(hex)
  print(len %/% 2)
  print(len %% 2)
  len <- 2*(len %/% 2 + len %% 2)
  hex <- format(hex, width = len, upper.case = TRUE)
  print(len)
  print(hex)
  part <- substring(hex, seq(1, max(1, len - 1), 2), seq(2, len, 2))
  
  paste(sprintf("0x%s",part), collapse = "\t")
}

hex <- paste(c(getHexStr(hex_rus[[rus_idx[1]]]), getHexStr(hex_eng[[eng_idx[1]]]), getHexStr(hex_rus[[rus_idx[2]]])), collapse = "\t")

ch <- as.vector(c("я", "а"), mode = "character")
utf8 <- enc2utf8(ch)
q <- sapply(utf8, utf8ToInt)
i <- 2
names(q)[i]
q[[i]]
hex <- format(as.hexmode(q), width = num %/% 2 + num %% 2, upper.case = TRUE)
sub <- substring(hex[[1]], c(1,3), c(2,4))
paste(sprintf("0x%s",sub), collapse = "\t")

sample(1:46, 2, replace=FALSE)

ch <- "7"
ch_ <- enc2utf8(ch)
ch__ <- utf8ToInt(ch)
Encoding(ch) <- "bytes"
as.hexmode(ch__)
Encoding(ch)
Encoding(ch_)

