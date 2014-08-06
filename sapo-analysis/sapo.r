dados <- read.delim("C:/arduino/result.txt")
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
dados$date <- as.Date(dados$date, "%d/%b/%Y")
dados$week_day <- format(dados$date,'%A')