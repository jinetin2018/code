library(quantmod)
library(TTR)
library(PerformanceAnalytics)
library(ggplot2)
library(scales)
options(stringAsFactors = FALSE)

symbols <- c("GSPC","HSI","000001.SS","601318.SS","600519.SS","000651.SZ","000333.SZ",
             "000858.SZ","600887.SS","603993.SS","000002.SZ","600036.SS","600085.SS",
             "603288.SS","000423.SZ","000538.SZ","600436.SS")
suppressWarnings(getSymbols(symbols,src = "yahoo",from="2012-01-01"))

#getSymbols(symbols,src="yahoo",from="2012-01-01")
df <- merge(GSPC$GSPC.Adjusted,HSI$HSI.Adjusted,`000001.SS`$`000001.SS.Adjusted`,
            `601318.SS`$`601318.SS.Adjusted`,`600519.SS`$`600519.SS.Adjusted`,
            `000651.SZ`$`000651.SZ.Adjusted`,
            `000333.SZ`$`000333.SZ.Adjusted`,`000858.SZ`$`000858.SZ.Adjusted`,
            `600887.SS`$`600887.SS.Adjusted`,`603993.SS`$`603993.SS.Adjusted`,
            `000002.SZ`$`000002.SZ.Adjusted`,`600036.SS`$`600036.SS.Adjusted`,
            `600085.SS`$`600085.SS.Adjusted`,`603288.SS`$`603288.SS.Adjusted`,
            `000423.SZ`$`000423.SZ.Adjusted`,`000538.SZ`$`000538.SZ.Adjusted`,
            `600436.SS`$`600436.SS.Adjusted`)

names(df) <- c("GSPC","HSI","000001.SS","601318.SS","600519.SS","000651.SZ","000333.SZ",
               "000858.SZ","600887.SS","603993.SS","000002.SZ","600036.SS","600085.SS","603288.SS",
               "000423.SZ","000538.SZ","600436.SS")
head(df)
