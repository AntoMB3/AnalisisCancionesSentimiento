### WebScraping

install.packages("tidyverse")
install.packages("RSelenium")
install.packages("netstat")

library(tidyverse)
library(RSelenium)
library(netstat)

rdriver <- rsDriver(browser = "chrome",
                    port = 2122L,
                    chromever  = "113.0.5672.63")

object <- rdriver$client

object$navigate("https://www.geeksforgeeks.org/")

# closing the browser
object$close()