library(XML)
library(stringr)

getBio <- function(url)
{
  # This function takes the URL for an official's page and returns the appropriate table.
  #
  # Args:
  #   url: The URL for an official. Example: "http://www.chinavitae.com/biography/Shen_Weichen/career"
  #
  # Returns:
  #   A dataframe of the official's professional history.
  
  page <- htmlParse(url)
  
  name <- xpathSApply(page, "//*/div[@class='bioName']", xmlValue)
  
  # Get official's name
  chinese.name <- str_extract(name, "\\s+[^ ]+$")
  chinese.name <- gsub("^ ", "", chinese.name)
  english.name <- gsub("\\s+[^ ]+$", "", name)
  english.name <- gsub("\\s+$", "", english.name)
  
  # Get official's biography
  bio <- xpathSApply(page, "//*/div[@class='bioDetails']", xmlValue)
  birth.date <- gsub("^[^ ]+\\s", "", bio[1])
  birth.place <- gsub("^[^ ]+\\s", "", bio[2])
  
  # Get history
  tabs <- readHTMLTable(page, header=F)
  history <- tabs[[1]]
  history <- cleanHistory(history)
  
  return.df <- data.frame(chinese.name, english.name, birth.date, birth.place, history)
  return(return.df)
}

cleanHistory <- function(history.df)
{
  # Cleans an official's history data frame.
  #
  # Args:
  #   history.df: A dataframe of official's history.
  # Returns:
  #   A cleaned dataframe of official's history.
  
  start.date <- str_extract(history.df[,1], "^[[:digit:]]+")
  end.date <- str_extract(history.df[,1], "[[:digit:]]+$")
  history.df[,2] <- gsub("\\(|\\)", "", history.df[,2])
  position <- str_extract(history.df[,2], "^[^,]+")
  location <- str_extract(history.df[,2], "\\s{3}.+$")
  temp <- gsub("  ","~~",history.df[,2])
  institution <- str_extract(temp, ", [^[~~]]+")
  institution <- gsub("^, ", "", institution)
  
  return.df <- data.frame(start.date, end.date, position, institution, location)
  return(return.df)
}

getOfficialsList <- function(url)
{
  # Get's a list of officials' names (and links) from the library page.
  #
  # Args:
  #   url: The URL of a "Browse by Name" page from chinavitae.com.
  #
  # Returns:
  #   A vectory of career URL's to scrape for officials' bios.
  
  page <- htmlParse(url)
  links <- str_extract_all(toString.XMLNode(page), "biography/[^ ]+")[[1]]
  links <- gsub("[[:punct:]]*$","",links)
  links <- paste("http://www.chinavitae.com/",links,"/career",sep="")
  
  return(links)
}

# Create a base URL, then all 26 letters, then paste them together to get all 26 library pages.
base.url <- "http://www.chinavitae.com/biography_browse.php?l="
page.letters <- letters[1:26]
library.urls <- paste(base.url, page.letters, sep="")

# This will be the final data frame we produce.
official.df <- list()
failure.list <- NULL

# Loop through all URLs and get officials' information.
for(uu in library.urls)
{
  official.list <- getOfficialsList(uu)
  for(oo in official.list)
  {
    cat("\r",oo,"                                     ")
    flush.console()
    
    official.bio <- NULL
    try(official.bio <- getBio(oo))
    if(is.null(official.bio))
      failure.list <- c(failure.list, oo)
    
    official.df <- c(official.df, list(official.bio))
    Sys.sleep(runif(1,0.5,2))
  }
}
official.df <- do.call(rbind,official.df)

write.csv(official.df,"chinese_officials.csv",row.names=F)
write.csv(failure.list,"failures.csv",row.names=F)