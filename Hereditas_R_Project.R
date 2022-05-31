library(xlsx)
library(rvest)

main_function_hereditas <- function(year) {
  
  starting_URL <- ""
  
  if( year == "2015" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=152&searchType=&tab=keyword"
  }
  else if( year == "2016" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=153&searchType=&tab=keyword"
  }
  else if( year == "2017" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=154&searchType=&tab=keyword"
  }
  else if( year == "2018" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=155&searchType=&tab=keyword"
  }
  else if( year == "2019" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=156&searchType=&tab=keyword"
  }
  else if( year == "2020" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=157&searchType=&tab=keyword"
  }
  else if( year == "2021" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=158&searchType=&tab=keyword"
  }
  else if( year == "2022" ) {
    starting_URL <- "https://hereditasjournal.biomedcentral.com/articles?query=&volume=159&searchType=&tab=keyword"
  }
  
  starting_URL_read <- read_html(starting_URL)
  
  article_URL_list <- starting_URL_read %>% html_nodes(".c-listing__title a") %>% html_attr("href")
  
  concatenate01 <- function(issue) {
    paste( paste("https://hereditasjournal.biomedcentral.com",issue,sep="") )
  }
  
  new_article_URL <- sapply( article_URL_list, concatenate01)

  df <- data.frame("Title"=character(), "Published"=character(), "Authors"=character(), "Cor.Author"=character(), "Cor. Author email"=character(), "DOI"=character(),"Keywords"=character() ,"Abstract"=character())
  df_test <- data.frame("FullBody"=character())
  
  for( i in 1:length(new_article_URL)) 
  {
    article_URL_read <- read_html(new_article_URL[i])
    
    pullTitle <- article_URL_read %>% html_nodes(".c-article-title") %>% html_text()
    
    pullPublished <- article_URL_read %>% html_nodes(".c-article-identifiers__item time") %>% html_text()
    
    pullAuthor <- article_URL_read %>% html_nodes(".c-article-author-affiliation__authors-list") %>% html_text()
    df_pullAuthor <- paste(pullAuthor, collapse=", ")
    
    pullCorrespondingAuthor <- article_URL_read %>% html_nodes("#corresponding-author-list a") %>% html_text()
    df_pullCorrespondingAuthor <- paste(pullCorrespondingAuthor, collapse = ", ")
    
    pullCorrespondingAuthorEmail <- article_URL_read %>% html_nodes("#corresponding-author-list a") %>% html_attr("href")
    pullCorrespondingAuthorEmail_remove_mailto <- sapply(pullCorrespondingAuthorEmail, function(x) substring(x, first=8))
    df_pullCorrespondingAuthorEmail <- paste(pullCorrespondingAuthorEmail_remove_mailto, collapse = ", ")
    
    pullKeyWords <- article_URL_read %>% html_nodes(".c-article-subject-list__subject span") %>% html_text()
    pullKeyWords <- paste(pullKeyWords, collapse=", ")
    
    pullAbstract <- article_URL_read %>% html_nodes(".c-article-section__content") %>% html_text()
    
    pullDOI <- article_URL_read %>% html_nodes(".c-bibliographic-information__list-item--doi .c-bibliographic-information__value") %>% html_text()
    df_pullDOI <- paste(pullDOI, collapse=", ")
    
    pullFullBody <- article_URL_read %>% html_nodes("#Sec20-content p , #Sec20 , #Sec19-content p , #Sec19 , #Sec13-content p , #Sec13 , #Sec2 , #Sec1 , #Abs1-content .c-article__sub-heading , #Abs1 , #Sec2-content p , #Sec1-content p , #Abs1-content p, #Sec17 , #Sec17-content p , #Sec16 , #Sec16-content p , #Sec10 , #Sec10-content p, #Sec3 , #Sec3-content p , #Sec4 , #Sec4-content, #Sec5 , #Sec5-content") %>% html_text()
    pullFullBody <- paste(pullFullBody,collapse = " ")
     
    
    df[nrow(df)+1,]=c(pullTitle,pullPublished,df_pullAuthor,df_pullCorrespondingAuthor,df_pullCorrespondingAuthorEmail,df_pullDOI,pullKeyWords,pullAbstract)
    df_test[nrow(df_test)+1,]=c(pullFullBody)
    
    df_final <- data.frame(cbind(df,df_test))
  }
  
  write.csv(df_final,'final.csv')
  
  library(readr)
  final <-read_csv("final.csv")
  View(final)
}
