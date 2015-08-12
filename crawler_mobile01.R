library(rvest)
library(magrittr)
library(stringr)
library(gdata)
library(RCurl)

####################
# variables
####################
# Mobile01 web address 
mobile01Addr <- "http://www.mobile01.com/"  
# Mobile01 web address of Android forum 
mobile01.android.SubAddr <- "forumtopic.php?c=16&s=20"
# article data matrix
mobile01.android.data <- ""

####################
# functions
####################
# Function GetHtml()
GetHtml <- function(webaddr){
  return
    webaddr %>% 
      getURLContent(useragent = "R") %>% 
        html()
}
# Function GetHtmlText()
GetHtmlText <- function(para){
  return
    para %>%
      html_text() %>%
        gsub('[\r\n\t]', '', .) %>%
          trim()
}
# Function GetHtmlAttr()
GetHtmlAttr <- function(para,attr){
  return
    para %>%
      html_attr(attr) %>%
        gsub('[\r\n\t]', '', .) %>%
          trim()
}

####################
# Procedures
####################
# Get android page html
mobile01.android.html <- paste0(mobile01Addr,mobile01.android.SubAddr) %>% 
  GetHtml()

# Get the articles links
mobile01.android.link <-
  mobile01.android.html %>% 
    html_nodes(".topic_gen") %>% 
      html_attr("href")

# Get more links of the articles
mobile01.android.link.more <-
  mobile01.android.html %>% 
    html_nodes(xpath = "//span[@class='otherpages']//a") %>% 
      html_attr("href")

# Combine links
mobile01.android.linklist <- 
  c(mobile01.android.link, mobile01.android.link.more)


####################
# Get articles data
####################
# Get articles
for (i in 1:length(mobile01.android.linklist))
{
  # Mobile01 server timeout 
  Sys.sleep(2)
 
  # Get articles html
  mobile01.android.article <-
    paste0(mobile01Addr,mobile01.android.linklist[i]) %>% 
    GetHtml() 

  user <-
    mobile01.android.article %>% 
    html_nodes(xpath = "//div[@class='fn']//a") 
    
  mobile01.android.user <-
    user %>% 
      GetHtmlText()
  
  mobile01.android.userProfile <-
    user %>%
      GetHtmlAttr("href")
  
  mobile01.android.time <-
    mobile01.android.article %>% 
      html_nodes(xpath = "//div[@class='date']") %>% 
        GetHtmlText()
  
  mobile01.android.content <-
   mobile01.android.article %>% 
    html_nodes(xpath = "//div[@class='single-post-content']//div[1]") %>% 
      GetHtmlText()
  
  mobile01.android.title <-
    mobile01.android.article %>% 
      html_nodes(".topic") %>% 
        GetHtmlText() %>%
          rep(length(mobile01.android.content)) 
  
  mobile01.android.url <-
    mobile01.android.linklist[i] %>% 
      rep(length(mobile01.android.content))

  mobile01.android.data <-
      cbind(mobile01.android.title)%>%
        cbind(mobile01.android.url)%>%
          cbind(mobile01.android.user) %>%
            cbind(mobile01.android.userProfile) %>%
              cbind(mobile01.android.time) %>%
                cbind(mobile01.android.content) %>%
                  rbind(mobile01.android.data)
 
  # ==> mobile01.android.data <- rbind(mobile01.android.data, rep(mobile01.android.title, length(mobile01.android.content)), rep(mobile01.android.linklist[i], length(mobile01.android.content)), mobile01.android.user, mobile01.android.time, mobile01.android.content))
  
}
