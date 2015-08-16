getRPackages <- function(pageURL){
  require(RCurl)
  require(XML)
  webURL <- getURL(pageURL)
  webpage <- readLines(tc <- textConnection(webURL)); close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
  link<-xpathSApply(pagetree, "//*/table/tr/td/a", xmlGetAttr, "href")
  date<-xpathSApply(pagetree, "//*/table/tr/td[1]", xmlValue)
  name<-xpathSApply(pagetree, "//*/table/tr/td[2]", xmlValue)
  desc<-xpathSApply(pagetree, "//*/table/tr/td[3]", xmlValue)
  data.frame(name,desc,date,link)
}

#packages<- getRPackages("http://cran.r-project.org/web/packages/available_packages_by_date.html")

getGoogleData <- function(link){
        require(httr)
        search = substring(link,7,100)
#  print(search)
        url <- paste('http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=',search,sep="")
        print(url)
        d<-GET(url)
  #    print(d)
        content(d, type="application/json")
}


getGoogleData2 <- function(link){
  require(RCurl)
  require(XML)
  search = substring(link,7,100)
  url <- paste('http://www.google.co.uk/search?q=%22',search,'%22',sep="")
  print(url)
  webURL <- getURL(url)
  webpage <- readLines(tc <- textConnection(webURL)); close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
}

getGoogleDataCustom <- function(link){
  require(RCurl)
  require(XML)
  search = substring(link,7,100)
#  https://www.googleapis.com/customsearch/v1?googlehost=google.co.uk&safe=medium&searchType=image&key={apikey}&cx=005132080528465015721:new4cpqwe0c&q={search}
  url <- paste('http://www.google.co.uk/search?q=%22',search,'%22',sep="")
  print(url)
  webURL <- getURL(url)
  webpage <- readLines(tc <- textConnection(webURL)); close(tc)
  pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
}


getGoogleHitCount <- function(link){
  googleData <- getGoogleData(link)
  if (googleData$responseStatus!=200){
    print(paste("response is", googleData$responseStatus))
  }
  as.numeric(gsub(",","",googleData$responseData$cursor$resultCount))
}


#http://gigablast.com/search?fast=1&format=json&n=1&q=%22packages/rpart%22

getGigablastData <- function(link){
  require(httr)
  search = substring(link,11,100)
  url <- paste('http://gigablast.com/search?fast=1&format=json&n=1&q=%22',search,'%22',sep="")
  #    print(url)
  d<-GET(url)
  #    print(d)
  content(d, type="application/json")
}

getGigablastHitCount <- function(link){
  gigablastData <- getGigablastData(link)
  n<=as.numeric(gsub(",","",gigablastData$hits))
  print(link)
  print(n)
  n  
}

populateHits<-function(packages, i){
        print(packages[i,])
        hits<-getGoogleHitCount(packages[i,"link"])
        print(hits)
        if (length(hits)==0){
                print("done for now")
        }
        packages[i,"hits"]<-hits
        packages
}

nextNAHits<-function(packages){
        which(is.na(packages$hits))[1]
}

doSomeMore<-function(n, sleep){
        for (i in 1:n){
                packages<<-populateHits(packages, nextNAHits(packages))
                sl<-10+round(20*abs(rnorm(1)))
                print(paste("sleep: ",sl))
                Sys.sleep(sl)
        }
}

#ggplot(packages, aes(x = hits)) + geom_histogram(binwidth=0.1, color="darkgreen") + scale_x_log10()
#ggplot(packages, aes(x = hits)) + geom_histogram(binwidth=0.1, color="darkgreen") + scale_x_log10()+ggtitle("R Packages - Distribution by Google hits (log scale)")+theme(plot.title = element_text(color="darkgreen", size=14, face="italic"))
#save(packages, file="Rpackages2.Rda")
#doSomeMore(70,1)
