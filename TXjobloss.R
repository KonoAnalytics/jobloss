txjobloss <- function()
{
    library("KonostdlibR")
    dfjobloss <- gethistory()
    dftxcities <- gettxcitygps()
    dfjobloss <- merge(dfjobloss, dftxcities,by.x="CITY_NAME",by.y = "city", all.x = TRUE)
    dfjobloss$longjitter <- jitter(dfjobloss$long)
    dfjobloss$latjitter <- jitter(dfjobloss$lat)
    pushtodomo(df=dfjobloss,datasetname="TX WARN Notifications", api_key=KonostdlibR::getcredentials("Domo"))
}

pushtodomo <- function(df, datasetname, api_key, organization="KonoAnalytics", createdataset=FALSE)
{
    #library("devtools")
    #install_github('konoanalytics/KonostdlibR')
    library("KonostdlibR")
    library("DomoR")
    
    apikey <- as.character(KonostdlibR::getcredentials("Domo")$api_key)
    DomoR::init(organization,apikey)
    if(createdataset)
    {
        DomoR::create(df, name=datasetname)
    }else
    {
        DataSetInfo <- DomoR::list_ds(name=datasetname)
        replace <- DomoR::replace_ds(DataSetInfo$id, df)
    }
}

gethistory <- function()
{
    library("xlsx")
    library("plyr")

    #http://www.twc.state.tx.us/businesses/worker-adjustment-and-retraining-notification-warn-notices
    destpath <- "~/jobloss/"
    dfcum <- data.frame()
    for (i in 2004:2013)
    {
        url <- paste0("http://www.twc.state.tx.us/files/news/warn-act-listings-",i,".xls")
        destfile <- paste0(destpath,i,".xls")
        download.file(url,destfile = destfile, mode = "wb")
        if (i == 2010)
        {   
            #first row is not headers in 2010 alone
            df <- read.xlsx(paste0(destpath,i,".xls"),sheetIndex = 1,startRow = 2)
            df <- df[,1:8]
            names(df) <- c("NOTICE_DATE","JOB_SITE_NAME","COUNTY_NAME","WDA_NAME","TOTAL_LAYOFF_NUMBER",
                           "LayOff_Date","WFDD_RECEIVED_DATE","CITY_NAME")
            dfcum <- rbind(dfcum, df)
            
        }else
        {
            df <- read.xlsx(paste0(destpath,i,".xls"),sheetIndex = 1)
            df <- df[,1:8]
            names(df) <- c("NOTICE_DATE","JOB_SITE_NAME","COUNTY_NAME","WDA_NAME","TOTAL_LAYOFF_NUMBER",
                           "LayOff_Date","WFDD_RECEIVED_DATE","CITY_NAME")
            dfcum <- rbind(dfcum, df)
        }
    }
    
    #changed to xlsx format in 2014
    for (i in 2014:2015)
    {
        url <- paste0("http://www.twc.state.tx.us/files/news/warn-act-listings-",i,".xlsx")
        destfile <- paste0(destpath,i,".xlsx")
        download.file(url,destfile = destfile, mode = "wb")
        df <- read.xlsx(paste0(destpath,i,".xlsx"),sheetIndex = 1)
        df <- df[,1:8]
        names(df) <- c("NOTICE_DATE","JOB_SITE_NAME","COUNTY_NAME","WDA_NAME","TOTAL_LAYOFF_NUMBER",
                       "LayOff_Date","WFDD_RECEIVED_DATE","CITY_NAME")
        dfcum <- rbind(dfcum, df)
    }
    
    #introduced new fields in 2016
    i <- 2016
    url <- paste0("http://www.twc.state.tx.us/files/news/warn-act-listings-",i,".xlsx")
    destfile <- paste0(destpath,i,".xlsx")
    download.file(url,destfile = destfile, mode = "wb")
    df <- read.xlsx(paste0(destpath,i,".xlsx"),sheetIndex = 1)
    dfcum <- rbind.fill(dfcum, df)

    dfcum$layoffyear <- as.numeric(format(dfcum$LayOff_Date,"%Y"))
    dfcum$TOTAL_LAYOFF_NUMBER <- as.numeric(dfcum$TOTAL_LAYOFF_NUMBER)
    dfcum
}

gettxcitygps <- function(fileandpath="~/jobloss/txcities.xlsx",uploaddomo=TRUE)
{
    library("xlsx")
    
    df <- read.xlsx(fileandpath,sheetIndex = 1)
    df <- df[,c("city","lat","long")]
    df$city <- as.character(df$city)
    df$classification <- NA
    df$classification[grep(" city",df$city)] <- "city"
    df$classification[grep(" town",df$city)] <- "town"
    df$city[grep(" city",df$city)] <- gsub(" city","",df$city[grep(" city",df$city)])
    df$city[grep(" town",df$city)] <- gsub(" town","",df$city[grep(" town",df$city)])
    if(uploaddomo)
    {
        pushtodomo(df,datasetname="GPS of Texas Cities")
    }
    df
}



