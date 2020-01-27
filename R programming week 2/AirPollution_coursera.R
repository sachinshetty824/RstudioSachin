pollutantmean<-function(directory,pollutant,id=1:322){
        csvPath<-  paste("./",directory,"/",formatC(id,width=3,
                        flag = "0"),".csv",sep="")
        #csvPath <- list.files("C:/Users/NMBRSystems01/Documents/
        #                       specdataSample", full.names = TRUE)
        df<- lapply(csvPath,read.csv,header = TRUE, stringsAsFactors = FALSE )
        df1<-data.frame()
        for (i in id){
                df1<-rbind(df1,na_df[[i]])
        }
        mean(df1[,pollutant],na.rm=TRUE)
}


complete<-function(directory,id=1:10){
        csvPath<-  paste("./",directory,"/",formatC(id,width=3,
                                                    flag = "0"),".csv",sep="")
        df<- lapply(csvPath,read.csv,header = TRUE, stringsAsFactors = FALSE )
        sum_CompleteCases_df<-vector()
        bigframe <- do.call(rbind,df)
        index<-1
        for (i in id){
                
                sum_CompleteCases_df[index]<-
                        sum(complete.cases
                            (subset(bigframe,ID==i)))
                index<-index+1
        }
        output<-cbind(id,nobs=sum_CompleteCases_df)
        output
}

corr<-function(directory,threshold=0){
        dataPath <- paste("C:/Users/NMBRSystems01/Documents/",directory,sep="")
        csvPaths<- list.files(path = dataPath,pattern = ".csv",full.names = TRUE)
        
        cor_vector<-vector()
        
        for( i in csvPaths)
        { 
               # csvPath<-  paste("./",directory,"/",formatC(i,width=3,
                #                                            flag = "0"),".csv",sep="")
                df<- lapply(i,read.csv,header = TRUE, stringsAsFactors = FALSE )
                bigframe <- do.call(rbind,df)
        completeCases_df<-bigframe[complete.cases(bigframe),]
        if (nrow(completeCases_df) > threshold){  
          cor_vector <- c(cor_vector, 
                                cor(completeCases_df[,"sulfate"], 
                                    completeCases_df[, "nitrate"]))
        }
        
        }
        cor_vector
}
        