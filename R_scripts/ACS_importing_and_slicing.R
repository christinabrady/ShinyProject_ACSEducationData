#ACS: SCHL:EDUCATIONAL ATTAINMENT, ST:STATE OF CURRENT RESIDENCE (colIndex=5), FOD2P: recoded field of degree- second entry
# PERNP: total person's earnings: pg 137, use values from ADJINC to adjust PERNP to constant dollars
#PINCP: total person's income, pg 137, use values from ADJINC to adjust PINCP to constant dollars
#http://www2.census.gov/acs2013_5yr/pums/
#PWGTP (person weight) colIndex= 7, FOD1P=colIndex=86
#read a small portion of a file to understand the structure and find the desired variables
library(ggplot2)
library(maps)
library(mapproj)
library(plyr)
#usa11 <- read.csv("~/Documents/Datafiles/ACS/PUMS_US_ind_yearly/csv_pus11/ss11pusa.csv", nrow=10)
#names <- colnames(usall)

#col.pos <- match(VarInt, names)
#cols[col.pos] <- "character"
#usa11small <- read.csv("~/Documents/Datafiles/ACS/PUMS_US_ind_yearly/csv_pus11/ss11pusa.csv", nrow=10, colClasses=cols)

#create a list of file names
filenames <- list.files("~/Documents/Datafiles/ACS/PUMS_US_ind_yearly/All", full.names=TRUE)
#create a vector of the names of variables of interest in order to find their column indeces
VarInt <- c("SCHL", "ST", "PWGTP", "PINCP", "ADJINC")

#save file path
#dir <- "~/Documents/Datafiles/ACS/PUMS_US_ind_yearly/All" 

#create a list of the positions of positions of the variables of interest in each file
varlist <- as.data.frame(lapply(filenames, function(x){
    y <- read.csv(x, nrow=2)
    names <- colnames(y)
    col.pos <- match(VarInt, names)
    return(col.pos)
}))

#create a list of column lengths of each file in order to create a list of colClasses vectors
colinfo <- unlist(lapply(filenames, function(x){
    y <- read.csv(x, nrow=2)
    columns <- ncol(y)
    return(columns)
}))

#create a list of vectors to use in the colClasses argument of read.csv
collist <- lapply(colinfo, function(x){
    c <- rep("NULL", times=x)
    return(c)
})
i <- 1:14
colindeces <- lapply(i, function(i){
    l <- unlist(collist[i])
    l[unlist(varlist[i])] <- "numeric"
    collist[i] <- l
})   

#create a variable to hold the directory address to be used when writing new csv files
write.dir <- "~/Documents/Coursera courses/Data Science Specialization/Developing Data Products/Shiny Project//R_scripts"

#create a function that will read each csv file, group by the state and schl variables 
#and sum the pwgtp, and create a new data frame that will later be saved as a new csv file

acsprocess <- function(n){
    df <- read.csv(filenames[n], colClasses=unlist(colindeces[n]))
    df2 <- ddply(df, .(ST, SCHL), summarize, sum(PWGTP))
    colnames(df2) <- c("state_code", "school_code", "people")
    return(df2)
}
educ08b <- acsprocess(2)
educ09a <- acsprocess(3)
educ09b <- acsprocess(4)
educ10a <- acsprocess(5)
educ10b <- acsprocess(6)
educ11a <- acsprocess(7)
educ11b <-acsprocess(8)
educ12a <-acsprocess(9)
educ12b <- acsprocess(10)
educ13a <- acsprocess(11)
educ13b <- acsprocess(12)
educ13c <- acsprocess(13)

#combine data frames that correspond to the same year add a year variable to all 
#of the new data frames
educ08 <- rbind(educ08a, educ08b)
educ08$year <- 2008
educ09 <- rbind(educ09a, educ09b)
educ09$year <- 2009
educ10 <- rbind(educ10a, educ10b)
educ10$year <- 2010
educ11 <- rbind(educ11a, educ11b)
educ11$year <- 2011
educ12 <- rbind(educ12a, educ12b)
educ12$year <- 2012
educ13 <- rbind(educ13a, educ13b, educ13c, educ13d)
educ13$year <- 2013

#write new csv files in order to have backups
write.csv(educ13, file=paste(write.dir, "NewCSVs/processededdata", "educ13.csv", sep=""), row.names=FALSE)
write.csv(educ12, file=paste(write.dir, "NewCSVs/processededdata", "educ12.csv", sep=""), row.names=FALSE)
write.csv(educ11, file=paste(write.dir, "NewCSVs/processededdata", "educ11.csv", sep=""), row.names=FALSE)
write.csv(educ10, file=paste(write.dir, "NewCSVs/processededdata", "educ10.csv", sep=""), row.names=FALSE)
write.csv(educ09, file=paste(write.dir, "NewCSVs/processededdata", "educ09.csv", sep=""), row.names=FALSE)
write.csv(educ08, file=paste(write.dir, "NewCSVs/processededdata", "educ08.csv", sep=""), row.names=FALSE)

#create a function to read and combine all of the new csv files
newfiles <- list.files(paste(write.dir, "NewCSVs/processededdata", sep='/'), full.names=TRUE)
n <- length(newfiles)
eddata <- data.frame()
for(i in 1:n){
     eddata<- rbind(eddata, read.csv(newfiles[i]))
}
#match state names to state codes
acsstates <- c('alabama', 'alaska', 'arizona', 'arkansas', 'california', 
               'colorado', 'connecticut', 'delaware', 'district of columbia', 
               'florida', 'georgia', 'hawaii', 'idaho', 'illinois', 'indiana', 
               'iowa', 'kansas', 'kentucky', 'louisiana', 'maine', 'maryland', 
               'massachusetts', 'michigan', 'minnesota', 'mississippi', 
               'missouri', 'montana', 'nebraska', 'nevada', 'new hampshire', 
               'new jersey', 'new mexico', 'new york', 'north carolina', 
               'north dakota', 'ohio', 'oklahoma', 'oregon', 'pennsylvania', 
               'rhode island', 'south carolina', 'south dakota', 'tennessee', 
               'texas', 'utah', 'vermont', 'virginia', 'washington','west virginia', 
               'wisconsin', 'wyoming', 'puerto rico')
state.code <- unique(eddata$state_code)
names(acsstates) <- state.code
eddata$acs_state_name <- acsstates[eddata$state_code]
#backup data:
write.csv(eddata, file= paste(write.dir, "NewCSVs", "acseddata.csv", sep="/"))

state.map  <- map_data("state")
states <- unique(state.map$region)

#calculate state populations and proportions
state_pop <- aggregate(people~state_code+year, sum, data=eddata)
colnames(state_pop) <- c('state_code', 'year', 'totalpop')

### need to match 2 columns... this doesn't work
eddata2 <- merge(eddata, state_pop, by=c("state_code", "year"))
eddata2$prop <- eddata2$people/eddata2$totalpop * 100
#check proportions
check <- aggregate(prop~state_code + year, sum, data=eddata2)
#save work to a csv file
edprops <- paste(write.dir, "NewCSVs/edprops.csv", sep="/")
write.csv(eddata2, edprops)

#subset based on the states that are included in ggplot's mapping function
shinydata <- eddata2[eddata2$acs_state_name %in% states,]
shinydata2 <- merge(shinydata, state.map, by.x="acs_state_name", by.y="region", all.x=TRUE)
shinydata2 <- arrange(shinydata2, order)
phd08 <- subset(shinydata2, school_code==24 & year==2008)
phd08g <- ggplot(phd_map08, aes(x=long, y=lat, group=group, fill=prop)) +geom_polygon(colour="blue") + coord_map("polyconic")
