#building the package and generating a.zip file



malt<- function(major,minor1,minor2,act=TRUE){

olt<-getwd()
 #if the version is on the R path
vor<-paste("c:/programs/r/R-",major,'.',minor1,'.',minor2,"/bin/",sep='')
# else just use R-act for the actually installed version
vor<-NULL
##setwd("i:/biometri/LiSciDataDevel")
## setwd("d:/LiSciDataDEVEL")
setwd("d:/dataRepDEVEL")

Rcmd<-if (act) {"R CMD "} else { 
# for the 32 bit version
 paste("C:/programs/R/R-",major,".",minor1,".",minor2,"/bin/i386/Rcmd.exe ",sep='')
# version 2.11
# paste("C:/programs/R/R-",major,".",minor1,".",minor2,"/bin/Rcmd.exe ",sep='')  
}

#the zip files
system(paste(vor,Rcmd, " INSTALL --build LiSciData",sep=''))
# the tar.gz files
system(paste(vor,Rcmd, " build LiSciData",sep=''))
#system('rm *.tar')
setwd("d:/study/Dropbox/StudyDesignAnalysis/Rpackages/grafdat")
system(paste(vor,Rcmd, " INSTALL --build GrafenHails",sep=''))
system(paste(vor,Rcmd, " build  GrafenHails",sep=''))
setwd(olt)

setwd("d:/study/Dropbox/StudyDesignAnalysis/Rpackages/oehlert")
system(paste(vor,Rcmd, " INSTALL --build oehlert",sep=''))
system(paste(vor,Rcmd, " build  oehlert",sep=''))
setwd(olt)

##setwd("i:/biometri/LiSciData")
# makes the packages file for data repository

library(tools)




rpackageNew<-paste("c:/temp/rpackage",major,'.',minor1,sep='')


dir.create(rpackageNew)

hum<- "d:/study/Dropbox/StudyDesignAnalysis/Rpackages/grafdat/"
system(paste("cp ",hum,"GrafenHails*.zip ", rpackageNew,sep=''))
system(paste("cp ",hum,"GrafenHails*.tar.gz ",rpackageNew,sep=''))


system(paste("mv ",hum,"GrafenHails*.zip  RpackZipped",sep=''))
system(paste("mv ",hum,"GrafenHails*.tar.gz  RpackZipped",sep=''))

##system(paste("cp d:/grafdat/GrafenHails*.zip ", rpackageNew))
##system("mv d:/grafdat/GrafenHails*.zip  RpackZipped")

hum<- "d:/study/Dropbox/StudyDesignAnalysis/Rpackages/oehlert/"
system(paste("cp ",hum,"oehlert*.zip ", rpackageNew,sep=''))
system(paste("cp ",hum,"oehlert*.tar.gz ",rpackageNew,sep=''))

system(paste("mv ",hum,"oehlert*.zip  RpackZipped",sep=''))
system(paste("mv ",hum,"oehlert*.tar.gz  RpackZipped",sep=''))



system(paste("cp LiSciData*.zip ",rpackageNew))
system(paste("cp LiSciData*.tar.gz ",rpackageNew))

system("mv LiSciData*.zip  RpackZipped ")
system("mv LiSciData*.tar.gz  RpackZipped ")


##



##write_PACKAGES('rpackage/')
write_PACKAGES(paste(rpackageNew,'/',sep=''))

hum<- "d:/study/Dropbox/StudyDesignAnalysis/Rpackages/Rpackzipped"
system(paste('cp  RpackZipped/*.* ',hum))



##system(paste("mv rpackage ",rpackageNew,sep=''))
##system(paste("cp rpackage ",rpackageNew,sep=''))
##system('removePackages.bat &')
             }


#malt(2,9,2)
#malt(2,10,0)
##malt(2,11,1,act=FALSE )
malt(2,14,1,act=TRUE)
malt(2,14,2,act=FALSE)
malt(2,15,0,act=FALSE)



#Finally puttin onto the web
#system('build.bat')


	
