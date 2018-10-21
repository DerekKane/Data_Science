######################################################################
# Where is Waldo?
######################################################################

install.packages("raster")

library(raster)

waldo=brick("C:/Users/Derek/Documents/RPackages/Image Processing/DepartmentStoreW.jpg",package="raster")

plot(waldo,useRaster=FALSE)

# white component
white = min(waldo[[1]] , waldo[[2]] , waldo[[3]])>220
focalswhite = focal(white, w=matrix(3,3,3), fun=mean)
plot(focalswhite,useRaster=FALSE)

# red component
red = (waldo[[1]]>150)&(max(  waldo[[2]] , waldo[[3]])<90)
focalsred = focal(red, w=3, fun=mean)
plot(focalsred,useRaster=FALSE)


######################################################################
# Grading of exams
######################################################################


# A first step is to identify regions where we expect to find some "red" part (I assume here that students have to use a red pencil). 
# Let us start to check on the template and the filled form if we can identify red areas, 

exam=brick("C:/Users/Derek/Documents/RPackages/Image Processing/exam-blank.png",package="raster")

red = (exam[[1]]>150)&(max(  exam[[2]] , exam[[3]])<150)
focalsred = focal(red, w=matrix(3,3,3), fun=mean)
plot(focalsred,useRaster=FALSE) 


exam=brick("C:/Users/Derek/Documents/RPackages/Image Processing/exam-filled.png",package="raster")
red = (exam[[1]]>150)&(max(  exam[[2]] , exam[[3]])<150)
focalsred = focal(red, w=matrix(3,3,3), fun=mean)
plot(focalsred,useRaster=FALSE)

# First, we have to identify areas where students have to fill the blanks. So in the template, identify black boxes, and get the coordinates (here manually) 

exam=brick("C:/Users/Derek/Documents/RPackages/Image Processing/exam-blank.png",package="raster")
black = max(  exam[[1]] ,exam[[2]] , exam[[3]])<50
focalsblack = focal(black, w=matrix(3,3,3), fun=mean)
plot(focalsblack,useRaster=FALSE)
correct=locator(20)
coordinates=locator(20)
X1=c(73,115,156,199,239)
X2=c(386,428.9,471,510,554)
Y=c(601,536,470,405,341,276,210,145,79,15)
LISTX=c(rep(X1,each=10),rep(X2,each=10))
LISTY=rep(Y,10)
points(LISTX,LISTY,pch=16,col="blue")


# The blue points above are where we look for students' answers. Then, we have to define the vector of correct answers,

CORRECTX=c(X1[c(2,4,1,3,1,1,4,5,2,2)],
           X2[c(2,3,4,2,1,1,1,2,5,5)])
CORRECTY=c(Y,Y)
points(CORRECTX, CORRECTY,pch=16,col="red",cex=1.3)
UNCORRECTX=c(X1[rep(1:5,10)[-(c(2,4,1,3,1,1,4,5,2,2)
                              +seq(0,length=10,by=5))]],
             X2[rep(1:5,10)[-(c(2,3,4,2,1,1,1,2,5,5)
                              +seq(0,length=10,by=5))]])
UNCORRECTY=c(rep(Y,each=4),rep(Y,each=4))

# Here, we simply have to compare what the student answered with areas where we expect to find some red in, 

ind=which(values(focalsred)>.3)
yind=750-trunc(ind/610)
xind=ind-trunc(ind/610)*610
points(xind,yind,pch=19,cex=.4,col="blue")
points(CORRECTX, CORRECTY,pch=1,
       col="red",cex=1.5,lwd=1.5)

# Crosses on the graph on the right below are the answers identified as correct (here 13), 

icorrect=values(red)[(750-CORRECTY)*
                       + 610+(CORRECTX)]
points(CORRECTX[icorrect], CORRECTY[icorrect],pch=4,col="black",cex=1.5,lwd=1.5)
sum(icorrect)

# In the case there are negative points for non-correct answer, we can count how many incorrect answers we had. Here 4. 

iuncorrect=values(red)[(750-UNCORRECTY)*610+ (UNCORRECTX)]
sum(iuncorrect)
