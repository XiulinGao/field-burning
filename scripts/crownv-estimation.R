#crownv-estimation.R
## script used for estimating tree crown volume
## procedure as following:
##1. manually define the contour and vertical axis of each half of a crown using ImageJ 
## by polygon selection, and save the coordinates of vertices of the polygon. scale set
##2. calculate area and centroid of each of the non-self-intersecting closed polygon  
##3. estimating volume based on Pappus's centroid theorem by rotating the lamina
## around the vertical axis, average two for each image and then average four
## for each tree (4 images taken from cardinal directions)

#library(stringr) ##if script is ran solely then load these packages.
#library(dplyr)
#library(tidyr)

read_coord_file <- function(filename) {
  image.id <- str_sub(filename, 16, 19)  #image id that will be used to refer back to tree
  poly.pst <- str_sub(filename, -5, -5) # polygon position relative to vertical axis
  label <- str_sub(filename, 21, -6)    
  coord <- read.csv(filename, sep = "", header=FALSE)
  names(coord)[1:2] <- c("x", "y")
  coord$image.id <- image.id
  coord$poly.pst <- poly.pst
  coord$label <- label
  return(coord)
}

concat_coord_files <- function(filelist){
  l <- lapply(filelist, read_coord_file)
  r <- bind_rows(l)
  return(r)
}
all_coords <- concat_coord_files(list.files("../data/imagej", full.names=TRUE))

#also read in tree-image ID matching data

tree_image <- read.csv("../data/image-tree-id.csv", stringsAsFactors = FALSE)
tree_image <- tree_image %>% mutate(tree.id = paste(site.id, tree.id, sep = ""))

#bad naming of image, need to convert ton character
tree_image$image.id <- as.character(tree_image$image.id)


all_coords <- all_coords %>% mutate(polyid = paste(image.id,label,poly.pst, sep = "-"))
polyn <- unique(all_coords$polyid) #number of polygons
poly.df <- data.frame()

for (d in 1:length(polyn)){
    poly.d <- all_coords[which(all_coords$polyid == polyn[d]), ]           
    n <- length(poly.d$x) #number of vertices
    label <-polyn[d]
    
    i<-1
    A.sum<-0
    C.xsum<-0
    C.ysum<-0
   
    
    while(i<n){
      
      A <-(poly.d$x[i]*poly.d$y[i+1]-poly.d$x[i+1]*poly.d$y[i])
      A.sum<-A.sum+A
      
      C.x<-(poly.d$x[i]+poly.d$x[i+1])*(poly.d$x[i]*poly.d$y[i+1]-poly.d$x[i+1]*poly.d$y[i])
      C.xsum<-C.xsum+C.x
      
      C.y<-(poly.d$y[i]+poly.d$y[i+1])*(poly.d$x[i]*poly.d$y[i+1]-poly.d$x[i+1]*poly.d$y[i])
      C.ysum<-C.ysum+C.y
      
      i <- i + 1
      }
    A.sum <- 0.5*A.sum
    C.xsum <- C.xsum/(6*A.sum)
    C.ysum <- C.ysum/(6*A.sum)
    poly <- as.data.frame(cbind(A.sum, C.xsum, C.ysum))
    names(poly) <- c("area", "C.x", "C.y")
    poly$polyid <- polyn[d]
    poly.df <- rbind(poly.df, poly)
    rm("A", "C.x", "C.xsum", "C.y", "C.ysum", "A.sum", "poly.d", "poly", "d", "i",
       "n")
}

#convert value of area into positive. Negative value resulted from the 
# the clockwise order of vertices. 

poly.df <- poly.df %>% mutate(area = abs(area)) %>% 
  mutate_at(c("area", "C.x", "C.y"), list(~round(., 2))) 

#Volume estimation is: area * (distance tranveled by centroid when roate it
#around vertical axis)
# distance traveled by centroid is 2*pi*(|C.x - V.x|).
# V.x is the x coordinate of the vertical axis, here
#is the x of the two far-left (polygon position right) or far-right(polygon position left)
#vertex on the vertical axis

vaxis_x <- all_coords %>% left_join(poly.df, by = "polyid") %>%
  group_by(polyid) %>% 
  summarise(X.min = min(x), X.max = max(x)) #get both max and min X of vertices 
                                            #for each polygon
          
vol <- poly.df %>% mutate(poly.pst = str_sub(polyid, -1, -1),
                          image.id = str_sub(polyid, 1, 4),
                          time.taken = str_sub(polyid, 6, -3)) %>% 
  left_join(vaxis_x, by = "polyid")


estimate_volume <- function(poly.pst){
  if (poly.pst == "r"){
   dist <- 2*pi* abs(vol$C.x - vol$X.min)
   vol <- vol$area*dist
  }
  else {
    dist <- 2*pi* abs(vol$X.max - vol$C.x)
    vol <- vol$area*dist
  }
  
  return(vol)
}

vol$v.est <- sapply(vol$poly.pst, estimate_volume) 

#average volume for each image and calcualte %crown loss

vol_est <- vol %>% select(polyid, poly.pst, image.id, time.taken, v.est) %>% 
  group_by(image.id, time.taken) %>% 
  summarise(vol.ave = mean(v.est, na.rm = TRUE)) %>% 
  spread(time.taken, vol.ave) %>% #wide data to calculate %crown volume loss or dead (for survival measurement)
  mutate(crown.diff = 1 - post/pre) %>% 
  mutate(crown.diff = round(crown.diff, 4))
  

#match image to tree, also covert % crown dead to % crown survied for survival measurements

percnt_crwn <- tree_image %>% 
  left_join(vol_est, by = "image.id") %>% 
  group_by(tree.id, measurement) %>% 
  summarise(crown.diff = mean(crown.diff, na.rm = TRUE)) %>% 
  mutate(crown.diff = round(crown.diff, 4)) %>% 
  spread(measurement, crown.diff) %>% #spread to convert %crown dead to %crown survied for 'survival'
  mutate(survival = 1 - survival)

#rename column
 percnt_crwn <- percnt_crwn %>% rename(crown.loss = damage,crown.live = survival)

#As for trees of which images were not taken is because of there was not crown loss
# or survived crown is less than 5% (so impossible to detect from image analysis, we did
# not bother to take image), those will be direcly assigned from the visual assessment. So
# 'NA' is not truely 'NA'.

rm("all_coords", "vaxis_x", "vol", "polyn")

