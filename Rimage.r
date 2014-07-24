# R script to locate GPMVs and measure Kp.
# Author: Kevin Spring
# Date: 10 Mar 2014

# Need to take into account more than one object in the image

# Import libraries
library(EBImage)
library(data.table)

# Load images
#fileLocG <- 

fileLocG <- '~/02g.tif'
fileLocR <- '~/02r.tif'

Gimg <- readImage(fileLocG)
Rimg <- readImage(fileLocR)


# Detect GPMV objects and remove noise
    Robj <- thresh(Rimg) #, w=40, h=40, offset=0.01)

#   Fill the empty spaces

    RobjFill <- fillHull(Robj) # fills empty space between objects

#   Erode the empty space, removes the small noise

    erode_brush <- makeBrush(size=10, shape='disc')
    Robj_erode <- erode(RobjFill, erode_brush)

    # Dilate the remaining objects, 
    # the idea is to make it larger than the actual GPMV

    dilate_brush <- makeBrush(size=15, shape='disc')
    RobjDilate <- dilate(Robj_erode, dilate_brush)

    # Turn the image into bitmap for object count
    rImgLabel <- bwlabel(RobjDilate)
    
# Keep the largest object

    
# Create a mask image of the membrane

    # Smooth out the object.
    # Find the center of the object

    moment <- computeFeatures.moment(rImgLabel)
    xcenter <- moment[,1]
    ycenter <- moment[,2]
    center <- cbind(xcenter, ycenter)

    # Find the longest radis of the objects

    shape <- computeFeatures.shape(rImgLabel)
    radius <- shape[,5]

    coord <- cbind(center, radius)


    #   For this take the center pixel

    #   Create the matrix
        blank <- matrix(data=0, nrow=dim(Gimg)[1], ncol=dim(Gimg)[2])
        blank <- bwlabel(Image(blank))

        #for (i in 1:nrow(coord)) {
            blank <- drawCircle(img=blank, 
                                x=round(coord[1,1]), 
                                y=round(coord[1,2]), 
                                radius=round(coord[1,3]), 
                                fill=TRUE, col=1)
        #}
        
        #for (i in 1:nrow(coord)) {
            blank <- drawCircle(img=blank,
                                x=round(coord[1,1]),
                                y=round(coord[1,2]),
                                radius=round(coord[1,3]-16),
                                fill=TRUE, col=0)
        #}
            
# Measure Kp

    # Calculate the diameter
        # http://stackoverflow.com/questions/1829429/index-value-for-matrix-in-r
    
    # Create a new Green image using the membrane mask.
    mask <- which(blank!=1, arr.ind=TRUE)
    gImgNew <- Gimg
    
    for(i in (1:nrow(mask))) {
        gImgNew[mask[i,][1], mask[i,][2] ] <- 0
    }
    
    # Rearrange the pixels in the gImgNew so there is a 
    # continuum of highest to lowest intensity
        
    # Convert that range of intensities/location into a vector
    pixels <- which(gImgNew != 0, arr.ind=TRUE)
    intensity <- matrix(data=0, nrow=nrow(pixels), ncol=1)
  
    #Apply the mask on the Red image
    rImgNew <- Rimg
    for(i in (1:nrow(mask))) {
        rImgNew[mask[i,][1], mask[i,][2] ] <- 0
    }
    
    ## Add red intensity to dat
    for (i in (1:nrow(pixels))) {
        intensity.R[i,] <- rImgNew[pixels[i,][1], pixels[i,][2]]
    }
    
    for(i in (1:nrow(dat))) {
        rImgNew[dat[i,][
        

    
    dat <- cbind(pixels, intensity)
    
    #Look at the mask
    gImgNew2 <- matrix(data=0, nrow=dim(Gimg)[1], ncol=dim(Gimg)[2])
    for(i in (1:nrow(dat))) {
        gImgNew2[dat[i,][1], dat[i,][2] ] <- dat[i,][3]
    }
    
    
    
    # Make a data table that sorts the table by the Green intensity
    colnames(dat) <- c('rowY', 'colX', 'intensityG')
    dat.dt <- data.table(dat, key='intensityG')
    
    # make a new image that shows the merged intensity
    gImgMerg <- matrix(data=0, nrow=dim(Gimg)[1], ncol=dim(Gimg)[2])
    for(i in (1:nrow(dat))) {
        gImgMerg[dat[i,][1], dat[i,][2] ] <- dat.dt$intensityG[i]
    }
    
    # make a new column in the original dat matrix for the Red intensity
    for(i in (1:nrow(dat))) {
        dat.dt$intensityR[i] <- Rimg[dat.dt$rowY[i], dat.dt$colX[i]]
    }
    
    #dat <- cbind(dat, dat.dt$intensityR)
    colnames(dat) <- c('rowY', 'colX', 'intensityG', 'intensityR')
    
    # Get the quantiles of the intensities.
    qbottom <- as.vector(quantile(dat[,3])[1])
    q2nd <- as.vector(quantile(dat[,3])[2])
    qMed <- as.vector(quantile(dat[,3])[3])
    q3rd <- as.vector(quantile(dat[,3])[4])
    qtop <- as.vector(quantile(dat[,3])[5])
    
    # Determine the Kp(o)
        # Make a new data frame with only the Red intensities from the
        # bottom and 2nd quantiles
    dat.dt.or <- dat.dt$intensityR[which(dat.dt$intensityG >= qbottom & dat.dt$intensityG <= q2nd)]
    orMean <- median(dat.dt.or)

    
    # Determine the Kp(d)
        # Make a new data frame with only the Red intensisties from the
        # 3rd and top
    dat.dt.do <- dat.dt$intensityR[which(dat.dt$intensityG >= q3rd & dat.dt$intensityG <= qtop)]
    doMean <- median(dat.dt.do)
        
    # Make the rearranged Red image    
    rImgRR <- matrix(data=0, nrow=dim(Gimg)[1], ncol=dim(Gimg)[2])
    for(i in (1:nrow(dat))) {
        rImgRR[dat[i,][1], dat[i,][2] ] <- dat.dt$intensityR[1]
    }
    
    # Find Kp
    Kp <- orMean / doMean
    