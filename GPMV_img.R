
library(EBImage)

# 1.) Use the red image to find the object

file_loc_r <- '/Users/kevin/Dropbox/levental_lab/projects/1.0_TMD/data/microscope/Lab_micro/images/20130410_img_an/wt/4r_20.tif'
r_img_20 <- readImage(file_loc_r)

# 1A.) Use thresh to find objects

r_img_20t2 <- thresh(r_img_20, w=40, h=40, offset=0.01)

# 1B.) Fill the empty spaces

r_img_fill <- fillHull(r_img_20t2) # fills empty space between objects

# 1C.) Erode the empty space, removes the small noise

erode_brush <- makeBrush(size=10, shape='disc')
r_img_20t2_erode <- erode(r_img_fill, erode_brush)

# 1D.) Dilate the remaining objects, the idea is to make it larger than the actual GPMV

dilate_brush <- makeBrush(size=20, shape='disc')
r_img_20t2_dilate <- dilate(r_img_20t2_erode, dilate_brush)

# 1E.) Turn the image into bitmap for object count
r_img_label <- bwlabel(r_img_20t2_dilate)

# 1F.) Remove objects intersecting with the edge of the image.

# if the object is x% from the edge, delete it
    x05 <- ceiling(dim(r_img_label)[1]*.05)
    y05 <- ceiling(dim(r_img_label)[2]*.05)
    top <- y05
    bottom <- dim(r_img_label)[2] - y05
    left <- x05
    right <- dim(r_img_label)[1] - x05
    rm_object <- vector()
        
    for (i in 1:max(r_img_label)) {
        # find area that is 5% from the edge
        object <- which(r_img_label==i, arr.ind=TRUE)

        # if that object is in the 5% area then delete it
        if ( min(object[,1]) < left | max(object[,1]) > right | min(object[,2]) < top | max(object[,2]) > bottom) {
            rm_object <- c(rm_object, i)
            }
        }

    r_img_label_rm <- rmObjects(r_img_label, rm_object)
        
# 2.) Smooth out the object.
# 2A.) Find the center of the object

moment <- computeFeatures.moment(r_img_label_rm)
xcenter <- moment[,1]
ycenter <- moment[,2]
center <- cbind(xcenter, ycenter)

# 2B.) Find the longest radis of the objects

shape <- computeFeatures.shape(r_img_label_rm)
radius <- shape[,5]

coord <- cbind(center, radius)

# 2C.) Create a new image
        # For this take the center pixel

        # Create the matrix
        blank <- matrix(data=0, nrow=dim(r_img_20)[1], ncol=dim(r_img_20)[2])
        blank <- bwlabel(Image(blank))

        # for each object in the original image

        for (i in 1:nrow(coord)) {
            blank <- drawCircle(img=blank, x=round(coord[i,1]), y=round(coord[i,2]), radius=round(coord[i,3]), fill=TRUE, col=i)
            }
            
# 3.) Measure Kp

        # For each of the identified objects, find the corresponding bright area that means the Ld phase on the
        # green image

        # Using the bright side of the green image
        # Take multiple slices from each object to make sure there are 2 peaks in the red image.


              

