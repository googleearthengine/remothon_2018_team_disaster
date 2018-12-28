# Load a library to handle geo-tiff images.
library(raster)

# Define a function of ratation in order to make match mathematical 
# dimension with geographic dimension.
rotate <- function(x) t(apply(x, 2, rev))

# Load the geo-tiff image which is produced by GEE.
img <- raster('/home/yufujimoto/Downloads/gee_.tif')

# Convert the loaded image to a numeric vector.
img.vect <- as.vector(img)

# Convert NA to 0.
img.vect[is.na(img.vect)] <- 0

# Calculate the standard deviation and the mean.
img.vect.sd <- sd(img.vect)
img.vect.mean <- mean(img.vect)

# Define the threshold with the -2 sigma.
thresh <- img.vect.mean-(2 * img.vect.sd)

# Conver the loaded image to matrix.
img.matrix <- as.matrix(img)

# Produce a binary image by using the threshold.
img.matrix[img.matrix >= thresh] <- 1
img.matrix[img.matrix < thresh] <- 0

# Calculate the percentage of landslide area in designated area.
res <- table(img.matrix)

p_ls <- round((res[[1]]/res[[2]])*100,2)
p_nr <- 100 - p_ls

# Prepare PNG image container.
png(width=1000, height=1000)

# Set the layout of 2x2.
par(mfrow=c(2,2))

# Plot the geo-tiff image which is produced by using GEE.
plot(img, main="Differences Between Before and After")

# Draw the histgram of differences.
plot(density(img.vect), main="Histgram of Diferences")
abline(h=0)
abline(v=thresh, lty=2, col="red")
abline(v=img.vect.mean, lty=2, col="blue")

# Draw a binary image.
image(rotate(img.matrix))

# Draw a pie chart as the result.
pie(res,
    labels=c(
        paste("Landslide\n",paste(p_ls,"%",sep=""),
        paste("Normal Condition\n", paste(p_nr,"%",sep="")
    ),col=c("red","blue")
)

# Closing
dev.off()

