library(base64enc)
library(magick)
library(rvest)
library(xml2)

# Read the HTML file
doc <- read_html("tempsample.html")

# Extract base64 images
img_nodes <- doc %>% html_nodes("img")
img_data <- list()

for (i in seq_along(img_nodes)) {
  src <- html_attr(img_nodes[i], "src")
  if (grepl("^data:image/png;base64,", src)) {
    base64_str <- sub("^data:image/png;base64,", "", src)
    
    # Decode Base64 to raw binary
    decoded_img <- tryCatch({
      base64decode(base64_str)
    }, error = function(e) {
      message("Error in Base64 decoding: ", e$message)
      return(NULL)
    })
    
    # Check if decoding was successful
    if (!is.null(decoded_img)) {
      # Write raw data to a temporary file for verification
      temp_file <- tempfile(fileext = ".png")
      writeBin(decoded_img, temp_file)
      
      # Try to read the image
      img <- tryCatch({
        image_read(temp_file)
      }, error = function(e) {
        message("Error in reading image: ", e$message)
        return(NULL)
      })
      
      # Store the image in the list if valid
      if (!is.null(img)) {
        img_data[[i]] <- img
      }
    }
  }
}

# Check if images were successfully extracted
if (length(img_data) == 0) {
  message("No valid images extracted from Base64 data.")
} else {
  message("Successfully extracted ", length(img_data), " images.")
}
