library(hexSticker)
library(magick)

# sticker ---------------------------------------------------------------------

# <div>Icons made by <a href="https://www.flaticon.com/free-icon/zoom_900930" title="phatplus">phatplus</a> from <a href="https://www.flaticon.com/" title="Flaticon">www.flaticon.com</a></div>

img <- image_read("figures/zoom.png")

# the output should go to a temporary file
outfile <- tempfile(fileext=".png")

sticker(img, package="iTIME",
        p_size=6, p_color = "#74bc1f", p_y = 1.1, p_x = 1.1,
        s_x=1, s_y=1, s_width=1.4, s_height=1.25,
        h_fill="white", h_color="#74bc1f", 
        url = "https://github.com/FridleyLab/iTIME", u_color = "#74bc1f", u_size = 1.3,
# This line throws an error starting the app  filename="figures/hex.png"
       filename=outfile)
