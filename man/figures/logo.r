library(hexSticker)

img <- "~/Downloads/logo_bg.jpg"
sticker(img,
        package="MedicareR",
        p_size=8, p_y = 1.3,
        s_x=1, s_y=0.8,  # position
        s_width=0.95, s_height=2,
        h_fill = "#05060b", h_color = "#0071BC", p_color="#FFCB05",
        filename="man/figures/logo.png")


geom_hexagon(size = 1.2, fill = "#1881C2", color = "#87B13F")
