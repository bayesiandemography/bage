
library(hexSticker)
library(ggplot2)

data <- data.frame(x = 1, y = 1)

p <- ggplot(data, aes(x = x, y = y)) +
    geom_point() +
    theme_void()

sticker(subplot = p,
        package = "bage",
        p_size = 32,
        p_y = 1,
        p_color = "brown",
        s_width = 0,
        h_fill = "beige",
        h_color = "brown",
        filename = "sticker.png")

