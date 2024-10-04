# fit model
library(tidyverse)
hw2 <- HoltWinters(
  ap_red,
  seasonal = "mult",
  alpha = .2,
  # gamma = .2,
  beta = .2
)
hw_pred2 <- predict(hw2, n.ahead = 4*12, prediction.interval = TRUE)

c1 <- "#0D395F"
# c2 <- "#5CB8B2"
c2 <- "#FF6A13"
c2 <- "#FF8C3D"
c2 <- "#f9cca9"
c3 <- "#e6ccf3"
c2 <- "#ce9f80"

# plot
# library(ggthemr)
# ggthemr_reset()
# ggthemr("dust", layout = "scientific")
bind_rows(
  tibble(
    time = time(ap_full),
    val = c(ap_full)
  ) %>% mutate(type = "obs"),
  bind_rows(
    # tibble(
    #   time = time(hw2$fitted[,1]),
    #   val = c(hw2$fitted[,1])
    # ),
    tibble(
      time = time(hw_pred2),
      val = c(hw_pred2[,1]),
      lwr = c(hw_pred2[,3]),
      upr = c(hw_pred2[,2])
    )
  )  %>% mutate(type = "pred")
) %>%
  ggplot(aes(x = time, y = val, col = type)) + 
  geom_line(
    # linewidth = 1.15
  ) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr), 
    linetype = 2,
    alpha = 0.15
    # linewidth = 1.15
  ) + 
  # theme_bw() +
  # theme_minimal() +
  theme_void() + 
  geom_vline(aes(xintercept = 1957), linetype = "dotted") +
  labs(
    x = "Date",
    y = "Air Passengers (1000s)"
  ) +
  # scale_color_manual(values = c("#bcc4d2", "#d8ae5a")) +
  # scale_color_manual(values = c("#425563", "#DFD1A7"))
  scale_color_manual(values = c(c1, c2)) +
  theme(legend.position="none")
ggsave(
  filename = "img0219.png", dpi = 2000, 
  scale = .25,
  width = 10, 
  height = 6,
  units = "in"
) 

# create sticker
#Load Packages
library(hexSticker)
library(ggplot2)
library(showtext)
library(magick)
library(tidyverse)
font_add_google("Bitter", "bitter")

#Load Image
img0219 <- image_read("img0219.png")

#Create Sticker
stat0219_hex <- sticker( 
  #specific image & characteristics
  img0219, 
  s_x = 1, s_y = 0.85, s_width = 5, s_height=0.8,
  #specify package name & characteristics
  package="STAT 0219", 
  p_size=7, p_color = c1, p_family = "bitter",p_fontface = "bold",
  # border characteristics
  h_fill="white", h_color=c2,h_size = 1, 
  #specify text in bottom right & characteristics
  url = "Time Series Analysis",
  u_size=2.5, u_color = c1,u_family="bitter", 
  u_angle=0,u_x=0.53, u_y=0.37,
  #save to... 
  filename = "STAT0116_hex_sticker.png")

plot(stat0219_hex)





