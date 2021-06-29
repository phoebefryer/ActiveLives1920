theme_GMM <- function(
  base_size = 12,
  base_colour = "#63666A"
) {
  ##Axis
  theme(
    legend.position = "none",
  
    axis.ticks = element_blank(),
    axis.line = element_line(colour = base_colour),
    axis.line.x = element_line(colour = x_col),
    axis.line.y = element_line(colour = y_col),
    
    ##Text
    text = element_text(
      colour = base_colour, size = base_size,
      hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.8
    ),
    axis.text = element_text(colour = base_colour, size = base_size),
    plot.title = element_text(face = "bold", 
                              hjust = 0, colour = "black", vjust = 0),
    
    ## Axis title attributes. Adjustments of
    
    axis.title.y = element_text(hjust = 1, vjust = 1),
    axis.title.x = element_text(hjust = 1, vjust = 0),
    
    ## Background attributes (currently all blank)
    
    panel.grid.major.y = element_line(color = "#a3a3a2"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),   
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    
    ## Strip attributes for facet grid and facet wrap
    
    strip.background =   element_blank(),
    strip.text =         element_text(color = "black", face = "bold", size = base_size + 1),
    strip.text.x =       element_text(),
    strip.text.y =       element_text(angle = -90)
  )}

theme_GMM2 <- function() {
  
  font <- windowsFonts("Helvetica")  #gOTHAM TITLES
  fontColour <- "#706F6F"
  
  theme(
    #grid elements
    panel.grid.major.y = element_line(color = "#a3a3a2"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),   
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    
    #key text
    plot.title = element_text(
      family = font,
      colour = "#009FE3",
      size = 16,
      face = 'bold',          
      hjust = 0,              
      vjust = 2),         
    
    plot.subtitle = element_text(         
      family = font,
      colour = fontColour,
      size = 14),               
    
    plot.caption = element_text(          
      family = font,  
      colour = fontColour,
      size = 12,                 
      hjust = 0),               
    
    #general text
    axis.title = element_text(             
      family = font,    
      colour = fontColour,
      size = 12),             
    
    axis.text = element_text(            
      family = font, 
      colour = fontColour,
      size = 12),              
    
    axis.title.y = element_text(hjust = 1, vjust = 1),
    axis.text.x = element_text(          
      margin=margin(5, b = 10)),
    
    #legend
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family = font,
                               size = 12,
                               color  = fontColour)
  )
}
  
GMM_cols <- c(
  `blue` = "#009FE3",
  `pink` = "#E5007E",
  `orange` = "#F39200",
  `green` = "#95C11F",
  `purple` = "#8C1D82",
  `pale purple` = "#8A5EA4",
  `turquoise` = "#009E83",
  `dark blue` = "#0069b4",
  `yellow` = "#Fdc300",
  `grey` = "#706f6f" 
)

GS_cols <- c(
  `purple` = "#5B2D86",
  `pink` = "#E5007E",
  `pale purple` = "#8A6EAD",
  `pale pink` = "#D7697A",
  `yellow` = "#FCCA58",
  `blue` = "#5197D1"
)

GS_cols_grad <- c(
  `white` = "#FFFFFF",
  `pink` = "#E5007E",
  `purple` = "#5B2D86"
)

scale_GMM <- function(type = "fill", ...) {
  
  type = match.arg(type, c("colour", "fill"))
  
  cols <- unname(GMM_cols)
  
  if (type == "fill") {
    s <- scale_fill_manual(values = cols)
  } else if (type == "colour") {
    s <- scale_colour_manual(values = cols)
  }
  
  return(s)
  
}

scale_GS <- function(type = "fill", ...) {
  
  type = match.arg(type, c("colour", "fill"))
  
  cols <- unname(GS_cols_grad)
  
  if (type == "fill") {
    s <- scale_fill_manual(values = cols)
  } else if (type == "colour") {
    s <- scale_colour_manual(values = cols)
  }
  
  return(s)
  
}

check_pal <- function(
  x = GMM_cols
) {
  
  if (is.numeric(x)) {
    
    if (length(x) > 1) {
      
      x <- GMM_cols[x]
      
    } else
      x <- GMM_cols[1:x]
  }
  
  graphics::pie(
    rep(1, length(x)),
    col = x,
    labels = names(x))
  
}

check_pal(5)