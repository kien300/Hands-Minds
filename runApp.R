if (!require("pacman")) install.packages("pacman")
pacman::p_load(RCurl)

#Static Plots (ggplot)
scripts1 <- getURL('https://raw.githubusercontent.com/kien300/Hands-Minds/master/app.R',
                  ssl.verifypeer = FALSE)
eval(parse(text = scripts1))

#Interactive Plots (plotly)
scripts2 <- getURL('https://raw.githubusercontent.com/kien300/Hands-Minds/Plotly/app.R',
                  ssl.verifypeer = FALSE)
eval(parse(text = scripts2))
