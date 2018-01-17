library(RCurl)

#for Master
scripts1 <- getURL('https://raw.githubusercontent.com/kien300/Hands-Minds/master/app.R',
                  ssl.verifypeer = FALSE)
eval(parse(text = scripts1))

#for Plotly Branch
scripts2 <- getURL('https://raw.githubusercontent.com/kien300/Hands-Minds/Plotly/app.R',
                  ssl.verifypeer = FALSE)
eval(parse(text = scripts2))
