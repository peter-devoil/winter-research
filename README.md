# Predicting (and Plotting) Minimum Soil Temps

## Overview
Deployed at http://apsimpoama.ddns.net/lwells/
So there's this trendy farming technique of winter-sowing sorghum.

There is lots of data from APSIM. Our goal is to plot that data in a way that's helpful to farmers.

[Peter deVoil](https://qaafi.uq.edu.au/profile/864/peter-devoil), a great mentor, is supervising me.

## Installation

### Dependencies
- [R version 3.6.3 is the oldest working version I've seen](https://www.r-project.org/)
- [Shiny (For making my humble plot into a reactive web app)](https://shiny.rstudio.com/)
- [Tidyverse (For ggplot and dyplr)](https://www.tidyverse.org/)
- [Lubridate (For EZ date manipulation)](https://lubridate.tidyverse.org/)
- [HMisc (Easiest way I could find to make a quantile plot)](https://cran.r-project.org/web/packages/Hmisc/index.html)

### Instructions to install and run
[Install R first](https://www.r-project.org/), then within an R console, type `install.packages(c("tidyverse", "shiny", "lubridate", "Hmisc"))`

Copy app.R and ddf.RData to the same directory and open app.R in RStudio (The default R IDE). Click the "Run App" button and you should be set 🤞

![image](https://user-images.githubusercontent.com/62700647/124210194-5a9ab900-db2e-11eb-9d5b-a42733894e0e.png)

## Licence
The plotted data was obtained from [SILO](https://www.longpaddock.qld.gov.au/silo/). As such, it's for research purposes only.

## Umm

Now I realise that no-one would want to run this locally when they can visit the websites where it's deployed. I just want to show I can write a half-decent README ok? Cool.

## Hey!

If you're wondering, "What on earth is he doing there?", I'd rather you get in touch than go about your day with your brows furrowed. Always up for a chat.

