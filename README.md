# Ziggurat plot app

The Ziggurat plot app (https://alecchristie888.shinyapps.io/ziggurat-plot-app/) is deployed online using a shiny app. This README file is about the shiny app itself - for more details on the methodology behind the app (Balance Evidence Assessment Model (BEAM)) please visit the app itself or read the corresponding article (https://osf.io/ujk6n/). A peer-reviewed article will be published soon.
As a brief introduction, the purpose of the app is to support decision-makers who are faced with weighing and assessing diverse sources of evidence. The Balance Evidence Assessment Model (BEAM) is designed to be used to help do this and as part of this method users can generate a Ziggurat plot (which this app specifically supports) that visualises the weight and support of evidence for a given assumption (e.g., a claim or hypothesis that can be refuted or supported by available evidence).

# Shiny app
The code to run the shiny app is contained with a single file called app.R within this repository (ziggurat-plot-app/app.R). A folder called www stores images for the app to display. All these images are licensed under a Creative Commons Attribution-ShareAlike 4.0 International License as part of the article and app.

## App.R
The app receives a table as an input that can be entered within the app itself or as a csv upload with values for the three aspects of evidence. You can download the data or the Ziggurat plot that the app generates. The app can be customised based on the users preferences for values for assessing strength of support, information and source reliability and relevance of evidence. All these terms are explained in Christie et al. (2023; https://osf.io/ujk6n/) or on the app itself.  
The app supports bookmarking and has three separate tabs for information on the app, defining values for assessing the evidence, and generating a ziggurat plot.

## www
The www folder, as is convention with all shiny apps, contains static files and data that the app draws upon to run. This includes images, guidance documents, and an offline template. 

# Running the app locally
To run the shiny app locally, you should to simply select and run all of the lines of code in `app.R`. In R Studio, there is also a button for `Run App`. The app should open in a browser on your computer or in R Studio depending on your settings. You can publish the app using a shinyapps.io account for which free licenses are avaiable (at time of writing, this gives you 5 free applications).

# Copyright
There are two separate licenses, one for the open source code and one for the online tool (i.e., about its name and concept).

The repository ziggurat-plot-app is copyright (c) 2023 Alec Christie, but it is Open Source and licensed under the MIT License.

The Ziggurat plot app (including its online implementation at https://alecchristie888.shinyapps.io/ziggurat-plot-app/) by Alec P. Christie and authors listed in Christie et al. 2023 (https://osf.io/ujk6n/) is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (based on a work at https://github.com/alecchristie888/ziggurat-plot-app). 

# Acknowledgements
I'd like to acknowledge the help of Dr Hannah Wauchope and Dr William Morgan in coding early versions of the Ziggurat plot app, which I then refined and adapted into the app that is now published.
