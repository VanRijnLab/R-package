# SlimStampeRData
This R package helps you with analyzing data from the App-version of SlimStampen.

## Beginner guide
If you are new to RStudio, please follow these steps.

1. Install R: https://cran.rstudio.com/

    Choose the download that fits your system.

2. Install RStudio: https://www.rstudio.com/products/rstudio/download/#download

    Install the recommended version for your system.

3. Open RStudio

4. Run this in the console

    ```
    install.packages("devtools")
    ```
    You will probably get a warning about installing RTools, you can ignore this.
   
    If you receive the error "Error in install.packages : cannot open file ..." it means R has issues accessing your files or folders. For solutions to this problem see [this page](https://github.com/VanRijnLab/SlimStampeRData/wiki/Common-problems).
   
5. Run these commands in the console

    ```
    library(devtools)
    devtools::install_github("VanRijnLab/SlimStampeRData", build_vignettes = TRUE, dependencies = TRUE)
    ```
    The console will show a prompt asking whether all packages should be updated. Select option 1 (by typing 1 and then pressing enter), this will update all packages.

6. Run this command in the console

    ```
    vignette("SlimStampeRVignette")
    ```
    This shows the tutorial in your viewer. Follow this tutorial for an explanation of the package functions. Good luck!  

## Expert guide

1. This package expects at least version 2.15.1 of R
2. The "devtools" package is used to install from GitHub.

    ```
    install.packages("devtools")
    library(devtools)
    ```
3. If you want to be able to view the tutorial vignette, you need all dependencies to be installed.

    ```
    devtools::install_github("VanRijnLab/SlimStampeRData", build_vignettes = TRUE, dependencies = TRUE)
    ```
    
    If you don't have a need to see the vignette, you can install without the suggested packages instead (a slightly leaner install).
    ```
    devtools::install_github("VanRijnLab/SlimStampeRData")
    ```
    


## Planning and Documentation
For the documentation, planning and other information check out the [Wiki](../../wiki) of this repository!

## Something missing?
Feel free to open a new issue if you see any mistakes in the documentation or have an idea for a feature!
