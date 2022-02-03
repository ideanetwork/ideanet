# Ideanet
# An application to map a network of ideas 
Included here is code to do the analyses and visualize the results for a concept mapping project.

**NOTE ON VERSION of R**
 This code has been tested using  version 4.12 of R. R can be downloaded here: https://cran.r-project.org/bin/windows/base/. Rstudio can be found here:  https://www.rstudio.com/products/rstudio/download/.  The code has been developed and tested on computers running Windows 10.
 
# Run appllication

*Run the following lines of code in R console on your local machine.*

*After running next line of code the first time,add hash tag (#) at beginning of line, prior to word "install" to avoid rerunning this line each time and downloading a new copy. Remove the hash tag, to download a new copy whenever desired in case of updates and new functionality.*

install.packages("devtools") # add hash tag at the beginning of this line, prior to the word "install" to skip this line of code.

library(devtools) # do not add a hash tag at the beginning of this line, run this line of code every time.

*After running this next line of code the first time, add hash tag (#) at beginning of line to avoid rerunning this line each time and downloading a new copy. Remove the hash tag, to download a new copy whenever desired or in case of updates with new functionality.*

install_github("ideanetwork/ideanet") # add hash tag at the beginning of this line, prior to the word "install" to skip this line of code.

library(Ideanet)# do not add a hash tag at the beginning of this line, run this line of code every time.

ideanet() # do not add a hash tag at the beginning of this line, run this line of code every time.
