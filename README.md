# Ideanet
# An application to map a network of ideas 
Included here is code to do the analyses and visualize the results for a concept mapping project.

**NOTE ON VERSION of R**
 This code has been tested using  version 4.12 of R. R can be downloaded here: https://cran.r-project.org/bin/windows/base/. Rstudio can be found here:  https://www.rstudio.com/products/rstudio/download/.  The code has been developed and tested on computers running Windows 10.
 
**Run Ideanet application**
*Run the following lines of code in R console on your local machine.*

*After running these five lines of code the first time, add hash a tag (#) at beginning of the first three lines prior to the first letter of the first word.  This action means running the code will skip these lines and run faster for subsequent work.  However, there is no problem if these three lines run every time.  Occasionally remove the hash tag  to once again run these three lines so that any updates and new functionality will be installed on your computer.*

install.packages("devtools") # run once then add hash tag (#) at the beginning of this line, prior to the word "install."

library(devtools)  # run once then add hash tag (#) at the beginning of this line, prior to the word "library."

install_github("ideanetwork/ideanet") # run once and the add hash tag (#) at the beginning of this line, prior to the word "install." 

library(Ideanet)# do not add a hash tag, run this line of code every time.

ideanet() # do not add a hash tag, run this line of code every time.
