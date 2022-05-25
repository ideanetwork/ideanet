### About concept mapping

Concept mapping a method for crowd-sourcing the understanding of and/or
the design of interventions of complex challenges. Unlike other group
processes which rely on consensus; this process elicits the many and
diverse viewpoints on a complex issue. The methodology relies on member
of a community to work invidually to:

-   Generate ideas in response to a specific challenge,

-   Organize all of the collected ideas into groups of similar ideas
    that they [participants] individually create and name, and,

-   Optionally, assess (e.g., rate) the ideas on one or more measure of
    value (e.g., importance, feasibility).

Mathematical algorithms translate the contributions from multiple
individuals into maps that shows the detailed ideas, themes and
priorities; the collective wisdom of the group. While visualization is
driven by sophisticated algorithms, the graphics are accessible without
knowledge of the underlying mathematics. As such, community members that
provided individual input can work together to interpret their results
and plan actions.

### About the Ideanet application

Ideanet is a software application to support concept mapping. The
Ideanet application provides a menu-driven interface for data
management, computation, and visualization for a concept mapping
project.

The program distributed here is shared in the hope that this will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. This work is
licensed under the GNU General Public License v3.0, a link to the
details of the license can be found at:
[License](https://www.gnu.org/licenses/gpl-3.0.en.html)

You are free to use this program, share with others and make and share
changes to the code. If you do make changes, the software license
requires that the code and any ensuing modifications be made publicly
available, allowing the entire community to benefit.

Send suggestions, questions, improvements, etc. to:
[info\@ideanetworks.io](mailto:info@ideanetworks.io){.email}

This is a development version, improvements to the code and additional
functions are in the process of being created and tested. The code has
been developed and tested in a Windows PC environment. This application
has [not]{.underline} been tested in a macOS environment.

## How to get started with Ideanet

### 1. Download supporting software

This code has been developed ad tested using version 4.1.3 of R. At
present the newest version of R (version 4.2) is causing problems
unrelated to the application. I recommend downloading this older version
of R until this problem is fixed. R version 4.1.3 can be downloaded at
the URL below. Navigate to to R version 4.1.3 and the click on the link
for **R-4.1.3-win.exe** to download and then open the file to install. I
recommend accepting all of the defaults. [Navigate to R version
4.1.3](https://cran.r-project.org/bin/windows/base/old/4.1.3/){style="color: blue"}

Rstudio desktop is recommended for running R code, a free version can be
accessed here:
[Rstudio](https://www.rstudio.com/products/rstudio/download/ "Download free desktop version"){style="color: blue"}

### 2. Install Ideanet application

Run the following code to install the application. This code only needs
to be run once to install the application. Run this code in the future,
as needed, to install newer versions of the Ideanet application. While I
prefer using RStudio for editing and running code, launching Base R
without RStudio will open an interface that can be used to install and
run the software.

#### RStudio install

-   If you downloaded the file "*ideanet_install.R*" then you can
    double-click on that file to open Rstudio with the code in the
    source window.

-   If RStudio is already open then, on the menu click on *File, Open
    file,* and navigate to the file and click to open.

-   If you do not have the the file "*ideanet_install.R*" then copy and
    paste the code below into RStudio *source* window.

-   Once the code is visible in the RStudio source window, click on
    **Ctrl+Alt+R** to run the code or using the RStudio menu click on
    *Code*, *Run region*, *Run all*.

#### Base R install

-   If you downloaded the file "*ideanet_install.R*" then from the menu,
    click on *open script* and navigate to the file.

-   If you do not have the the file "*ideanet_install.R*" then from the
    menu, click on *File, Open script* and then copy and paste the code
    below into editor window.

-   Once the code is available then in the menu click on *Edit*, *Run
    All*.


```r
if (!require(rlang)){
  install.packages("rlang")
  suppressPackageStartupMessages(library(rlang))
}
if (!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman))
}
if (!require(devtools)){
  install.packages("devtools")
  suppressPackageStartupMessages(library(devtools))
}

pacman::p_unlock() # remove 00lock if present

remotes::install_github("ideanetwork/ideanet", force = TRUE) 
```

### 3. Run the Ideanet application

If you have a copy of "*run_ideanet.R"* on your computer then follow the
same procedure as described above for installing Ideanet except, in this
case use, *run_ideanet.R*. If you do not have the the file
"run_ideanet.R" then copy and paste the code below following the same
procedures described above.

Once executed the code will open the program. Sometimes the Ideanet app
will open behind other windows applications. Look on your task bar for
[*feathericon*]{.underline} or use the hot keys (Alt+Tab) to tab through
open applications to find Ideanet.

Note that once the Ideanet application is open, R and RStudio will not
accept input because the Ideanet application has control of R. To stop
using the app and regain control of R or RStudio click *Quit, Close
app*.


```r
if (!require(pacman)){
  install.packages("pacman")
  suppressPackageStartupMessages(library(pacman))
}
pacman::p_unlock() # remove 00lock if present

Ideanet::ideanet() # open the application.
```

### 4. Use the Ideanet application

The following is a brief description of the menu options. Additional
details can be found in application.

##### Data entry

Use the menu option to create a blank Excel template for data entry. The
Ideanet software is structured to read data from this template. A
concept map requires data in two worksheets. (1) Sorting data is
required. Depending on how the sorting data was collected the data is
added to either the racked worksheet if manually entered or stacked
worksheet if downloaded from an online sorting program. (2) The text for
ideas must be entered in the ideas worksheet. If rating data is
collected the values worksheet accommodates up to three demographics and
up to two measures (e.g., importance, feasibility).

##### Review & define data

Sorting data can be checked for errors such as missing cards and data
entry errors so that these may be corrected. Rating data can be defined
by entering details about demographics and the measures.

##### Compute maps

Multidimensional scaling (MDS) algorithm to compute the location of
points in two dimensions and computes a stress value. Label analysis
computes the location each label from each participant on the map. The
map of items is partitioned with hierarchical cluster analysis using
Ward's method and produces multiple cluster solutions. The app will
create three output file; (1) an Excel workbook with cluster solutions
and cluster labels, (2) a slide deck illustrating cluster membership and
top labels for each cluster solution and (3) a slide deck illustrating
cluster membership in a dendrogram. Output files are useful for choosing
an optimal cluster solution for further analysis.

##### Create cluster report

Once a cluster solution is selected, this option creates a slide deck
that is a detailed report for that cluster solution.

##### Analyze values

*Pattern Analysis* creates a cluster rating map to illustrate relative
value among the clusters with a third (i.e., height) dimension. This
option outputs a PowerPoint file. *Pattern matching* compares patterns
in values between measures (e.g., importance & feasibility) or between
demographic groups (e.g., management & staff) and saves output as a
PowerPoint file. Two types of graphics are created for pattern matching;
(1) a ladder graph compares values at the cluster level and a bivariate
plot comparing the item means within each cluster.

##### Assess cluster solution

Once a cluster solution is chosen, in some cases, a review of the map
may evoke assertions that a given point may be better placed in a nearby
cluster instead of the cluster where the point is currently located. The
analysis done here will validate cluster membership in the current map
or indicate if the map may be improved by placing the item in
neighboring cluster.
