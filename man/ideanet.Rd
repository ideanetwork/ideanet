\name{Ideanet}
\alias{Ideanet}
\title{Ideanet}
\description{
Ideanet is a software application to support concept mapping, a method for crowdsourcing the understanding of and/or the design of interventions for any number of complex challenges. Unlike other group processes which rely on consensus; this process elicits the many and diverse viewpoints on an issue. The methodology relies on brainstorming, sorting, and rating to collect input from the group. Computation creates a visual representation, a map, of the group's thinking.

This application provides a menu of options for users.

Data Entry: Creates a template to enter sorting and (optionally) rating data.  Sorting data can be in either the racked or stacked format.  Rating data allows one or two measures (e.g., importance, feasibility) and, optionally, up to three demographic variables. This format must be used for data entry.

Revies & Define data:
  Checks sort data for errors and, if any errors are present, writes out an excel file with a description of errors.
  Checks rating data to determine if there are demographic variables and, if any, returns a description of variables for user review. Asks user to define rating variables. If rating data is present a check of rating data must be performed.

Compute maps: Reads sorting data and used multidimensional scaling to compute the location of points and produces a stress value for the solution.  The analysis applies cluster analysis to group points into clusters of items with similar meaning. The cluster analysis produces cluster solutions from five to 15 clusters.  Output includes an excel file documenting cluster membership of items in each of the cluster solutions, a powerpoint file illustrating cluster membership on the maps and a powerpoint file illustrating cluster membership with a dendrogram. Output files are used to choose an optimal cluster solution.

Create a cluster report: Once a cluster solution has been chosen,this option reads in the excel output file from compute maps and creates a detailed report in powerpoint.

Analyze values: Reads in the input excel file with the rating data and the output excel file with the cluster solutions and produces either a pattern analysis (cluster rating map) or a pattern matching (ladder graphs and go zones graphs) output file in powerpoint.

Assess validity: Read in the output excel file with the cluster solutions and assesses the validity of the chosen cluster solution using silhouette analysis.  The results are output to an excel file and indicate which, if any, items might be better suited for membership in a neighboring cluster.

Help:  Provides additional details about this applications, licensing and more specific help on concept mapping generally and menu options.

Quit: Closes the application.

NOTE:  All powerpoint objects are editable and may ungrouped, resized, and so on as needed for the creation of reports and presentatons.
}


