---
title: "Web app documentation"
permalink: /web-app-docs
layout: pages
---
<br>
# PinPath web app
The PinPath web app can be used to visualize data onto pathway diagrams, and pinpoint where in the pathway the relevant changes occur.
Results from (epi)genomics, transcriptomics, (phospho)proteomics, metabolomics and many more experiments can be visualized onto pathway diagrams from KEGG and WikiPathways. 
You can also upload your own GPML and KGML files to visualize data onto custom pathways.
<br>
<br>

### Table of content
1. [Data upload](#data)
2. [Pathway visualization](#vis)
3. [Overrepresentation analysis](#ora)

<br>

### Data upload <a name="data"></a>
The uploaded data should include at least two columns: one column with the gene, protein, or metabolite IDs and one column that can be used for coloring. 
Including more columns is also possible, as long as there is at least one column with feature IDs and at least one column that can be used for coloring.

This is how a statistics table could look like:
<hr>

| Gene              | &nbsp;&nbsp;&nbsp;&nbsp;log2FC | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;p-value |
|-------------------|-------------------------------:|--------------------------------------------:|
| MECP2             | &nbsp;&nbsp;&nbsp;&nbsp;2.24   | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0001  |
| LHX1              | &nbsp;&nbsp;&nbsp;&nbsp;1.23   | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.0345  |
| FUT5              | &nbsp;&nbsp;&nbsp;&nbsp;0.98   | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0.6418  |
| ....              | &nbsp;&nbsp;&nbsp;&nbsp;....   | &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;....    |

<hr>
If your statistics table looks like this, you can later decide to color the pathway nodes with the log2FC, the p-value, or both!

<br>
<br>

### Pathway visualization <a name = "vis"></a>
After you have uploaded the data, you can visualize the data onto the pathway. Particularly, you can color the nodes of the pathway by one or more variables. 
The color palette can be fully customized and the pathway image, legend image, and node table can be downloaded as separate files. Furthermore, all pathways 
diagrams can also be converted to a network visualization.

<br>

### Overrepresentation analysis <a name="ora"></a>
You can also perform overrepresentation analysis to identify relevant pathways. 
For this, you need to have column in the statistics table that indicates whether a gene is differentially expressed or not. 
If this column does not exist, you can add this column (see <code>Adding columns to data</code> tab).
The clusterProfiler package is used for the overrepresentation analysis. 
Note that overrepresentation analysis on the metabolites is not supported (yet).

<br>