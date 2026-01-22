# PinPath

# Table of contents
1. [What is PinPath?](#introduction)
2. [Uploading your statistics table](#table)
3. [Adding columns](#columns)
4. [Overrepresentation analysis](#ora)
5. [Contact](#contact)

## 1. What is PinPath? <a name="introduction"></a>
PinPath is a tool that can be used to visualize data onto pathway diagrams, and pinpoint where in the pathway the relevant changes occur.
Results from genomics, epigenomics, transcriptomics, (phospho)proteomics, and many more experiments can be shown onto pathway diagrams from KEGG and WikiPathways. 
Users can also upload GPML and KGML files to visualize data onto custom pathways.
As long as your data can be linked to genes or proteins, you can visualize it using PinPath.
<br>

## Uploading your statistics table <a name="table"></a>
A statistics table should include at least two columns: one column with the gene IDs and one column that can be used for coloring. 
Including more columns is also possible, as long as there is at least one column with gene IDs and at least one column that can be used for coloring.


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

## Adding columns <a name="columns"></a>
After you have uploaded the statistics table, you can add and remove columns using the + button in the <code>Gene info</code> page. 

#### Discretization
The rule system for discretizing a contiuous variable (e.g., p-value or logFCs) consist of the following structure:
```
Label 1: rule that defines label 1
Label 2: rule that defines label 2
...
Label n: rule that defines label n
```
For instance, you can add a column that indicates whether a gene is significantly differentially expressed by setting the following rules. 
Note that column name is put in-between backticks (`` ` ``column name`` ` ``). 

```
Yes: `p-value` < 0.05
No: `p-value` >= 0.05
```

Here is another example to make a column that indicates up and downregulation:
```
Up: (`Significant` == "Yes") & (`log2FC` > 0)
Down: (`Significant` == "Yes") & (`log2FC` < 0)
Unchanged: `Significant` == "No"
```

#### Transformation
You can also transform a continuous variable. For instance, perform log10-transformation on the p-value.

```
log10(`p-value`)
```
Moreover, you can convert the log2FC to the fold change.

```
2^`log2FC`
```

<br>

## Overrepresentation analysis <a name="ora"></a>
You can also perform overrepresentation analysis to identify relevant pathways. 
For this, you need to have column in the statistics table that indicates whether a gene is differentially expressed or not. 
If this column does not exist, you can add this column (see [Adding columns](#columns)).
The clusterProfiler package is used for the overrepresentation analysis. 
Note that overrepresentation analysis on the metabolites is not supported (yet).


