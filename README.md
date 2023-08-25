# rowwisenorm
Row-wise normalization R package

**Required input files:**
1. proteinGroups.txt 
2. experimental design file 

> #### Important:
>
> User is asked to create the second file as a **tsv** file according to *experimentalDesignTemplate.txt*.
>
> - Each row represents a different condition.
> - The first column in each row represents the condition itself. The condition must be written the same way as its named inside the following columns.
> - The remaining columns represent the repeats. Insert here the column names of proteinGroups.txt that correspond to the intensities for the respective condition and the respective repeat, named as *R1*, *R2* etc. (starting with repeat 1 for column 2, repeat 2 for column 3 etc.)
> - **Important:** If a certain condition is not present for a certain repeat, leave the respective place empty and go on with a second tab.  

