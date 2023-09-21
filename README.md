# rowwisenorm
Row-wise normalization R package

**Required input files:**
1. Data as for example proteinGroups.txt from MaxQuant
2. experimental design file 

> #### Important:
>
> User is asked to create the second file as a **tsv** file according to *experimentalDesignTemplate.txt*.
>
> - Each row represents a different condition.
> - The first column in each row represents the condition name itself. The condition must be written the same way as its named inside the following columns.
> - The remaining columns represent the repeats. Insert here the column names present inside the data that correspond to the respective condition and the respective repeat (starting with repeat 1 for column 2, repeat 2 for column 3 etc.)
> - **Important:** If a certain condition is not present for a certain repeat, leave the respective place empty and go on with a second tab.  
> - **Important:** Add an empty line at the end of the file

