# rowwisenorm
Row-wise normalization R package

**Required input files:**
1. Data as for example proteinGroups.txt from MaxQuant
2. Experimental Design File 

**Important:**
User is asked to create the second file as a **tsv** file according to *experimentalDesignTemplate.txt*.

> #### Experimental Design File:  
> - Each row represents a different condition.
> - The first column in each row represents the condition name itself. 
> - The remaining columns represent the batches. Insert here the column names present inside the data that correspond to the respective condition and the respective batch (starting with batch 1 for column 2, batch 2 for column 3 etc.)
> - **Repeats:** If there is more than one repeat for a single batch, the repeats should be handled as separate conditions.
> - **Important:** If a certain condition is not present for a certain batch, leave the respective place empty and go on with a second tab.  
> - **Important:** Add an empty line at the end of the file

Example look of the experimental design:

Attempt | #1 | #2 | #3 | #4 | #5 | #6 | #7 | #8 | #9 | #10 | #11
--- | --- | --- | --- |--- |--- |--- |--- |--- |--- |--- |---
Seconds | 301 | 283 | 290 | 286 | 289 | 285 | 287 | 287 | 272 | 276 | 269
