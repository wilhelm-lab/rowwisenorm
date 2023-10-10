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

**Example look of the experimental design table:**
- Two conditions: C1, C2
- Two repeats: R1, R2
- Two batches: Represented by the second and third column

|             |                           |                         |
|-------------|---------------------------|-------------------------|
| C1 R1       | column_name_c1r1_batch1   | column_name_c1r1_batch2 |
| C1 R2       | column_name_c1r2_batch1   | column_name_c1r2_batch2 |
| C2 R1       | column_name_c2r1_batch1   | column_name_c2r1_batch2 |
| C2 R2       | column_name_c2r2_batch1   | column_name_c2r2_batch2 |

