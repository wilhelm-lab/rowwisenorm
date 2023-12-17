# rowwisenorm
Row-Wise Normalization R package including an R Shiny application to perform this normalization, as well as the normalization methods total sum normalization, VST, VSN, quantile normalization, ComBat, and M-ComBat.

## **Required input files:**
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
| C1 R1       | column name C1R1 batch1   | column name C1R1 batch2 |
| C1 R2       | column name C1R2 batch1   | column name C1R2 batch2 |
| C2 R1       | column name C2R1 batch1   | column name C2R1 batch2 |
| C2 R2       | column name C2R2 batch1   | column name C2R2 batch2 |
|             |                           |                         |


## **R Shiny application for Omics Data Normalization:**

**Description:**
This is an R Shiny Application which allows omics data to be uploaded and normalized. The application offers some pre-processing steps as well as the ability to view and download the normalized data. More than that, the results can also be displayed in the form of plots. The plots can also be downloaded as a PDF file and SVG files.

The available normalization methods include row-wise normalization, total sum normalization, VST, VSN, quantile normalization, ComBat, and M-ComBat.

**Important for Linux:**
Add the directory where ```zip``` executable is located to the system's ```PATH``` variable.
Alternatively: Specify the path to ```zip``` executable in the environment variable ```R_ZIPCMD```. 
This can be done by the following line of code in R: 
```
Sys.setenv("R_ZIPCMD" = "path/to/zip")
```

**Usage:**
Call the function ```run_app``` from the package ```rowwisenorm``` to run the Shiny application.

Terminal:
```
Rscript app.R 
```

Optional: The parameter 'local' can be added to obtain additional features inside the application.

Terminal:
```
Rscript app.R local 
```

**Important:** This is only recommended in case the application is used locally.

**More detailed description:**

The application is divided into three tabs.
1. First, there is a tab that includes the upload of data and the settings. In this place, the data can be processed, and the normalization can be performed. 
2. Following this, there is a tab allowing the user to have a preview of the normalized values, download the normalized data, and download plots of the raw data as well as plots of the normalized data. 
3. The third tab can be used to directly view the plots inside the shiny app. This symbolizes an easy way to determine whether the normalization helped reduce batch effects in comparison with the raw data.

**Acknowledgement:**

The included normalization method M-ComBat is implemented using the code of Caleb K. Stein.
The code is accessed via the GitHub repository https://github.com/SteinCK/M-ComBat

