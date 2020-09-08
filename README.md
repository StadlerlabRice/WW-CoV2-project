# WW-CoV2-project
Takes excel file output from qPCR and ddPCR and makes neat plots with appropriate labels and calculations using metadata from other google sheets'

## Git organization
1. Each branch of this repo is the code run on a file named after the experiment (ex: WW21) with the desired plotting formats and extra items specific to the run
2. As the methods become standardized, all data can be run just from the master branch without making new branches

## Readme step by step guide
### Current workflow for qPCR data analysis

#### Github (version control)
1. make sure you are on the master branch: `git checkout master`
2. Get master upto date to the remote branch : `git pull`
	
#### Sample naming in the '*calculations (lab notebook)/Plate layouts*' google sheet
1. Make sure the WWx or dd.WWx ID matches the ID in the name of the data file
2. Set sample names using the automatic labeller to the right side of the plate in this format
	**Target_Sample category-tube name.replicate number**
	
#### Quantstudio
1. Open the .eds file in Quantstudio (Applied Biosystems)
2. Check amplification curves if everything seems right
a. Check for systematic amplifications in non technical controls (NTC) - Or stochastic amplifications that seem problematic; by looking at raw data (multicomponent plots)
3. If using standard curves across runs - check that the threshold is at the desired value for each target (by selecting each target): Used to set thresholds to 0.04. Not doing it right now since each plate has its own standard  
4. Export excel (.xls) file from Quantstudio with the same name as the qPCR file and include the serial number for standard curve. Example **WW66_831 Rice_BCoV_Std45.xlsx** 
	
	*<Make sure you mirror the directory structure for the excel files and qPCR analysis folders>*
	
#### ddPCR (Quantalife)
1. Once the droplets reader run is done, the samples are auto processed (each well has a different threshold). Open the data file in Quantalife or Quantalife Pro
2. Identify the wells where automatic processing has not worked, by looking for the *NO CALL* in the quantity column
3. Select all the wells with No CALL and set an appropriate threshold for both targets in the 2d view
4. Check all the thresholds by selecting 1 column at a time and correct any thresholds that are set too low or two high (*aim for 1 cluster in each quadrant, without the thresholds cutting through any clusters*)

	*Catch: If exporting data from Quantalife Pro - change the column name of Copies/20ul to CopiesPer20ul to be consistent with the regular csv output data*
	
#### Sample registry
1. Check to make sure all the samples are entered in the *Sample registry/concentrated samples* sheet and they match the sample names entered in the template file in *calculations (lab notebook) Cov2/Plate layouts*
a. Check that the DI water sample has a unique name and doesn't match with previous weeks. Name it such as : 831Control Water1
2. Check that these columns are filled: **Biobot/other ID, Stock ID of Spike, Total WW volume calculated (ml), WW volume extracted (ml)**

#### Rstudio
1. Open the Rproject file **qPCR** in Rstudio - this will load from the current directory (all subpaths are relative to this) 
2. Run (source) the **processing_functions.R** file (clicking on the *save* button top left or *Run* - top right)
3. Make a variable with the names of the files to be processed: Ex- **read_these_sheets <- c('dd.WW31_831_N1N2', 'WW66_831_BCoV_Std48')**
4. Run this command: ** map(read_these_sheets, process_all_pcr): This will process Standard curve, qPCR data and ddPCR data for each file automatically (according to the name)
a. This dumps the data for each file with the appropriate sample labels in a google sheet 'qPCR data dump' google sheet
5. Open **calculations_multiple_runs** file
a. Change the read_these_sheets to the 1 or more sheets to plot together that are already in the 'qPCR data dump' and change the title_name to appropriate week :Ex- *713 Rice*
6. Source or run the script after saving the changes. This saves data in 3 different locations with the same name as **title_name**
a. qPCR complete data - all data including controls
b. WWTP data for HHD - only the 38 WWTPs
c. WWTP data for HHD - Designated manhole samples - saved to a sheet with *Special samples* in the name
d. Plots are saved to a html file in qPCR analysis folder
  
	  
In case you see any errors, follow the line number in the error and do extensive google searches - That's how I learn't it
