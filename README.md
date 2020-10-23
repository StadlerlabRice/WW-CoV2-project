# WW-CoV2-project
Takes excel file output from qPCR and ddPCR, attaches the sample names and makes neat plots with appropriate labels and calculations using metadata from other google sheets'

## Git organization
1. WW_master is the default/master branch of this project
2. Any changes should be made in a different side branch and pushed to the remote. Then submit a pull request and then merge it on github 
  *This is to ensure that there are no conflicts if someone changed the master while you were working on a side branch*

## Readme step by step guide
### Current workflow for qPCR data analysis

#### Github (version control)
1. Make sure you are on the master branch by typing: `git checkout master` in *git bash* (black window)
2. Get the branch upto date to the remote branch *(in case others made any changes)*: `git pull`
	
#### File and sample name convention
1. qPCR excel filename should include the WWxy ID, Target name and standard curve ID, Ex: **WW66_831_BCoV_Std48** 
2. ddPCR sheet name *(in Raw ddpcr)* should include the dd.WWxy ID (*good to add a descriptive title after the ID*)
3. Sample naming in the '*calculations (lab notebook)/Plate layouts*' google sheet
    a. Make sure the WWx or dd.WWx ID matches the ID in the name of the data file
    b. Set sample names using the automatic labeller to the right side of the plate in this format
	**Target-Sample category_tube name.replicate number**
	Example: *BCoV-Vaccine_S32.2* or *N1N2-908_W.1*
	
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
	
#### Sample registry
1. Check to make sure all the samples are entered in the *Sample registry/concentrated samples* sheet and they match the sample names entered in the template file in *calculations (lab notebook) Cov2/Plate layouts*
a. Check that the DI water sample has a unique name and doesn't match with previous weeks. Name it such as : 831Control Water1
2. Check that these columns are filled: **Biobot/other ID, Stock ID of Spike, Total WW volume calculated (ml), WW volume extracted (ml)**

#### Other metadata sheets
1. Biobot_ID should be updated with the biobot id vs the WWTP name table for each week's samples

#### Rstudio
1. Open the Rproject file **qPCR** in Rstudio - this will load from the current directory (all subpaths are relative to this) 
2. Run (source) the **processing_functions.R** file (clicking on the *save* button top left or *Run* - top right)
3. Make a variable with the names of the files to be processed: Ex- **read_these_sheets <- c('dd.WW31_831_N1N2', 'WW66_831_BCoV_Std48')**
4. Run this command: ** map(read_these_sheets, process_all_pcr): This will process Standard curve, qPCR data and ddPCR data for each file automatically (according to the name)
    a. This dumps the data for each file with the appropriate sample labels in a google sheet 'qPCR data dump' google sheet
    - If you have a single file to run, you can call the function directly, instead of using `map` **process_all_qpcr('WW66_831_BCoV_Std48')**
    b. If you already ran standard curve, and only need to process the qPCR. use `process_qpcr('WW66_831_BCoV_Std48')` instead
    c. If the qPCR plate does not have a standard curve in it and you want to use an older standard curve, run something like this: `process_qpcr('WW66_831_BCoV_Std48', std_override = 'Std21')`
    d. If you are processing baylor data *(whose name/volume information is emailed to us and saved in a local excel sheet)* give a regular expression indicating the baylor_samples location to the baylor_wells input to the functions. Example: Baylor samples present in wells A1 to D11, then run: `process_all_pcr('WW66_831_BCoV_Std48', baylor_wells = '[A-D]([1-9]$|10|11)')` 
    
5. Open **calculations_multiple_runs** file
    a. Change the read_these_sheets to the 1 or more sheets to plot together that are already in the 'qPCR data dump' and change the title_name to appropriate week :Ex- *713 Rice*
6. Source or run the script after saving the changes. This saves data in 3 different locations with the same name as **title_name**
    a. qPCR complete data - all data including controls
    b. WWTP data for HHD - only the 38 WWTPs
    c. WWTP data for HHD - Designated manhole samples - saved to a sheet with *Special samples* in the name
    d. Plots are saved to a html file in qPCR analysis folder


In case you see any errors,
1. look for `Show Traceback` key next to the error (not always available)
2. figure out the line number in the error and trace back from there. 
- **Do extensive google searches** - That's how I learn't coding *(PK)*


### Meta-analysis across weeks

1. Run Weekly_comparisons.R with the names of all weeks to include in the analysis
      - Ensure that the week's samples have already been processed and saved in the *complete qPCR data* sheet