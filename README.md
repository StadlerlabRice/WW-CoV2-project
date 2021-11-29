# WW-CoV2-project
Takes excel file output from qPCR and ddPCR, attaches the sample names and makes neat plots with appropriate labels and calculations using metadata from other google sheets'

## Git organization
Here's a handy guide to simple git commands : [git the simple guide](https://rogerdudler.github.io/git-guide/)
1. WW_master is the default/master branch of this project
2. Any experimental changes should be made in a different side branch and pushed to the remote. Then submit a pull request and then merge it on github 
  *This is to ensure that there are no conflicts if someone changed the master while you were working on a side branch*

## Script-functions organization
If you are looking to run subsets of these scripts to plot specific data (sub)sets, please familiarize yourself with the code organization and how the plotting functions are being called, and the default values.
1. The main functions are written in these scripts, divided up by their tasks
	- `0-general_functions_main.R` : Essential - loads all packages, all smaller functions (see point 2 below)
	- `1-processing_functions.R` : first step in processing ddPCR/qPCR data (is called by `2-calc..` script)
	- `2-calculations_multiple_runs.R` : **Main function for ddPCR data processing** - this is the only script you should run for processing raw data to handoff to HHD - single or small sets of runs done in a day/week
	- `3-weekly_comparisions.R` : Run this for comparing data across ddPCR runs/weeks etc as a metaanalysis with plotting
	- `2.1-make_html_plots.Rmd` : plotting multiple graphs into a html file - for single run/sets of runs (called by `2-calc..` script)
	- `3.1-weekly_comparison plots.Rmd` : plotting multiple graphs into a html file - for the weekly comparisons (called by `3-weekly..` script)
2. Minor tasks (_lots of them_) are modularized into functions, and written in multiple `.R` files in the `scripts_general functions` folder
	- If you are ever looking for a specific function called by the program _(for debugging specific errors and such)_, and are confused which general_function script it is in: open `git bash` or `Terminal` in linux/mac and type `grep -r "plot_mean_sd" *.R` ; replace `plot_mean_sd` with the function/code snippet you are looking for. _This will search all the `.R` files in the current folder for this specific text._ 
	- There are a couple of **custom plotting functions** using ggplot2 that do most of the plotting in the `.Rmd` scripts. `g.8-plot_mean_sd_jitter` and `g.9-plot_scatter`. The main advantage of these functions is that I put in defaults for frequently used kinds of plots so you can make many different plots with slight variants in the x_axis variable : `x_var`, y_axis variable = `y_var`, colour_variable = `colour_var`... quite easily with less repetitive code. **Definitely look through the arguments list of the plotting functions before using them/to troubleshoot for unexpected plotting errors.**
	- If you have a readymade csv file output by the script with the data you want to plot, just proceed to source the `0-general_functions_main.R` and `read.csv('path_to_file', col_names = TRUE)` then you can get right into the plotting using the functions mentioned above
	
## Readme step by step guide
### Current workflow for COVID data analysis

#### Git (version control)
1. Make sure you are on the main branch by typing: `git checkout WW_master` in *git bash* (black window)
2. Get the branch upto date to the remote branch *(in case others made any changes)* using: `git pull`
	
#### File and sample name convention
1. ddPCR sheet name *(in Raw ddpcr)* should include the dd.WWxy ID (*good to add a descriptive title after the ID*)
2. Sample naming in the '*calculations (lab notebook)/Plate layouts*' google sheet
    a. Make sure the WWx or dd.WWx ID matches the ID in the name of the data file
    b. Set sample names using the automatic labeller to the right side of the plate in this format
	**Target-Sample category_tube name.replicate number**
	Example: *BCoV-Vaccine_S32.2* or *N1N2-908_W.1*
3. qPCR excel filename should include the WWxy ID, Target name and standard curve ID, Ex: **WW66_831_BCoV_Std48** 
	
#### qPCR (//obsolete) (Quantstudio software)
1. Open the .eds file in Quantstudio (Applied Biosystems)
2. Check amplification curves if everything seems right
    a. Check for systematic amplifications in non technical controls (NTC) - Or stochastic amplifications that seem problematic; by looking at raw data (multicomponent plots)
3. If using standard curves across runs - check that the threshold is at the desired value for each target (by selecting each target): Used to set thresholds to 0.04. Not doing it right now since each plate has its own standard  
4. Export excel (.xls) file from Quantstudio with the same name as the qPCR file and include the serial number for standard curve. Example **WW66_831 Rice_BCoV_Std45.xlsx** 
	
	
#### ddPCR (Quantalife software)
1. Once the droplets reader run is done, open the data file in Quantalife or Quantalife analysis Pro
2. Ignore the automatic thresholding (each well has a different threshold), and set a manual threshold to be optimally separating the positive and negative clusters in the 2D view. (**aim for 1 cluster in each quadrant, without the thresholds cutting through any clusters**).*You can rely on positive/negative controls for assistance*
3. Check all the thresholds in the 1D view, by selecting 1 column at a time and correct any thresholds that are set too low or two high. 
	
#### Metadata 

##### Sample registry
1. Check to make sure all the samples are entered in the *Sample registry/concentrated samples* sheet and they match the sample names entered in the template file in *calculations (lab notebook) Cov2/Plate layouts*
a. Check that the DI water sample has a unique name and doesn't match with previous weeks. Name it such as : *0304 DI1*
2. Check that these columns are filled: **Biobot/other ID, WW volume filtered (ml)**. And if samples are being spiked, these columns should be filled in too **Stock ID of Spike, Total WW volume received (ml)**

##### Other metadata sheets
1. Biobot_ID should be updated with should be updated with any new manhole samples in sheet **All manhole**

#### Directory map
If you are running this set of scripts for the first time, all essential directories should be loaded by git automatically. If running additional functions, make sure that you mimic the directory structures mentioned below. These directories should exist in the folder in which the Rprojct file + all the scripts exist
- qPCR analysis/Standards *only if running qPCR files with standards on them* (saving plot with standard curves)

#### Source data files
Source data and metada is in google sheets, ask Prashant/Camille Mccall for access
URLs for all the spreadsheets are in the *0-general_functions_main.R/sheeturls* variable

#### Rstudio (scripts)
1. Open the Rproject file **COVID-qPCR work** in Rstudio - this will load Rstudio from the current directory (all subpaths are relative to this which will enable the script to run properly) 
2. File name and other inputs 
- Go to the google sheet [User inputs, ...for R code](https://docs.google.com/spreadsheets/d/1SAINnazqMrjTBSuhYiIBbx8B7_reHzaEuwGTdkNA6wk/edit#gid=931502497)
- duplicate the **template** sheet and rename it after the desired final output file name. Ex *0304 Liftstations*
- **Drag it the the leftmost position**

2. Run (source) the **2-calculations_multiple_runs** file (clicking on the *save* button top left or *Run* - top right)
3. Authenticate the google drive and follow other prompts and it should lead to the outputs being saved to various other google sheets and local CSV files. This saves data in 3 different locations with the same name as **title_name**
    a. qPCR complete data - all data including controls
    b. WWTP and manhole data for HHD - only the 38 WWTPs
    c. WWTP and manhole data for HHD - non WWTP samples (Ex: Lift station, congregate facilities, manholes, Bayou etc.) - saved to a sheet with *manhole* in the name
    d. Plots are saved to a html file in qPCR analysis folder


4. --- Exceptions ---
- If you have any qPCR data, then run this command: `list_quant_data <- map(read_these_sheets, process_all_pcr)` after sourcing *1-processing_functions.R* and before running *2-calculations_multiple_runs.R*. Comment out the call to this list_quant_data <- map(read_these_sheets ... in the 2-calcs file. There might be errors that need troubleshooting since this has not been extensively tested. 
: This will process Standard curve, qPCR data and ddPCR data for each file automatically (according to the name)
    - This dumps the data for each file with the appropriate sample labels in a google sheet 'qPCR data dump' google sheet.
    - If you have a single file to run, you can call the function directly, instead of using `map` **process_all_qpcr('WW66_831_BCoV_Std48')**
    - If you already ran standard curve, and only need to process the qPCR. use `process_qpcr('WW66_831_BCoV_Std48')` instead
    - If the qPCR plate does not have a standard curve in it and you want to use an older standard curve, run something like this: `process_qpcr('WW66_831_BCoV_Std48', std_override = 'Std21')`
    - If you are processing baylor data *(whose name/volume information is emailed to us and saved in a local excel sheet)* give a regular expression indicating the baylor_samples location to the baylor_wells input to the functions. Example: Baylor samples present in wells A1 to D11, then run: `process_all_pcr('WW66_831_BCoV_Std48', baylor_wells = '[A-D]([1-9]$|10|11)')` 
    

In case you see any errors,
1. look for `Show Traceback` key next to the error (not always available)
2. figure out the line number in the error and trace back from there. 
- **Do extensive google searches** - That's how I learn't coding *(PK)*


### Meta-analysis across weeks

1. Run **3-Weekly_comparisons.R** with the names of all weeks to include in the analysis
      - Ensure that the week's samples have already been processed and saved in the *complete qPCR data* sheet
