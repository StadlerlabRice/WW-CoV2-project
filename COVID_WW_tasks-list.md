# Improvements - long term
1. Do the y-axis renaming thing (like plate_reader plotting) to take col names to meaningful long names using a lookup list -- to prevent errors from manually passing the name in //but will make it less usable outside the pipeline?


# Covid tasks
16-11-21
- [x] _(in branch: grab_composite_assignment)_ 1. Add sample registry new column : grab vs composite
- [x] (done) 2. Make the code plot stuff -- focus on pMMoV data -- + Kyle

_(optional)_ --
- [x] (done) 3. Clean up the github to ignore csv / xlsx files
- [x] (done) 4. Update the code to output in specific places .. 
	- I'm tired of maintaining this, let's ignore this since I'm not the one using the script regularly

## Bugs
- [x] Fixed with `^DI$` clashing with samples containing `DI` in them. As for the E sample issue, it's been a recurring issue for some time. I just always copy paste it from the complete data sheet and fix the formatting manually. Most recently it would've occurred on plate 561 with 063022 EDDI. It also happens with EDIM, but that's a school sample so we haven't had it in a while
- [ ] What could cause the LOD to be higher on one plate than another even if both plates have 0 droplets in NTCs? Seems like the lift station MPX plates LOD is off for some reason? _Kyle, 16-9-22_ `091222 Rice WWTP and Lift Station MPX`

## Features
- [x] Change LOD calculation with avg droplets in plate (harmonic mean..?) and update the positivity calling with 3 droplet threshold
- [ ] Add the default volume for new targets so they don't need to be accounted for in the ddPCR template volume sheet : `1-processing_functions.R` lines 72-77. Or make a default entry in the same sheet for folks to know about -- with a warning in the code?.
	- Where should the name entered match to- the template sheet or the ddPCR well
- [x] Incorporate solids mass and calculate copies/g : 
	- What is the formula? ` (copies/uL WW*50)/(pellet mass in g * dry mass %) to calculate the copies/g` : Read the pellet calculations sheet : Columns J and M
		- How do we deal with negative weights.. Get R to throw a warning + remove the values and name NA? 
		- [ ] _to do future:_ Need to implement error check for negative values for weight or fraction dry wight column
	- is this tube weight 3.382 g always constant? - can put it in the R code/the user inputs sheet
	- How is the `dry mass` being calculated, could have the raw data in the sheet and the calculations in R if it is constant..
	- Removing the prefix `p` : check with Lauren Stadler
- [ ] Save time and memory by reading only the relevant rows in the sample registry? _read last 300 or so rows? and then read older ones if needed/ look for the plate ID match. How can you read the header + few other rows below without reading twice and joining.._
- [x] Make the code robust to new columns in the sample registry -- _Loading specific columns only, tell users to add new columns only after a certain column.._

## Streamlining
- [ ] Collapse the spiking/vaccine processing into separate functions so the code is more consise
- [ ] Need more clarity and an easier way to see which variable is being used where - Tube_label vs Biobot_id etc. and need to remove redundancy.. _ex: Biobot ID column is generated with a formula in google sheet, can be done in R -- but will miss user evaluation for edge cases_
- [ ] Simplify all temporary column names in the script for easy understanding. _Sample_name -> Sample_week ; assay_variable -> ? ; 

# 9-4-22 - plotting tasks for Lauren

Here is the relevant metadata from Rebecca. `C:\Users\new\Box Sync\Stadler lab\Data\COVID-qPCR work\excel files\boxplots_for_LS_7-4-22`

I would like to plot the following in box and whisker plots (exclude “negatives” and only plot Rice data):

1. [x] Schools grouped by “Grade_Level” (from HISD_school_list_clean)
2. [x] Schools individually, ordered by enrollment on x-axis from largest to smallest
3. [x] Lift stations (all combined) vs WWTPs (all combined) —> from **March 2021** to present
4. [x]  Lift stations individually, ordered by population size on x axis from largest to smallest (pop size in “Rice_LS_All_Results”)
5. [x] WWTPs individually, ordered by population size on x axis from largest to smallest (pop size in “Rice_WWTP_All_Results”), March 2021 - present :: *Plot w abbreviated names too*
	- Show median; Vertical plot ; ALso filter by only N1_replicate1 
6. [x] Lift stations nested within WWTPs (so each LS individually, next to the corresponding WWTP that it feeds into. The information about which LS feeds into wsshich WWTP (Primary_WWTP) is in the “LS Facility Name Master List” spreadsheet.
	- Need SIMS BAYOU matched to both north and south
	- How do we plot this?
	- How to regex match 
7. [x] I'd like to see these last set of plots as a time series. So: concentration (y) vs. time (x) with each source WWTP and its feeding lift stations on the same plot. Only plot average for each date (can do just N2 or average all N1 and N2). Connect the averages for each date with a line for each site. If the value is below the LOD, put a circle or shade it differently.
	- How do you show multiple colours with independent legend for each facet?
	- Make independent plots using a for loop.. and save into a .html.. 

- [x] Attach abbreviations -- from Biobot google sheet?
- [x] CHange CLN to CLN2 in Lift station data ; and 13 others..

---

# Plots updated for Lauren: 22/6/22

1. Setup R-scripts for weekly analysis :: Friday morning (24/6/22)
2. Recreate plots, average all N1 and N2 measurements (N12, replicates) to reduce visual clutter
_Plot all samples independant of LOD, maybe indicate LOD location(s) if useful.._
- [x] For plot with elementary, middle, and highschool:  School data by dots--enrollment, bars--grade
	 - Get rid of Elem/Secondary  
	 - Order the grade levels : youngest to oldest  

For flu data, only use Influenza A results (Rep1_A and Rep2_A)  
> Wastewater Flu surveillance manhole report 05092022
- Create plot of flu data organized by grade level  
- Create boxplot of all SARS CoV 2 and flu data
- Plot SARS COV2 and FLU - two distributions side by side : only schools 

----------
Recreate plots, average all N1 and N2 measurements --
- What about the inconclusive cases when few replicates are below LOD or just 0s?
	- I feel like excluding 0s from the averages but letting the below LOD be ; since LOD is not stored and I'm feeling lazy



