# Improvements - long term
1. Do the y-axis renaming thing (like plate_reader plotting) to take col names to meaningful long names using a lookup list -- to prevent errors from manually passing the name in //but will make it less usable outside the pipeline?


# 16-11-21 - covid tasks

_(in branch: grab_composite_assignment)_ 1. Add sample registry new column : grab vs composite
(done) 2. Make the code plot stuff -- focus on pMMoV data -- + Kyle

_(optional)_ --
(done) 3. Clean up the github to ignore csv / xlsx files
(done) 4. Update the code to output in specific places .. I'm tired of maintaining this, let's ignore this since I'm not the one using the script regularly

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


# Plots updated for Lauren: 22/6/22

1. Setup R-scripts for weekly analysis :: Friday morning (24/6/22)
2. Plot changes: Avg N12, replicates

--- Something about imputing data below the LOD? -- someone is giving imputed data, which we will work on later with the same pipeline/plots
-   School data by dots--enrollment, bars--grade (remove "elementary/secondary" etc. ; order the grade levels)
-   Plot the school by grade --bars ; for influenza data too : Wastewa Flu surveillance manhole report 05092022 -- for flu only use infA (Rep1,2_A) since B's are 0
-   Plot SARS COV2 and FLU on the same data : in two distributions