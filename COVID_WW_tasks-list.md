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
5. [ ] WWTPs individually, ordered by population size on x axis from largest to smallest (pop size in “Rice_WWTP_All_Results”), March 2021 - present :: *Plot w abbreviated names too*
	- Show median; Vertical plot ; ALso filter by only N1_replicate1 
6. [ ] Lift stations nested within WWTPs (so each LS individually, next to the corresponding WWTP that it feeds into. The information about which LS feeds into which WWTP (Primary_WWTP) is in the “LS Facility Name Master List” spreadsheet.