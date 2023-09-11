# specify temperature synthesis pathways to local
# author(s): ctw, caitlin.t.white@colorado.edu
# project: nwt temperature synthesis
# date create: 2023-09-11 (summer 2023 GRA, PI = Chris Ray)


# script purpose:
# some data files used for NWT temperature synthesis are not published online. examples:
# > currently unpublished datasets (with plans to publish later, e.g., pika datasets) 
# > intermediate data products that are convenient to save to avoid recreating each time but theh are not meant to publish and would be too large to store in github repo

# for ease in multiple users running code, this script will store local pathways to data files
# until better method determined, follow pathway template and add your local info after last entry
# can note date last modified in your section so others can see who uses script and when they last used it
# maybe add contact email too if someone has questions about any contributions you made to project

# important notes:
# this script creates a list of lists. each element on the list contains path info for a different user
# this is a living file, so any are free to edit. just be sure how your changes affect .R or .Rmd dependencies so don't break code for others :]

# local file structure needs to be:
# main level: [your name for temperature synthesis project]
## sub level (3 folders): [1. unpublished pika data folder], [2. unpublished other temp data folder], [3. output]
### 1. unpublished pika data sublevel: [however many individual folders for pika data]
### 2. unpublished other temperature data sublevel: [however many folders]
### 3. output sublevel (2 folders): [1. markdowns], [2. figs]


## TEMPLATE TO FOLLOW ##
# name: [your name]
# contact: [contact email (other other way for others to contact you)]
# datpath_pika <- "" # your local computer/individual path to where static pika data folder for temperature synthesis scripts live
# datpath_othertemp <- "" # your local computer/individual path to where static pika data folder for temperature synthesis scripts live
# outpath <- "" # your local computer/individual path for where to knit R markdowns (output can be sizeable and is dynamic, i.e., will update each time it's executed, so better to not store on repo)
# last mod: [date your paths were created or last modified if amended]
# notes: [any notes to other users (optional)]


# -- PATHWAYS ------
# caitlin white
# caitlin.t.white@colorado.edu
# datpath <- "/Users/scarlet/Documents/nwt_lter/temp_synth/unpublished_data"
# outpath <- "/Users/scarlet/Documents/nwt_lter/temp_synth/output"
# last mod: 2023-09-11