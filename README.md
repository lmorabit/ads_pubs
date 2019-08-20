# ads_pubs
Take custom formatted ADS publication list and format it nicely for latex.

INSTRUCTIONS FOR USE:
-----------
Installing R

On linux systems, R can be installed by running

sudo apt-get install R

But it may already be present!  To check simply try to start R:

user@mycomputer$ R

and to quit:

> q()
-----------
BEFORE USING THE SCRIPT

1) conduct your author search on https://ui.adsabs.harvard.edu/
2) Select 'Export' > 'Other Formats'
3) Select 'Custom format'
4) Use the following custom format:
   %ZEncoding:latex %5.3A XXXtextit{%T}. %J, %V, %p-%P. XXXtextbf{%Y}, cit. %c %D  \n
      NB: do not change this format line or it will break the code
5) Click 'Download to File' and save the file somewhere.
      NB: File must exist in directory you run the script, or you should give an absolute path
6) In the script, change the 'myname' value to your last name, and change 'myfile' if you did not use the default downloaded file name. Select if you want to sort on year, citation or on chronological only.
7) Run directly from the command line (see below)
NOTE: you will probably need to add/update the journal names/journal abbreviation lists as necessary. Just make sure the index of the name and abbreviation are the same in each vector. 

-----------
Using the script

You do not need to be in an R environment. To run the script, at the command line simply type:

user@mycomputer$ Rscript format_pubs.r
