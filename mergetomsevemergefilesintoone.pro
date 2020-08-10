;+
; NAME:
;   MergeTomsEVEMergeFilesIntoOne
;
; PURPOSE:
;   Tom periodically merges EVE data into single files, but not into a single one for the whole mission.
;   This code merges his merge files into a single save set. 
;
; INPUTS:
;   No formal input, but need to specify the directory where Tom's savesets are in the first line of hardcode
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   IDL Saveset with EVE lines merged
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Uses JPMsystime() and JPMPrintNumber(), but just for status so can comment out if undesired
;
; EXAMPLE:
;   MergeTomsEVEMergeFilesIntoOne
;
; MODIFICATION HISTORY:
;   2013-02-28: James Paul Mason: Wrote script
;   2016-09-13: James Paul Mason: Now ignoring metalines variable stored in Tom's savesets because it is redundant 
;                                 with what is already inside meta_all. Also renamed meta_all to eveMeta and 
;                                 datalines to eveLines. Also made file path use getenv('username'). 
;-
PRO MergeTomsEVEMergeFilesIntoOne
tic

; Find the files to merge
files = file_search('/Users/'+ getenv('username') + '/Dropbox/Research/Data/EVE/eve_lines_*_005.sav', COUNT = numFiles)

; Loop through the files and concatenate the emission line data and meta data
FOR i = 0, numFiles - 1 DO BEGIN
  restore, files(i)
  message, /INFO, JPMsystime() + ' Restored ' + files(i)
  merged_datalines = (n_elements(merged_datalines) EQ 0) ? datalines : [merged_datalines, datalines]
  merged_meta_all =  (n_elements(merged_metal_all) EQ 0) ? meta_all :  [merged_meta_all,  meta_all]
ENDFOR

; Rename the emission line and meta data and save
eveLines = temporary(merged_datalines)
eveMeta = temporary(merged_meta_all)
save, eveLines, eveMeta, FILENAME = '/Users/' + getenv('username') + '/Dropbox/Research/Data/EVE/JPMMerge.sav', /COMPRESS

message, /INFO, JPMsystime() + ' Merge and save complete in ' + JPMPrintNumber(toc(), /NO_DECIMALS) + ' seconds'
END