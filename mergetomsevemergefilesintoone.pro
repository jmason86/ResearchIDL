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
; OPTIONAL INPUTS:r
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
;   None
;
; EXAMPLE:
;   MergeTomsEVEMergeFilesIntoOne
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013-02-28
;-
PRO MergeTomsEVEMergeFilesIntoOne

files = file_search('/Users/jama6159/Dropbox/Research/Woods_LASP/Data/EVE/eve_lines_*.sav', COUNT = numFiles)

FOR i = 0, numFiles - 1 DO BEGIN
  restore, files(i)
  message, /INFO, "Restoring " + files(i) + "..."
  merged_datalines = (n_elements(merged_datalines) EQ 0) ? datalines : [merged_datalines, datalines]
  merged_metalines = (n_elements(merged_metalines) EQ 0) ? metalines : [merged_metalines, metalines]
  merged_meta_all =  (n_elements(merged_metal_all) EQ 0) ? meta_all :  [merged_meta_all,  meta_all]
ENDFOR

datalines = merged_datalines
metalines = merged_metalines
meta_all = merged_meta_all
save, datalines, metalines, meta_all, filename = '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/EVE/JPMMerge.sav', /COMPRESS
  
END