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

  files = file_search('/Users/jama6159/Documents/Research/Data/SDO/EVE/EVE Merged/eve_lines_*.sav', COUNT = numFiles)
  
  FOR i = 0, numFiles - 1 DO BEGIN
    RESTORE, files(i)
    message, /info, "Restoring "+files(i)+"..."
    merged_datalines = (n_elements(merged_datalines) eq 0) ? datalines : [merged_datalines, datalines]
  ENDFOR
  
  datalines = merged_datalines
  save, datalines, file='/Users/jama6159/Documents/Research/Data/SDO/EVE/EVE Merged/JPMMerge.sav', /compress
  
END