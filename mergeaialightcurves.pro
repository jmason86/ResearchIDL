;+
; NAME: 
;   MergeAIALightCurves
;   
; PURPOSE:
;   Combine AIA light curves that had to be generated separately in order to avoid memory overruns. 
;   
; INPUTS: 
;   event: (hardcoded) Set to either 1 or 2 to select 2011046_15FEB_0156_X2.2 or 2011221_09AUG_0805_X6.9, respectively. 
;   useIDLSave: (hardcoded) If the saveset already exists from a prior run, skip computing the spatially integrated intensities
;   ngrid: (hardcoded) Set to the number of boxes desired in each direction e.g. ngrid = 10 results in a 10x10 grid
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   None
;   
; OUTPUTS:
;   Plot of full disk integrated intensity at various wavelengths
;   Plot of integrated intensity in each grid as a ngrid x ngrid multiplot, one version with auto-scaled y-axis and another with constant y-axis
;   
; OPTIONAL OUTPUTS: 
;   None
;   
; RESTRICTIONS:
;   Requires: 
;     
; EXAMPLE: 
;   ProduceAIALightCurves2
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2013/1/23
;-
PRO MergeAIALightCurves

; Hardcode options for program execution
event = 1
numberOfChunks = 3

; Setup
CASE event OF
  1: BEGIN
    flareID = '2011046_15FEB_0156_X2.2'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011046_15FEB_0156_X2.2/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011046_15FEB_0156_X2.2/'
    message, /INFO, 'Running on event 2011046_15FEB_0156_X2.2'
  END
  2: BEGIN
    flareID = '2011221_09AUG_0805_X6.9'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011221_09AUG_0805_X6.9/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011221_09AUG_0805_X6.9/'
    message, /INFO, 'Running on event: 2011221_09AUG_0805_X6.9' 
  END
  3: BEGIN
    flareID = '2011216_04AUG_0357_M9.3'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011216_04AUG_0357_M9.3/'
    message, /INFO, 'Running on event: 2011216_04AUG_0357_M9.3' 
  END  
ENDCASE

; Restore the first chunk and change the names of the variables so they won't be overwritten on the next restore
RESTORE, saveloc + 'AIAIntegratedIntensities1.sav'
help, NAMES = 'AIA*', OUTPUT = names
relevantIndices = where(strmid(names, 0, 1) EQ 'A')
names = names[relevantIndices]
FOR i = 0, n_elements(names) - 1 DO void = execute(names[i] + '_first' + ' = ' + names[i])
tOBSJD_first = tOBSJD

; Restore second chunk
RESTORE, saveloc + 'AIAIntegratedIntensities2.sav'

CASE numberOfChunks OF 
  2: BEGIN
    ; Combine the chunks
    arrayLength = n_elements(AIAINTEGRATEDINTENSITY094)
    FOR i = 0, n_elements(names) - 1 DO BEGIN 
      IF strmid(names[i], 2, 3, /REVERSE_OFFSET) EQ 'BOX' THEN $
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/2-1, *, *], ' + names[i] + '[arrayLength/2:-1, *, *]]') $
      ELSE $ 
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/2-1], ' + names[i] + '[arrayLength/2:arrayLength-1]]')
    ENDFOR
    tOBSJD = [tOBSJD_first[0:arrayLength/2-1], tOBSJD[arrayLength/2:arrayLength-1]]
  END
  3: BEGIN
    ; Change the names of the variables from chunk 2 to avoid overwriting
    FOR i = 0, n_elements(names) - 1 DO void = execute(names[i] + '_second' + ' = ' + names[i])
    tOBSJD_second = tOBSJD
    
    ; Restore the third chunk
    RESTORE, saveloc + 'AIAIntegratedIntensities3.sav'
    
    ; Combine the chunks
    arrayLength = n_elements(AIAINTEGRATEDINTENSITY094)
    FOR i = 0, n_elements(names) - 1 DO BEGIN 
      IF strmid(names[i], 2, 3, /REVERSE_OFFSET) EQ 'BOX' THEN $
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/3-1, *, *], ' + names[i] + '_second[arrayLength/3:2*arrayLength/3-1, *, *], ' + names[i] + '[2*arrayLength/3:-1, *, *]]') $
      ELSE $ 
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/3-1], ' + names[i] + '_second[arrayLength/3:2*arrayLength/3-1], ' + names[i] + '[2*arrayLength/3:-1]]')
    ENDFOR
    tOBSJD = [tOBSJD_first[0:arrayLength/3-1], tOBSJD_second[arrayLength/3:2*arrayLength/3-1], tOBSJD[2*arrayLength/3-1:-1]]
  END
  4: BEGIN
    ; Change the names of the variables from chunk 4 to avoid overwriting
    FOR i = 0, n_elements(names) - 1 DO void = execute(names[i] + '_third' + ' = ' + names[i])
    tOBSJD_third = tOBSJD
    
    ; Restore the third chunk
    RESTORE, saveloc + 'AIAIntegratedIntensities4.sav'
    
    ; Combine the chunks
    arrayLength = n_elements(AIAINTEGRATEDINTENSITY094)
    FOR i = 0, n_elements(names) - 1 DO BEGIN 
      IF strmid(names[i], 2, 3, /REVERSE_OFFSET) EQ 'BOX' THEN $
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/4-1, *, *], ' + names[i] + '_second[arrayLength/4:2*arrayLength/4-1, *, *], ' + names[i] + '_third[2*arrayLength/4:3*arrayLength/4-1, *, *], '+ names[i] + '[3*arrayLength/4:-1, *, *]]') $
      ELSE $ 
        void = execute(names[i] + ' = ' + '[' + names[i] + '_first[0:arrayLength/4-1], ' + names[i] + '_second[arrayLength/4:2*arrayLength/4-1], ' + names[i] + '_third[2*arrayLength/4:3*arrayLength/4-1], ' + names[i] + '[3*arrayLength/4:-1]]')
    ENDFOR
    tOBSJD = [tOBSJD_first[0:arrayLength/4-1], tOBSJD_second[arrayLength/4:2*arrayLength/4-1], tOBSJD_third[2*arrayLength/4:3*arrayLength/4-1], tOBSJD[3*arrayLength/4:-1]]
  END
ENDCASE

; Write new file
SAVE, tOBSJD, AIAIntegratedIntensity171, AIAIntegratedIntensity211, AIAIntegratedIntensity094, AIAIntegratedIntensity193, AIAIntegratedIntensity131, AIAIntegratedIntensity171Box, AIAIntegratedIntensity211Box, AIAIntegratedIntensity094Box, AIAIntegratedIntensity193Box, AIAIntegratedIntensity131Box, AIAIntegratedIntensity335Box, FILENAME = saveloc + 'AIAIntegratedIntensities.sav'
;SAVE, tOBSJD, AIAIntegratedIntensity171, AIAIntegratedIntensity211, AIAIntegratedIntensity094, AIAIntegratedIntensity193, AIAIntegratedIntensity131, AIAIntegratedIntensity335, AIAIntegratedIntensity171Box, AIAIntegratedIntensity211Box, AIAIntegratedIntensity094Box, AIAIntegratedIntensity193Box, AIAIntegratedIntensity131Box, AIAIntegratedIntensity335Box, FILENAME = saveloc + 'AIAIntegratedIntensities2.sav'

print, '-=File merger complete=-'
END