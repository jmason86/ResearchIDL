;+
; NAME:
;   aiaDerotation
;
; PURPOSE:
;   Remove rotation from a series of AIA images. 
;
; INPUTS:
;   aiaFiles [string]: A list of the files to be converted (should be level 1.5 and corrected for exposure time (fits/exposuretime)
;   
; OPTIONAL INPUTS:
;   rotationTime [string]: Time that all images will be derotated to. If not specified, the first AIA frame will be used. 
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   FITS aiaFiles that have been derotated, have '_rot' at the end of the filename 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   Written by:
;     Barabara Thompson
;     2013/?/?
;   James Mason 2013/6/5: Added header, generalized code
;-
PRO aiaDerotation, aiaFiles, rotationTime
TIC

IF ~keyword_set(rotationTime) THEN BEGIN
  header = headfits(aiaFiles[0]) & header = head2stc(header)
  rotationTime = header.T_OBS
ENDIF

FOR i = 0, n_elements(aiaFiles)-1 DO BEGIN
  header = headfits(aiaFiles[i]) & header = head2stc(header)
  levelNumber = header.LVL_NUM
  IF levelNumber EQ 1.0 THEN $
    make_aia, aiaFiles[i], header, image ELSE $
    read_sdo, aiaFiles[i], header, image 
    
  index2map, header, image, aiamap
  rotmap = aia_rotmap(aiamap, rotationTime, /keeplimb, outh = header, outn = aiaFiles[i])
  
  message, /INFO, strtrim(float(i+1) / n_elements(aiaFiles) * 100., 2) + '% complete'
ENDFOR

print, '-=AIA derotation normal completion in ' + strtrim(round(TOC()), 2) + ' seconds=-'
END