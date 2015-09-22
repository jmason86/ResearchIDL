;+
; NAME:
;   AIA4kTo1k
;
; PURPOSE:
;   Convert 4k x 4k AIA images to 1k x 1k 
;
; INPUTS:
;   aiaFiles [string array]: A list of the files to be converted
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   FITS files at 1k x 1k with '_1k' at the end of the filename
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/06/05
;-
PRO AIA4kTo1k, aiaFiles
TIC

FOR i = 0, n_elements(aiaFiles) - 1 DO BEGIN
  ; Read in fits file and change header to reflect new size
  read_sdo, aiaFiles[i], header, image
  header.NAXIS1 = 1024
  header.NAXIS2 = 1024
  
  ; Output the regrided size
  outputFilename = strmid(aiaFiles[i], 0, strpos(aiaFiles[i], '.', /REVERSE_SEARCH)) + '_1k.fits'
  mwritefits, header, congrid(image, 1024, 1024), outfile = outputFilename

  message, /INFO, strtrim(float(i) / n_elements(aiaFiles) * 100., 2) + '% complete'
ENDFOR

print, '-=Program normal completion in ' + strtrim(round(TOC()), 2) + ' seconds=-'
END