;+
; NAME:
;   QuickBatchMakeAIA
;
; PURPOSE:
;   Run make_aia in batch to bring level 1.0 AIA fits files to level 1.5, plus do exposure fix
;
; INPUTS:
;   aiaFiles [string array]: A list of the files to be converted if necessary
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Level 1.5 AIA fits files in /prepped directory (relative to the path of aiaFiles)
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;   aiaFiles = file_search('/Users/jama6159/Desktop/AIA/2010219_07AUG_1824_M1.0_PSF/*_psf.fits')
;   QuickBatchMakeAIA, aiaFiles
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/12/30
;-
PRO QuickBatchMakeAIA, aiaFiles

preppedto = 'prepped/'

FOR filesIndex = 0, n_elements(aiaFiles) - 1 DO BEGIN
  make_aia, aiaFiles(filesIndex), header, AIAcorrected
  parsedFilename = ParsePathAndFilename(aiaFiles(filesIndex))
  outputFilename = parsedFilename.Path + preppedto + 'AIA' + strmid(parsedFilename.Filename, 14, 17) + strmid(parsedFilename.Filename, 9, 3) + 'A.fits'
  mwritefits, header, AIAcorrected, outfile = outputFilename
  message, /INFO, strtrim(float(filesIndex + 1) / n_elements(aiaFiles) * 100., 2) + '% complete.'
ENDFOR ; aiaFiles loop

END