;+
; NAME:
;   QuickBatchAIAPSFDeconvolution
;
; PURPOSE:
;   Do PSF deconvolution on a bunch of AIA images
;
; INPUTS:
;   aiaFiles [string array]: A list of the files to be converted if necessary
;   wavelength [string]: The wavelength of the aiaFiles. Only one wavelength at a time.
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   FITS files that have been deconvolved. Will attach _psf to the end of the filename.
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;   aiaFiles = file_search('/Users/jama6159/Desktop/AIA/2010219_07AUG_1824_M1.0_PSF/aia.lev1.304A*lev1.fits')
;   wavelength = '304'
;   QuickBatchAIAPSFDeconvolution, aiaFiles, wavelength
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/12/31
;-
PRO QuickBatchAIAPSFDeconvolution, aiaFiles, wavelength

;restore, '/archvol1/mason/ResearchData/SDO/AIA/PSFs/aiapsf_' + wavelength + '.save'
restore, '/Volumes/Archer4TB/PSFs/aiapsf_' + wavelength + '.save'
FOR filesIndex = 0, n_elements(aiaFiles) - 1 DO BEGIN
  read_sdo, aiaFiles(filesIndex), header, image
  deconvolvedImage = aia_deconvolve_richardsonlucy(float(image), float(psf), NITER = 8)
  
  ; Write out file
  outputFilename = strmid(aiaFiles(filesIndex), 0, strlen(aiaFiles(filesIndex)) - 5) + '_psf.fits'
  mwritefits, header, deconvolvedImage, outfile = outputFilename
  message, /INFO, 'Deconvolved image ' + strtrim(string(filesIndex + 1), 2) + ' of ' + strtrim(string(n_elements(aiaFiles)), 2)
ENDFOR ; files loop
  
END


