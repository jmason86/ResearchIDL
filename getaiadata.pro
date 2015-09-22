;+
; NAME: 
;   GetAIAData
;   
; PURPOSE:
;   Program to download SDO/AIA data from the Virtual Solar Observatory (VSO, http://sdac.virtualsolar.org/cgi/search)
;   
; INPUTS: 
;   None
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   DO_PSF: If set, will do point spread function deconvolution of all images for relevant wavelengths. NOTE: Must be performed on level 1.0 images. 
;   
; OUTPUTS:
;   AIA FITS files
;   
; OPTIONAL OUTPUTS: 
;   None
;   
; RESTRICTIONS:
;   Requires manual editing of code
;   
; EXAMPLE: 
;   GetAIAData
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2012/3/20
;     2013/12/03: JPM: Added DO_PSF keyword
;-
PRO GetAIAData, DO_PSF = DO_PSF, SKIP_DOWNLOAD = SKIP_DOWNLOAD

; Make edits here
series = 'aia__lev1' ; 'aia.lev1_euv_12s' ; aia__lev1 ; aia.lev1
wavelengths = ['171', '193', '304']
startTime = '2011-mar-3 02:00'
endTime =   '2011-mar-3 07:00'
cadence = 60 ; Seconds
resolution = 4096 ; One-dimensional resolution e.g., 1024 means 1024 x 1024, 4096 means 4096 x 4096
saveto = '/archvol1/mason/ResearchData/SDO/AIA/FullDisk/2011082_23MAR_0217_M1.4/'
preppedto = 'prepped/'
downloadSite = 'SAO' ; Other options: nso, sdac, sao
; End edits                                         `                                                 

spawn, 'mkdir ' + saveto
spawn, 'mkdir ' + saveto + preppedto
CD, saveto

FOR wavelengthIndex = 0 , n_elements(wavelengths) - 1 DO BEGIN

  wavelength = wavelengths(wavelengthIndex)
  
  IF ~keyword_set(SKIP_DOWNLOAD) THEN BEGIN
  
    ; Search data at the VSO
    list = vso_search(startTime, endTime, WAVE = wavelength, INSTRUMENT = 'aia', SAMPLE = cadence, PIXELS = resolution, SITE = downloadSite, /RICE)
    
    ; Sometimes the VSO search won't find the series you're looking for so skip this wavelength
    IF n_elements(list) EQ 1 THEN BEGIN
      print, 'VSO_SEARCH found no match for ' + wavelength
      CONTINUE
    ENDIF
    select = where(strcmp(series, list.FILEID, strlen(series)) EQ 1)
    IF select EQ [-1] THEN BEGIN
      print, 'VSO_SEARCH found no match for ' + wavelength
      CONTINUE
    ENDIF
    
    ; Download data from the VSO
    status = vso_get(list, OUTDIR = saveto, /FORCE, /RICE, SITE = downloadSite)
     
    ; Files seems to come back undefined often, so check for it to avoid crashing
    files = file_search(saveto + '*.fits')
    checkvar, files, ''
    IF files EQ [''] THEN BEGIN
      print, 'VSO_GET failed to return filenames'
      CONTINUE
    ENDIF
  
  ENDIF ELSE files = file_search(saveto + '*' + wavelength +  '*.fits') ; SKIP_DOWNLAOD
  
  ; Do point spread function deconvolution if keyword set
  ; NOTE: Must be performed on level 1.0 images, not 1.5
  IF keyword_set(DO_PSF) THEN BEGIN 
    filesOfWavelength = file_search(saveto + '*' + wavelength + '*.fits')
    QuickBatchAIAPSFDeconvolution, filesOfWavelength, wavelength
    
    ; Point files variable to new psf deconvolved files
    files = file_search(saveto + '*' + wavelength + '*_psf.fits')
  ENDIF
    
  ; Make level 1.5 and correct AIA images for expsoure time
  FOR filesIndex = 0, n_elements(files) - 1 DO BEGIN
    make_aia, files(filesIndex), header, AIAcorrected
    parsedFilename = ParsePathAndFilename(files(filesIndex))
    outputFilename = parsedFilename.Path + preppedto + 'AIA' + strmid(parsedFilename.Filename, 14, 17) + strmid(parsedFilename.Filename, 9, 3) + 'A.fits'
    mwritefits, header, AIAcorrected, outfile = outputFilename
  ENDFOR ; files loop
  
ENDFOR ; wavelength loop
END