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
;   wavelengths [strarr]: An array of string for the wavelegnth(s) to download. Default is ['171', '193', '304']. 
;   startTimeIso [string]: An ISO format time for the start range of interest. Default is '2011-03-03T02:00:00Z'. 
;   stopTimeIso [string]:  An ISO format time for the stop range of interest. Default is 1 day beyond the start time. 
;   cadenceSeconds [integer]: The cadence of desired observations in seconds. Default is 3600. 
;   resolution [integer]: The resolution of the images to download. Options are 1024 or 4096. Default is 4096. 
;   saveTo [string]: The path to the location to save the output. Default is '/Users/' + getenv('username') + '/Dropbox/Research/Data/AIA/'
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
;   2012-03-20: James Paul Mason: Wrote script
;   2013-12-03: James Paul Mason: Added DO_PSF keyword
;   2017-02-08: James Paul Mason: Changed from hard code to optional inputs for what to download and where
;-
PRO GetAIAData, wavelengths = wavelengths, startTimeIso = startTimeIso, stopTimeIso = stopTimeIso, cadenceSeconds = cadenceSeconds, resolution = resolution, saveto = saveto, $
                DO_PSF = DO_PSF, SKIP_DOWNLOAD = SKIP_DOWNLOAD

; Defaults
IF wavelengths EQ !NULL THEN wavelengths = ['171', '193', '304']
IF startTimeIso EQ !NULL THEN startTimeIso = '2011-03-03T02:00:00Z'
startTimeIsoTemp = startTimeIso
IF stopTimeIso EQ !NULL THEN stopTimeIso = JPMjd2iso(JPMiso2jd(startTimeIsoTemp) + 1.)
IF cadenceSeconds EQ !NULL THEN cadenceSeconds = 3600
IF resolution EQ !NULL THEN resolution = 4096
IF saveTo EQ !NULL THEN saveTo = '/Users/' + getenv('username') + '/Dropbox/Research/Data/AIA/'

; Fixed values
series = 'aia__lev1' ; 'aia.lev1_euv_12s' ; aia__lev1 ; aia.lev1
preppedto = 'prepped/'
downloadSite = 'SAO' ; Other options: nso, sdac, sao                                       `                                                 

cd, saveto

FOR wavelengthIndex = 0 , n_elements(wavelengths) - 1 DO BEGIN

  wavelength = wavelengths(wavelengthIndex)
  
  IF ~keyword_set(SKIP_DOWNLOAD) THEN BEGIN
  
    ; Search data at the VSO
    list = vso_search(strmid(startTimeIso, 0, 10), strmid(stopTimeIso, 0, 10), WAVE = wavelength, INSTRUMENT = 'aia', SAMPLE = cadence, $
                        PIXELS = resolution, SITE = downloadSite, /RICE)
    
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
    status = vso_get(list[0], OUTDIR = saveto, /FORCE, /RICE, SITE = downloadSite)
     
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