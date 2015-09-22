;+
; NAME:
;   AnalyzeAIADimmingRegions
;
; PURPOSE:
;
;
; INPUTS:
;   eventID [string]:        A unique ID for the event being studied e.g. '2011216_04AUG_0357_M9.3' (yyyydoy_ddMON_hhmm_flareclass)
;   aiaFiles [string array]: A list of the files to be converted if necessary (should be level 1.5 and corrected for exposure time (fits/exposuretime)). 
;                            The first file in this list will be used to obtain the directory that should contain all wavelengths. The code will separate the wavelengths automatically. 
;   
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;   DO_DEROTATION:       If the files to be loaded haven't been derotated but you want them to be, set this keyword
;   DO_IMAGE_4K_TO_1K:   If the AIA images are still 4k x 4k, set this keyword to convert them to 1k x 1k
;   GENERATE_SAVE_FILES: If the save files for the images have not been made yet, set this keyword to generate them. The save files will be loaded by default if this keyword is not set. 
;   DEFINE_REGIONS:      Set if you want to define new regions of interest (e.g. active region, dimming)
;   COMPUTE_SECTOR_MAPS: Set if sector maps have not yet been computed, else restores from saveset
;   AUTOFLARE:           If set, an ellipse around the user selected "event point" (i.e. flare point) is created. If not set, the user will draw the region of interest around the flaring area. 
;   SEPARATE_ARS:        Set to treat each identified active region (AR) separately in the light curves and ROIs
;   MAKE_MOVIE:          Create a movie: timeseries of DimmingByFeature.png, AIA image through time and vertical bar tracking time through light curves
;   
; OUTPUTS:
;   DimmingByFeature.png: A plot that shows the regions of interest on the Sun (left) and light curves for those regions and other relevant light curves for 171Å, 193Å, and 304Å (right)
; 
; OPTIONAL OUTPUTS:
;   DimmingByFeature.mp4: IF /MAKE_MOVIE then a timeseries of DimmingByFeature.png
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLES:
;   aiaFiles = file_search('/archvol1/mason/ResearchData/SDO/AIA/FullDisk/2010219_07AUG_1824_M1.0/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2010219_07AUG_1824_M1.0', aiaFiles, /SEPARATE_ARS
;   aiaFiles = file_search('/archvol1/mason/ResearchData/SDO/AIA/FullDisk/2011082_23MAR_0217_M1.4/prepped/rot/1k/*.fits')
;   AnalyzeAIADimmingRegions, '2011082_23MAR_0217_M1.4', aiaFiles, /SEPARATE_ARS
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011223_11AUG_1020_/prepped/*.fits')
;   AnalyzeAIADimmingRegions, '2011223_11AUG_1020_', aiaFiles, /GENERATE_SAVE_FILES
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011055_24FEB_0740_/prepped/*.fits')
;   AnalyzeAIADimmingRegions, '2011055_24FEB_0740_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K, /GENERATE_SAVE_FILES
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011041_10FEB_0740_/prepped/*_rot.fits')
;   AnalyzeAIADimmingRegions, '2011041_10FEB_0740_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K, /GENERATE_SAVE_FILES
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011042_11FEB_0746_/prepped/*.fits')
;   AnalyzeAIADimmingRegions, '2011042_11FEB_0746_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K, /GENERATE_SAVE_FILES
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011043_12FEB_0605_/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2011043_12FEB_0605_', aiaFiles, /DEFINE_REGIONS, /COMPUTE_SECTOR_MAPS, /SEPARATE_ARS
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011044_13FEB_0355_/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2011044_13FEB_0355_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K
;   aiaFiles = file_search('/Volumes/Archer4TB/ResearchData/SDO/AIA/FullDisk/2011044_13FEB_1600_/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2011044_13FEB_1600_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K
;   aiaFiles = file_search('/Volumes/archvol1/mason/ResearchData/SDO/AIA/FullDisk/2011045_14FEB_0705_/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2011045_14FEB_0705_', aiaFiles, /DEFINE_REGIONS, /COMPUTE_SECTOR_MAPS, /SEPARATE_ARS
;   aiaFiles = file_search('/Volumes/archvol1/mason/ResearchData/SDO/AIA/FullDisk/2011045_14FEB_1729_AND_2011045_14FEB_1930_/prepped/*rot.fits')
;   AnalyzeAIADimmingRegions, '2011045_14FEB_1729_AND_2011045_14FEB_1930_', aiaFiles, /DO_DEROTATION, /DO_IMAGE_4K_TO_1K
;   
;   
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/06/05
;-
PRO AnalyzeAIADimmingRegions, eventID, aiaFiles, $
                              DO_DEROTATION = do_derotation, DO_IMAGE_4K_TO_1K = do_image_4k_to_1k, GENERATE_SAVE_FILES = generate_save_files, $
                              DEFINE_REGIONS = define_regions, COMPUTE_SECTOR_MAPS = compute_sector_maps, AUTOFLARE = autoflare, SEPARATE_ARS = separate_ars, $
                              MAKE_MOVIE = make_movie
analyzeAIADimmingRegionsTIC = TIC('AnalyzeAIADimmingRegions')

; Defaults
IF ~keyword_set(eventID) THEN eventID = systim()

; Setup
;saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/' + eventID + '/'
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/' + eventID + '/'
imageSaveloc = ParsePathAndFilename(aiaFiles[0]) & imageSaveloc = imageSaveloc.Path
spawn, 'mkdir ' + str_replace(saveloc, ' ', '\ ') ; Unix doesn't like spaces in path strings

;GOTO, SKIPTOPLOTTING ; I know, I know. 

; If keyword is set, convert 4k x 4k images to 1k x 1k
IF keyword_set(do_image_4k_to_1k) THEN BEGIN
  AIA4kTo1k, aiaFiles
  parser = ParsePathAndFilename(aiaFiles[0])
  searchTerm = parser.Path + '*_1k.fits'
  aiaFiles = file_search(searchTerm)
ENDIF

; If keyword is set, do the derotation of AIA images
IF keyword_set(do_derotation) THEN BEGIN
  AIADerotation, aiaFiles
  parser = ParsePathAndFilename(aiaFiles[0])
  searchTerm = parser.Path + '*_rot.fits'
  aiaFiles = file_search(searchTerm)
ENDIF

; -= READ IN EACH WAVELENGTH TO AN ARRAY OR RESTORE FROM A SAVESET =- ;
IF keyword_set(generate_save_files) THEN BEGIN
  parser = ParsePathAndFilename(aiaFiles[0])
  aia171Files = file_search(parser.Path + '*171A_rot_1k*', COUNT = count171)
  aia193Files = file_search(parser.Path + '*193A_rot_1k*', COUNT = count193)
  aia304Files = file_search(parser.Path + '*304A_rot_1k*', COUNT = count304)
  aia211Files = file_search(parser.Path + '*211A_1k_rot*', COUNT = count211)
  
  ; Prepare the 3D arrays
  IF count171 GT 0 THEN aia171Array = fltarr(1024, 1024, count171) ELSE BEGIN aia171Array = fltarr(1, 1, 2) & all171Headers = '' & ENDELSE
  IF count193 GT 0 THEN aia193Array = fltarr(1024, 1024, count193) ELSE BEGIN aia193Array = fltarr(1, 1, 2) & all193Headers = '' & ENDELSE
  IF count304 GT 0 THEN aia304Array = fltarr(1024, 1024, count304) ELSE BEGIN aia304Array = fltarr(1, 1, 2) & all304Headers = '' & ENDELSE
  IF count211 GT 0 THEN aia211Array = fltarr(1024, 1024, count211) ELSE BEGIN aia211Array = fltarr(1, 1, 2) & all211Headers = '' & ENDELSE
  
  ; Read in to the 3D arrays each of the wavelengths and headers 
  FOR i = 0, count171 - 1 DO BEGIN
    read_sdo, aia171Files[i], header171, temporaryImage
    aia171Array[*, *, i] = temporary(temporaryImage)
    IF i EQ 0 THEN all171Headers = header171 ELSE all171Headers = [all171Headers, header171]
    message, /INFO, '171Å fits reading ' + strtrim(float(i + 1) / count171 * 100., 2) + '% complete'
  ENDFOR
  FOR i = 0, count193 - 1 DO BEGIN
    read_sdo, aia193Files[i], header193, temporaryImage
    aia193Array[*, *, i] = temporary(temporaryImage)   
    IF i EQ 0 THEN all193Headers = header193 ELSE all193Headers = [all193Headers, header193]
    message, /INFO, '193Å fits reading ' + strtrim(float(i + 1) / count193 * 100., 2) + '% complete'
  ENDFOR
  FOR i = 0, count304 - 1 DO BEGIN
    read_sdo, aia304Files[i], header304, temporaryImage
    aia304Array[*, *, i] = temporary(temporaryImage)
    IF i EQ 0 THEN all304Headers = header304 ELSE all304Headers = [all304Headers, header304]
    message, /INFO, '304Å fits reading ' + strtrim(float(i + 1) / count304 * 100., 2) + '% complete.'
  ENDFOR
  FOR i = 0, count211 - 1 DO BEGIN
    read_sdo, aia211Files[i], header211, temporaryImage
    aia211Array[*, *, i] = temporary(temporaryImage)
    IF i EQ 0 THEN all211Headers = header211 ELSE all211Headers = [all211Headers, header211]
    message, /INFO, '211Å fits reading ' + strtrim(float(i + 1) / count211 * 100., 2) + '% complete.'
  ENDFOR
  
  ; Save to saveset to avoid unncessary slow fits reading
  save, aia171Array, all171Headers, FILENAME = imageSaveloc + 'aiaImages171.sav'
  save, aia193Array, all193Headers, FILENAME = imageSaveloc + 'aiaImages193.sav'
  save, aia304Array, all304Headers, FILENAME = imageSaveloc + 'aiaImages304.sav'
  save, aia211Array, all211Headers, FILENAME = imageSaveloc + 'aiaImages211.sav'
ENDIF $ ; GENERATE_SAVE_FILES set
ELSE BEGIN
  message, /INFO, 'Restoring AIA images'
  restore, imageSaveloc + 'aiaImages171.sav'
  restore, imageSaveloc + 'aiaImages193.sav'
  restore, imageSaveloc + 'aiaImages304.sav'
  restore, imageSaveloc + 'aiaImages211.sav'
ENDELSE
; -= END READ IN EACH WAVELENGTH =- ;

; -= MAKE REFERENCE AND DIMMING IMAGES =- ;
IF keyword_set(define_regions) THEN BEGIN
  !p.multi=[0,3,2,0,1]
  
  ; 193 Å
  IF total(aia193Array) GT 0 THEN BEGIN
    referenceImage193 = total(aia193Array[*, *, 0:4], 3) / 5 
    dimmingImage193 = smooth(referenceImage193 - min(aia193Array, DIMENSION = 3), 2)
    dimmingImage193 = 255 - bytscl(alog10(dimmingImage193 > 0) > 0 < 2.7)
    aia_lct, red, green, blue, WAVELNTH = '193', /load
    plot_image, bytscl(alog10(referenceImage193 > 0) > 1.25 < 3)
    loadct, 0
    plot_image, dimmingImage193
  ENDIF ; AIA 193å data exists
  
  ; 171 Å
  IF total(aia171Array) GT 0 THEN BEGIN
    referenceImage171 = total(aia171Array[*, *, 0:4], 3) / 5 
    dimmingImage171 = smooth(referenceImage171 - min(aia171Array, DIMENSION = 3), 2)
    dimmingImage171 = 255 - bytscl(alog10(dimmingImage171 > 0) > 0 < 2.7)
    aia_lct, red, green, blue, WAVELNTH = '171', /load
    plot_image, bytscl(alog10(referenceImage171 > 0) > 1.25 < 3)
    loadct, 0
    plot_image, dimmingImage171
  ENDIF ; AIA 171Å data exists
  
  ; 304 Å
  IF total(aia304Array) GT 0 THEN BEGIN
    referenceImage304 = total(aia304Array[*, *, 0:1], 3) / 2.
    dimmingImage304 = smooth(referenceImage304 - min(aia304Array, DIMENSION = 3), 2)
    dimmingImage304 = 255 - bytscl(alog10(dimmingImage304 > 0) > 0 < 1.7)
    aia_lct, red, green, blue, WAVELNTH = '304', /load
    plot_image, bytscl(alog10(referenceImage304 > 0) > 0.5 < 2.5)
    loadct, 0
    plot_image, dimmingImage304
  ENDIF ; AIA 304Å data exists
  
  ; 211 Å
  IF total(aia211Array) GT 0 THEN BEGIN
    referenceImage211 = total(aia211Array[*, *, 0:1], 3) / 2.
    dimmingImage211 = smooth(referenceImage211 - min(aia211Array, DIMENSION = 3), 2)
    dimmingImage211 = 255 - bytscl(alog10(dimmingImage211 > 0) > 0 < 1.7)
    aia_lct, red, green, blue, WAVELNTH = '211', /load
    plot_image, bytscl(alog10(referenceImage211 > 0) > 0.5 < 2.5)
    loadct, 0
    plot_image, dimmingImage211
  ENDIF ; AIA 211Å data exists
  
  !p.multi = 0
ENDIF
; -= END MAKE REFERENCE AND DIMMING IMAGES =- ;

; Create mask for limb and disk
referenceHeader = all171Headers[0]
pb0r = pb0r(referenceHeader.t_obs) ; Solar position (P) and B0 angles, and semi-diameter (r)
xx = ((findgen(4096) # replicate(1, 4096)) - referenceHeader.crpix1) * referenceHeader.cdelt1
yy = ((replicate(1, 4096) # findgen(4096)) - referenceHeader.crpix2) * referenceHeader.cdelt2
maskLimb = xx * 0
maskLimb[where((xx^2 + yy^2) LE (0.975 * pb0r[2] * 60.)^2 )] = 1
maskDisk = congrid(maskLimb, 1024, 1024)

; -= EITHER DEFINE NEW REGIONS OR LOAD EXISTING ONES =- ;
IF keyword_set(DEFINE_REGIONS) THEN BEGIN 
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    read, numARs, PROMPT = 'Specify the number of ARs you wish to identify: '
    maskAR = dblarr(1024, 1024, numARs)
    FOR arIndex = 0, numARs - 1 DO BEGIN
      print, 'Define AR region ' + strtrim(arIndex + 1, 2) + ' of ' + strtrim(numARs, 2) + ':'
      maskARInterior = roimask(bytscl(alog10(referenceImage193 > 0) > 1.25 < 3.5))
      maskAR[*, *, arIndex] = maskARInterior * maskDisk
    ENDFOR
  ENDIF ELSE BEGIN
    print, 'Define AR region:'
    maskARInterior = roimask(bytscl(alog10(referenceImage193 > 0) > 1.25 < 3.5))
    maskAR = maskARInterior * maskDisk
  ENDELSE

  print, 'Define core dimming region:'
  maskCoreDimmingInterior = roimask(dimmingImage171)
  maskCoreDimming = maskCoreDimmingInterior * maskDisk
  
  print, 'Define filament region (or select small off-limb region to exclude):'
  maskFilament = roimask(dimmingImage304)
  
  IF ~keyword_set(AUTOFLARE) THEN BEGIN
    print, 'Define flare region:'
    maxValue = max(aia171Array, maxIndex)
    indices3D = array_indices(aia171Array, maxIndex)
    maskFlare = roimask(bytscl(alog10(aia171Array[*, *, indices3D[2]] > 0) > 1.25 < 3))
  ENDIF
  
  IF keyword_set(AUTOFLARE) THEN $ 
  save, maskAR, maskCoreDimming, maskFilament, FILENAME = saveloc + 'DimmingMasks.sav', /COMPRESS ELSE $
  save, maskAR, maskCoreDimming, maskFilament, maskFlare, FILENAME = saveloc + 'DimmingMasks.sav', /COMPRESS
ENDIF ELSE restore, saveloc + 'DimmingMasks.sav'
; -= END DEFINE/LOAD REGIONS =- ;

; Either create sector maps or load existing ones
IF keyword_set(compute_sector_maps) THEN BEGIN 
  print, 'Identify flare location:'
  maxValue = max(aia171Array, maxIndex)
  indices3D = array_indices(aia171Array, maxIndex)
  aia_lct, red, green, blue, WAVELNTH = '171', /LOAD
  plot_image, bytscl(alog10(aia171Array[*, *, indices3D[2]] > 0) > 1.25 < 3)
  cursor, x, y
  xRecentered = ((x - referenceHeader.crpix1/4.) * (referenceHeader.cdelt1 * 4.)) / 60. ; / 60 to put in arcmin
  yRecentered = ((y - referenceHeader.crpix2/4.) * (referenceHeader.cdelt2 * 4.)) / 60. ; / 60 to put in arcmin
  referenceHeader = all171Headers[indices3D[2]]
  latlon = arcmin2hel(xRecentered, yRecentered, date = referenceHeader.T_OBS)
  message, /INFO, 'Computing sector maps'
  CreateSectorMaps, saveloc = saveloc, all171Headers, lat0 = latlon[0], lon0 = latlon[1]
ENDIF
restore, saveloc + 'SectorMaps.sav'

; -= DEFINE FEATURE MASKS =- ;
; Flare mask
radius = 16
kernel = shift(dist(2 * radius + 1), radius, radius) LE radius
tmp = sectorMap * 0 + 1
tmp[where(sectorMap EQ 1)] = 0
tmp = dilate(tmp, kernel)
IF keyword_set(AUTOFLARE) THEN maskFlare = 1 - tmp

; Filament mask
radius = 4
kernel = shift(dist(2 * radius + 1), radius, radius) LE radius
IF maskFilament EQ [-1] THEN maskFilament = fltarr(1024, 1024) ; In case no 304 image available
maskFilament = dilate(maskFilament, kernel)
maskFilament = tmp * maskFilament * maskLimb

; AR mask
radius = 2
kernel = shift(dist(2 * radius + 1), radius, radius) LE radius
IF keyword_set(SEPARATE_ARS) THEN BEGIN
  FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
    maskAR[*, *, arIndex] = dilate(maskAR[*, *, arIndex], kernel)
    maskAR[*, *, arIndex] = (maskAR[*, *, arIndex] - maskFlare) > 0
  ENDFOR
ENDIF ELSE BEGIN
  maskAR = dilate(maskAR, kernel)
  maskAR = (maskAR - maskFlare) > 0
ENDELSE

; Remove overlap between ROIs
maskCoreDimming = (maskCoreDimming - maskFlare) > 0
maskFilamentFiltered = (maskFilament - maskFlare - maskCoreDimming) > 0
maskQS = (maskLimb - maskFlare - maskCoreDimming - maskFilamentFiltered) > 0
IF keyword_set(SEPARATE_ARS) THEN BEGIN
  FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
    maskCoreDimming = (maskCoreDimming - maskAR[*, *, arIndex]) > 0
    maskFilamentFiltered = (maskFilamentFiltered - maskAR[*, *, arIndex]) > 0
    maskQS = (maskQS - maskAR[*, *, arIndex]) > 0
  ENDFOR
ENDIF ELSE BEGIN
  maskCoreDimming = (maskCoreDimming - maskAR) > 0
  maskFilamentFiltered = (maskFilamentFiltered - maskAR) > 0
  maskQS = (maskQS - maskAR) > 0
  ;maskCoreDimming = (maskCoreDimming - maskFlare - maskAR) > 0 ; Code before switch to separate AR keyword
  ;maskFilamentFiltered = (maskFilament - maskAR - maskFlare - maskCoreDimming) > 0
  ;maskQS = (maskLimb - maskFlare - maskAR - maskCoreDimming - maskFilamentFiltered) > 0
ENDELSE
; -= END DEFINE FEATURE MASKS =- ;

; -= DETERMINE NUMBER OF ROIS FOR EACH TYPE =- ;
IF keyword_set(SEPARATE_ARS) THEN BEGIN
  numberOfARROIs = 0
  FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
    maskARObject = obj_new('Blob_Analyzer', maskAR[*, *, arIndex])
    numberOfARROIs = numberOfARROIs + maskARObject -> numberofblobs()
  ENDFOR
ENDIF ELSE BEGIN
  maskARObject = obj_new('Blob_Analyzer', maskAR)
  numberOfARROIs = maskARObject -> numberofblobs()
ENDELSE

maskDimmingObject = obj_new('Blob_Analyzer', maskCoreDimming)
numberOfDimmingROIs = maskDimmingObject -> numberofblobs()
; -= END DETERMINE NUMBER OF ROIS FOR EACH TYPE =- ;

; -= CALCULATE LIGHT CURVES =- ;
message, /INFO, 'Computing light curves'

; Convert times to JD and get number of images
jd171 = anytim2jd(all171Headers.T_OBS)
jd171 = jd171.int + jd171.frac
n171 = n_elements(jd171)
jd193 = anytim2jd(all193Headers.T_OBS)
jd193 = jd193.int + jd193.frac
n193 = n_elements(jd193)
IF total(aia304Array) GT 0 THEN BEGIN
  jd304 = anytim2jd(all304Headers.T_OBS)
  jd304 = jd304.int + jd304.frac
  n304 = n_elements(jd304)
ENDIF ELSE n304 = 1 ; Whether 304Å data exists
jd211 = anytim2jd(all211Headers.T_OBS)
jd211 = jd211.int + jd211.frac
n211 = n_elements(jd211)

; Full-disk irradiances
total171 = total(total(aia171Array, 1), 1)
total193 = total(total(aia193Array, 1), 1) 
total304 = total(total(aia304Array, 1), 1)
total211 = total(total(aia211Array, 1), 1)

; Allocate 171Å arrays
nSectors = max(sectorMap)
nOfflimbSectors = max(limbMap)
cutouts171 = fltarr(n171, n_elements(maskAR)/1024.^2 + 4) ; 2nd index refers to the number of regions (flare, core dimming, all IDed active regions, quiet sun, filament)
offlimb171 = fltarr(n171)
disk171 = fltarr(n171)
sectors171 = fltarr(nSectors, n171)
tmp = histogram(sectorMap, REVERSE_INDICES = sectorIndices171)
offlimbSectors171 = fltarr(nOfflimbSectors, n171)
tmp = histogram(limbMap, REVERSE_INDICES = offlimbSectorIndices171)

; Allocate 193Å arrays
cutouts193 = fltarr(n193, n_elements(maskAR)/1024.^2 + 4) ; 2nd index refers to the number of regions (flare, core dimming, all IDed active regions, quiet sun, filament)
offlimb193 = fltarr(n171)
disk193 = fltarr(n193)
sectors193 = fltarr(nSectors, n193)
tmp = histogram(sectorMap, REVERSE_INDICES = sectorIndices193)
offlimbSectors193 = fltarr(nOfflimbSectors, n193)
tmp = histogram(limbMap, REVERSE_INDICES = offlimbSectorIndices193)

; Allocate 304Å arrays
cutouts304 = fltarr(n304, n_elements(maskAR)/1024.^2 + 4) ; 2nd index refers to the number of regions (flare, core dimming, all IDed active regions, quiet sun, filament)
offlimb304 = fltarr(n304)
disk304 = fltarr(n304)
sectors304 = fltarr(nSectors, n304)
tmp = histogram(sectorMap, REVERSE_INDICES = sectorIndices304)
offlimbSectors304 = fltarr(nOfflimbSectors, n304)
tmp = histogram(limbMap, REVERSE_INDICES = offlimbSectorIndices304)

; Allocate 211Å arrays
cutouts211 = fltarr(n211, n_elements(maskAR)/1024.^2 + 4) ; 2nd index refers to the number of regions (flare, core dimming, all IDed active regions, quiet sun, filament)
offlimb211 = fltarr(n211)
disk211 = fltarr(n211)
sectors211 = fltarr(nSectors, n211)
tmp = histogram(sectorMap, REVERSE_INDICES = sectorIndices211)
offlimbSectors211 = fltarr(nOfflimbSectors, n211)
tmp = histogram(limbMap, REVERSE_INDICES = offlimbSectorIndices211)

; 171Å sub-image radiances 
FOR imageIndex = 0, n171 - 1 DO BEGIN
  currentImage = aia171Array[*, *, imageIndex]
  cutouts171[imageIndex, 0] = total(currentImage * maskCoreDimming)
  cutouts171[imageIndex, 1] = total(currentImage * maskFilamentFiltered)
  cutouts171[imageIndex, 2] = total(currentImage * maskQS)
  cutouts171[imageIndex, 3] = total(currentImage * maskFlare)
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
      cutouts171[imageIndex, arIndex + 4] = total(currentImage * maskAR[*, *, arIndex])
    ENDFOR
  ENDIF ELSE cutouts171[imageIndex, 4] = total(currentImage * maskAR)
  
  offlimb171[imageIndex] = total(currentImage * maskOverlimb)
  disk171[imageIndex] = total(currentImage * maskLimb)
  
  FOR sectorIndex = 0, nSectors - 1 DO BEGIN
    ;sectorIndices = sectorIndices171[sectorIndices171[sectorIndex + 1]:sectorIndices171[sectorIndex + 2] - 1]
    ;sectors171[sectorIndex, imageIndex] = total(currentImage[sectorIndices])
  ENDFOR
  
  FOR offlimbSectorIndex = 0, nOfflimbSectors - 1 DO BEGIN
    offlimbSectorIndices = offlimbSectorIndices171[offlimbSectorIndices171[offlimbSectorIndex + 1]:offlimbSectorIndices171[offlimbSectorIndex + 2] - 1]
    offlimbSectors171[offlimbSectorIndex, imageIndex] = total(currentImage[offlimbSectorIndices])
  ENDFOR
ENDFOR

; 193Å sub-image radiances
FOR imageIndex = 0, n193 - 1 DO BEGIN
  currentImage = aia193Array[*, *, imageIndex]
  cutouts193[imageIndex, 0] = total(currentImage * maskCoreDimming)
  cutouts193[imageIndex, 1] = total(currentImage * maskFilamentFiltered)
  cutouts193[imageIndex, 2] = total(currentImage * maskQS)
  cutouts193[imageIndex, 3] = total(currentImage * maskFlare)
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
      cutouts193[imageIndex, arIndex + 4] = total(currentImage * maskAR[*, *, arIndex])
    ENDFOR
  ENDIF ELSE cutouts193[imageIndex, 4] = total(currentImage * maskAR)
  
  offlimb193[imageIndex] = total(currentImage * maskOverlimb)
  disk193[imageIndex] = total(currentImage * maskLimb)
  
  FOR sectorIndex = 0, nSectors - 1 DO BEGIN
    ;sectorIndices = sectorIndices193[sectorIndices193[sectorIndex + 1]:sectorIndices193[sectorIndex + 2] - 1]
    ;sectors193[sectorIndex, imageIndex] = total(currentImage[sectorIndices])
  ENDFOR
  
  FOR offlimbSectorIndex = 0, nOfflimbSectors - 1 DO BEGIN
    offlimbSectorIndices = offlimbSectorIndices193[offlimbSectorIndices193[offlimbSectorIndex + 1]:offlimbSectorIndices193[offlimbSectorIndex + 2] - 1]
    offlimbSectors193[offlimbSectorIndex, imageIndex] = total(currentImage[offlimbSectorIndices])
  ENDFOR
ENDFOR

; 304Å sub-image radiances
FOR imageIndex = 0, n304 - 1 DO BEGIN
  currentImage = aia304Array[*, *, imageIndex]
  cutouts304[imageIndex, 0] = total(currentImage * maskCoreDimming)
  cutouts304[imageIndex, 1] = total(currentImage * maskFilamentFiltered)
  cutouts304[imageIndex, 2] = total(currentImage * maskQS)
  cutouts304[imageIndex, 3] = total(currentImage * maskFlare)
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
      cutouts304[imageIndex, arIndex + 4] = total(currentImage * maskAR[*, *, arIndex])
    ENDFOR
  ENDIF ELSE cutouts304[imageIndex, 4] = total(currentImage * maskAR)

  offlimb304[imageIndex] = total(currentImage * maskOverlimb)
  disk304[imageIndex] = total(currentImage * maskLimb)
  
  FOR sectorIndex = 0, nSectors - 1 DO BEGIN
    ;sectorIndices = sectorIndices304[sectorIndices304[sectorIndex + 1]:sectorIndices304[sectorIndex + 2] - 1]
    ;sectors304[sectorIndex, imageIndex] = total(currentImage[sectorIndices])
  ENDFOR
  
  FOR offlimbSectorIndex = 0, nOfflimbSectors - 1 DO BEGIN
    offlimbSectorIndices = offlimbSectorIndices304[offlimbSectorIndices304[offlimbSectorIndex + 1]:offlimbSectorIndices304[offlimbSectorIndex + 2] - 1]
    offlimbSectors304[offlimbSectorIndex, imageIndex] = total(currentImage[offlimbSectorIndices])
  ENDFOR
ENDFOR 

; 211Å sub-image radiances
FOR imageIndex = 0, n211 - 1 DO BEGIN
  currentImage = aia211Array[*, *, imageIndex]
  cutouts211[imageIndex, 0] = total(currentImage * maskCoreDimming)
  cutouts211[imageIndex, 1] = total(currentImage * maskFilamentFiltered)
  cutouts211[imageIndex, 2] = total(currentImage * maskQS)
  cutouts211[imageIndex, 3] = total(currentImage * maskFlare)
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
      cutouts211[imageIndex, arIndex + 4] = total(currentImage * maskAR[*, *, arIndex])
    ENDFOR
  ENDIF ELSE cutouts211[imageIndex, 4] = total(currentImage * maskAR)

  offlimb211[imageIndex] = total(currentImage * maskOverlimb)
  disk211[imageIndex] = total(currentImage * maskLimb)

  FOR sectorIndex = 0, nSectors - 1 DO BEGIN
    ;sectorIndices = sectorIndices211[sectorIndices211[sectorIndex + 1]:sectorIndices211[sectorIndex + 2] - 1]
    ;sectors211[sectorIndex, imageIndex] = total(currentImage[sectorIndices])
  ENDFOR

  FOR offlimbSectorIndex = 0, nOfflimbSectors - 1 DO BEGIN
    offlimbSectorIndices = offlimbSectorIndices211[offlimbSectorIndices211[offlimbSectorIndex + 1]:offlimbSectorIndices211[offlimbSectorIndex + 2] - 1]
    offlimbSectors211[offlimbSectorIndex, imageIndex] = total(currentImage[offlimbSectorIndices])
  ENDFOR
ENDFOR

; Create reference image for display of contours 
referenceImage = total(aia171Array[*, *, 0:4], 3) / 5.
referenceImage = smooth(referenceImage - min(aia171Array, DIMENSION = 3), 2)
referenceImage = 255 - bytscl(alog10(referenceImage > 0) > 0 < 2.7)

; Compute first frame totals for % change computations
initial171Total = total(aia171Array[*, *, 0])
initial193Total = total(aia193Array[*, *, 0])
initial304Total = total(aia304Array[*, *, 0])
initial211Total = total(aia211Array[*, *, 0])

; Compute totals as a light curve
total171 = total(total(aia171Array, 1), 1)
total193 = total(total(aia193Array, 1), 1)
total304 = total(total(aia304Array, 1), 1)
total211 = total(total(aia211Array, 1), 1)
STOP
save, jd171, jd193, jd304,jd211, cutouts171, cutouts193, cutouts304, cutouts211, maskFlare, maskAR, maskCoreDimming, maskFilamentFiltered, maskQS, referenceImage, initial171Total, initial193Total, initial304Total, initial211Total, total171, total193, total304, total211, $
   FILENAME = saveloc + 'LightCurveData.sav'
; -= END CALCULATE LIGHT CURVES =- ;

; -= CREATE MOVIE OF LIGHT CURVES AND AIA =- ;
IF keyword_set(MAKE_MOVIE) THEN BEGIN
  message, /INFO, 'Creating light curve movie'
  names = ['Region 1', 'Region 2', 'Remaining Area', 'Flare']
  IF keyword_set(SEPARATE_ARS) THEN FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO names = [names, 'Region ' + strtrim(arIndex + 3, 2)] ELSE $
    names = [names, 'Active Regions']
  
  ; Create arcsecond array
  lowerBound = (-512 * 2.4) - 2.4/2.
  upperBound = lowerBound + 1024. * 2.4
  arcsecArray = range(lowerBound, upperBound, NPTS = 1024)
  
  ; Get AIA color for 171
  aia_lct, wave = 171, r, g, b
  colortable = [[r], [g], [b]]
  
  ; Setup movie
  movieObject = IDLffVideoWrite(saveloc + 'DimmingByFeature.mp4')
  xsize = 2000
  ysize = 1000
  fps = 10
  bitrate = 1e7
  vidStream = movieObject.AddVideoStream(xsize, ysize, fps, BIT_RATE = bitrate)
  
  FOR timeStep = 0, n_elements(aia171Array[0, 0, *]) - 1 DO BEGIN
    imageToDisplay = bytscl(alog(aia171Array[*, *, timeStep]), min = 2.0, max = 8.0)
    
    w = window(DIMENSIONS = [2000, 1000], /DEVICE, /BUFFER)
    i = image(imageToDisplay, arcsecArray, arcsecArray, AXIS_STYLE = 2, /CURRENT, POSITION = [0.05, 0.05, 0.45, 0.95], RGB_TABLE = colortable, $
              TITLE = 'AIA 171 Dimming Image', $
              XTITLE = 'Arcsec', $
              YTITLE = 'Arcsec')
    c = contour(maskCoreDimming, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'red', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
    IF keyword_set(SEPARATE_ARS) THEN BEGIN
      FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
        c = contour(maskAR[*, *, arIndex], arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, C_VALUE = [0,1,2,3,4], COLOR = JPMColors(arindex + 5, /SIMPLE), /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
      ENDFOR
    ENDIF ELSE $
    c = contour(maskAR, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, C_VALUE = [0,1,2,3,4], COLOR = 'green', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
    c = contour(maskFilamentFiltered, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'green', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
    c = contour(congrid(maskFlare, 1024, 1024), arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'magenta', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
  
    ; 193Å
    dimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4:*], 2)
    p = plot(jd193, perdiff(dimmingTotal193, initial193Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.5, 0.7, 0.98, 0.92], $
             TITLE = 'AIA 193 Light Curves', $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd193), XTICKUNITS = 'Hours', $
             YTITLE = '% Change');, YRANGE = [-6, 4])
    FOR i = 0, n_elements(cutouts193[0, *]) - 1 DO $
      p = plot(jd193, perdiff(cutouts193[*, i], initial193Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
    p = plot([jd193[timeStep], jd193[timeStep]], p.YRANGE, '--', /OVERPLOT)

    ; 171Å
    dimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4:*], 2)
    p = plot(jd171, perdiff(dimmingTotal171, initial171Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.5, 0.4, 0.98, 0.62], $
             TITLE = 'AIA 171 Light Curves', $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd171), XTICKUNITS = 'Hours', $
             YTITLE = '% Change');, YRANGE = [-2, 2])
    FOR i = 0, n_elements(cutouts171[0, *]) - 1 DO $
      p = plot(jd171, perdiff(cutouts171[*, i], initial171Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
    p = plot([jd171[timeStep], jd171[timeStep]], p.YRANGE, '--', /OVERPLOT)
    
    ; 304Å
    dimmingTotal304 = total(cutouts304[*, 0:1], 2) + total(cutouts304[*, 4:*], 2)
    p = plot(jd304, perdiff(dimmingTotal304, initial304Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.5, 0.12, 0.98, 0.32], $
             TITLE = 'AIA 304 Light Curves', $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd304), XTICKUNITS = 'Hours', $
             YTITLE = '% Change', $ ;YRANGE = [-0.6, 0.4], $
             NAME = 'Total of Regions')
    leg = legend(TARGET = p, POSITION = [0.98, 0.4])
    FOR i = 0, n_elements(cutouts304[0, *]) - 1 DO BEGIN
      p = plot(jd304, perdiff(cutouts304[*, i], initial304Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT, NAME = names[i])
      leg.add, p
    ENDFOR
    p = plot([jd304[timeStep], jd304[timeStep]], p.YRANGE, '--', /OVERPLOT)
    
    ; Insert frame into movie
    timeInMovie = movieObject.Put(vidStream, w.CopyWindow()) ; time returned in seconds
    
    ; Save memory
    w.Close
    
    message, /INFO, strtrim(float(timeStep + 1) / n_elements(aia171Array[0, 0, *]) * 100., 2) + '% of movie complete'
  ENDFOR
  
  movieObject.Cleanup
  
ENDIF
; -= END MAKE MOVIE =- ;

; -= PLOT LIGHT CURVES =- ;
PlotAIADimming, saveloc, /SEPARATE_ARS ;/PUBLICATION

; -= END PLOT LIGHT CURVES =- ;

message, /INFO, '-=Program normal completion in ' + strtrim(round(TOC(analyzeAIADimmingRegionsTIC)), 2) + ' seconds=-'
END