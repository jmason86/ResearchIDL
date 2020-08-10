;+
; NAME:
;   CenterToLimbVariationAia171
;
; PURPOSE:
;   Quantify the center to limb variation in 171 Å
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   pathAndFilename [string]: Specify this for the AIA image you wish to use. Default is 
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Plot of center to limb variation with quantified value annotated
;
; OPTIONAL OUTPUTS:
;   Raw AIA image of the sun
;
; RESTRICTIONS:
;   Requires an AIA 171 image, assuming a particular one in a particular directory
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2017-02-08: James Paul Mason: Wrote script.
;-
PRO CenterToLimbVariationAia171, pathAndFilename = pathAndFilename

; Defaults
IF pathAndFilename EQ !NULL THEN BEGIN
  dataloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/AIA/prepped/'
  filename = 'AIA_171_2017-02-08.fits'
  headerNumber = 1
  rebinDivisor = 25.
  minimumBin = 30
  halfMaxHistogram = 50450.0 / 2.
ENDIF ELSE BEGIN
  parsedPathAndFilename = ParsePathAndFilename(pathAndFilename)
  dataloc = parsedPathAndFilename.path
  filename = parsedPathAndFilename.filename
  headerNumber = 2
  rebinDivisor = 24.
  minimumBin = 100
  halfMaxHistogram = 1.599e4 / 2.
ENDELSE

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/AIA Center to Limb Variation/'

; Load data
mreadfits, dataloc + filename, fitsHeader, aiaImage

; Compute the pixel radius to each point in the image
xPositionPixels = findgen(4096) - 4096./2.
yPositionPixels = xPositionPixels
radiusPixels = sqrt(xPositionPixels # xPositionPixels + yPositionPixels # yPositionPixels)
radiusPixels1D = reform(reform(radiusPixels, 1, 4096.^2))
radiusSunPixels = fitsHeader.r_sun / fitsHeader.cdelt1 ; arcsec / (arcsec / pixel) = [pixels]. Note should be using im_scale not cdelt1 but it's not present. 
onDiskIndices = where(radiusPixels1D LT radiusSunPixels - 50)

; Make histogram of data
aiaImage1D = reform(reform(aiaImage, 1, 4096.^2))
aiaImage1DOnDisk = aiaImage1D[onDiskIndices]
hist = histogram(aiaImage1DOnDisk, LOCATIONS = bins)

; Bound to just the pixels at the half max and above
boundedBinIndices = where(bins GE minimumBin AND hist GT halfMaxHistogram)
boundedIndices = where(aiaImage1DOnDisk GE bins[boundedBinIndices[0]] AND aiaImage1DOnDisk LE bins[boundedBinIndices[-1]])

; Plot the histogram
p1 = barplot(bins, hist, /HISTOGRAM, COLOR = 'dodger blue', FILL_COLOR = 'dodger blue', $
             TITLE = 'AIA 171 Å ' + fitsHeader.date_obs + ' Intensity Histogram', $
             XTITLE = 'Intensity [DN]', $
             YTITLE = '#', $
             NAME = 'All pixels')
p2 = barplot(bins[boundedBinIndices], hist[boundedBinIndices], /HISTOGRAM, COLOR = 'tomato', FILL_COLOR = 'tomato', /OVERPLOT, $
             NAME = 'Selected pixels')
l1 = legend(TARGET = [p1, p2], POSITION = [0.90, 0.85])

; Convert pixel radius to heliographic angle
theta = asin(bins[boundedBinindices] / radiusSunPixels) 

; Normalize radius and order the arrays to plot
radiusNormalized = radiusPixels1d[boundedindices] / radiussunpixels
intensitySelectedPixels = aiaimage1dOnDisk[boundedIndices]
sortIndices = sort(radiusNormalized)
radiusNormalized = radiusNormalized[sortIndices]
intensitySelectedPixels = intensitySelectedPixels[sortIndices]

; Rebin the arrays to plot to have fewer points
radiusNormalizedRebinned = rebin(radiusNormalized, n_elements(radiusNormalized) / rebinDivisor)
intensitySelectedPixelsRebinned = rebin(intensitySelectedPixels, n_elements(intensitySelectedPixels) / rebinDivisor)

; Plot intensity versus normalized distance from center
p3 = scatterplot(radiusNormalizedRebinned, intensitySelectedPixelsRebinned, $
                 TITLE = 'AIA 171 Å ' + fitsHeader.date_obs + ' Center to Limb Variation', $
                 XTITLE = 'Normalized distance from disk center', $
                 YTITLE = 'Intensity [DN]')

END