;+
; NAME:
;   MakeCoronagraphDiskOverlay
;
; PURPOSE:
;   Make tri-panel plot for Hock et al. 2013 ApJL on coronal dimming
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   .eps of tri-panel plot
;   .sav of all image arrays
;   
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
;   N/A
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/3/18
;-
PRO MakeCoronagraphDiskOverlay

; Begin load and prep images

; Coronagraph data
; LASCO
;lasco1File = '/Users/jama6159/Documents/Research/Data/SOHO/LASCO/2011216_04AUG_0357_M9.3/22382093.fts'
;lascoProcessed = mk_img(lasco1File, 0, 255, lascohdr, /MASK_OCC, /FIXGAPS, /DIFF)
;; COR2
;cor1A1 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/a/img/cor2/20110804/20110804_042400_d4c2A.fts', cor1A1hd)
;cor1A2 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/a/img/cor2/20110804/20110804_043900_d4c2A.fts', cor1A2hd)
;cor1B1 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/b/img/cor2/20110804/20110804_042400_d4c2B.fts', cor1B1hd)
;cor1B2 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/b/img/cor2/20110804/20110804_043900_d4c2B.fts', cor1B2hd)
;cor_prep, cor1A1hd, cor1A1
;cor_prep, cor1A2hd, cor1A2
;cor_prep, cor1B1hd, cor1B1
;cor_prep, cor1B2hd, cor1B2
;
;; EUV disk image data
;; AIA
;aia1 = readfits('/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/AIA20110804_041936_0171.fits')
;aia2 = readfits('/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/AIA20110804_042648_0171.fits')
;aiadiff = aia2 - aia1
;aia1Small = rebin(aia1, 64, 64)
;aiadiffSmall = rebin(aiadiff, 64, 64)
; EUVI
euviA1 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/a/img/euvi/20110804/20110804_040100_n5euA.fts', euviA1hd)
euviA2 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/a/img/euvi/20110804/20110804_040215_n5euA.fts', euviA2hd)
euviB1 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/b/img/euvi/20110804/20110804_041400_n4euB.fts', euviB1hd)
euviB2 = readfits('/Users/jama6159/Documents/Research/Data/STEREO/2011216_04AUG_0357_M9.3/secchi/L0/b/img/euvi/20110804/20110804_061400_n4euB.fts', euviB2hd)
STOP
euvi_prep, euviA1hd, euviA1
euvi_prep, euviA2hd, euviA2
euvi_prep, euviB1hd, euviB1
euvi_prep, euviB2hd, euviB2

; End load and prep images

; Produce plot
w = window(DIMENSIONS = [1080, 360])
i1 = image(lascoProcessed, image_dimensions = [360, 360], /CURRENT)
i2 = image(alog10(aia1Small), OVERPLOT = 1, IMAGE_LOCATION = [147, 147])

STOP
i1.Close

SAVE, /VARIABLES, FILENAME = '/Users/jama6159/Dropbox/Research/Woods_LASP/Publish/Coronal Dimming ApJL/CoronagraphDiskOverlay.sav'

END