;+
; NAME:
;   PlotAIADimming
;
; PURPOSE:
;   Create Rachel-style plot showing dimming in AIA regions
;
; INPUTS:
;   saveloc [string]: Path to the directory with LightCurveData.sav, also outputs to this directory
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   SEPARATE_ARS: Set to treat each identified active region (AR) separately in the light curves and ROIs
;   PUBLICATION:  Set to make publication style plot. This makes the stack vertical instead of horizontal, uses increased font size, and saves as .eps in addition to .png. 
; 
; OUTPUTS:
;   Plot file
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMColors
;
; EXAMPLE:
;   PlotAIADimming, '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2010219_07AUG_1824_M1.0/', /SEPARATE_ARS, /PUBLICATION
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2014/2/13
;-
PRO PlotAIADimming, saveloc, SEPARATE_ARS = SEPARATE_ARS, PUBLICATION = PUBLICATION
TIC

restore, saveloc + 'LightCurveData.sav'

; -= PLOT LIGHT CURVES =- ;
message, /INFO, 'Plotting light curves'
names = ['Region 1', 'Region 2', 'Remaining Area', 'Flare']
IF keyword_set(SEPARATE_ARS) THEN FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO names = [names, 'Region ' + strtrim(arIndex + 3, 2)] ELSE $
  names = [names, 'Active Regions']

; Create arcsecond array
lowerBound = (-512 * 2.4) - 2.4/2.
upperBound = lowerBound + 1024. * 2.4
arcsecArray = range(lowerBound, upperBound, NPTS = 1024)

IF ~keyword_set(PUBLICATION) THEN BEGIN
  ; -= STANDARD NON-PUBLICATION PLOTS =- ;
  
  w = window(DIMENSIONS = [2000, 1000], /DEVICE, /BUFFER)
  i = image(referenceImage, arcsecArray, arcsecArray, AXIS_STYLE = 2, /CURRENT, POSITION = [0.05, 0.05, 0.45, 0.95], $
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
  IF total(maskFilamentFiltered) GT 0 THEN c = contour(maskFilamentFiltered, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'green', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
  c = contour(congrid(maskFlare, 1024, 1024), arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'magenta', /CURRENT, POSITION = [0.00, 0.00, 0.5, 1.0])
  
  ; 193Å
  IF n_elements(cutouts193[0, *]) GT 5 THEN dimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4:*], 2) ELSE $
    dimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4])
  IF dimmingTotal193 GT [0] THEN BEGIN
    p = plot(jd193, perdiff(dimmingTotal193, initial193Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.5, 0.7, 0.98, 0.92], $
             TITLE = 'AIA 193 Light Curves', $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd193), XTICKUNITS = 'Hours', $
             YTITLE = '% Change');, YRANGE = [-6, 4])
    FOR i = 0, n_elements(cutouts193[0, *]) - 1 DO $
      p = plot(jd193, perdiff(cutouts193[*, i], initial193Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
  ENDIF ; 193 data exists
  
  ; 171Å
  IF n_elements(cutouts171[0, *]) GT 5 THEN dimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4:*], 2) ELSE $
    dimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4])
  IF dimmingTotal171 GT [0] THEN BEGIN
    p = plot(jd171, perdiff(dimmingTotal171, initial171Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.5, 0.4, 0.98, 0.62], $
             TITLE = 'AIA 171 Light Curves', $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd171), XTICKUNITS = 'Hours', $
             YTITLE = '% Change');, YRANGE = [-2, 2])
    FOR i = 0, n_elements(cutouts171[0, *]) - 1 DO $
      p = plot(jd171, perdiff(cutouts171[*, i], initial171Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
  ENDIF ; 171 data exists
  
  ; 304Å
  IF n_elements(cutouts304[0, *]) GT 5 THEN dimmingTotal304 = total(cutouts304[*, 0:1], 2) + total(cutouts304[*, 4:*], 2) ELSE $
    dimmingTotal304 = total(cutouts304[*, 0:1], 2) + total(cutouts304[*, 4])
  IF dimmingTotal304 GT [0] THEN BEGIN
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
  ENDIF ; 304 data exists
  p.Save, saveloc + 'DimmingByFeature.png'
ENDIF ELSE BEGIN 
  
  ; -= PUBLICATION STYLE PLOTS =- ;
  fontSize = 24
  contourPosition = [0.07, 0.5, 1.03, 1.0]
  
  w = window(DIMENSIONS = [1000, 2000], /DEVICE, /BUFFER)
  i = image(referenceImage, arcsecArray, arcsecArray, AXIS_STYLE = 2, /CURRENT, POSITION = [0.12, 0.55, 0.98, 0.95], $ 
            TITLE = 'AIA 171Å Dimming Image', FONT_SIZE = fontSize, $
            XTITLE = 'Arcsec', $
            YTITLE = 'Arcsec')
  c = contour(maskCoreDimming, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'red', /CURRENT, POSITION = contourPosition)
  IF keyword_set(SEPARATE_ARS) THEN BEGIN
    FOR arIndex = 0, n_elements(maskAR[0, 0, *]) - 1 DO BEGIN
      c = contour(maskAR[*, *, arIndex], arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, C_VALUE = [0,1,2,3,4], COLOR = JPMColors(arindex + 5, /SIMPLE), /CURRENT, POSITION = contourPosition)
    ENDFOR
  ENDIF ELSE $
    c = contour(maskAR, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, C_VALUE = [0,1,2,3,4], COLOR = 'green', /CURRENT, POSITION = contourPosition)
  IF total(maskFilamentFiltered) GT 0 THEN c = contour(maskFilamentFiltered, arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'green', /CURRENT, POSITION = contourPosition)
  c = contour(congrid(maskFlare, 1024, 1024), arcsecArray, arcsecArray, AXIS_STYLE = 0, C_THICK = [3], C_LABEL_SHOW = 0, N_LEVELS = 2, COLOR = 'magenta', /CURRENT, POSITION = contourPosition)

  ; 171Å
  IF n_elements(cutouts171[0, *]) GT 5 THEN dimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4:*], 2) ELSE $
    dimmingTotal171 = total(cutouts171[*, 0:1], 2) + total(cutouts171[*, 4])
  IF dimmingTotal171 GT [0] THEN BEGIN
    p = plot(jd171, perdiff(dimmingTotal171, initial171Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.1, 0.37, 0.98, 0.48], $
             TITLE = 'AIA 171Å Light Curves', FONT_SIZE = fontSize, $
             XRANGE = minmax(jd171), XTICKUNITS = 'Hours', $
             YTITLE = '% Change');, YRANGE = [-2, 2])
    FOR i = 0, n_elements(cutouts171[0, *]) - 1 DO $
      p = plot(jd171, perdiff(cutouts171[*, i], initial171Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
  ENDIF ; 171 data exists
  
  ; 193Å
  IF n_elements(cutouts193[0, *]) GT 5 THEN dimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4:*], 2) ELSE $
    dimmingTotal193 = total(cutouts193[*, 0:1], 2) + total(cutouts193[*, 4])
  IF dimmingTotal193 GT [0] THEN BEGIN
    p = plot(jd193, perdiff(dimmingTotal193, initial193Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.1, 0.21, 0.98, 0.32], $
      TITLE = 'AIA 193Å Light Curves', FONT_SIZE = fontSize, $
      XRANGE = minmax(jd193), XTICKUNITS = 'Hours', $
      YTITLE = '% Change');, YRANGE = [-6, 4])
    FOR i = 0, n_elements(cutouts193[0, *]) - 1 DO $
      p = plot(jd193, perdiff(cutouts193[*, i], initial193Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT)
  ENDIF ; 193 data exists
  ; 304Å
  IF n_elements(cutouts304[0, *]) GT 5 THEN dimmingTotal304 = total(cutouts304[*, 0:1], 2) + total(cutouts304[*, 4:*], 2) ELSE $
    dimmingTotal304 = total(cutouts304[*, 0:1], 2) + total(cutouts304[*, 4])
  IF dimmingTotal304 GT [0] THEN BEGIN
    p = plot(jd304, perdiff(dimmingTotal304, initial304Total, /RELATIVE), THICK = 4, /CURRENT, POSITION = [0.1, 0.05, 0.98, 0.16], $
             TITLE = 'AIA 304Å Light Curves', FONT_SIZE = fontSize, $
             XTITLE = 'Time [UTC Hours]', XRANGE = minmax(jd304), XTICKUNITS = 'Hours', $
             YTITLE = '% Change', $ ;YRANGE = [-0.6, 0.4], $
             NAME = 'Total of Regions')
    leg = legend(TARGET = p, POSITION = [0.98, 0.19])
    FOR i = 0, n_elements(cutouts304[0, *]) - 1 DO BEGIN
      p = plot(jd304, perdiff(cutouts304[*, i], initial304Total, /RELATIVE), THICK = 4, COLOR = JPMColors(i + 1, /SIMPLE), /OVERPLOT, NAME = names[i])
      leg.add, p
    ENDFOR
  ENDIF ; 304 data exists
  p.Save, saveloc + 'DimmingByFeaturePublication.png'
  p.Save, saveloc + 'DimmingByFeaturePublication.eps'

ENDELSE
; -= END PLOT LIGHT CURVES =- ;

message, /INFO, '-=Program normal completion in ' + strtrim(round(TOC()), 2) + ' seconds=-'
END