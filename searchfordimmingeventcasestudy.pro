;+
; NAME:
;   SearchForDimmingEventCaseStudy
;
; PURPOSE:
;   Messy code to explore and identify potential dimming events
;
; INPUTS:
;   See hardcode setup in code
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Various outputs: plots, correlation numbers, etc
;
; OPTIONAL OUTPUTS:
; 
;
; RESTRICTIONS:
; 
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/5/28
;-
PRO SearchForDimmingEventCaseStudy

; Setup
plotall = 0
doCorrelations = 1

; Restore flare catalog and store relevant parameters
restore, getenv('flare_catalog')
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On 6-May-2013 16:46:20/CoronalDimmingParametersComputedOn_ 6-May-2013 17:02:04.sav'
ids = flare_catalog.flare_id
cdaw = flare_catalog.CME.CME_LASCO_CDAW.Velocity
stereoA = flare_catalog.CME.CME_SECCHI_A.Velocity
stereoB = flare_catalog.CME.CME_SECCHI_B.Velocity
cactus = flare_catalog.CME.CME_LASCO_CACTUS.Velocity

IF plotall EQ 1 THEN BEGIN
  p1 = scatterplot(indgen(1513), cdaw, MIN_VALUE = 0, /SYM_FILLED, SYM_FILL_COLOR = 'red', $
                   title = 'All CME velocities in flare catalog', $
                   xtitle = 'index', $
                   ytitle = 'Velocity [km/s]', $
                   NAME = 'CDAW-LASCO')
  p2 = scatterplot(indgen(1513), cactus, MIN_VALUE = 0, /OVERPLOT, /SYM_FILLED, SYM_FILL_COLOR = 'green', $
                   NAME = 'CACTus-LASCO')
  p3 = scatterplot(indgen(1513), stereoA, MIN_VALUE = 0, /OVERPLOT, /SYM_FILLED, SYM_FILL_COLOR = 'blue', $
                   NAME = 'STEREO A')
  p4 = scatterplot(indgen(1513), stereoB, MIN_VALUE = 0, /OVERPLOT, /SYM_FILLED, SYM_FILL_COLOR = 'purple', $
                   NAME = 'STEREO B')                
  l = legend(TARGET = [p1, p2, p3, p4], POSITION = [0.92, 0.88])
  ;p1.Close
ENDIF ; plotall = 1

gt750indices = where(cdaw GE 750 OR cactus GE 750 OR stereoA GE 750 OR stereoB GE 750, count)
gt750ids = ids[gt750indices]

FOR i = 0, count - 1 DO BEGIN
  matchedIDindices = where(coronalDimmingFlareArray.FlareID EQ gt750ids[i])
  IF matchedIDindices EQ [-1] THEN CONTINUE
  tmp = coronalDimmingFlareArray[matchedIDindices].PercentDepth
  tmp = tmp[0] * 100 ; Only want the 171 percent depth, and convert from fraction to %
  gt750PercentDepth = PushOnArray(gt750PercentDepth, tmp)
  tmp = coronalDimmingFlareArray[matchedIDindices].Slope
  tmp = tmp[0] ; Only want 171 slope
  gt750Slope = PushOnArray(gt750Slope, tmp)
  tmp = coronalDimmingFlareArray[matchedIDindices].FlareID
  tmp = tmp[0]
  gt750idsMatched = PushOnArray(gt750idsMatched, tmp)
  tmp = cdaw[gt750indices[i]] > cactus[gt750indices[i]] > stereoA[gt750indices[i]] > stereoB[gt750indices[i]]
  gt750velocityMatched = PushOnArray(gt750velocityMatched, tmp)
ENDFOR

IF doCorrelations EQ 1 THEN BEGIN
  ; Check correlations
  p1 = scatterplot(gt750velocityMatched, gt750PercentDepth, /SYM_FILLED, SYM_FILL_COLOR = 'red', $
                   title = 'CMEs with v ≥ 750 and 171Å EVE %Depths', $
                   xtitle = 'CME Velocity from CDAW (LASCO) and CACTus (LASCO or StereoA/B) [km/s]', $
                   ytitle = 'Depth of 171Å Dimming [%]')
  ;p1.Close
  velocitySlopeCorrelation = correlate(gt750velocityMatched, gt750Slope)
  velocityDepthCorrelation = correlate(gt750velocityMatched, gt750PercentDepth)
ENDIF ; doCorrelations = 1


STOP

END