;+
; NAME:
;   RegressDimmingAndCMEVelocity
;
; PURPOSE:
;   Perform linear regression on dimming slope and depth for multiple lines with CME velocity
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
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
;     2013/5/6
;-
PRO RegressDimmingAndCMEVelocity

; Setup 
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On 6-May-2013 16:46:20/'

; Load data
restore, saveloc + 'CoronalDimmingParametersComputedOn_ 6-May-2013 17:02:04.sav'

; Store data in simple arrays
slopes = CoronalDimmingFlareArray.Slope
depths = CoronalDimmingFlareArray.PercentDepth * 100.
velocities = CoronalDimmingFlareArray.CMEVelocity
wavelengths = CoronalDimmingFlareArray.Wavelength
flareIDs = CoronalDimmingFlareArray.FlareID

; Filter data for finite velocities
goodVelocityIndices = where(finite(velocities) AND velocities GT 0, count)
slopes = slopes[goodVelocityIndices]
depths = depths[goodVelocityIndices]
velocities = velocities[goodVelocityIndices]
wavelengths = wavelengths[goodVelocityIndices] 
flareIDs = flareIDs[goodVelocityIndices]

; Require that 171Å and 195Å have ≥1% depth, else throw out the event
depth171 = depths[where(wavelengths EQ 171)] & depth195 = depths[where(wavelengths EQ 195)]
slope171 = slopes[where(wavelengths EQ 171)] & slope195 = slopes[where(wavelengths EQ 195)]
velocity171 = velocities[where(wavelengths EQ 171)] & velocity195 = velocities[where(wavelengths EQ 195)]
FOR i = 0, n_elements(depth171) - 1 DO BEGIN
  IF depth171[i] GE 1 AND depth195[i] GE 1 THEN BEGIN
    newSlopes = (i EQ 0) ? [slope171[i], slope195[i]] : [newSlopes, slope171[i], slope195[i]]
    newDepths = (i EQ 0) ? [depth171[i], depth195[i]] : [newDepths, depth171[i], depth195[i]]
    newVelocities = (i EQ 0) ? [velocity171[i], velocity195[i]] : [newVelocities, velocity171[i], velocity195[i]]
  ENDIF
ENDFOR
numberOfEvents = n_elements(newDepths)/2
numberOfWavelengths = 2
slopes = newSlopes
depths = newDepths
velocities = newVelocities

; Organize array by event
j=0
velocitiesByEvent = fltarr(numberOfEvents)
FOR i = 0, n_elements(depths) - 1, numberOfWavelengths DO BEGIN
  slopesByEvent = (i EQ 0) ? transpose(slopes[0:numberOfWavelengths-1]) : [slopesByEvent, transpose(slopes[i:i+numberOfWavelengths-1])]
  depthsByEvent = (i EQ 0) ? transpose(depths[0:numberOfWavelengths-1]) : [depthsByEvent, transpose(depths[i:i+numberOfWavelengths-1])]
  velocitiesByEvent(j) = velocities[i] & j++
ENDFOR
slopesByEvent = transpose(slopesByEvent)
depthsByEvent = transpose(depthsByEvent)

; Perform the regression
regressionResult = regress(depthsByEvent, velocitiesByEvent, CONST = regressionConstant, CORRELATION = regressionCorrelation, MCORRELATION = allCorrelations, SIGMA = sigma)
;print, 'Percent depth of 171, 177, 180, 195, 202, 211, 284, for 18 events'
print, 'Percent depth of 171, 195 for ' + strtrim(string(numberOfEvents), 2) + ' events'
print, 'Constant: ', regressionConstant
print, 'Coefficients: ', regressionResult
print, 'Correlations: ', regressionCorrelation
print, 'Overall correlation: ', allCorrelations

regressionResult = regress(slopesByEvent, velocitiesByEvent, CONST = regressionConstant, CORRELATION = regressionCorrelation, MCORRELATION = allCorrelations, SIGMA = sigma)
;print, 'Slopes of 171, 177, 180, 195, 202, 211, 284, for 18 events'
print, 'Slopes of 171, 195 for ' + strtrim(string(numberOfEvents), 2) + ' events'
print, 'Constant: ', regressionConstant
print, 'Coefficients: ', regressionResult
print, 'Correlations: ', regressionCorrelation
print, 'Overall correlation: ', allCorrelations

; Scatter plot
wavelengthNames = ['Fe IX 171Å', 'Fe X 177Å', 'Fe XI 180Å', 'Fe XII 195Å', 'Fe XIII 202Å', 'Fe XIV 211Å', 'Fe XV 284Å']
w = window(dimensions = [800, 800])
FOR i = 0, numberOfWavelengths - 1 DO $
  p = scatterplot(velocitiesByEvent, depthsByEvent[i,*], /SYM_FILLED, SYM_FILL_COLOR = JPMColors(i+10), LAYOUT = [ceil(numberOfWavelengths/2), ceil(numberOfWavelengths/2) > 2, i+1], /CURRENT, $
                  xtitle = 'Velocity [km/s]', $
                  ytitle = 'Depth [%]', $ 
                  title = wavelengthNames(i))
p.Save, saveloc + 'DepthVsVelocity.png'
p.Close

w = window(dimensions = [900, 800])
FOR i = 0, numberOfWavelengths - 1 DO $
  p = scatterplot(velocitiesByEvent, slopesByEvent[i,*], /SYM_FILLED, SYM_FILL_COLOR = JPMColors(i+10), LAYOUT = [ceil(numberOfWavelengths/2), ceil(numberOfWavelengths/2) > 2, i+1], /CURRENT, $
                  xtitle = 'Velocity [km/s]', $
                  ytitle = 'Slope [W/m!U2!N/s]', $
                  title = wavelengthNames(i))
p.Save, saveloc + 'SlopeVsVelocity.png'
p.Close

END