;+
; NAME: 
;   GenerateCoronalDimmingParametersCaseStudy
;   
; PURPOSE:
;   Explore methods for determining slope and depth of EVE coronal dimming light curves. 
;   
; INPUTS: 
;   
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   None
;   
; OUTPUTS:
;   IDL save file containing a structure with the coronal dimming parameters for each spectral line of interest for the 
;   flare of interest. 
;   
; OPTIONAL OUTPUTS: 
;   SAVE_PLOT (Keyword): Create and save a graphical display
;   
; RESTRICTIONS:
;   Requires: 
;      
; EXAMPLE: 
;   GenerateCoronalDimmingParametersCaseStudy
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2013/1/6
;-
PRO GenerateCoronalDimmingParametersCaseStudy

; Begin timer
timerStart = systime(1)

; Hardcode options for program execution
event = 3
spectralLineWavelengthsInAngstrom = [304] ;[094, 131, 171, 177, 180, 193, 195, 202, 211, 284, 304, 335]
compute_preflare_irradiance = 1
useAIABandProduct = 1
loud = 1

; Setup
CASE event OF
  1: BEGIN
    flareID = '2011046_15FEB_0156_X2.2'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011046_15FEB_0156_X2.2/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011046_15FEB_0156_X2.2/'
    message, /INFO, 'Running on event 2011046_15FEB_0156_X2.2'
  END
  2: BEGIN
    flareID = '2011221_09AUG_0805_X6.9'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011221_09AUG_0805_X6.9/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011221_09AUG_0805_X6.9/'
    message, /INFO, 'Running on event: 2011221_09AUG_0805_X6.9' 
  END
  3: BEGIN
    flareID = '2011216_04AUG_0357_M9.3'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011216_04AUG_0357_M9.3/'
    message, /INFO, 'Running on event: 2011216_04AUG_0357_M9.3' 
  END  
  4: BEGIN
    flareID = '2011158_07JUN_0641_M2.5'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011158_07JUN_0641_M2.5/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011158_07JUN_0641_M2.5/'
    message, /INFO, 'Running on event: 2011158_07JUN_0641_M2.5'
  END
ENDCASE

; Create structure template for storing coronal dimming parameters
coronalDimmingStructure = { FlareID: '', Wavelength: 0, WavelengthUnit: 'Å', FormationTemperature: 0d, FormationTemperatureUnit: 'K', Depth: 0d, DepthUnit: 'W/m^2', PercentDepth:0d, PercentDepthUnit: '%', Slope: 0d, SlopeUnit: 'W/m^2/s', Duration: 0, DurationUnit: 's', PreflareIrradiance: 0d, PreflareIrradianceUnit: 'W/m^2' }

; Load Rachel Hock's flare catalog
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/Coronal Dimming/merged_flare_catalog.sav'

; Get relevant EVE data
indexOfFlareID = where(flare_catalog.flare_id EQ flareID)
yearPlusDayOfYear = flare_catalog[indexOfFlareID].goes.year + flare_catalog[indexOfFlareID].goes.doy
eveLines = eve_merge_evl((yearPlusDayOfYear - 1L), (yearPlusDayOfYear + 1L), n_average = 16, meta = eveMetaData)

; Loop through the specified spectral lines
FOR spectralLineIndex = 0, n_elements(spectralLineWavelengthsInAngstrom) - 1 DO BEGIN
  
  ; Set single spectralLine and grab the index in the flare catalog corresponding to it
  spectralLine = spectralLineWavelengthsInAngstrom(spectralLineIndex)
  indexOfSpectralLine = where(flare_catalog[indexOfFlareID].evl.evl_lines.evl_tag EQ 'MEGS_' + strtrim(spectralLine, 2))
  IF useAIABandProduct EQ 1 THEN $
    indexOfSpectralLine = where(strtrim(eveMetaData.BandsMeta.Name, 2) EQ 'AIA_A' + strtrim(spectralLine, 2))
  IF indexOfSpectralLine EQ -1 THEN BEGIN
    print, string(spectralLine) + ' not found. Skipping.'
    CONTINUE
  ENDIF
  
  ; If user specified keyword, compute the preflare irradiance from the full time resolution EVE data else use the catalog's preflare irradiance
  IF compute_preflare_irradiance EQ 1 THEN BEGIN
    eveLinesNoTimeAverage = eve_merge_evl((yearPlusDayOfYear - 1L), (yearPlusDayOfYear + 1L))
    preflareStartTime = anytim2tai(flare_catalog[indexOfFlareID].preflare.start_time)
    preflareEndTime = anytim2tai(flare_catalog[indexOfFlareID].preflare.end_time)
    irradianceDuringPreflare = eveLinesNoTimeAverage[where(eveLinesNoTimeAverage.tai GE preflareStartTime AND eveLinesNoTimeAverage.tai LE preflareEndTime)].line_irradiance[indexOfSpectralLine]
    IF useAIABandProduct EQ 1 THEN $
      irradianceDuringPreflare = eveLinesNoTimeAverage[where(eveLinesNoTimeAverage.tai GE preflareStartTime AND eveLinesNoTimeAverage.tai LE preflareEndTime)].band_irradiance[indexOfSpectralLine]
    preflareIrradiance = mean(irradianceDuringPreflare) 
  ENDIF ELSE preflareIrradiance = flare_catalog[indexOfFlareID].evl.evl_lines[indexOfSpectralLine].preflare_irrad
  coronalDimmingStructure.PreflareIrradiance = preflareIrradiance

  ; Get important times from catalog
  GOESFlareStartTime = flare_catalog[indexOfFlareID].goes.start_time_jd
  GOESFlareMaxTime = flare_catalog[indexOfFlareID].goes.peak_time_jd
  GOESFlareEndTime = flare_catalog[indexOfFlareID].goes.end_time_jd
  GOESFlareDate = flare_catalog[indexOfFlareID].goes.date ; Formatted as dd-MON-yy

  ; Show user the spectral irradiance light curve for time expanded around the GOES reported flare time range and have them select a new end time
  plot, eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.line_irradiance[indexOfSpectralLine] * 1E6, min = 0, /ynozero, $
                     title = spectralLine, $
                     xtitle = GOESFlareDate, xtickformat = 'label_date', xtickunit = 'hour', xrange = [GOESFlareStartTime - 4/24.0d, GOESFlareStartTime + 18/24.0d], /xstyle, $
                     ytitle = 'Irradiance ['+ greek('mu') + 'W/m!U2!N]'
  IF useAIABandProduct EQ 1 THEN $
    plot, eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.band_irradiance[indexOfSpectralLine] * 1E6, min = 0, /ynozero, $
                       title = spectralLine, $
                       xtitle = GOESFlareDate, xtickformat = 'label_date', xtickunit = 'hour', xrange = [GOESFlareStartTime - 4/24.0d, GOESFlareStartTime + 18/24.0d], /xstyle, $
                       ytitle = 'Irradiance ['+ greek('mu') + 'W/m!U2!N]'
  oplot, !x.crange, preflareIrradiance * [1, 1] * 1E6 ; crange is the range from the previous plot
  oplot, GOESFlareStartTime * [1, 1], !y.crange
  message, /INFO, 'Click beginning of dimming event. '
  cursor, xcursor1, ycursor1, /data, /down
  message, /INFO, 'Click end of dimming event. '
  cursor, xcursor2, ycursor2, /data, /down
  wdelete
  
  ; Store the user selected flare end time in normal date format and compute the new duration of the flare
  caldat, xcursor2, userFlareEndMonth, userFlareEndDay, userFlareEndHour, userFlareEndMinute, userFlareEndSecond
  durationOfFlare = xcursor2 - xcursor1
  
  ; Begin computation of dimming parameters
  ; Compute dimming depth
  expandedTimeRange = eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d)
  userTimeRange = where(expandedTimeRange GE xcursor1 AND expandedTimeRange LE xcursor2 AND eveLines.line_irradiance[indexOfSpectralLine] GT 0)
  irradianceAtMinimumDepth = min(eveLines[userTimeRange].line_irradiance[indexOfSpectralLine], indexOfIrradianceMinimum)
  IF useAIABandProduct EQ 1 THEN BEGIN
    userTimeRange = where(expandedTimeRange GE xcursor1 AND expandedTimeRange LE xcursor2 AND eveLines.band_irradiance[indexOfSpectralLine] GT 0)
    irradianceAtMinimumDepth = min(eveLines[userTimeRange].band_irradiance[indexOfSpectralLine], indexOfIrradianceMinimum)
  ENDIF
  timeOfMinimumDepth = expandedTimeRange[userTimeRange[indexOfIrradianceMinimum]]
  depthFromPreflare = preflareIrradiance - irradianceAtMinimumDepth
  
  ; Compute dimming percentage depth from preflare irradiance
  percentDepth = depthFromPreflare / preflareIrradiance * 100
  
  ; Compute max height of light curve during dimming
  irradianceAtMaximumHeight = max(eveLines[userTimeRange].line_irradiance[indexOfSpectralLine], indexOfIrradianceMaximum)
  IF useAIABandProduct EQ 1 THEN $
    irradianceAtMaximumHeight = max(eveLines[userTimeRange].band_irradiance[indexOfSpectralLine], indexOfIrradianceMaximum)
  timeOfMaximumHeight = expandedTimeRange[userTimeRange[indexOfIrradianceMaximum]]
  heightFromPreflare = irradianceAtMaximumHeight - preflareIrradiance
  
  ; Obtain peak irradiance NOTE: Need to rerun code selecting around the peak irradiance for this to be valid
  peakIrradiance = max(eveLines[userTimeRange].line_irradiance[indexOfSpectralLine], indexOfPeakIrradiance)
  IF useAIABandProduct EQ 1 THEN $
    peakIrradiance = max(eveLines[userTimeRange].band_irradiance[indexOfSpectralLine], indexOfPeakIrradiance)
  
  ; Compute duration of dimming 
  durationOfDimming = (xcursor2 - xcursor1) * 86400.0d ; Converted from JD to seconds
  
  ; Compute slope of dimming
  slopeBoxBottom = preflareIrradiance - depthFromPreflare; / 2.0
  slopeBoxIndices = where(expandedTimeRange GE xcursor1 AND expandedTimeRange LE xcursor2 AND eveLines.line_irradiance[indexOfSpectralLine] GE slopeBoxBottom, slopeBoxCount)
  IF useAIABandProduct EQ 1 THEN $
    slopeBoxIndices = where(expandedTimeRange GE xcursor1 AND expandedTimeRange LE xcursor2 AND eveLines.band_irradiance[indexOfSpectralLine] GE slopeBoxBottom, slopeBoxCount)
  IF slopeBoxCount GT 1 THEN BEGIN
    slopeTimeRange = expandedTimeRange[slopeBoxIndices]
    slopeTimeElapsedInSeconds = (slopeTimeRange - min(expandedTimeRange)) * 24d * 60d * 60d ; Convert days from Julian day to seconds
    slopeIrradianceRange = (eveLines.line_irradiance[indexOfSpectralLine])[slopeBoxIndices]
    IF useAIABandProduct EQ 1 THEN $
      slopeIrradianceRange = (eveLines.band_irradiance[indexOfSpectralLine])[slopeBoxIndices]
    lineCoefficients = reform(poly_fit(slopeTimeElapsedInSeconds, slopeIrradianceRange, 1, yfit = fittedIrradiance, /DOUBLE))
    slope = lineCoefficients[1] ; Don't care about the y-intercept in lineCoefficients[0]
  ENDIF ELSE slope = 0 
  ; End computation of dimming parameters
  
  ; Print to screen
  IF loud EQ 1 THEN BEGIN
    print, 'Wavelength = ', spectralLine
    print, 'Slope = ' + strtrim(slope, 2)
    print, 'Depth = ' + strtrim(depthFromPreflare, 2)
    print, '%Depth = ' + strtrim(percentDepth, 2)
    print, 'Preflare Irradiance = ' + strtrim(preflareIrradiance, 2)
    print, 'Peak Irradiance =' + strtrim(peakIrradiance, 2)
    print, 'Peak Time = ', jd2anytim(expandedTimeRange[userTimeRange[indexOfPeakIrradiance]], out_style = 'ex')
    print, 'Minimum Irradiance = ' + strtrim(irradianceAtMinimumDepth, 2)
    print, 'Min Time = ', jd2anytim(expandedTimeRange[userTimeRange[indexOfIrradianceMinimum]], out_style = 'ex')
  ENDIF
  
  ; Store dimming parameters in structure
  coronalDimmingStructure.FlareID = flareID
  coronalDimmingStructure.Wavelength = spectralLine
  coronalDimmingStructure.Depth = depthFromPreflare
  coronalDimmingStructure.PercentDepth = percentDepth
  coronalDimmingStructure.Slope = slope
  coronalDimmingStructure.Duration = fix(durationOfDimming)
  coronalDimmingFlareArray = (n_elements(coronalDimmingFlareArray) eq 0) ? coronalDimmingStructure : [coronalDimmingFlareArray, coronalDimmingStructure]
  
  ; Assign the appropriate ion formation temperature and store in structure
  IF spectralLine EQ 171 THEN coronalDimmingStructure.FormationTemperature = 6.31E5
  IF spectralLine EQ 177 THEN coronalDimmingStructure.FormationTemperature = 933254
  IF spectralLine EQ 180 THEN coronalDimmingStructure.FormationTemperature = 1.1481E6
  IF spectralLine EQ 195 THEN coronalDimmingStructure.FormationTemperature = 1.26E6
  IF spectralLine EQ 202 THEN coronalDimmingStructure.FormationTemperature = 1.58E6
  IF spectralLine EQ 211 THEN coronalDimmingStructure.FormationTemperature = 1.8621E6
  IF spectralLine EQ 284 THEN coronalDimmingStructure.FormationTemperature = 2.1878E6
  ; TODO: 131, 094, 335
  
  ; Light curve 
  p1 = plot(eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.line_irradiance[indexOfSpectralLine] * 1E6, 'b2', /BUFFER, $
            title  = strtrim(string(spectralLine),2) + 'Å Light Curve of Flare: ' + flareID, $
            xtitle = 'Time', xrange = [GOESFlareStartTime - 4/24.0d, GOESFlareStartTime + 18/24.0d], xtickformat = '(C(CMOI2.2, "/", CDI2.2, " ", CHI2.2, ":", CMI2.2))', /xstyle, $
            ytitle = 'Irradiance [µW/m!U2!N]', yrange = [min(abs(eveLines.line_irradiance[indexOfSpectralLine] * 1E6)), max(eveLines.line_irradiance[indexOfSpectralLine] * 1E6)], /ystyle)
  IF useAIABandProduct EQ 1 THEN $
    p1 = plot(eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.band_irradiance[indexOfSpectralLine] * 1E6, 'b2',$; /BUFFER, $
              title  = strtrim(string(spectralLine),2) + 'Å Light Curve of Flare: ' + flareID, $
              xtitle = 'Time', xrange = [GOESFlareStartTime - 4/24.0d, GOESFlareStartTime + 18/24.0d], xtickformat = '(C(CMOI2.2, "/", CDI2.2, " ", CHI2.2, ":", CMI2.2))', /xstyle, $
              ytitle = 'Irradiance [µW/m!U2!N]', yrange = [2.8E14, max(eveLines.band_irradiance[indexOfSpectralLine] * 1E6)], /ystyle)
  
  ; Preflare irradiance line
  p1 = plot(p1.xrange, preflareIrradiance * [1, 1] * 1E6, 'r2', /OVERPLOT)
  
  ; GOES flare start time line
  p1 = plot(GOESFlareStartTime * [1, 1], p1.yrange, 'g2', /OVERPLOT)
 
  ; GOES flare max time line
  p1 = plot(GOESFlareMaxTime * [1, 1], p1.yrange, 'g2', /OVERPLOT)
 
  ; GOES flare end time line
  p1 = plot(GOESFlareEndTime * [1, 1], p1.yrange, 'g2', /OVERPLOT)
 
  ; Arrow indicating depth
  a1 = arrow([timeOfMinimumDepth, timeOfMinimumDepth], [preflareIrradiance * 1E6, irradianceAtMinimumDepth * 1E6], /DATA, COLOR = 'Deep Sky Blue', FILL_COLOR = 'Deep Sky Blue', THICK = 3)

  ; Arrow indicating duration
  ;a1 = arrow([xcursor1, xcursor2], [preflareIrradiance * 1E6, preflareIrradiance * 1E6], /DATA, COLOR = 'Gold', FILL_COLOR = 'Gold', THICK = 2, ARROW_STYLE = 3)
  
  ; Text indicating slope, depth, and percent depth
  t1 = text(0.6, 0.8, 'Slope = ' + number_formatter(slope, DECIMALS = 2) + ' W/m!U2!N/s', COLOR = 'Dark Orange')
  t1 = text(0.6, 0.75, 'Depth = ' + number_formatter(depthFromPreflare, DECIMALS = 2) + ' W/m!U2', COLOR = 'Deep Sky Blue')
  t1 = text(0.6, 0.70, 'Depth = ' + number_formatter(percentDepth, DECIMALS = 2) + ' %', COLOR = 'Deep Sky Blue')
  
  ; Line indicating slope
  IF slope NE 0 THEN $
    p6 = plot(expandedTimeRange(slopeBoxIndices), fittedIrradiance * 1E6, COLOR = 'Dark Orange', '5', /OVERPLOT)
  
  ; Overplot X-ray data from GOES
  gfits_r, stime = jd2anytim(GOESFlareStartTime - 2/24.0d, /stc), etime = jd2anytim(GOESFlareEndTime + 6/24.0d, /stc), tarray = timeForXrayIrradianceGOES, yarray = xrayIrradianceGOES ; TODO: Procedure returns flux, check units 
  ;p7 = plot(
  
  ;p1.Save, saveloc + 'EVELightCurve_' + strtrim(string(spectralLine), 2) + 'A.png'
  ;p1.Close
  STOP
ENDFOR ; spectralLineIndex

; Plot peak formation temperature versus slope and depth
nonzeroIndices = where(coronaldimmingflarearray.Slope NE 0 AND coronaldimmingflarearray.FormationTemperature NE 0)
p1 = plot(coronaldimmingflarearray[nonzeroIndices].FormationTemperature, coronaldimmingflarearray[nonzeroIndices].Slope, '2D-', /BUFFER, $
          title = flareID, $
          xtitle = 'Peak Formation Temperature [K]', $
          ytitle = 'Slope [W/m!U2!N/s]')
p2 = plot(coronaldimmingflarearray[nonzeroIndices].FormationTemperature, coronaldimmingflarearray[nonzeroIndices].Depth, '2D-', /BUFFER, $
          title = flareID, $
          xtitle = 'Peak Formation Temperature [K]', $
          ytitle = 'Depth [W/m!U2!N]')
p3 = plot(coronaldimmingflarearray[nonzeroIndices].FormationTemperature, coronaldimmingflarearray[nonzeroIndices].PercentDepth, '2D-', /BUFFER, $
          title = flareID, $
          xtitle = 'Peak Formation Temperature [K]', $
          ytitle = '%Depth [%]')
;p1.Save, saveloc + 'SlopeDepthVsT.png'
;p2.Save, saveloc + 'DepthVsT.png'
;p3.Save, saveloc + 'PercentDepthVsT.png'
p1.close & p2.Close & p3.Close

; Create IDL save file with structure(s?)
;save, coronalDimmingFlareArray, FILENAME = saveloc + 'CoronalDimmingParameters.sav'

; Assume everything completed successfully so alert user of this.
print, '-=Program normal completion in ' + strtrim(string(fix(systime(1)-timerStart)), 2), ' seconds=-'

END