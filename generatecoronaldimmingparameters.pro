;+
; NAME: 
;   GenerateCoronalDimmingParameters
;   
; PURPOSE:
;   To compute coronal dimming parameters (depth, duration, and slope) in spectral light curves from SDO/EVE. 
;   
; INPUTS: 
;   None
;   
; OPTIONAL INPUTS:
;   flareIDs: [string]                       The SDO/EVE specific flare identification string(s). Used for searching Rachel Hock's flare catalog 
;                                            for the relevant time period. 
;   spectralLineWavelengthsInAngstrom: [int] The spectral line(s) to generate the coronal dimming parameters for e.g. 171
;   outputDirectory: [string]                If specified, use specified directory for output instead of default (current directory). 
;   
; KEYWORD PARAMETERS:
;   COMPUTE_PREFLARE_IRRADIANCE: As of 2012/9/24 the flare catalog preflare irradiance is systematically too large. Setting this keyword will 
;                                use the time range given in the flare catalog for the preflare and do the computation using the full time
;                                resolution SDO/EVE data. 
;   
; OUTPUTS:
;   IDL save file containing a structure with the coronal dimming parameters for each spectral line of interest for the 
;   flare of interest. 
;   
; OPTIONAL OUTPUTS: 
;   SAVE_PLOT (Keyword): Create and save a graphical display
;   
; RESTRICTIONS:
;   None
;   
; EXAMPLE: 
;   GenerateCoronalDimmingParameters, spectralLineWavelengthsInAngstrom = 171, flareID = '2010121_01MAY_0139_C5.7'
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2012/8/17
;-
PRO GenerateCoronalDimmingParameters, flareIDs = flareIDs, spectralLineWavelengthsInAngstrom = spectralLineWavelengthsInAngstrom, $
                                      outputDirectory = outputDirectory, $ 
                                      COMPUTE_PREFLARE_IRRADIANCE = compute_preflare_irradiance, SAVE_PLOT = save_plot

; Begin timer
timerStart = systime(1)

; Begin setup
IF keyword_set(outputDirectory) THEN BEGIN
  computedOnTime = systim()
  spawn, 'mkdir "' + outputDirectory + '/Computed On' + computedOnTime + '"'
  outputDirectory = outputDirectory + '/Computed On' + computedOnTime
  CD, outputDirectory
ENDIF ELSE BEGIN
  computedOnTime = systim()
  spawn, 'mkdir "Computed On' + computedOnTime + '"'
  outputDirectory = 'Computed On' + computedOnTime
  CD, outputDirectory
ENDELSE

indexCounter = 0
lastFlareIndex = 0
; End setup

; Load Rachel Hock's flare catalog
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/Coronal Dimming/merged_flare_catalog.sav'

; Define defaults
; flareIDs defaults to all flares in the catalog flagged with coronal dimming
IF NOT keyword_set(flareIDs) THEN BEGIN
  indicesOfCoronalDimming = where(flare_catalog.flags.coronal_dimming EQ 1)
  flareIDs = flare_catalog[indicesOfCoronalDimming].flare_id
ENDIF

; spectralLineWavelengthsInAngstrom default
IF NOT keyword_set(spectralLineWavelengthsInAngstrom) THEN spectralLineWavelengthsInAngstrom = [171, 177, 180, 195, 202, 211, 284]

; Ensure that the provided spectral line is an array to prevent code crashing
spectralLineWavelengthsInAngstrom = [spectralLineWavelengthsInAngstrom]
; End defaults

; Create structure template for storing coronal dimming parameters
coronalDimmingStructure = { FlareID: '', Wavelength: 0, WavelengthUnit: 'Å', FormationTemperature: 0d, FormationTemperatureUnit: 'K', Depth: 0d, DepthUnit: 'W/m^2', PercentDepth: 0d, PercentDepthUnit: '%', Slope: 0d, SlopeUnit: 'W/m^2/s', Duration: 0, DurationUnit: 's', PreflareIrradiance: 0d, PreflareIrradianceUnit: 'W/m^2', CMEVelocity: 0d, CMEVelocityUnit: 'km/s' }

; Loop through the specified flareIDs
FOR flareIDIndex = 0, n_elements(flareIDs)-1 DO BEGIN
  
  ; Set single flare ID
  flareID = flareIDs[flareIDIndex]
  
  ; Ensure that flare has coronal dimming flagged, if not then skip
  numberOfFlareSkips = 0
  IF flare_catalog[flareIDIndex].flags.coronal_dimming NE 1 THEN BEGIN 
    message, /INFO, 'Flare: '+ flareID + ' is not flagged with coronal dimming. Skipping.'
    numberOfFlareSkips ++
    CONTINUE
  ENDIF
  
  ; Get relevant EVE data
  indexOfFlareID = where(flare_catalog.flare_id EQ flareID)
  yearPlusDayOfYear = flare_catalog[indexOfFlareID].goes.year + flare_catalog[indexOfFlareID].goes.doy
  eveLines = eve_merge_evl((yearPlusDayOfYear - 1L), (yearPlusDayOfYear + 1L), n_average = 16, meta = eveMetaData)
  
  ; Loop through the specified spectral lines
  FOR spectralLineIndex = 0, n_elements(spectralLineWavelengthsInAngstrom) - 1 DO BEGIN
    
    ; Set single spectralLine and grab the index in the flare catalog corresponding to it
    spectralLine = spectralLineWavelengthsInAngstrom(spectralLineIndex)
    indexOfSpectralLine = where(flare_catalog[indexOfFlareID].evl.evl_lines.evl_tag EQ 'MEGS_' + strtrim(spectralLine, 2))
    
    ; If user specified keyword, compute the preflare irradiance from the full time resolution EVE data else use the catalog's preflare irradiance
    IF keyword_set(compute_preflare_irradiance) THEN BEGIN
      eveLinesNoTimeAverage = eve_merge_evl((yearPlusDayOfYear - 1L), (yearPlusDayOfYear + 1L))
      preflareStartTime = anytim2tai(flare_catalog[indexOfFlareID].preflare.start_time)
      preflareEndTime = anytim2tai(flare_catalog[indexOfFlareID].preflare.end_time)
      irradianceDuringPreflare = eveLinesNoTimeAverage[where(eveLinesNoTimeAverage.tai GE preflareStartTime AND eveLinesNoTimeAverage.tai LE preflareEndTime)].line_irradiance[indexOfSpectralLine]
      preflareIrradiance = mean(irradianceDuringPreflare) 
    ENDIF ELSE preflareIrradiance = flare_catalog[indexOfFlareID].evl.evl_lines[indexOfSpectralLine].preflare_irrad
    coronalDimmingStructure.PreflareIrradiance = preflareIrradiance
    
    ; Get important times from catalog
    GOESFlareStartTime = flare_catalog[indexOfFlareID].goes.start_time_jd
    GOESFlareMaxTime = flare_catalog[indexOfFlareID].goes.peak_time_jd
    GOESFlareEndTime = flare_catalog[indexOfFlareID].goes.end_time_jd
    GOESFlareDate = flare_catalog[indexOfFlareID].goes.date ; Formatted as dd-MON-yy
    
;    ; Show user the spectral irradiance light curve for time expanded around the GOES reported flare time range and have them select a new end time
;    plot, eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.line_irradiance[indexOfSpectralLine] * 1E6, min = 0, /ynozero, $
;                       title = spectralLine, $
;                       xtitle = GOESFlareDate, xtickformat = 'label_date', xtickunit = 'hour', xrange = [GOESFlareStartTime - 2/24.0d, GOESFlareStartTime + 18/24.0d], /xstyle, $
;                       ytitle = 'Irradiance ['+ greek('mu') + 'W/m!U2!N]'
;    oplot, !x.crange, preflareIrradiance * [1, 1] * 1E6 ; crange is the range from the previous plot
;    message, /INFO, 'Please click end of flare.'
;    cursor, xcursor, ycursor, /data, /down
;    wdelete
    
    xcursor = GOESFlareStartTime + 4./24. ; Make the end time a constant 4 hours after the start time
    
    ; Store the user selected flare end time in normal date format and compute the new duration of the flare
    caldat, xcursor, userFlareEndMonth, userFlareEndDay, userFlareEndHour, userFlareEndMinute, userFlareEndSecond
    ;durationOfFlare = xcursor - GOESFlareStartTime
    
    ; Begin computation of dimming parameters
    ; Compute dimming depth in absolute and percent
    expandedTimeRange = eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d)
    userTimeRange = where(expandedTimeRange GE GOESFlareStartTime AND expandedTimeRange LE xcursor AND eveLines.line_irradiance[indexOfSpectralLine] GT 0)
    irradianceAtMinimumDepth = min(eveLines[userTimeRange].line_irradiance[indexOfSpectralLine], indexOfIrradianceMinimum)
    timeOfMinimumDepth = expandedTimeRange[userTimeRange[indexOfIrradianceMinimum]]
    depthFromPreflare = preflareIrradiance - irradianceAtMinimumDepth
    percentDepth = depthFromPreflare / preflareIrradiance * 100
    
    ; Compute max height of light curve during dimming
    irradianceAtMaximumHeight = max(eveLines[userTimeRange].line_irradiance[indexOfSpectralLine], indexOfIrradianceMaximum)
    timeOfMaximumHeight = expandedTimeRange[userTimeRange[indexOfIrradianceMaximum]]
    heightFromPreflare = irradianceAtMaximumHeight - preflareIrradiance
    
    ; Compute duration of dimming 
    durationOfDimming = (timeOfMinimumDepth - GOESFlareStartTime) * 86400.0d ; Converted from JD to seconds
    
    ; Compute slope of dimming
    slopeBoxBottom = preflareIrradiance - depthFromPreflare;/2.0
    slopeBoxIndices = where(expandedTimeRange GE timeOfMaximumHeight AND expandedTimeRange LE timeOfMinimumDepth AND eveLines.line_irradiance[indexOfSpectralLine] LE preflareIrradiance AND eveLines.line_irradiance[indexOfSpectralLine] GE slopeBoxBottom, slopeBoxCount)
    IF slopeBoxCount GT 1 THEN BEGIN
      slopeTimeRange = expandedTimeRange[slopeBoxIndices]
      slopeTimeElapsedInSeconds = (slopeTimeRange - min(expandedTimeRange)) * 24d * 60d * 60d ; Convert days from Julian day to seconds
      slopeIrradianceRange = (eveLines.line_irradiance[indexOfSpectralLine])[slopeBoxIndices]
      lineCoefficients = reform(poly_fit(slopeTimeElapsedInSeconds, slopeIrradianceRange, 1, yfit = fittedIrradiance, /DOUBLE))
      slope = lineCoefficients[1] ; Don't care about the y-intercept in lineCoefficients[0]
    ENDIF ELSE slope = 0 
    ; End computation of dimming parameters
    
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
    
    IF keyword_set(save_plot) THEN BEGIN
      ; Begin output a plot of the light curve overlaid with dimming parameters
      
      ; Light curve
      p = plot(eve_yd_to_jd(eveLines.yyyydoy + eveLines.sod / 86400.0d), eveLines.line_irradiance[indexOfSpectralLine] * 1E6, 'b2', /BUFFER, $
               title  = strtrim(string(spectralLine),2) + 'Å Light Curve of Flare: ' + flareID, $
               xtitle = 'Time', xrange = [GOESFlareStartTime - 2/24.0d , xcursor + 2/24.0d], xtickformat = '(C(CMOI2.2, "/", CDI2.2, " ", CHI2.2, ":", CMI2.2))', /xstyle, $
               ytitle = 'Irradiance [µW/m!U2!N]', yrange = [min(abs(eveLines.line_irradiance[indexOfSpectralLine] * 1E6)), max(eveLines.line_irradiance[indexOfSpectralLine] * 1E6)], /ystyle)
      
      ; Preflare irradiance line
      p1 = plot(p.xrange, preflareIrradiance * [1, 1] * 1E6, 'r2', /OVERPLOT)
      
      ; GOES flare start time line
      p2 = plot(GOESFlareStartTime * [1, 1], p.yrange, 'g2', /OVERPLOT)
     
      ; GOES flare max time line
      p3 = plot(GOESFlareMaxTime * [1, 1], p.yrange, 'g2', /OVERPLOT)
     
      ; GOES flare end time line
      p4 = plot(GOESFlareEndTime * [1, 1], p.yrange, 'g2', /OVERPLOT)
     
      ; Arrow indicating depth
      a1 = arrow([timeOfMinimumDepth, timeOfMinimumDepth], [preflareIrradiance * 1E6, irradianceAtMinimumDepth * 1E6], /DATA, COLOR = 'Deep Sky Blue', FILL_COLOR = 'Deep Sky Blue', THICK = 3)
    
      ; Arrow indicating duration
      a2 = arrow([GOESFlareStartTime, xcursor], [preflareIrradiance * 1E6, preflareIrradiance * 1E6], /DATA, COLOR = 'Gold', FILL_COLOR = 'Gold', THICK = 2, ARROW_STYLE = 3)
     
      ; Line indicating slope
      IF slope NE 0 THEN $
        p5 = plot(expandedTimeRange(slopeBoxIndices), fittedIrradiance * 1E6, COLOR = 'Dark Orange', '5', /OVERPLOT)
      
      ; Overplot X-ray data from GOES
      gfits_r, stime = jd2anytim(GOESFlareStartTime - 2/24.0d, /stc), etime = jd2anytim(GOESFlareEndTime + 6/24.0d, /stc), tarray = timeForXrayIrradianceGOES, yarray = xrayIrradianceGOES ; TODO: Procedure returns flux, check units 
      ;p6 = plot(
      
      p.save, flareID + '_' + strtrim(string(spectralLine),2) + 'A.png'
    ENDIF ; End output plot
    
    ; Keep track of total index
    indexCounter ++
    
  ENDFOR ; spectralLineIndex
  
  ; Get relevant CME data
  velocity = flare_catalog[indexOfFlareID].CME.CME_LASCO_CDAW.VELOCITY
  IF velocity LE 0 THEN velocity = !VALUES.F_NAN
  coronalDimmingStructure.CMEVelocity = velocity  
  
ENDFOR ; flareIDIndex

; Restore CME catalog


; Extract CME kinetic parameters


; Store CME kinetic parameters in (separate?) structure


; If the structure(s?) are still empty then alert user that nothing was done
IF n_elements(coronalDimmingParameters) EQ 0 THEN message, /INFO, 'No data stored in coronal dimming structure.'

; Create IDL save file with structure(s?)
save, coronalDimmingFlareArray, FILENAME = 'CoronalDimmingParametersComputedOn_' + systim() + '.sav'

; Assume everything completed successfully so alert user of this.
print, 'Program normal completion in ' + strtrim(string(fix(systime(1)-timerStart)), 2), ' seconds'

END