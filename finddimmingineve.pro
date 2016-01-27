;+
; NAME:
;   FindDimmingInEVE
;
; PURPOSE:
;   To find and characterize dimming events in the EVE light curves.
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
;   coronalDimmingOutput [Structure]: Contains the start and end times of dimming, the time and value of peak and minimum irradiance,
;                                     the GOES flare magnitude, and slope of dimming
;
; OPTIONAL OUTPUTS:
;
;
; RESTRICTIONS:
;   Requires solarsoft for obtaining GOES event list, and EVE software
;
; EXAMPLE:
;   
;
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2013/2/18
;-
PRO FindDimmingInEVE
TIC

; Setup
eveDataLoc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/EVE/'
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Automatic Dimming Database/'
producePlot = 1
useSaveFile = 0

IF useSaveFile NE 1 THEN BEGIN

  ; Hard-coded parameters
  leftBracketHours = 0.  ; Number of hours before GOES event start time to bracket EVE data, used for computing pre-flare irradiance ;TODO: Check this value with Tom/Amir
  rightBracketHours = 4. ; Number of hours after GOES event start time to bracket EVE data given there's event ocurring prior to this
  
  ; Load GOES event list from EVE day 1 (2010/120 April 30) to present
  rd_gev, '30-apr-10', systim(), goesEvents
  
  ; Restrict to select+ flares
  selectFlares = goesEvents[where(string(goesEvents.ST$CLASS) GE 'M', numFlares)]
  
  ; Get event start times from GOES event list
  goesEventStartTimes = anytim(selectFlares, /MJD)
  
  ; Load EVE lines from EVE day 1 (2010/120 April 30) to present, merged, and averaged to 1 minute
  restore, eveDataLoc + 'eve_lines_2010121-2014120 MEGS-A Mission.sav'
  eveDOYAndTime = datalines.YYYYDOY  + datalines.SOD / 86400.d0
  
  ; Get rid of bad EVE data
  FOR wavelengthIndex = 0, n_elements(datalines[0].line_irradiance) - 1 DO BEGIN
    badIndices = where(datalines.line_irradiance[wavelengthIndex] EQ -1)
    datalines[badIndices].line_irradiance[wavelengthIndex] = !VALUES.F_NAN
  ENDFOR
  
  ; Create array of EVE line centers in Å
  eveLineCenters = ['94', '131', '133', '171', '177', '180', '195', '202', '211', '256', $
                    '284', '304', '335', '361', '368', '446', '465', '499', '521', '526', $ 
                    '537', '554', '568', '584', '592', '596', '601', '625', '630', '719', $
                    '722', '770', '790', '836', '950', '973', '977', '1026', '1032']
                      
  ; Create the output structure
  coronalDimmingStructure = { GoesClass: '', GoesFlux: 0d, EveLineCenters: eveLineCenters, EventStartTime: dblarr(39), StartIrradiance: dblarr(39), EventPeakTime: dblarr(39), PeakIrradiance: dblarr(39), EventMinimumTime: dblarr(39), MinimumIrradiance: dblarr(39), EventDurationInSeconds: fltarr(39), Slope: dblarr(39), SlopeUnit: 'W/m^2/s', PercentDepth: dblarr(39), PercentDepthUnit: 'W/m^2'}
 
  ; Begin loop through all GOES events 
  FOR goesEventIndex = 0, numFlares - 1 DO BEGIN
    
    ; Convert dumb* anytim MJD to eve yyyydoy and store current GOES event start time. *Why is it milliseconds into day, not fraction of day? Ugh. 
    currentGoesEventStartDay = eve_jd_to_yd(goesEventStartTimes[goesEventIndex].MJD + 2400000.5)
    currentGoesEventStartTime = goesEventStartTimes[goesEventIndex].TIME / 1E3 ; converted to seconds from milliseconds
  
    ; Select EVE data from N1 minutes before goesStartTime to the sooner of [Next GOES event start or N2 hours after GOES start]
    eventBeginBracket = eve_jd_to_yd(goesEventStartTimes[goesEventIndex].MJD + 2400000.5 + goesEventStartTimes[goesEventIndex].TIME / 8.64D7 - leftBracketHours / 24.)
    IF goesEventIndex LT numFlares - 1 THEN eventEndBracket = eve_jd_to_yd(goesEventStartTimes[goesEventIndex + 1].MJD + 2400000.5 + goesEventStartTimes[goesEventIndex + 1].TIME / 8.64D7) < eve_jd_to_yd(goesEventStartTimes[goesEventIndex].MJD + 2400000.5 + goesEventStartTimes[goesEventIndex].TIME / 8.64D7 + rightBracketHours / 24.) $
      ELSE eventEndBracket = eve_jd_to_yd(goesEventStartTimes[goesEventIndex].MJD + 2400000.5 + goesEventStartTimes[goesEventIndex].TIME / 8.64D7 + rightBracketHours / 24.)
    eveInRangeIndices = where(eveDOYAndTime GE eventBeginBracket AND eveDOYAndTime LE eventEndBracket, numEveInRange)
    eveIrradianceInRange = datalines[eveInRangeIndices].LINE_IRRADIANCE
    
    ; Skip if no EVE data in range found
    IF numEveInRange EQ 0 THEN BEGIN
      message, /INFO, 'No EVE data found between ' + strtrim(eventBeginBracket, 2) + 'and ' + strtrim(eventEndBracket, 2)
      message, /INFO, 'Dimming search: ' + strtrim(float(goesEventIndex) / numFlares * 100., 2) + '% complete'
      CONTINUE
    ENDIF
    
    ; Get and store the peak time and irradiance value in EVE within the bracketed time range
    peakIrradiance = dblarr(39) & timeOfPeakIrradiance = dblarr(39)
    FOR i = 0, 29 DO BEGIN
      peakIrradiance[i] = max(eveIrradianceInRange[i, *], indexOfPeak)
      timeOfPeakIrradiance[i] = eveDOYAndTime[eveinRangeIndices[indexOfPeak]]
    ENDFOR ; i loop
    
    ; Smooth EVE data to reduce noise for finding minimum 
    eveIrradianceInRangeSmoothed = smooth(eveIrradianceInRange, [1, 5])
    
    ; Get and store the minimum time and irradiance value in EVE within the EVE peak time and GOES end time
    minIrradiance = dblarr(39) & timeOfMinIrradiance = dblarr(39)
    FOR i = 0, 29 DO BEGIN
      ; Only look at times between peak time (rather than start time) and end time
      eveInMinRangeIndices = where(eveDOYAndTime GE timeOfPeakIrradiance[i] AND eveDOYAndTime LE eventEndBracket)
      minIrradiance[i] = min(eveIrradianceInRangeSmoothed[i, eveInMinRangeIndices], indexOfMin)
      timeOfMinIrradiance[i] = eveDOYAndTime[eveInMinRangeIndices[indexOfMin]]
    ENDFOR
    
    ; Compute and store the time between the GOES start time and the EVE minimum time
    durationInSeconds = dblarr(39)
    FOR i = 0, 29 DO BEGIN
      durationInFractionalDays = timeOfMinIrradiance[i] - eventBeginBracket
      durationInSeconds[i] = durationInFractionalDays * 24.d0 * 3600.d0
      IF durationinseconds[i] GT (rightBracketHours * 3600) THEN BEGIN
        message, /INFO, 'Duration computed exceeds threshold. Skipping.'
        message, /INFO, 'Dimming search search: ' + strtrim(float(goesEventIndex) / numFlares * 100., 2) + '% complete'
        CONTINUE
      ENDIF
    ENDFOR
      
    ; Compute and store the slope between the EVE irradiance at the GOES start time and the EVE irradiance at the minimum time
    slope = dblarr(39)
    FOR i = 0, 29 DO BEGIN
      rise = eveIrradianceInRange[i, 0] - minIrradiance[i] ; TODO: Change index 0 to whatever the GOES start time is, since if leftBracketHours ≠ 0 then index 0 will be incorrect
      run = durationInSeconds(i)
      slope[i] = rise / run
      IF slope[i] EQ 0 AND i EQ 3 THEN STOP
    ENDFOR 
    
    ; Compute and store the depth between the EVE irradiance at the GOES start time and the EVE irrdiance at the minimum time
    depth = dblarr(39)
    FOR i = 0, 29 DO BEGIN
      depth[i] = (eveIrradianceInRange[i, 0] - minIrradiance[i]) / eveIrradianceInRange[i, 0] * 100. 
    ENDFOR 
    
    ; Get GOES class and convert to flux value
    goesClass = string(selectFlares[goesEventIndex].ST$CLASS)
    goesClassLinearScale = float(strmid(goesClass, 1))
    goesFlux = 0d
    IF strmid(goesClass, 0, 1) EQ 'C' THEN goesFlux = goesClassLinearScale * 1E-6
    IF strmid(goesClass, 0, 1) EQ 'M' THEN goesFlux = goesClassLinearScale * 1E-5
    IF strmid(goesClass, 0, 1) EQ 'X' THEN goesFlux = goesClassLinearScale * 1E-4
    ; goesFlux = goesClassLinearScale * (10.^(indgen(5)-8))[where(['A','B','C','M','X'] eq strmid(goesClass, 0, 1))] ; Amir's fancy one liner to do the same thing
    
    ; Store all relevant parameters into structure and save
    coronalDimmingStructure.GoesClass = string(selectFlares[goesEventIndex].ST$CLASS)
    coronalDimmingStructure.GoesFlux = goesFlux
    coronalDimmingStructure.EventStartTime = eventBeginBracket
    coronalDimmingStructure.StartIrradiance = eveIrradianceInRange[*, 0] ; TODO: Change index 0 to whatever the GOES start time is, since if leftBracketHours ≠ 0 then index 0 will be incorrect
    coronalDimmingStructure.EventPeakTime = timeOfPeakIrradiance
    coronalDimmingStructure.PeakIrradiance = peakIrradiance
    coronalDimmingStructure.EventMinimumTime = timeOfMinIrradiance
    coronalDimmingStructure.MinimumIrradiance = minIrradiance
    coronalDimmingStructure.EventDurationInSeconds = durationInSeconds
    coronalDimmingStructure.Slope = slope
    coronalDimmingStructure.PercentDepth = depth
    coronalDimmingOutput = (n_elements(coronalDimmingOutput) eq 0) ? coronalDimmingStructure : [coronalDimmingOutput, coronalDimmingStructure]
    
    message, /INFO, 'Dimming search: ' + strtrim(float(goesEventIndex) / numFlares * 100., 2) + '% complete'
    
  ENDFOR ; goesEventIndex loop
  
  SAVE, coronalDimmingOutput, FILENAME = saveloc + 'DimmingDatabaseResults.sav', /COMPRESS
  
ENDIF ELSE $ ; useSaveFile ≠ 1
  RESTORE, saveloc + 'DimmingDatabaseResults.sav'

IF producePlot EQ 1 THEN BEGIN
  ; Remove zeros
  FOR i = 0, 29 DO BEGIN
    zeroIndices = where(coronalDimmingOutput.Slope[i] EQ 0)
    coronalDimmingOutput[zeroIndices].Slope[i] = !VALUES.F_NAN
    zeroIndices = where(coronalDimmingOutput.EventDurationInSeconds[i] LT 300)
    coronalDimmingOutput[zeroIndices].EventDurationInSeconds[i] = !VALUES.F_NAN
  ENDFOR
  
  ; Produce plot
  numLines = n_elements(datalines[0].line_irradiance)
  FOR i = 0, numLines - 1 DO BEGIN
    ; Compute correlation coefficients
    
    finiteSlope = where(finite(coronalDimmingOutput.Slope[i]) EQ 1, finiteSlopeCount)
    IF finiteSlopeCount GT 1 THEN BEGIN
      spearmanCoefficientSlope = r_correlate(coronalDimmingOutput[finiteSlope].GoesFlux, coronalDimmingOutput[finiteSlope].Slope[i])
      pearsonCoefficientSlope = correlate(coronalDimmingOutput[finiteSlope].GoesFlux, coronalDimmingOutput[finiteSlope].Slope[i])
    ENDIF
    
    finiteSlope1PercentDepthThreshold = where(finite(coronalDimmingOutput.Slope[i]) EQ 1 AND coronalDimmingOutput.PercentDepth[i] GE 1, finiteSlope1PercentDepthThresholdCount)
    IF finiteSlope1PercentDepthThresholdCount GT 1 THEN BEGIN
      spearmanCoefficientSlope1PercentDepthThreshold = r_correlate(coronalDimmingOutput[finiteSlope1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope1PercentDepthThreshold].Slope[i])
      pearsonCoefficientSlope1PercentDepthThreshold = correlate(coronalDimmingOutput[finiteSlope1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope1PercentDepthThreshold].Slope[i])
    ENDIF
    
    finiteSlope2PercentDepthThreshold = where(finite(coronalDimmingOutput.Slope[i]) EQ 1 AND coronalDimmingOutput.PercentDepth[i] GE 2, finiteSlope2PercentDepthThresholdCount)
    IF finiteSlope2PercentDepthThresholdCount GT 1 THEN BEGIN
      spearmanCoefficientSlope2PercentDepthThreshold = r_correlate(coronalDimmingOutput[finiteSlope2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope2PercentDepthThreshold].Slope[i])
      pearsonCoefficientSlope2PercentDepthThreshold = correlate(coronalDimmingOutput[finiteSlope2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope2PercentDepthThreshold].Slope[i])
    ENDIF
    
    finiteDuration = where(finite(coronalDimmingOutput.EventDurationInSeconds[i]) EQ 1, finiteDurationCount)
    IF finiteDurationCount GT 1 THEN BEGIN
      spearmanCoefficientDuration = r_correlate(coronalDimmingOutput[finiteDuration].GoesFlux, coronalDimmingOutput[finiteDuration].EventDurationInSeconds[i])
      pearsonCoefficientDuration = correlate(coronalDimmingOutput[finiteDuration].GoesFlux, coronalDimmingOutput[finiteDuration].EventDurationInSeconds[i])
    ENDIF
    
    finiteDuration1PercentDepthThreshold = where(finite(coronalDimmingOutput.EventDurationInSeconds[i]) EQ 1 AND coronalDimmingOutput.PercentDepth[i] GE 1, finiteDuration1PercentDepthThresholdCount)
    IF finiteDuration1PercentDepthThresholdCount GT 1 THEN BEGIN
      spearmanCoefficientDuration1PercentDepthThreshold = r_correlate(coronalDimmingOutput[finiteDuration1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration1PercentDepthThreshold].EventDurationInSeconds[i])
      pearsonCoefficientDuration1PercentDepthThreshold = correlate(coronalDimmingOutput[finiteDuration1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration1PercentDepthThreshold].EventDurationInSeconds[i])
    ENDIF
    
    finiteDuration2PercentDepthThreshold = where(finite(coronalDimmingOutput.EventDurationInSeconds[i]) EQ 1 AND coronalDimmingOutput.PercentDepth[i] GE 2, finiteDuration2PercentDepthThresholdCount)
    IF finiteDuration2PercentDepthThresholdCount GT 1 THEN BEGIN
      spearmanCoefficientDuration2PercentDepthThreshold = r_correlate(coronalDimmingOutput[finiteDuration2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration2PercentDepthThreshold].EventDurationInSeconds[i])
      pearsonCoefficientDuration2PercentDepthThreshold = correlate(coronalDimmingOutput[finiteDuration2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration2PercentDepthThreshold].EventDurationInSeconds[i])
    ENDIF
    
    finiteDepth = where(finite(coronalDimmingOutput.PercentDepth[i]) EQ 1, finiteDepthCount)
    IF finiteDepthCount GT 1 THEN BEGIN
      spearmanCoefficientDepth = r_correlate(coronalDimmingOutput[finiteDepth].GoesFlux, coronalDimmingOutput[finiteDepth].PercentDepth[i])
      pearsonCoefficientDepth = correlate(coronalDimmingOutput[finiteDepth].GoesFlux, coronalDimmingOutput[finiteDepth].PercentDepth[i])
    ENDIF          
    
    ; Plot correlations 
    
    IF finiteSlopeCount GT 1 THEN BEGIN
      ; Slope vs GOES class
      p1 = plot(coronalDimmingOutput.GoesFlux, coronalDimmingOutput.Slope[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Slope Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteSlopeCount, 2) + ' events', $
                YTITLE = 'Slope [W/m!U2!N/s]', YRANGE = [0, 9E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientSlope[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientSlope[0], decimals=2))
      p1.Save, saveloc + 'SlopeVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
      p1.Close
    ENDIF
    
    IF finiteSlope1PercentDepthThresholdCount GT 1 THEN BEGIN
      ; Slope vs GOES class with 1% depth threshold
      p2 = plot(coronalDimmingOutput[finiteSlope1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope1PercentDepthThreshold].Slope[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Slope For ≥ 1% Depth Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteSlope1PercentDepthThresholdCount, 2) + ' events', $
                YTITLE = 'Slope [W/m!U2!N/s]', YRANGE = [0, 9E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientSlope1PercentDepthThreshold[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientSlope1PercentDepthThreshold[0], decimals=2))
      p2.Save, saveloc + 'SlopeWith1PercentDepthThresholdVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
      p2.Close
    ENDIF
    
    IF finiteSlope2PercentDepthThresholdCount GT 1 THEN BEGIN
      ; Slope vs GOES class with 2% depth threshold
      p3 = plot(coronalDimmingOutput[finiteSlope2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteSlope2PercentDepthThreshold].Slope[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Slope For ≥ 2% Depth Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteSlope2PercentDepthThresholdCount, 2) + ' events', $
                YTITLE = 'Slope [W/m!U2!N/s]', YRANGE = [0, 9E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientSlope2PercentDepthThreshold[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientSlope2PercentDepthThreshold[0], decimals=2))
      p3.Save, saveloc + 'SlopeWith2PercentDepthThresholdVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
      p3.Close
    ENDIF
    
    IF finiteDurationCount GT 1 THEN BEGIN
      ; Duration vs GOES class
      p4 = plot(coronalDimmingOutput.GoesFlux, coronalDimmingOutput.EventDurationInSeconds[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Dimming Duration Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteDurationCount, 2) + ' events', $
                YTITLE = 'Dimming Duration [seconds]', $;, YRANGE = [-2E-9, 2E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientDuration[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientDuration[0], decimals=2))
      p4.Save, saveloc + 'DurationVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
    ENDIF
    
    IF finiteDuration1PercentDepthThresholdCount GT 1 THEN BEGIN
      ; Duration vs GOES class with 1% depth threshold
      p5 = plot(coronalDimmingOutput[finiteDuration1PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration1PercentDepthThreshold].EventDurationInSeconds[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Dimming Duration For ≥ 1% Depth Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteDuration1PercentDepthThresholdCount, 2) + ' events', $
                YTITLE = 'Dimming Duration [seconds]', $;, YRANGE = [-2E-9, 2E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientDuration1PercentDepthThreshold[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientDuration1PercentDepthThreshold[0], decimals=2))
      p5.Save, saveloc + 'DurationWith1PercentDepthThresholdVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
    ENDIF
    
    IF finiteDuration2PercentDepthThresholdCount GT 1 THEN BEGIN
      ; Duration vs GOES class with 2% depth threshold
      p6 = plot(coronalDimmingOutput[finiteDuration2PercentDepthThreshold].GoesFlux, coronalDimmingOutput[finiteDuration2PercentDepthThreshold].EventDurationInSeconds[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Dimming Duration For ≥ 2% Depth Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteDuration2PercentDepthThresholdCount, 2) + ' events', $
                YTITLE = 'Dimming Duration [seconds]', $;, YRANGE = [-2E-9, 2E-9], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientDuration2PercentDepthThreshold[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientDuration2PercentDepthThreshold[0], decimals=2))
      p6.Save, saveloc + 'DurationWith2PercentDepthThresholdVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
    ENDIF
    
    IF finiteDepthCount GT 1 THEN BEGIN
      p7 = plot(coronalDimmingOutput.GoesFlux, coronalDimmingOutput.PercentDepth[i], SYMBOL = 'square', LINESTYLE = 'none', /BUFFER, $
                TITLE = 'Dimming %Depth Versus Flare Class for ' + coronalDimmingOutput[0].EveLineCenters[i] + 'Å, ' + strtrim(finiteDepthCount, 2) + ' events', $
                YTITLE = 'Dimming Depth [%]', YRANGE = [0, 10], $
                XTITLE = 'GOES Flare Class', XRANGE = [1E-5, 1E-3], XTICKNAME = ['M', 'X', ''], /XLOG)
      t1 = text(0.7, 0.7, 'Spearman = ' + number_formatter(spearmanCoefficientDepth[0], decimals=2))
      t2 = text(0.7, 0.65, 'Pearson = ' + number_formatter(pearsonCoefficientDepth[0], decimals=2))
      p7.Save, saveloc + 'PercentDepthVsFlareClass_' + coronalDimmingOutput[0].EveLineCenters[i] + '.png'
    ENDIF
    
    message, /INFO, 'Dimming vs GOES correlations: ' + strtrim((i + 1.) / numLines * 100., 2) + '% complete'
    
  ENDFOR ; loop over emission lines

ENDIF ; producePlot = 1

print, '-=Program normal completion in ' + strtrim(round(TOC()), 2) + ' seconds=-'
END