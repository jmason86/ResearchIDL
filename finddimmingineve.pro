;+
; NAME:
;   FindDimmingInEVE
;
; PURPOSE:
;   To find and characterize dimming events in the EVE light curves.
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   thresholdTimePriorFlareMinutes [float]: How long before a particular event does the last one need to have happened for this event to be 
;                                           considered independent. If the previous one was too recent, will use that events preflare irradiance. 
;                                           Default is 240 (4 hours). 
;
; KEYWORD PARAMETERS:
;   USE_SAVE_FILE: Set this to use the thermal corrected light curve IDL savesets rather than re-detect an recompute all of the dimming parameters.  
;                  Defaults to 0 (no). 
;   VERBOSE:       Set this to print processing messages to console.
;                  Defaults to 0 (no). 
;
; OUTPUTS:
;   dimmingParameters [Structure]:  Contains the start and end times of dimming, the time and value of peak and minimum irradiance,
;                                   the GOES flare magnitude, and slope of dimming
;   dimmingIrradiances [Structure]: Contains the EVE irradiance, line labels, and GOES class and peak time
;
; OPTIONAL OUTPUTS:
;
;
; RESTRICTIONS:
;   Requires solarsoft for obtaining GOES event list, and EVE software for loading EVE data, and EveCoreDimmingCorrection for applying thermal correction. 
;   Since this analysis is on static data sets, can save those as savesets and avoid solarsoft each time its run. Only need it if the source data change. 
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2017-03-21: James Paul Mason: Wrote script
;   2016-09-02: James Paul Mason: Updated eveDataLoc and saveloc and filled out this header more extensively
;   2016-09-22: James Paul Mason: Added depthThreshold optional input and created an new method for computing slope that is more
;   2016-09-26: James Paul Mason: Code now exports live curves for each wavelength for each event to disk as well
;   2016-09-30: James Paul Mason: Added EVE correction calculation (calls EveCoreDimmingCorrection.pro)
;   2016-10-08: James Paul Mason: Now store GOES events to IDL saveset so don't need solar soft. Got rid of depthThreshold optional input. 
;                                 Depths have to be positive else it's not a dimming by definition. Any filtering can be done in analysis phase. 
;   2016-10-11: James Paul Mason: Removed the NO_PLOTS keyword and moved all the plots to EveDimmingCatalogStatistics. 
;-
PRO FindDimmingInEVE, thresholdTimePriorFlareMinutes = thresholdTimePriorFlareMinutes, $
                      USE_SAVE_FILE = USE_SAVE_FILE, VERBOSE = VERBOSE
!EXCEPT = 0 ; Turn off annoying underflow / illegal operand messages even if it unfortunately supresses other messages

; Defaults
IF ~keyword_set(thresholdTimePriorFlareMinutes) THEN thresholdTimePriorFlareMinutes = 240.

; Start a timer for the program
ticFindDimmingInEve = tic()

; Defaults
IF USE_SAVE_FILE EQ !NULL THEN USE_SAVE_FILE = 0
IF VERBOSE EQ !NULL THEN VERBOSE = 0

; Setup
eveDataLoc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/EVE/'
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/'
measurementErrors = CalculateEVELinePrecision()

; Hard-coded parameters
leftBracketHours = 0.  ; Number of hours before GOES event start time to bracket EVE data, used for computing pre-flare irradiance ; FIXME: Need to determine good background
rightBracketHours = 4. ; Number of hours after GOES event start time to bracket EVE data given there's no event ocurring prior to this

; Load GOES event list from EVE day 1 (2010 April 30) to end of MEGS-A (2014 May 26)
IF ~file_test('/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events/GoesEventsMegsAEra.sav') THEN BEGIN
  GetGoesEvents, '2010-05-01T00:00:00Z', '2014-05-26T00:00:00Z', minFlareClass = 'C', filePathAndName = '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events/GoesEventsMegsAEra.sav' 
ENDIF
restore, '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/events/GoesEventsMegsAEra.sav'

; Load EVE lines from EVE day 1 (2010/120 April 30) to present, merged, and averaged to 1 minute and convert time to JD and human
restore, eveDataLoc + 'eve_lines_2010121-2014146 MEGS-A Mission.sav'
lineNames = strtrim(evemeta.linesmeta.name, 2) + ' ' + JPMPrintNumber(evemeta.linesmeta.wave_center * 10, /NO_DECIMALS)
numLines = n_elements(eveLines[0].line_irradiance)
eveDOYAndTime = eveLines.YYYYDOY + eveLines.SOD / 86400.d0
eveTimeJd = eve_yd_to_jd(eveDOYAndTime)
eveTimeHuman = JPMjd2iso(eveTimeJd, /NO_T_OR_Z)

; Get rid of bad EVE data
goodIndices = where(eveLines.line_irradiance[0] NE -1)
eveLines = eveLines[goodIndices]
eveDOYAndTime = eveDoyAndTime[goodIndices]
eveTimeJd = eveTimeJd[goodIndices]
eveTimeHuman = eveTimeHuman[goodIndices]

; Create array of EVE line centers in Å
eveLineCenters = ['94',  '131', '133', '171', '177', '180', '195', '202', '211', '256', $
                  '284', '304', '335', '361', '368', '446', '465', '499', '521', '526', $ 
                  '537', '554', '568', '584', '592', '596', '601', '625', '630', '719', $
                  '722', '770', '790', '836', '950', '973', '977', '1026', '1032']
                    
; Create the output structure
dimmingParameterStructure = {goesClass:'', goesFlux:0d, eveLineCenters:eveLineCenters, eventStartTime:'', startIrradiance:dblarr(numLines), $
                             eventPeakTime:strarr(numLines), peakIrradiance:dblarr(numLines), eventMinimumTime:strarr(numLines), flareInterrupt:0, $
                             eventDuration:fltarr(39), durationUnit:'s', slope:dblarr(numLines), slopeUncertainty:dblarr(numLines), slopeUnit:'W/m^2/s', $
                             depth:dblarr(numLines), depthUncertainty:dblarr(numLines), depthUnit:'%'}
dimmingIrradianceStructure = {irradiance:dblarr(numLines, rightBracketHours * 60. + 2), $ ; irradiance makes space for the max # of points in time, anything not used will be !NAN
                              wavelength:eveLineCenters, goesClass:'', goesPeakTime:''}  
dimmingIrradianceStructure.irradiance[*, *] = !VALUES.D_NAN

; Prepare 24 hour queue lists
preflareTimesLast24HoursJd = list()
preflareIrradiancesLast24Hours = list()
preflareUncertaintiesLast24Hours = list() 

; Begin loop through all GOES events
ticObject = tic()
FOR goesEventIndex = 0, numFlares - 1 DO BEGIN
  
  ; Zero flareInterrupt 
  flareInterrupt = 0 
  
  ; Update events in the last 24 hours - dequeue 
  IF n_elements(preflareTimesLast24HoursJd) GT 0 THEN BEGIN
    firstInQueue = preflareTimesLast24HoursJd[0]
    endTimeOfFirstInQueue = firstInQueue[1]
    WHILE goesEvents[goesEventIndex].eventStartTimeJd - endTimeOfFirstInQueue GT 1. DO BEGIN
      preflareTimesLast24HoursJd.remove, 0
      preflareIrradiancesLast24Hours.remove, 0
      preflareUncertaintiesLast24Hours.remove, 0
    ENDWHILE
  ENDIF
  
  ; Select EVE data from N1 minutes before goesStartTime to the sooner of [Next GOES event start or N2 hours after GOES start]
  eventBeginBracketJd = goesEvents[goesEventIndex].eventStartTimeJd - leftBracketHours / 24.
  IF goesEventIndex LT numFlares - 1 THEN BEGIN
    eventEndBracketJd = goesEvents[goesEventIndex].eventStartTimeJd + rightBracketHours / 24. < goesEvents[goesEventIndex + 1].eventStartTimeJd
    IF goesEvents[goesEventIndex].eventStartTimeJd + rightBracketHours / 24. GT goesEvents[goesEventIndex + 1].eventStartTimeJd THEN flareInterrupt = 1
  ENDIF ELSE BEGIN
    eventEndBracketJd = goesEvents[goesEventIndex].eventStartTimeJd + rightBracketHours / 24.
  ENDELSE
  eveInRangeIndices = where(eveTimeJd GE eventBeginBracketJd AND eveTimeJd LE eventEndBracketJd, numEveInRange)
  eveIrradianceInRange = eveLines[eveInRangeIndices].LINE_IRRADIANCE
  eveTimeJdInRange = eveTimeJd[eveInRangeIndices]
  
  ; Separately select EVE data for just pre-flare time
  evePreflareIndices = where(eveTimeJd GE goesEvents[goesEventIndex].eventStartTimeJd - (thresholdTimePriorFlareMinutes / 1440.) AND $
                             eveTimeJd LE goesEvents[goesEventIndex].eventStartTimeJd)
  evePreflareJd = eveTimeJd[evePreflareIndices]
  IF evePreflareIndices EQ [-1] THEN BEGIN
    IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ': No pre-flare EVE irradiance available for event at ' + goesEvents[goesEventIndex].eventStartTimeHuman
    CONTINUE 
  ENDIF
  
  ; Store index of the end time of the dimming window
  eventBeginBracketIndex = closest(eventBeginBracketJd, eveTimeJd, /LOWER)
  eventEndBracketIndex = closest(eventEndBracketJd, eveTimeJdInRange, /DECIDE)
  
  ; Convert time to seconds since start of event
  eveTimeSecondsSinceStart = (eveTimeJd[eveInRangeIndices] - eventBeginBracketJd) * 24. * 3600.
  
  ; Skip if no (or only 1 point of) EVE data in range found
  IF numEveInRange LE 1 THEN BEGIN
    eventBeginBracketJdHumanTime = JPMjd2iso(eventBeginBracketJd, /NO_T_OR_Z)
    eventEndBracketHumanTime = JPMjd2iso(eventEndBracketJd, /NO_T_OR_Z)
    IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ': No EVE data found between ' + eventBeginBracketJdHumanTime + ' and ' + eventEndBracketHumanTime
    CONTINUE
  ENDIF
  
  ; Get and store the peak time and irradiance value in EVE within the bracketed time range
  peakIrradiance = dblarr(numLines) & timeOfPeakIrradiance = dblarr(numLines) & indexOfPeak = intarr(numLines)
  FOR i = 0, numLines - 1 DO BEGIN
    peakIrradiance[i] = max(eveIrradianceInRange[i, *], indexOfPeakTemp)
    timeOfPeakIrradiance[i] = eveTimeJd[eveinRangeIndices[indexOfPeakTemp]]
    indexOfPeak[i] = indexOfPeakTemp
  ENDFOR
  
  ;;
  ; Grab the pre-flare irradiance in EVE and convert irradiance to percent change from pre-flare irradiance
  ; Then fit the data with a polynomial up to order 5
  ;; 
  preflareIrradiance = dblarr(numLines)
  preflareIrradianceUncertainty = fltarr(numLines)
  preflareTimeRangeJd = dblarr(2, numLines)
  preflareIndices = intarr(2, numLines)
  eveIrradianceRelative = eveIrradianceInRange
  eveIrradianceRelative[*, *] = !VALUES.F_NAN
  eveUncertainties = eveIrradianceRelative
  lightCurveFit = eveIrradianceRelative
  bestFit = strarr(numLines)
  bestFitChi = fltarr(numLines)
  fitError = eveIrradianceRelative
  fitValid = intarr(numLines)
  FOR i = 0, numLines - 1 DO BEGIN
    ; Determine pre-flare irradiance
    preflareIrradiance[i] = DeterminePreflareIrradiance(eveLines.line_irradiance[i], evePreflareJd, lineNames[i], goesEvents[goesEventIndex].eventStartTimeJd, $
                                                        preflareTimesLast24HoursJd, preflareIrradiancesLast24Hours, preflareUncertaintiesLast24Hours, $ 
                                                        thresholdTimePriorFlareMinutes = thresholdTimePriorFlareMinutes, $
                                                        preflareTimeRangeJdOut = preflareTimeRangeJdThisLine, preflareIndicesOut = preflareIndicesThisLine, $
                                                        preflareIrradianceUncertaintyOut = preflareIrradianceUncertaintyThisLine, VERBOSE = VERBOSE)
    preflareIrradianceUncertainty[i] = preflareIrradianceUncertaintyThisLine
    preflareTimeRangeJd[*, i] = preflareTimeRangeJdThisLine
    preflareIndices[*, i] = preflareIndicesThisLine
    
    ; Compute relative irradiance and uncertainty
    eveIrradianceRelative[i, *] = perdiff(preflareIrradiance[i], eveIrradianceInRange[i, *], measurementError = measurementErrors[i], uncertaintiesOut = uncertaintiesPerdiff)
    eveUncertainties[i, *] = temporary(uncertaintiesPerdiff)
    
    ; Fit light curve with a polynomial up to order 5
    lightCurveFit[i, *] = AutomaticFitCoronalDimmingLightCurve(eveTimeJdInRange, eveIrradianceRelative[i, *], eveUncertainties[i, *], chiThreshold = 10.0, $ 
                                                               bestFitOut = bestFitTemp, bestFitChiOut = bestFitChiTemp, fitErrorOut = fitErrorTemp, fitValidOut = fitValidTemp, $
                                                               VERBOSE = VERBOSE)
    bestFit[i] = temporary(bestFitTemp)
    bestFitChi[i] = temporary(bestFitChiTemp)
    fitError[i, *] = temporary(fitErrorTemp)
    fitValid[i] = temporary(fitValidTemp)
    
    ; Fix for occasional overwrite of eveTimeJdInRange.. not sure why it gets overwritten
    IF n_elements(eveTimeJdInRange) EQ 1 THEN eveTimeJdInRange = eveTimeJd[eveInRangeIndices]
  ENDFOR
  
  ; Enqueue pre-flare irradiances to rolling queue list of last 24 hours' events
  preflareTimesLast24HoursJd.add, preflareTimeRangeJd
  preflareIrradiancesLast24Hours.add, preflareIrradiance
  preflareUncertaintiesLast24Hours.add, preflareIrradianceUncertainty
  
  ; Do the thermal correction for the dimming lines
;  eventName = 'Event ' + strtrim(goesEventIndex + 1, 2)
;  correctedSaveloc = '/Users/' + getenv('username') + $
;                      '/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/Event 2/Thermal Correction/' + $eventName
;  IF ~keyword_set(USE_SAVE_FILE) THEN EVECoreDimmingCorrection, !VALUES.F_NAN, !VALUES.F_NAN, JPMjd2sod(goesEventPeakTimesJd[goesEventIndex]), eventName, $
;                                                                eveLines = eveLines[eveInRangeIndices], reference_time = JPMjd2sod(eveTimeJd[evePreflareIndex]), $
;                                                                runOn = 'megsa' $
;  ELSE restore, correctedSaveloc + '/Warm correction/EVEScaledIrradiances.sav'
  
  ;;
  ; PARAMETERIZTION: DEPTH
  ;; 
  
  ; Get and store the minimum time and relative irradiance value in EVE within the EVE peak time and GOES end time
  depth = dblarr(numLines) & timeOfMinIrradiance = dblarr(numLines)
  FOR i = 0, numLines - 1 DO BEGIN
    
    ; Only look at times between peak time (rather than start time) and end time
    eveInMinRangeIndices = where(eveTimeJdInRange GE timeOfPeakIrradiance[i] AND eveTimeJd LE eventEndBracketJd)
    depth[i] = -min(lightCurveFit[i, eveInMinRangeIndices], indexOfMin)
    timeOfMinIrradiance[i] = eveTimeJdInRange[eveInMinRangeIndices[indexOfMin]]
    
    ; Throw out events that have too little dimming
    IF depth[i] LT 0 THEN BEGIN ; no dimming found at all
      depth[i] = !VALUES.D_NAN
      IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Depth for ' + lineNames[i] + ' line during GOES event starting at time ' + $
                                                                  goesEventStartTimesHuman[goesEventIndex] + $ 
                                                                  ' does not dim. Skipping this emission line for this event for all dimming calculations.'
    ENDIF
  ENDFOR
  
  ;;
  ; PARAMETERIZTION: DURATION
  ;;
  
  ; Compute and store the dimming duration: time between crossing 0 after the flare peak and crossing 0 indicating recovery
  duration = dblarr(numLines)
  durationStartTimeJd = dblarr(numLines)
  durationEndTimeJd = dblarr(numLines)
  FOR i = 0, numLines - 1 DO BEGIN
    
    ; Throw out events that have too shallow a depth
    IF ~finite(depth[i]) THEN CONTINUE ; i.e., depth = NAN
    
    ;dimmingSearchIndices = [indexOfPeak[i]:eventEndBracketIndex]
    first0CrossingIndex = JPMindexOfValueCrossing(lightCurveFit[i, *], 0., /FIRST) ; Searching * instead of dimmingSearchIndices for now 
    last0CrossingIndex = JPMindexOfValueCrossing(lightCurveFit[i, *], 0., /LAST) ; FIXME: Should be next 0 crossing, not last one ; Searching * instead of dimmingSearchIndices
    
    IF first0CrossingIndex EQ [-1] OR last0CrossingIndex EQ [-1] THEN BEGIN
      duration[i] = -1
      CONTINUE
    ENDIF
    
    durationStartTimeJd[i] = eveTimeJdInRange[first0CrossingIndex] ; Searching * instead of dimmingSearchIndices for now 
    durationEndTimeJd[i] = eveTimeJdInRange[last0CrossingIndex] ; Searching * instead of dimmingSearchIndices for now 
    ;durationStartTimeJd[i] = eveTimeJdInRange[dimmingSearchIndices[first0CrossingIndex]] 
    ;durationEndTimeJd[i] = eveTimeJdInRange[dimmingSearchIndices[last0CrossingIndex]]
    
    durationInFractionalDays = eveTimeJd[last0CrossingIndex] - eveTimeJd[first0CrossingIndex] ; Searching * instead of dimmingSearchIndices for now 
    ;durationInFractionalDays = eveTimeJd[dimmingSearchIndices[last0CrossingIndex]] - eveTimeJd[dimmingSearchIndices[first0CrossingIndex]]
    duration[i] = durationInFractionalDays * 24. * 3600.

    IF duration[i] GT (rightBracketHours * 3600) THEN BEGIN
      IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Duration for GOES event starting at time ' + $
                                                                  goesEventStartTimesHuman[goesEventIndex] + $
                                                                  ' exceeds threshold of event start +' + strtrim(rightBracketHours, 2) + $
                                                                  ' hours. Skipping event for duration calculation.'
      CONTINUE
    ENDIF
    
  ENDFOR
  
  ;; 
  ; PARAMETERIZTION: SLOPE
  ;;
  
  ; Compute and store the slope
  ; Must find the period of time to compute the slope over -- find 0-crossing, expand outward both directions until slope is no longer negative
  slope = dblarr(numLines)
  slopeUncertainty = dblarr(numLines)
  slopeWindowTimesJd = dblarr(numLines, 2)
  slopeWindowMinMaxIndices = intarr(numLines, 2)
  FOR i = 0, numLines - 1 DO BEGIN
    
    ; Throw out events that have too shallow a depth
    IF ~finite(depth[i]) THEN CONTINUE ; i.e., depth = NAN
    
    ; Find 0-crossing index
    ;dimmingSearchIndices = [indexOfPeak[i]:eventEndBracketIndex]
    first0CrossingIndex = JPMindexOfValueCrossing(lightCurveFit[i, *], 0., /FIRST) ; Searching * instead of dimmingSearchIndices until can figure out sub-indexing
    
    IF first0CrossingIndex EQ [-1] THEN BEGIN
      IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' No 0-crossing indicating dimming for GOES event starting at time ' + $ 
                                                                  goesEventStartTimesHuman[goesEventIndex] + $
                                                                  '. Skipping event for slope calculation.'
      CONTINUE
    ENDIF
    
    ; Compute first derivative of the irradiance
    eveIrradianceDerivative = reform(deriv(eveTimeSecondsSinceStart, lightCurveFit[i, *]))
    eveIrradianceDerivativeUncertainty = derivsig(eveTimeSecondsSinceStart, lightCurveFit[i, *], 0.0, stddev(lightCurveFit[i, *]))
    
    ; Expand the slope computation window
    expandSlopeWindowRight = 1
    expandSlopeWindowLeft = 1
    slopeWindowIndices = first0CrossingIndex
    expandSlopeWindowIndex = 1
    
    WHILE expandSlopeWindowRight EQ 1 OR expandSlopeWindowLeft EQ 1 DO BEGIN
      
      ; Check that we're not expanding beyond the whole dimming window
      IF expandSlopeWindowRight NE 0 THEN BEGIN
        IF first0CrossingIndex + expandSlopeWindowIndex GT n_elements(eveIrradianceDerivative) - 1 THEN BEGIN
          IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Slope window expansion ran too far to the right'
          expandSlopeWindowRight = 0
        ENDIF
      ENDIF
      IF expandSlopeWindowLeft NE 0 THEN BEGIN
        IF first0CrossingIndex - expandSlopeWindowIndex LT 0 THEN BEGIN
          IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Slope window expansion ran too far to the left'
          expandSlopeWindowLeft = 0
        ENDIF
      ENDIF
      
      ; Expand to the right
      IF expandSlopeWindowRight THEN BEGIN
        IF eveIrradianceDerivative[first0CrossingIndex + expandSlopeWindowIndex] LT 0 THEN BEGIN 
          slopeWindowIndices = [slopeWindowIndices, first0CrossingIndex + expandSlopeWindowIndex]
        ENDIF ELSE expandSlopeWindowRight = 0
      ENDIF 
      
      ; Expand to the left
      IF expandSlopeWindowLeft THEN BEGIN
        IF eveIrradianceDerivative[first0CrossingIndex - expandSlopeWindowIndex] LT 0 THEN BEGIN
          slopeWindowIndices = [first0CrossingIndex - expandSlopeWindowIndex, slopeWindowIndices]
        ENDIF ELSE expandSlopeWindowLeft = 0
      ENDIF 
      
      expandSlopeWindowIndex++
    ENDWHILE
    
    ; Store minmax of slope window times
    ;slopeWindowMinMaxIndices[i, *] = minmax(dimmingSearchIndices[slopeWindowIndices])
    
    slopeWindowMinMaxIndices[i, *] = minmax(slopeWindowIndices)
    slopeWindowTimesJd[i, *] = minmax(eveTimeJdInRange[slopeWindowMinMaxIndices[i, *]])
    
    ; Compute single value slope
    slope[i] = mean(eveIrradianceDerivative[slopeWindowIndices])
    slopeUncertainty[i] = sqrt(SumOfSquares(eveIrradianceDerivativeUncertainty[slopeWindowIndices])) / n_elements(eveIrradianceDerivativeUncertainty[slopeWindowIndices])
  ENDFOR 
  
  ; Get GOES class and convert to flux value
  goesClass = string(selectFlares[goesEventIndex].ST$CLASS)
  goesClassLinearScale = float(strmid(goesClass, 1))
  goesFlux = 0d
  IF strmid(goesClass, 0, 1) EQ 'C' THEN goesFlux = goesClassLinearScale * 1E-6
  IF strmid(goesClass, 0, 1) EQ 'M' THEN goesFlux = goesClassLinearScale * 1E-5
  IF strmid(goesClass, 0, 1) EQ 'X' THEN goesFlux = goesClassLinearScale * 1E-4
  ; goesFlux = goesClassLinearScale * (10.^(indgen(5)-8))[where(['A','B','C','M','X'] eq strmid(goesClass, 0, 1))] ; Amir's fancy one liner to do the same thing
  
  ; Store all relevant parameters into structure
  dimmingParameterStructure.goesClass = string(selectFlares[goesEventIndex].ST$CLASS)
  dimmingParameterStructure.goesFlux = goesFlux
  dimmingParameterStructure.eventStartTime = JPMjd2iso(eventBeginBracketJd, /NO_T_OR_Z)
  dimmingParameterStructure.startIrradiance = preflareIrradiance
  dimmingParameterStructure.eventPeakTime = JPMjd2iso(timeOfPeakIrradiance, /NO_T_OR_Z)
  dimmingParameterStructure.peakIrradiance = peakIrradiance
  dimmingParameterStructure.eventMinimumTime = JPMjd2iso(timeOfMinIrradiance, /NO_T_OR_Z)
  dimmingParameterStructure.flareInterrupt = FlareInterrupt
  dimmingParameterStructure.eventDuration = duration
  dimmingParameterStructure.slope = slope
  dimmingParameterStructure.slopeUncertainty = slopeUncertainty
  dimmingParameterStructure.depth = depth
  dimmingParameters = (n_elements(dimmingParameters) EQ 0) ? dimmingParameterStructure : [dimmingParameters, dimmingParameterStructure]
  
  ; Store the irradiance into structure
  dimmingIrradianceStructure.irradiance = eveIrradianceRelative
  dimmingIrradianceStructure.goesClass = string(selectFlares[goesEventIndex].ST$CLASS)
  dimmingIrradianceStructure.goesPeakTime = goesEventPeakTimesHuman[goesEventIndex]
  dimmingIrradiances = (n_elements(dimmingIrradiances) EQ 0) ? dimmingIrradianceStructure : [dimmingIrradiances, dimmingIrradianceStructure]
  
  ; Produce plot of light curves with depth, duration, and slope highlighted/annotated
  p1 = errorplot(eveTimeJdInRange, eveIrradianceRelative[0, *], eveUncertainties[0, *], '2', /BUFFER, $
                 TITLE = 'Event ' + JPMPrintNumber(goesEventIndex + 1, /NO_DECIMALS) + ' at ' + goesEventPeakTimesHuman[goesEventIndex] + ' for ' + eveLineCenters[0]  + ' Å', $
                 XTITLE = 'Time [UTC]', XTICKUNITS = 'Hours', $
                 YTITLE = 'Relative Irradiance [%]')
  p0 = plot(p1.xrange, [0, 0], 'r--', /OVERPLOT)
  p2 = errorplot(eveTimeJdInRange, lightCurveFit[0, *], fitError[0, *], COLOR = 'orange', ERRORBAR_COLOR = 'orange', /OVERPLOT)
  a1 = arrow([durationStartTimeJd[0], durationEndTimeJd[0]], [0, 0], /DATA, ARROW_STYLE = 3, COLOR = 'medium sea green', THICK = 3)
  a2 = arrow([timeOfMinIrradiance[0], timeOfMinIrradiance[0]], [0, -depth[0]], /DATA, COLOR = 'dodger blue', THICK = 3)
  p3 = plot(eveTimeJdInRange[slopeWindowMinMaxIndices[0, 0]:slopeWindowMinMaxIndices[0, 1]], lightCurveFit[0, [slopeWindowMinMaxIndices[0, 0]:slopeWindowMinMaxIndices[0, 1]]], $
            COLOR = 'indigo', '3', /OVERPLOT)
  t1 = text(0.7, 0.84, bestFit[0], COLOR = 'orange')
  t2 = text(0.7, 0.80, JPMPrintNumber(depth[0]) + ' %', COLOR = 'dodger blue')
  t3 = text(0.7, 0.76, JPMPrintNumber(-slope[0], /SCIENTIFIC_NOTATION) + ' % s$^{-1}$', COLOR = 'indigo')
  t4 = text(0.7, 0.72, JPMPrintNumber(duration[0], /SCIENTIFIC_NOTATION ) + ' s' , COLOR = 'medium sea green')
  p1.save, saveloc + 'Processing Plots/' + p1.title.string + '.png'
  FOR lineIndex = 1, numLines - 1 DO BEGIN
    p1.SetData, eveTimeJdInRange, eveIrradianceRelative[lineIndex, *], eveUncertainties[lineIndex, *]
    p1.title = 'Event ' + JPMPrintNumber(goesEventIndex + 1, /NO_DECIMALS) + ' at ' + goesEventPeakTimesHuman[goesEventIndex] + ' for ' + eveLineCenters[lineIndex]  + ' Å'
    p2.SetData, eveTimeJdInRange, lightCurveFit[lineIndex, *], fitError[lineIndex, *]
    a1.SetData, [durationStartTimeJd[lineIndex], durationEndTimeJd[lineIndex]], [0, 0]
    a2.SetData, [timeOfMinIrradiance[lineIndex], timeOfMinIrradiance[lineIndex]], [0, -depth[lineIndex]]
    p3.SetData, eveTimeJdInRange[slopeWindowMinMaxIndices[lineIndex, 0]:slopeWindowMinMaxIndices[lineIndex, 1]], $
                lightCurveFit[lineIndex, [slopeWindowMinMaxIndices[lineIndex, 0]:slopeWindowMinMaxIndices[lineIndex, 1]]]
    t1.string = bestFit[lineIndex]
    t2.string = JPMPrintNumber(depth[lineIndex]) + ' %'
    t3.string = JPMPrintNumber(-slope[lineIndex], /SCIENTIFIC_NOTATION) + ' % s$^{-1}$'
    t4.string = JPMPrintNumber(duration[lineIndex], /SCIENTIFIC_NOTATION) + ' s' 
    p1.save, saveloc + 'Processing Plots/' + p1.title.string + '.png'
  ENDFOR
  
  ; Save the built up arrays of structures
  ; Saving at each iteration means lots of overwriting but this is necessary since the code is unlikely to ever complete fully
  ; and finish the loop. Instead, when the code or IDL crash, need to manually rename the savesets and then pick up the loop
  ; where it crashed. 
  save, dimmingParameters, FILENAME = saveloc + 'DimmingParameters.sav', /COMPRESS
  save, dimmingIrradiances, FILENAME = saveloc + 'DimmingIrradiances.sav', /COMPRESS
  
  progressBar = JPMProgressBar(float(goesEventIndex) / numFlares * 100., progressBar = progressBar, name = 'Dimming Search', $
                               ticObject = ticObject, runTimeText = runTimeText, etaText = etaText)
  
ENDFOR ; goesEventIndex loop

IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Program normal completion in ' + JPMPrintNumber(toc(ticFindDimmingInEve), /NO_DECIMALS) + ' seconds'

!EXCEPT = 1 ; Turn warning messages back on
END