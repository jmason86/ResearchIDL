;+
; NAME:
;   EVECoreDimmingCorrection
;
; PURPOSE:
;   Messy code to figure out how to correct EVE 171Å to remove the irradiance spike and baseline change due to a flare in order to get 
;   something approximating the core dimming that can be gotten from AIA 171Å. 
;
; INPUTS:
;   yyyydoyStart [integer]:             The start date. Start time is 00:00:00 UTC. 
;   yyyydoyEnd   [integer]:             The end date. End time is 23:59:59 UTC. 
;   OR 
;   eveLines     [array of structures]: An array that indexes through time with a standard EVE lines product at each index (as returned from eve_merge_evl.pro). 
;                                       If this variable is provided, then it is not necessary to provide yyyydoyStart and yyyydoyEnd and vice versa. 
;   eventPeakSOD [integer]:             The rough second of day where the peak occurs. A 4 hour window surrounding this time will be used to find the peak
;   eventName    [string]:              Used for creating a unique directory. 
;   
; OPTIONAL INPUTS:
;   NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE [integer]: Determines the average of EVE data
;   REFERENCE_TIME [integer]:                        Second of day to use as reference. Nearest point in EVE timeseries will be used. 
;   measurementError [float]:                        If this value is provided, it will be passed to perdiff.pro and will calculate uncertainties for the correction. 
;   YRANGE [float, float]:                           Standard YRANGE optional input for the plot command. Intended for special case analysis where axis gets blown out. 
;   runOn [string]:                                  Set this to 'case' or 'twotwo' or 'megsa' to store into the Case Studies or Two Two Week Period or Automatic Dimming Database folder
; 
; KEYWORD PARAMETERS:
;   EXTRAPOLATE_PRE_FLARE: Set this keyword in order to extrapolate the pre-flare trend out to the end of the day. Intended for characterizing 
;                          the uncertainty of dimming depth parameterization. Uses 1 hour prior to reference_time to compute extrapolation. 
;
; OUTPUTS:
;   Plots of dimming lines corrected by brightening lines
;   Save file with corrected line and associated time in JD
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft if using the yyyydoyStart and yyyydoyEnd inputs rather than eveLines input
;   Requires perdiff.pro
;
; EXAMPLE:
;   EVECoreDimmingCorrection, 2011216, 2011217, 14220, '2011216_04AUG_0357_M9.3'
;   EVECoreDimmingCorrection, 2011158, 2011158, 24060, '2011158_07JUN_0641_M2.5', REFERENCE_TIME = 18000
;   
; MODIFICATION HISTORY:
;     2013/07/29: James Paul Mason: Wrote script. 
;     2014/05/22: James Paul Mason: Added eventPeakSOD input to remove hard coded search for peak
;     2015/06/03: James Paul Mason: Discovered a bug when doing 120 minutes around the eventPeakSOD. Fixed it.
;     2015/06/08: James Paul Mason: Added EXTRAPOLATE_PRE_FLARE keyword. 
;     2015/07/15: James Paul Mason: Code now exports the scaled bright curves as well, and output plot includes
;                                   raw dimming light curve, raw brightening, and the corrected
;     2015/07/28: James Paul Mason: Added YRANGE optional input to pass into the plot command. 
;     2015/08/05: James Paul Mason: Caught a mistake that was causing uncertaintines to be calculated wrong due to the peak index referencing the 
;                                   peak match window subarray rather than the full lightcurve array. 
;     2015/10/12: James Paul Mason: Introduced a fudge factor to deal with an event having a bright peak value of 0, that causes scaleFactor -> infinity. 
;                                   Now if brightPeak = 0, it forces brightPeak to 1E-3
;     2016/09/30: James Paul Mason: Added eveLines optional input as an alternative to specifying yyyydoyStart and yyyydoyEnd. Also changed caseOrTwoTwoWeek to 
;                                   runOn with additional optional input of 'megsa'.  
;                                   
;-
PRO EVECoreDimmingCorrection, yyyydoyStart, yyyydoyEnd, eventPeakSOD, eventName, $ 
                              eveLines = eveLines, NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE = number_of_10s_integrations_to_average, REFERENCE_TIME = reference_time, $ 
                              measurementError = measurementError, YRANGE = yRange, runOn = runOn, $
                              EXTRAPOLATE_PRE_FLARE = EXTRAPOLATE_PRE_FLARE
; TODO: eventPeakSOD should really be seconds since yyyydoyStart in case spanning multiple days but that's too annoying to deal with right now

; Defaults
IF ~keyword_set(NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE) THEN number_of_10s_integrations_to_average = 6
IF ~keyword_set(measurementError) THEN measurementError = CalculateEVEFeLinePrecision()
IF ~keyword_set(YRANGE) THEN yRange = -1
IF ~keyword_set(runOn) THEN runOn = 'twotwo'
!Except = 0 ; Disable annoying divide by 0, overflow, and illegal operand messages

; Setup
IF runOn EQ 'case' THEN saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/' + eventName + '/'
IF runOn EQ 'twotwo' THEN saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/' + eventName + '/Warm correction/'
IF runOn EQ 'megsa' THEN saveloc = '/Users/jama6159/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/' + eventName + '/Thermal Correction/'
spawn, 'mkdir -p ' + str_replace(saveloc, ' ', '\ ', /GLOBAL) ; Unix doesn't like spaces in path strings
eventNameParsed = ParsePathAndFilename(eventName)
IF eventNameParsed.path NE '' THEN BEGIN
  eventName = eventNameParsed.filename
  eventName = strmid(eventName, 0, 5) + ' #' + strmid(eventName, 5, strlen(eventName)) 
ENDIF ELSE BEGIN
  eventName = 'Event # N/A'
ENDELSE
numberBadData = 0 ; Counter to be printed at end of program

; Grab EVE data
IF eveLines EQ !NULL THEN eveLines = eve_merge_evl(yyyydoyStart, yyyydoyEnd, META = eveMeta, N_AVERAGE = number_of_10s_integrations_to_average) ; 10 second natural cadence averaged to specified number

; Deal with time
eveTimeTAI = eveLines.TAI
eveTimeJD = anytim2jd(eveLines.TAI)
eveTimeJD = eveTimeJD.int + eveTimeJD.frac
eveTimeSOD = (eveTimeJD - floor(eveTimeJD[0]) - 0.5) * 86400.
eventPeakIndex = closest(eventPeakSOD, eveTimeSOD)
yyyyDoy = strtrim(yyyyDoyStart, 2)
yyyymmdd = JPMyyyydoy2yyyymmdd(yyyyDoy, /RETURN_STRING)
hhmmss = JPMsod2hhmmss(eventPeakSod, /RETURN_STRING)
eventPeakSod = JPMPrintNumber(eventPeakSod, /NO_DECIMALS)

; If reference time provided, determine what index it corresponds to
IF ~keyword_set(REFERENCE_TIME) THEN referenceIndex = 0 ELSE BEGIN
  sod = eveLines.sod
  referenceIndex = closest(reference_time, sod, /DECIDE)
ENDELSE

; Convert EVE data to percent change from reference time
FOR i = 0, 29 DO eveLines[where(eveLines.line_irradiance[i] EQ -1)].line_irradiance[i] = !VALUES.F_NAN ; Get rid of bad data
percentEVE94  = perdiff(evelines[referenceIndex].line_irradiance[0],  eveLines.line_irradiance[0],  measurementError = measurementError[0], uncertaintiesOut = uncertaintiesPerdiff94)  ; brighten
percentEVE132 = perdiff(eveLines[referenceIndex].line_irradiance[2],  eveLines.line_irradiance[2],  measurementError = measurementError[1], uncertaintiesOut = uncertaintiesPerdiff132) ; brighten
percentEVE171 = perdiff(eveLines[referenceIndex].line_irradiance[3],  eveLines.line_irradiance[3],  measurementError = measurementError[2], uncertaintiesOut = uncertaintiesPerdiff171) ; dimm
percentEVE177 = perdiff(eveLines[referenceIndex].line_irradiance[4],  eveLines.line_irradiance[4],  measurementError = measurementError[3], uncertaintiesOut = uncertaintiesPerdiff177) ; dimm 
percentEVE180 = perdiff(eveLines[referenceIndex].line_irradiance[5],  eveLines.line_irradiance[5],  measurementError = measurementError[4], uncertaintiesOut = uncertaintiesPerdiff180) ; dimm
percentEVE195 = perdiff(eveLines[referenceIndex].line_irradiance[6],  eveLines.line_irradiance[6],  measurementError = measurementError[5], uncertaintiesOut = uncertaintiesPerdiff195) ; dimm
percentEVE202 = perdiff(eveLines[referenceIndex].line_irradiance[7],  eveLines.line_irradiance[7],  measurementError = measurementError[6], uncertaintiesOut = uncertaintiesPerdiff202) ; dimm 
percentEVE211 = perdiff(eveLines[referenceIndex].line_irradiance[8],  eveLines.line_irradiance[8],  measurementError = measurementError[7], uncertaintiesOut = uncertaintiesPerdiff211) ; sometimes dim, sometimes brighten
percentEVE284 = perdiff(eveLines[referenceIndex].line_irradiance[10], eveLines.line_irradiance[10], measurementError = measurementError[8], uncertaintiesOut = uncertaintiesPerdiff284) ; brighten
percentEVE335 = perdiff(eveLines[referenceIndex].line_irradiance[12], eveLines.line_irradiance[12], measurementError = measurementError[9], uncertaintiesOut = uncertaintiesPerdiff335) ; brighten

; Store uncertainties in arrays
uncertaintiesDimLines = [[uncertaintiesPerdiff171], [uncertaintiesPerdiff177], [uncertaintiesPerdiff180], [uncertaintiesPerdiff195], [uncertaintiesPerdiff202], [uncertaintiesPerdiff211]]
uncertaintiesBrightLines = [[uncertaintiesPerdiff94], [uncertaintiesPerdiff132], [uncertaintiesPerdiff211], [uncertaintiesPerdiff284], [uncertaintiesPerdiff335]]

; Extrapolate pre-flare trend
IF keyword_set(EXTRAPOLATE_PRE_FLARE) THEN BEGIN
  ; Truncate to extrapolation period
  hourPriorToReferenceIndex = closest(reference_time - 3600, sod, /DECIDE)
  sodPeriod = sod[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod171 = percentEVE171[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod177 = percentEVE177[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod180 = percentEVE180[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod195 = percentEVE195[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod202 = percentEVE202[hourPriorToReferenceIndex:referenceIndex]
  extrapolationPeriod211 = percentEVE211[hourPriorToReferenceIndex:referenceIndex]
  
  ; Use linear extrapolation
  fitParameters171 = poly_fit(sodPeriod, extrapolationPeriod171, 1)
  fitParameters177 = poly_fit(sodPeriod, extrapolationPeriod177, 1)
  fitParameters180 = poly_fit(sodPeriod, extrapolationPeriod180, 1)
  fitParameters195 = poly_fit(sodPeriod, extrapolationPeriod195, 1)
  fitParameters202 = poly_fit(sodPeriod, extrapolationPeriod202, 1)
  fitParameters211 = poly_fit(sodPeriod, extrapolationPeriod211, 1)
ENDIF

; Make 2D arrays for light curves that dimm versus brighten
dimmingCurves = [[percentEVE171], [percentEVE177], [percentEVE180], [percentEVE195], [percentEVE202], [percentEVE211]]
brighteningCurves = [[percentEVE94], [percentEVE132], [percentEVE211], [percentEVE284], [percentEVE335]]
dimNames = ['171Å', '177Å', '180Å', '195Å', '202Å', '211Å']
brightNames = ['94Å', '132Å', '211Å', '284Å', '335Å']

; Loop through all dimming and brightening lines
numberOfCombinations = n_elements(dimNames) * n_elements(brightNames)
correctedEVEDimmingCurves = fltarr(n_elements(eveTimeJD), numberOfCombinations)
scaledBrightCurves = fltarr(n_elements(eveTimeJD), n_elements(brightNames))
IF keyword_set(EXTRAPOLATE_PRE_FLARE) THEN correctedPreFlareTrendFits = fltarr(2, numberOfCombinations)
uncertaintiesScaledBrightCurves = fltarr(n_elements(eveTimeJD), numberOfCombinations)
uncertaintiesCorrectedEVEDimmingCurves = fltarr(n_elements(eveTimeJD), numberOfCombinations)
dimByBrightNames = strarr(numberOfCombinations)
k = 0
ticObjectCorrection = TIC()
FOR i = 0, n_elements(dimNames) - 1 DO BEGIN  
  uncertaintiesDimLine = uncertaintiesDimLines[*, i]
  
  FOR j = 0, n_elements(brightNames) - 1 DO BEGIN
    uncertaintiesBrightLine = uncertaintiesBrightLines[*, j]
    
    ; Compute scale factor - 120 minutes on either side of event peak time
    indexCorrespondingto120Minutes = 120 * 60 / (number_of_10s_integrations_to_average * 10)
    dimPeak = max(dimmingCurves[((eventPeakIndex - indexCorrespondingto120Minutes) > 0):((eventPeakIndex + indexCorrespondingto120Minutes) < n_elements(eveTimeSOD)-1), i], dimMaxIndex)
    brightPeak = max(brighteningCurves[((eventPeakIndex - indexCorrespondingto120Minutes) > 0):((eventPeakIndex + indexCorrespondingto120Minutes) < n_elements(eveTimeSOD)-1), j], brightMaxIndex)
    IF brightPeak EQ 0 THEN brightPeak = 1E-3 ; Else will get scaleFactor = infinity
    scaleFactor = dimPeak / brightPeak 
    
    ; Skip if no valid data in the window of interest
    IF ~finite(scaleFactor) THEN BEGIN
      numberBadData++
      CONTINUE
    ENDIF
    
    ; Perform correction
    timeShiftByIndex = dimMaxIndex - brightMaxIndex
    scaledBrightCurve = shift(brighteningCurves[*, j], timeShiftByIndex) * scaleFactor
    scaledBrightCurves[*, j] = scaledBrightCurve
    correctedEVEDimmingCurves[*, k] = dimmingCurves[*, i] - scaledBrightCurve
    dimByBrightNames[k] = dimNames[i] + ' Corrected By ' + brightNames[j]
    
    ; Extrapolation with scale factor
    IF keyword_set(EXTRAPOLATE_PRE_FLARE) THEN BEGIN
      extrapolationPeriod = correctedEVEDimmingCurves[hourPriorToReferenceIndex:referenceIndex, k]
      correctedPreFlareTrendFits[*, k] = poly_fit(sodPeriod, extrapolationPeriod, 1)
    ENDIF
    
    ; Store the left and right times of the 4 hour time window
    timeWindowLeftJD = eveTimeJD[(eventPeakIndex - indexCorrespondingto120Minutes) > 0]
    timeWindowRightJD = eveTimeJD[(eventPeakIndex + indexCorrespondingto120Minutes) < n_elements(eveTimeSOD)-1]

    ; Create plots of corrected lines
    p1 = plot(eveTimeJD, dimmingCurves[*, i], 'r2', /BUFFER, $
              TITLE = eventName + ', YYYYMMDD: ' + yyyymmdd + ', Peak Time: ' + hhmmss, $
              XTITLE = 'Hour', XTICKUNITS = 'Hours', $
              YTITLE = '% Change', YRANGE = yRange, $
              NAME = dimNames[i])
    p2 = plot(eveTimeJD, scaledBrightCurve, 'b2', /OVERPLOT, $
              NAME = 'Scaled ' + brightNames[j])
    p3 = plot(eveTimeJD, correctedEVEDimmingCurves[*, k], '2', /OVERPLOT, $
              NAME = 'Corrected')
    fullYRange = minmax([p1.yrange, p2.yrange, p3.yrange])
    poly1 = polygon([[timeWindowLeftJD, fullYRange[1]], [timeWindowRightJD, fullYRange[1]], [timeWindowRightJD, fullYRange[0]], [timeWindowLeftJD, fullYRange[0]]], /DATA, $
                    /FILL_BACKGROUND, FILL_COLOR = 'lime green', TRANSPARENCY = 50, $
                    NAME = 'Peak Match Window')
    t1 = text((timeWindowRightJD - timeWindowLeftJD) / 2. + timeWindowLeftJD, fullYRange[0], '$Peak \n Match \n Window$', FONT_SIZE = 8, /DATA, ALIGNMENT = 0.5, COLOR = 'white')
    p4 = plot([eveTimeJD[referenceIndex], eveTimeJD[referenceIndex]], fullYRange, '--', YRANGE = fullYRange, /OVERPLOT)
    t2 = text(eveTimeJD[referenceIndex], p1.yrange[1], 'Pre-flare Time', /DATA, ORIENTATION = 90, ALIGNMENT = 1)
    leg = legend(TARGET = [p1, p2, p3, poly1], POSITION = [0.92, 0.88]) ; FIXME: Remove this! Just for a plot in my 2-2 week paper
    p1.save, saveloc + dimNames[i] + ' by ' + brightNames[j] + '.png'
    
    ; Find the dimMaxIndex in the full array, not just the peak match window subarray (which is how its defined above)
    dimMaxIndexAbsolute = where(dimmingCurves[*, i] EQ dimPeak)
    brightMaxIndexAbsolute = where(brighteningCurves[*, j] EQ brightPeak)
    
    ; Compute uncertainty
    uncertaintyScaleFactorSquared = (uncertaintiesDimLine[dimMaxIndexAbsolute] / brightPeak)^2 + (uncertaintiesBrightLine[brightMaxIndexAbsolute] * dimPeak / brightPeak^2)^2
    uncertaintyScaleFactorSquared = uncertaintyScaleFactorSquared[0] ; Deals with an uncommon error where the return of the expression above is a 1-element array rather than just a float
    uncertaintiesScaledBrightCurves[*, k] = sqrt(uncertaintiesBrightLine^2 * scaleFactor^2 + uncertaintyScaleFactorSquared * scaledBrightCurve^2)
    uncertaintiesCorrectedEVEDimmingCurves[*, k] = sqrt(uncertaintiesDimLine^2 + uncertaintiesScaledBrightCurves[*, k]^2)
    
    IF total(finite(uncertaintiesCorrectedEVEDimmingCurves[*, k])) EQ 0 THEN STOP ; DEBUG: Catch potential problems with uncertainty calculation
    
    ;progressBarCorrection = JPMProgressBar(100. * (k + 1) / numberOfCombinations, progressBar = progressBarCorrection, NAME = 'Dimming Correction Progress', $
    ;                                       ticObject = ticObjectCorrection, runTimeText = runTimeTextCorrection, etaText = etaTextCorrection)
    k++
  ENDFOR
ENDFOR

; Output 
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, correctedEVEDimmingCurves, eveTimeJD, dimByBrightNames, dimmingCurves, brighteningCurves, FILENAME = saveloc + 'EVEScaledIrradiances.sav', /COMPRESS
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, scaledBrightCurves, brightNames, FILENAME = saveloc + 'EVEScaledBrightCurves.sav', /COMPRESS
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, eveLines, eveMeta, FILENAME = saveloc + 'EVELines.sav', /COMPRESS
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, uncertaintiesPerdiff94, uncertaintiesPerdiff132, uncertaintiesPerdiff171, uncertaintiesPerdiff177, uncertaintiesPerdiff180, uncertaintiesPerdiff195, $ 
      uncertaintiesPerdiff202, uncertaintiesPerdiff211, uncertaintiesPerdiff284, uncertaintiesPerdiff335, FILENAME = saveloc + 'UncertaintiesPerdiff.sav', /COMPRESS
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, uncertaintiesScaledBrightCurves, FILENAME = saveloc + 'UncertaintiesScaledBrightCurve.sav', /COMPRESS
save, yyyyDoy, yyyymmdd, hhmmss, eventPeakSod, uncertaintiesCorrectedEVEDimmingCurves, FILENAME = saveloc + 'UncertaintiesCorrectedEVEDimmingCurves.sav', /COMPRESS
IF keyword_set(EXTRAPOLATE_PRE_FLARE) THEN save, fitParameters171, fitParameters177, fitParameters180, fitParameters195, fitParameters202, fitParameters211, correctedPreFlareTrendFits, $
      FILENAME = saveloc + 'ExtrapolatedPreFlareTrend.sav', /COMPRESS

message, /INFO, JPMsystime() + ' Program completion with ' + JPMPrintNumber(numberBadData, /NO_DECIMALS) + ' cases of no data in window of interest'

!Except = 1
END