;+
; NAME:
;   AutomaticFitCoronalDimmingLightCurve
;
; PURPOSE:
;   Automatically select the best polynomial fit for the input light curve and return it along with the reduced chi^2. 
;   Parameterization of the light curve fit must be handled external to this function. 
;
; INPUTS:
;   timeJd [dblarr]:        The array of times corresponding to the points in lightCurve
;   lightCurve [fltarr]:    An array of irradiance data for a single emission line/wavelength as a function of time
;   uncertainties [fltarr]: An array of the uncertainty on each of the irradiance points in lightCurve
;   
; OPTIONAL INPUTS:
;   chiThreshold [float]: Set this to the maximum reduced chi^2 value acceptable for fits. If the best fit chi^2 is > chiThreshold, 
;                         this function will return [-1]. Default value is 6.0. 
;
; KEYWORD PARAMETERS:
;   VERBOSE: Set this to print processing messages to console
;
; OUTPUTS:
;   lightCurveFit [fltarr]: An array with the best fitted y-values at each point in timeJd. May contain fewer elements than lightCurve input
;                           if lightCurve contains NANs. 
;
; OPTIONAL OUTPUTS:
;   bestFitOut [string]:   A string with the name of the best fit by reduced chi squared.
;   bestFitChiOut [float]: The value of the best fit reduced chi squared
;   fitErrorOut [fltarr]:  An array with the same dimensions as lightCurveFit that contains the uncertainties of each point
;   fitValidOut [integer]: 0 means the best fit had a reduced chi^2 > chiThreshold. 1 means the opposite. 
;
; RESTRICTIONS:
;   Requires JPMPrintNumber.pro and closest.pro
;   Requires SumOfSquares function.
;
; EXAMPLE:
;   lightCurveFit = AutomaticFitCoronalDimmingLightCurve(timeJd, eveIrradianceInRange[3, *], eveIrradianceInRangeUncertainties[3, *], $ 
;                                                        bestFitOut = bestFit, bestFitChiOut = bestFitChi)
;
; MODIFICATION HISTORY:
;   2016-10-05: James Paul Mason: Wrote procedure, largely based on FitCoronalDimmingLightCurve.pro
;   2016-10-06: James Paul Mason: made poly_fit for 2nd and 3rd order also use /DOUBLE keyword and STATUS optional output to avoid occasional NaN error estimates
;-
FUNCTION AutomaticFitCoronalDimmingLightCurve, timeJd, lightCurve, uncertainties, $
                                               chiThreshold = chiThreshold, $
                                               bestFitOut = bestFit, bestFitChiOut = bestFitChi, fitErrorOut = fitError, fitValidOut = fitValid, $ 
                                               VERBOSE = VERBOSE

; Defaults
IF chiThreshold EQ !NULL THEN chiThreshold = 6.

; Check validity of input
IF total(lightCurve) EQ n_elements(lightCurve) * lightCurve[0] THEN BEGIN
  IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' Input light curve all zeros'
  lightCurveFit = [-1]
  bestFit = ''
  bestFitChi = -1
  fitError = [-1]
  fitValid = 0
  return, lightCurveFit
ENDIF

; Ignore NANs if they exist
uncertainties = uncertainties[where(finite(lightCurve))]
timeJd = timeJd[where(finite(lightCurve))]
lightCurve = lightCurve[where(finite(lightCurve))]

; Convert time in JD to minutes since start since poly_fit can't handle the long format of JD
; Note: It doesn't produce any error, it just returns garbage and it took a lot of debugging to figure this out. 
timeMinutesSinceStart = (timeJd - timeJd[0]) * 24. * 60. 

; 2nd Order Poly Fit (Parabola)
poly2Parameters = poly_fit(timeMinutesSinceStart, lightCurve, 2, poly2Curve, CHISQ = poly2Chi, MEASURE_ERRORS = uncertainties, YBAND = poly2Error, /DOUBLE, STATUS = status)
allReducedChis = poly2Chi / (n_elements(timeMinutesSinceStart) - 2.)

; 3rd Order Poly Fit
poly3Parameters = poly_fit(timeMinutesSinceStart, lightCurve, 3, poly3Curve, CHISQ = poly3Chi, MEASURE_ERRORS = uncertainties, YBAND = poly3Error, /DOUBLE, STATUS = status)
allReducedChis = [allReducedChis, poly3Chi / (n_elements(timeMinutesSinceStart) - 5. - 1.)]

; 4th Order Poly Fit
poly4Parameters = poly_fit(timeMinutesSinceStart, lightCurve, 4, poly4Curve, CHISQ = poly4Chi, MEASURE_ERRORS = uncertainties, YBAND = poly4Error, /DOUBLE, STATUS = status)
allReducedChis = [allReducedChis, poly4Chi / (n_elements(timeMinutesSinceStart) - 5. - 1.)]

; 5th Order Poly Fit
poly5Parameters = poly_fit(timeMinutesSinceStart, lightCurve, 5, poly5Curve, CHISQ = poly5Chi, MEASURE_ERRORS = uncertainties, YBAND = poly5Error, /DOUBLE, STATUS = status)
allReducedChis = [allReducedChis, poly5Chi / (n_elements(timeMinutesSinceStart) - 5. - 1.)]

; Determine which reduced chi squared was the best fit
bestChiIndex = closest(1.0, allReducedChis)
bestFitChi = allReducedChis[bestChiIndex]

; Set the valid flag for the fit

fitValid = 1
IF bestFitChi GT chiThreshold THEN BEGIN
  IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' All fits had reduced chi^2 > ' + JPMPrintNumber(chiThreshold)
  fitValid = 0
ENDIF
IF bestChiIndex EQ [-1] THEN BEGIN
  IF keyword_set(VERBOSE) THEN message, /INFO, JPMsystime() + ' All fits were NAN'
  lightCurveFit = [-1]
  bestFit = ''
  bestFitChi = -1
  fitError = [-1]
  fitValid = 0
  return, lightCurveFit
ENDIF

; Assign name for best fit and the array to return
IF bestChiIndex NE [-1] THEN BEGIN
  CASE bestChiIndex OF
    0: BEGIN
      bestFit = '2nd Order Poly'
      lightCurveFit = poly2Curve
      fitError = poly2Error
    END
    1: BEGIN
      bestFit = '3rd Order Poly'
      lightCurveFit = poly3Curve
      fitError = poly3Error
    END
    2: BEGIN
      bestFit = '4th Order Poly'
      lightCurveFit = poly4Curve
      fitError = poly4Error
    END
    3: BEGIN
      bestFit = '5th Order Poly'
      lightCurveFit = poly5Curve
      fitError = poly5Error
    END
  ENDCASE
ENDIF

return, lightCurveFit

END