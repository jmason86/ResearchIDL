;+
; NAME:
;   AnalyzeDimmingPropagatedErrors
;
; PURPOSE:
;
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
;   2012/07/05: James Paul Mason: Wrote script.
;   2012/07/06: James Paul Mason: Added nothing.
;-
PRO AnalyzeDimmingPropagatedErrors

; Restore numbers
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/Event3/Warm correction/EVEScaledIrradiances.sav'
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/Event3/Warm correction/UncertaintiesCorrectedEVEDimmingCurves.sav'
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/Event3/Warm correction/UncertaintiesPerdiff.sav'
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/Event3/Warm correction/UncertaintiesScaledBrightCurve.sav'

; Get time
sod = (eveTimeJD - floor(eveTimeJD[0]) - 0.5) * 86400.
sodSubsetRangeIndices = where(sod GE 23650 AND sod LE 32890) ; Event 3 2011042_21010

; Plots
p = plot(sod/3600., UNCERTAINTIESPERDIFF171, TITLE = '171 Å Perdiff Uncertainty')
p1 = plot(sod/3600., UNCERTAINTIESSCALEDBRIGHTCURVES[*, 3], title = '171 by 284 scaled bright curve uncertainty')
p2 = plot(sod/3600., UNCERTAINTIESCORRECTEDEVEDIMMINGCURVES[*, 3], title = '171 by 284 corrected dimming curve uncertainty')
p3 = plot(sod/3600., CORRECTEDEVEDIMMINGCURVES[*, 3], title = '171 by 284 corrected dimming curve')
s1 = errorplot(sod[sodSubsetRangeIndices]/3600., CORRECTEDEVEDIMMINGCURVES[sodSubsetRangeIndices, 3], UNCERTAINTIESCORRECTEDEVEDIMMINGCURVES[sodSubsetRangeIndices, 3], $
               title = '171 by 284 corrected diming curve with uncertainties')

END