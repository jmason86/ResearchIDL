;+
; NAME:
;   EVEDimmingWithTheory
;
; PURPOSE:
;   Quick analysis with Barbara Thompson looking at dimming 
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
;   2015/09/14: James Paul Mason: Wrote script.
;-
PRO EVEDimmingWithTheory

; Setup
eventNumber = 14
eventNumberString = 'Event' + strtrim(eventNumber, 2)

; Restore a light curve
restore, '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/' + eventNumberString + '/Warm correction/EVELines.sav'
timeHours = eveLines.SOD /3600.

; Trim time series
IF eventNumber EQ 3 THEN eventIndices = [266:334]
IF eventNumber EQ 9 THEN eventIndices = [14:96]
IF eventNumber EQ 14 THEN eventIndices = [213:288]
fe9full = evelines.line_irradiance[3]
fe9 = fe9full[eventIndices]

; Event 9 masking of flare
IF eventNumber EQ 9 THEN fe9[where(fe9 GT 6.28E-5)] = !VALUES.F_NAN

; Compute I0
I0 = max(fe9) - min(fe9) 
IF eventNumber EQ 9 THEN I0 = fe9[0] - min(fe9)

; Compute fifth root of I0 over I(t), which should be proportional to h(t)/h0
I0OverItFifthRoot = double((I0 / fe9))^(1/5.)

; Trimmed time array
timeHours = timeHours[eventIndices]

; Create plot
p = plot(timeHours, I0OverItFifthRoot, TITLE = 'I0OverItFifthRoot ' + eventNumberString,  $
         XTITLE = 'UTC Time [Hour]')
STOP



END