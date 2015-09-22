;+
; NAME:
;   GetCorrectedSoDAndIntensity171A
;
; PURPOSE:
;   Get 171 Å corrected by 284 Å data in the format needed by FitCoronalDimmingLightCurve from the output of EVECoreDimmingCorrection's EVEScaledIrradiances.sav file.
;   This code exists because it takes a long time to process the data each time. It's faster to just read the existing save file and convert. 
;
; INPUTS:
;   EVEScaledIrradianceFilename [string]: The path and filename to the EVEScaledIrradiace.sav file you want to convert
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   A save file called GetCorrectedSoDAndIntensity171A_RENAME_ME.sav with second of day and intensity stored. Make sure to change the filename after running this code. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires that an EVEScaledIrradiance.sav file already exists. 
;
; EXAMPLE:
;   GetCorrectedSoDAndIntensity171A, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/EVEPlots/Corrected/Event12/Warm correction/EVEScaledIrradiances.sav'
;
; MODIFICATION HISTORY:
;   2015/01/14: James Paul Mason: Wrote script. 
;-
PRO GetCorrectedSoDAndIntensity171A, EVEScaledIrradianceFilename

restore, EVEScaledIrradianceFilename

; Convert time to SOD
sod = (eveTimeJD - floor(eveTimeJD[0]) - 0.5) * 86400.

; Pull out the 171 Å corrected by 284 Å
intensity = correctedEVEDimmingCurves[*, 3]

save, sod, intensity, filename = 'GetCorrectedSoDAndIntensity171A_RENAME_ME.sav'

END