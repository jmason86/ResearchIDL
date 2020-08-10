;+
; NAME:
;   ConcatenateDimmingIrradiancesAndParameters
;
; PURPOSE:
;   Concatenate the output for grouped events into a single event as part of the JEDI backend processing. 
;
; INPUTS:
;   None, though it will be grabbing savesets from disk of the form:
;   '/Users/jmason86/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/DimmingIrradiancesEvent*.sav'
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   New files on disk for DimmingIrradiancesEvent*.sav and DimmingParametersEvent*.sav that concatenate all the others
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2017-12-07: James Paul Mason: Wrote script.
;-
PRO ConcatenateDimmingIrradiancesAndParameters

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Postdoc_LASP/Analysis/Coronal Dimming/Automatic Dimming Database/'

; Clean up from previous run of this code
file_delete, dataloc + 'DimmingIrradiancesEvent1-1520 Python Format.sav', /ALLOW_NONEXISTENT
file_delete, dataloc + 'DimmingParametersEvent1-1520 Python Format.sav', /ALLOW_NONEXISTENT

; Prepare variables to load concatenation into
goesClass = !NULL
goesPeakTime = !NULL
wavelength = !NULL
irradiance = !NULL
depth = !NULL

; Find the irradiance data
filenames = file_search(dataloc + 'DimmingIrradiancesEvent*.sav')

; Loop through and concatenate the irradiance data
FOREACH filename, filenames DO BEGIN
  restore, filename
  goesClass = [goesClass, dimmingIrradiances.goesClass]
  goesPeakTime = [goesPeakTime, dimmingIrradiances.goesPeakTime]
  wavelength = dimmingIrradiances.wavelength ; Can just overwrite this since it's always the same
  irradiance = [[[irradiance]], [[dimmingIrradiances.irradiance]]]
ENDFOREACH
STOP
; Fix wavelength
wavelength = wavelength[*, 0]

; Find the JEDI determined parameters data
filenames = file_search( dataloc + 'DimmingParametersEvent*.sav')

; Loop through and concatenate the irradiance data
FOREACH filename, filenames DO BEGIN
  restore, filename
  depth = [[depth], [dimmingParameters.depth]]
ENDFOREACH

save, goesClass, goesPeakTime, wavelength, irradiance, FILENAME = dataloc + 'DimmingIrradiancesEvent1-1520 Python Format.sav'
save, depth, FILENAME = dataloc + 'DimmingParametersEvent1-1520 Python Format.sav'

END