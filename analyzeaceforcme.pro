;+
; NAME:
;   AnalyzeACEForCME
;
; PURPOSE:
;   Look at ACE data for the 2-2 week period events and plot the data for preliminary analysis. 
;
; INPUTS:
;   eventNumber [integer]: The event number to run on. If not specified in call, can specify in hard code. 
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Plot of ACE proton density for time of event - T + 5 days. 
;   Sum and peak values within a hardcoded time range. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMPrintNumber.pro
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2015/02/20: James Paul Mason: Wrote script.
;-
PRO AnalyzeACEForCME, eventNumber

; Setup
IF n_params() EQ 0 THEN eventNumber = 33
saveloc =  '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/ACEPlots/'

; Get data
restore, '/Volumes/Archer4TB/ACE/ACE2010-2014.sav'

; Selected time range
CASE eventNumber OF 
  8: BEGIN
      selectedYear = 2011
      selectedStartDoy = 45
      selectedEndDoy = selectedStartDoy + 5
      sumStartDoy = 46.05
      sumEndDoy = 46.37
     END
  9: BEGIN
      selectedYear = 2011
      selectedStartDoy = 46
      selectedEndDoy = selectedStartDoy + 5
      sumStartDoy = 46.05
      sumEndDoy = 46.37
     END
  14: BEGIN
       selectedYear = 2011
       selectedStartDoy = 55
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 46.05
       sumEndDoy = 46.37
      END
  18: BEGIN
       selectedYear = 2011
       selectedStartDoy = 215
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 217.7
       sumEndDoy = 217.9
      END
  19: BEGIN
       selectedYear = 2011
       selectedStartDoy = 216
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 217.9
       sumEndDoy = 218.3
     END
  28: BEGIN
       selectedYear = 2011
       selectedStartDoy = 220
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 222.5
       sumEndDoy = 223.3
     END
  31: BEGIN
       selectedYear = 2011
       selectedStartDoy = 221
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 222.5
       sumEndDoy = 223.3
     END
  33: BEGIN
       selectedYear = 2011
       selectedStartDoy = 223
       selectedEndDoy = selectedStartDoy + 5
       sumStartDoy = 223.0
       sumEndDoy = 223.3
     END
ENDCASE
zoomedIndices = where(year EQ selectedYear AND fp_doy GE selectedStartDoy AND fp_doy LE selectedEndDoy)
toSumIndices = where(year EQ selectedYear AND fp_doy GE sumStartDoy AND fp_doy LE sumEndDoy)

; Create plot
p = plot(fp_doy[zoomedIndices], np[zoomedIndices], SYMBOL = 'dot', SYM_THICK = 2, LINESTYLE = 6, $
         TITLE = 'ACE Proton Density From Event ' + JPMPrintNumber(eventNumber) + ' to +5 Days', $
         XTITLE = 'Time [Day of Year]', $
         YTITLE = 'Proton Density [$cm^{-3}$]')
p2 = plot([sumStartDoy, sumStartDoy], p.yrange, '2', COLOR = 'deep sky blue', /OVERPLOT)
p3 = plot([sumEndDoy, sumEndDoy], p.yrange, '2', COLOR = 'deep sky blue', /OVERPLOT)
t = text(0.14, 0.83, 'Sum = ' + JPMPrintNumber(total(Np[toSumIndices])) + ' protons/$cm^{-3}$', COLOR = 'deep sky blue', FONT_STYLE = 'Bold')
p.save, saveloc + 'Event' + JPMPrintNumber(eventNumber) + ' ACE Proton Density.png'

; Output total and peak densities
print, 'Total density = ' + JPMPrintNumber(total(Np[toSumIndices])) + ' protons/cm^3'
print, 'Peak density = ' + JPMPrintNumber(max(Np[toSumIndices])) + ' protons/cm^3'

END