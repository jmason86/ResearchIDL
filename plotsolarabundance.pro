;+
; NAME:
;   PlotSolarAbundance
;
; PURPOSE:
;   Dissertation plot. 
;   Replot the solar abundance from Lang (2001) The Cambridge Encyclopedia of the Sun.
;   Their figure is normalized to silicon abundance. 
;   My figure is normalized to the total number of particles in the sun in %. 
;   Uses data that I datathiefed from the book. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Plot of solar abundance converted to % of total number of particles in sun with only relevant annotation. 
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
;   2016/03/16: James Paul Mason: Wrote script.
;-
PRO PlotSolarAbundance

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/Solar Abundance/' 
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'
hydrogenAbundance = 91.2 ; [% of number of particles in sun]
ironAtomicWeight = 55.845 ; [u] - u is the atomic weight unit and is shorthand for g/mol
ironAtomicWeight = 54.41 ; [u] override due to imperfections in datathief capture

; Read data
readcol, dataloc + 'SolarAbundance.txt', atomicWeight, relativeToSiliconAbundance, /SILENT

; Get key value
hydrogenRelativeToSiliconAbundance = max(relativeToSiliconAbundance)
ironIndex = closest(ironAtomicWeight, atomicWeight)

; Convert abundance from relative to silicon to relative to solar mass
relativeToTotalAbundance = relativeToSiliconAbundance * hydrogenAbundance / hydrogenRelativeToSiliconAbundance

p = plot(atomicWeight, relativeToTotalAbundance, '2', FONT_SIZE = 16, DIMENSIONS = [600, 600], $
         YTITLE = 'Relative Abundance [%]', /YLOG, YRANGE = [1e-11, 1e3], $
         XTITLE = 'Atomic Weight (u)')
a1 = arrow([ironAtomicweight + 20, ironatomicweight], [relativeToTotalAbundance[ironIndex], relativeToTotalAbundance[ironIndex]], /DATA)
t1 = text(ironAtomicWeight + 25, relativeToTotalAbundance[ironIndex], 'Iron', /DATA, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)

; Convert table values from relative to silicon to relative to total number of particles
tableAbundancesRelativeToSilicon = [2.79e10, 2.72e9, 1.01e7, 3.13e6, 2.38e7, 3.44e6, 5.74e4, 1.07e6, 8.49e4, 1e6, $
                                    1.04e4, 5.15e5, 5.24e3, 1.01e5, 3.77e3, 6.11e4, 1.35e4, 9.55e3, 9e5, 4.93e4]
tableAbundancesRelativeToTotal = tableAbundancesRelativeToSilicon * hydrogenAbundance / tableAbundancesRelativeToSilicon[0]
print, 'Table abundances relative to total number of particles in sun:'
print, tableAbundancesRelativeToTotal

; Save plot and data
p.save, saveloc + 'SolarAbundancePlot.png'
save, FILENAME = saveloc + 'IDLSavesets/SolarAbundancePlot.sav'

END