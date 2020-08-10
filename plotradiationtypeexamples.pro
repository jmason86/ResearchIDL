;+
; NAME:
;   PlotRadiationTypeExamples
;
; PURPOSE:
;   Dissertation plot. 
;   Plot real solar spectra showing examples of free-free, bound-bound, and free-bound emissions
;   Using RHESSI data to get HXR free-free. Datathief on composite flare from http://hesperia.gsfc.nasa.gov/hessi/flares.htm
;   Using EVE MEGS-B to get a few bound-bound lines and the H I continuum just below 912 Å. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;
; OUTPUTS:
;   Plot showing spectra for free-free, bound-bound, and free-bound spectra
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
PRO PlotRadiationTypeExamples, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
dark_background = 1
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white' 
  backgroundColor = 'black' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Setup 
datalocEve = '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/'
datalocRhessi = '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/Free-Free Emission Example/'

; Colors corresponding to PlotFeIonizationFraction.pro
feixColor = [255, 250, 0]
fexColor = [255, 230, 0]
fexiColor = [255, 210, 0]
fexiiColor = [255, 190, 0]
fexiiiColor = [255, 170, 0]
fexivColor = [255, 150, 0]
fexvColor = [255, 130, 0]
fexviColor = [255, 110, 0]
fexxivColor = [255, 0, 80]

; Constants
kev2A = 12.398521 ; kev to Å multiplicative factor

; Restore the data that was saved by PlotExampleEveWithLineLabels
restore, datalocEve + 'AIA Bandpass and EVE Line Labels.sav'
readcol, datalocRhessi + 'FreeFreeSpectrum.txt', photonEnergyRhessi, fluenceRhessi, /SILENT ; photon energy units = keV ; fluence units = photons / kev / cm^2

; The restore overwrites the saveloc variable so defining it here
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'

; Change variable names according to my more modern sensibilies 
wavelengths = temporary(wave) * 10. ; [Å]
spectrum = temporary(spec)

; Convert RHESSI photon energies to wavelengths
wavelengthsRhessi = 12.4 / photonEnergyRhessi

; Line identities 
lineWavelength = [9.39, 12.7, 13.1, 13.29, 14.87, 17.11, 17.7, $
                  18.04, 19.51, 20.20, 21.13, 25.63, $
                  28.42, 30.38, 33.54, 36.08, 36.81, $
                  44.37, 46.52, 49.94, 52.58, 55.44, 58.43, $
                  59.96, 62.49, 62.97, 71.85, 76.51, 77.04, $
                  79.02, 97.25, 97.70, 102.57, 103.19 ] * 10. ; [Å]
lineName = [ 'Fe XVIII', 'Ne VII', 'Fe VIII', 'Fe XX', 'Ne V', 'Fe IX', 'Fe X', $
             'Fe XI', 'Fe XII', 'Fe XIII', 'Fe XIV', 'He II', $
             'Fe XV', 'He II', 'Fe XVI', 'Fe XVI', 'Mg IX', $
             'Mg IX', 'Ne VII', 'Si XII', 'O III', 'O IV', 'He I', $
             'O III', 'Mg X', 'O V', 'O II', 'N IV', 'Ne VIII', $
             'O IV', 'H I', 'C III', 'H I', 'O VI' ]
neviiiIndex = where(lineName EQ 'Ne VIII')

; Define the range in the spectrum for the Ne VIII, O II and O III, and H I continuum to highlight them
; Ditto for the Fe ionization sequence between 171-335 Å
neviiiRange = [768.9, 771.5]
oiiRange = [831.9, 836.7]
hiRange = [800., 912.]
feixRange = [170, 172]
fexRange = [176.5, 178.5]
fexiRange = [179.5, 181.5]
fexiiRange = [194.3, 196]
fexiiiRange = [200.5, 203.5]
fexivRange = [210.5, 212.5]
fexvRange = [283, 285]
fexviRange = [334.5, 336.5]
fexxivRange = [191, 193]

; Find the indices of those highlight ranges in the wavelength array
neviiiIndices = where(wavelengths GE neviiiRange[0] AND wavelengths LE neviiiRange[1])
oiiIndices = where(wavelengths GE oiiRange[0] AND wavelengths LE oiiRange[1])
hiIndices = where(wavelengths GE hiRange[0] AND wavelengths LE hiRange[1])
feixIndices = where(wavelengths GE feixRange[0] AND wavelengths LE feixRange[1])
fexIndices = where(wavelengths GE fexRange[0] AND wavelengths LE fexRange[1])
fexiIndices = where(wavelengths GE fexiRange[0] AND wavelengths LE fexiRange[1])
fexiiIndices = where(wavelengths GE fexiiRange[0] AND wavelengths LE fexiiRange[1])
fexiiiIndices = where(wavelengths GE fexiiiRange[0] AND wavelengths LE fexiiiRange[1])
fexivIndices = where(wavelengths GE fexivRange[0] AND wavelengths LE fexivRange[1])
fexvIndices = where(wavelengths GE fexvRange[0] AND wavelengths LE fexvRange[1])
fexviIndices = where(wavelengths GE fexviRange[0] AND wavelengths LE fexviRange[1])
fexxivIndices = where(wavelengths GE fexxivRange[0] AND wavelengths LE fexxivRange[1])

; Define the range in the spectrum for thermal and non-thermal free-free ; and nuclear emission lines
; These definitions are provided by the source plot
thermalRange = [1, 25]
nonthermalRange = [25, 34136.]
nuclearRange = [1550., 7856.]

; Find the indices corresponding to those highlights
thermalIndices = where(photonEnergyRhessi GE thermalRange[0] AND photonEnergyRhessi LE thermalRange[1])
nonthermalIndices = where(photonEnergyRhessi GE nonthermalRange[0] AND photonEnergyRhessi LE nonthermalRange[1])
nuclearIndices = where(photonEnergyRhessi GE nuclearRange[0] AND photonEnergyRhessi LE nuclearRange[1])

;w = window(DIMENSIONS = [1600, 600], BACKGROUND_COLOR = backgroundColor)
;
;; EVE plot
;p1 = plot(wavelengths, 1d6 * spectrum, '2', COLOR = foregroundBlackOrWhite, FONT_SIZE = 22, /CURRENT, POSITION = [0.58, 0.13, 0.98, 0.9], $
;          XRANGE = [700, 950], XCOLOR = foregroundBlackOrWhite, $
;          YTITLE = 'Irradiance [$µW m^{-2} nm^{-1}$]', /YLOG, YTICKNAME = ['$10^{-2}$', '$10^{-1}$', '$10^0$', '$10^1$', '$10^2$', '$10^3$', '$10^4$'], YCOLOR = foregroundBlackOrWhite)
;p3 = plot(wavelengths[neviiiIndices], 1d6 * spectrum[neviiiIndices], 'r2', /OVERPLOT)
;p4 = plot(wavelengths[hiIndices], 1d6 * spectrum[hiIndices], 'b2', /OVERPLOT)
;p5 = plot(wavelengths[oiiIndices], 1d6 * spectrum[oiiIndices], 'r2', /OVERPLOT)
;pdash = plot([912, 912], p1.yrange, 'b--', /OVERPLOT)
;title1 = text(0.633, 0.94, 'Bound-Bound', COLOR = 'red', FONT_SIZE = 24)
;title2 = text(0.765, 0.94, 'and', FONT_SIZE = 24, COLOR = foregroundBlackOrWhite)
;title3 = text(0.808, 0.94, 'Free-Bound', COLOR = 'blue', FONT_SIZE = 24)
;t2 = text(lineWavelength[neviiiIndex], spectrum[lineWavelength[neviiiIndex]] + 180, 'Ne VIII', /DATA, COLOR = 'red', ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 18)
;t3 = text(834.3, spectrum[lineWavelength[neviiiIndex]] + 180, 'O II / O III', /DATA, COLOR = 'red', ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 18)
;t4 = text(870, 120, 'H I Continuum', /DATA, COLOR = 'blue', ALIGNMENT = 0.5, FONT_SIZE = 18)
;t5 = text(910, 0.05, 'Low Energy Threshold', /DATA, COLOR = 'blue', ORIENTATION = 90, FONT_SIZE = 14)
;
;; RHESSI plot
;p6 = plot(wavelengthsRhessi, fluenceRhessi, '3', COLOR = foregroundBlackOrWhite, FONT_SIZE = 22, /CURRENT, POSITION = [0.08, 0.13, 0.48, 0.9], $
;          /XLOG, XRANGE = [1e-4, 1e1], XTICKNAME = ['$10^{-4}$', '$10^{-3}$', '$10^{-2}$', '$10^{-1}$', '$10^0$', '$10^1$'], XCOLOR = foregroundBlackOrWhite, $
;          YTITLE = 'Fluence [$photons keV^{-1} cm^{-2}$]', /YLOG, YMAJOR = 5, YRANGE = [1e-5, 1e11], YCOLOR = foregroundBlackOrWhite)
;p7 = plot(wavelengthsRhessi[thermalIndices], fluenceRhessi[thermalIndices], 'g3', /OVERPLOT)
;p8 = plot(wavelengthsRhessi[nonthermalIndices], fluenceRhessi[nonthermalIndices], '3', COLOR = 'magenta', /OVERPLOT)
;p9 = plot(wavelengthsRhessi[nuclearIndices], fluenceRhessi[nuclearIndices], '3', COLOR = 'orange', /OVERPLOT)
;t6 = text(3e-1, 1e8, 'Thermal', /DATA, COLOR = 'green', FONT_SIZE = 18, TARGET = p6)
;t7 = text(6e-3, 200, 'Non-thermal', /DATA, COLOR = 'magenta', FONT_SIZE = 18, TARGET = p6)
;t8 = text(9e-4, 1, 'Nuclear', /DATA, COLOR = 'orange', FONT_SIZE = 18, TARGET = p6)
;title1 = text(0.17, 0.94, 'Nuclear', COLOR = 'orange', FONT_SIZE = 24)
;title2 = text(0.247, 0.94, 'and', COLOR = foregroundBlackOrWhite, FONT_SIZE = 24)
;title3 = text(0.29, 0.94, 'Free', COLOR = 'magenta', FONT_SIZE = 24)
;title4 = text(0.332, 0.94, '-', COLOR = foregroundBlackOrWhite, FONT_SIZE = 24)
;title5 = text(0.34, 0.94, 'Free', COLOR = 'green', FONT_SIZE = 24)
;xtitle = text(0.5, 0.02, 'Wavelength [Å]', COLOR = foregroundBlackOrWhite, FONT_SIZE = 22, ALIGNMENT = 0.53)

; Save plot and data
;IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
;p1.save, saveloc + 'RadiationTypeExamples.png', /TRANSPARENT

; Defense plot showing lines from 171-304 Å
w = window(BACKGROUND_COLOR = backgroundColor)
p1 = plot(wavelengths, 1d6 * spectrum, '2', COLOR = foregroundBlackOrWhite, FONT_SIZE = 16, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
          TITLE = 'Bound-bound Fe emission line examples', $
          XTITLE = 'Wavelength [Å]', XRANGE = [150, 350], XCOLOR = foregroundBlackOrWhite, $
          YTITLE = 'Irradiance [$µW m^{-2} nm^{-1}$]', YRANGE = [1, 1e4], /YLOG, YTICKUNITS = 'exponent', YCOLOR = foregroundBlackOrWhite)
p2 = plot(wavelengths[feixIndices], 1d6 * spectrum[feixIndices], '2', COLOR = feixColor, /OVERPLOT)
p3 = plot(wavelengths[fexIndices], 1d6 * spectrum[fexIndices], '2', COLOR = fexColor, /OVERPLOT)
p4 = plot(wavelengths[fexiIndices], 1d6 * spectrum[fexiIndices], '2', COLOR = fexiColor, /OVERPLOT)
p5 = plot(wavelengths[fexiiIndices], 1d6 * spectrum[fexiiIndices], '2', COLOR = fexiiColor, /OVERPLOT)
p6 = plot(wavelengths[fexiiiIndices], 1d6 * spectrum[fexiiiIndices], '2', COLOR = fexiiiColor, /OVERPLOT)
p7 = plot(wavelengths[fexivIndices], 1d6 * spectrum[fexivIndices], '2', COLOR = fexivColor, /OVERPLOT)
p8 = plot(wavelengths[fexvIndices], 1d6 * spectrum[fexvIndices], '2', COLOR = fexvColor, /OVERPLOT)
p9 = plot(wavelengths[fexviIndices], 1d6 * spectrum[fexviIndices], '2', COLOR = fexviColor, /OVERPLOT)
p10 = plot(wavelengths[fexxivIndices], 1d6 * spectrum[fexxivIndices], '2', COLOR = fexxivColor, /OVERPLOT)
t2 = text(171, 1e3, 'IX', /DATA, COLOR = feixColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t3 = text(177, 4e3, 'X', /DATA, COLOR = fexColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t4 = text(180, 1e3, 'XI', /DATA, COLOR = fexiColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t5 = text(195, 4e3, 'XII', /DATA, COLOR = fexiiColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t6 = text(202, 1e3, 'XIII', /DATA, COLOR = fexiiiColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t7 = text(211, 4e3, 'XIV', /DATA, COLOR = fexivColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t8 = text(284, 1e3, 'XV', /DATA, COLOR = fexvColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t9 = text(335, 4e3, 'XVI', /DATA, COLOR = fexviColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)
t10 = text(192, 1e3, 'XXIV', /DATA, COLOR = fexxivColor, ORIENTATION = 90, VERTICAL_ALIGNMENT = 0.5, FONT_SIZE = 14)


STOP


IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
p1.save, saveloc + 'EveFeSequence.png', /TRANSPARENT



save, FILENAME = saveloc + 'IDLSavesets/RadiationTypeExamples.sav'
END