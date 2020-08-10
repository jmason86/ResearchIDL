;+
; NAME:
;   PlotMagexCmeAccelerationProfile
;
; PURPOSE:
;   Read in data from the Toeroek figure and replot for the purposes of the MagEx proposal
;
; INPUTS:
;   None directly, but does read .csv files corresponding to the plot points of the four curves
;   that were obtained with GraphClick applied to T. Torok and B. Kliem AN 328, 2007, Figure 3b
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   DARK_BACKGROUND: Set this to make a transparent background and change axis color to white
;
; OUTPUTS:
;   Plot to disk
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the csv files described in input
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2018-10-05: James Paul Mason: Wrote script.
;-
PRO PlotMagexCmeAccelerationProfile, DARK_BACKGORUND = DARK_BACKGROUND

; Defaults
fontSize = 22
path = '/Users/jmason86/Dropbox/Research/Postdoc_NASA/Proposals/2018 MagEx/MagEx SunCET CME Figure/assets/Toeroek Data/'
aiaColor = 'goldenrod'
c2Color = 'tomato'
suncetColor = 'dodger blue'
dimensions = [1000, 564]

IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  blackOrWhite = 'white'
  backgroundColor = 'black'
ENDIF ELSE BEGIN
  blackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Make a template for reading the data if it hasn't already been done
IF NOT file_test(path + 'template.sav') THEN BEGIN
  template = ASCII_TEMPLATE(path + 'red_curve.txt')
  save, template, FILENAME = path + 'template.sav'
ENDIF ELSE BEGIN
  restore, path + 'template.sav'
ENDELSE

; Read the data
curve1 = read_ascii(path + 'black_curve.txt', TEMPLATE = template)
curve2 = read_ascii(path + 'green_curve.txt', TEMPLATE = template)
curve3 = read_ascii(path + 'blue_curve.txt', TEMPLATE = template)
curve4 = read_ascii(path + 'red_curve.txt', TEMPLATE = template)

; Convert the h/h_0 from the plot into R_s
R_s = 695.508e6
aiaOuterFov = 1.28125
lascoC2InnerFov = 2.39
suncetOuterFov = 4.0

curve1.x = (curve1.x * 50 * 1e6) / R_s + 1
curve2.x = (curve2.x * 50 * 1e6) / R_s + 1
curve3.x = (curve3.x * 50 * 1e6) / R_s + 1
curve4.x = (curve4.x * 50 * 1e6) / R_s + 1

; Make plot
; Torok curves
w = window(BACKGROUND_COLOR = backgroundColor, DIMENSIONS = dimensions)
p1 = plot(curve1.x, curve1.y, THICK = 3, FONT_SIZE = fontSize, FONT_COLOR = blackOrWhite, COLOR = 'indigo', /CURRENT, $
          XTITLE = 'radial distance [R$_S$]', XRANGE = [0.43, 4.87], b = blackOrWhite, $
          YTITLE = 'acceleration [normalized]', YCOLOR = blackOrWhite)
p2 = plot(curve2.x, curve2.y, THICK = 3, COLOR = 'dark orchid', /OVERPLOT)
p3 = plot(curve3.x, curve3.y, THICK = 3, COLOR = 'orchid', /OVERPLOT)
p4 = plot(curve4.x, curve4.y, THICK = 3, COLOR = 'violet', /OVERPLOT)
p1.yrange = [0, 0.072]


; FOV lines
p0 = plot([aiaOuterFov, aiaOuterFov], p1.yrange, '--', THICK = 3, COLOR = aiaColor, /OVERPLOT)
p0 = plot([lascoC2InnerFov, lascoC2InnerFov], p1.yrange, THICK = 3, '--', COLOR = c2Color, /OVERPLOT)
p0 = plot([suncetOuterFov, suncetOuterFov], p1.yrange, THICK = 3, '--', COLOR = suncetColor, /OVERPLOT)

; Arrow for AIA FOV
p0 = plot([aiaOuterFov - 0.37, aiaOuterFov], [0.035, 0.035], '--', THICK = 2, COLOR = aiaColor, /OVERPLOT)

; FOV text
t0 = text(aiaOuterFov - 0.4, 0.032, /DATA, 'AIA outer FOV', ORIENTATION = 90, TARGET = p1, COLOR = aiaColor, FONT_SIZE = fontSize - 2)
t0 = text(lascoC2InnerFov + 0.07, 0.032, /DATA, 'C2 inner FOV', ORIENTATION = 90, TARGET = p1, COLOR = c2Color, FONT_SIZE = fontSize - 2, VERTICAL_ALIGNMENT = 1)
t0 = text(suncetOuterFov - 0.07, 0.025, /DATA, 'SunCET outer FOV', ORIENTATION = 90, TARGET = p1, COLOR = suncetColor, FONT_SIZE = fontSize - 2)

; Unobserved box 
c1 = polygon([aiaOuterFov, lascoC2InnerFov, lascoC2InnerFov, aiaOuterFov], [0, 0, 0.072, 0.072], /DATA, $
             LINESTYLE = 'none', $
             /FILL_BACKGROUND, FILL_COLOR = 'light grey', TRANSPARENCY = 50, TARGET = p1)
c1.order, /SEND_TO_BACK
t0 = text((lascoC2InnerFov - aiaOuterFov)/2 + aiaOuterFov, 0.065, /DATA, 'gap', TARGET = p1, FONT_SIZE = fontSize - 2, ALIGNMENT = 0.5)

STOP
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  p1.save, path + '../FigureMagExTorokModifiedDarkBackground.png', /TRANSPARENT
ENDIF ELSE BEGIN
  p1.save, path + '../FigureMagExTorokModified.png'
ENDELSE


END