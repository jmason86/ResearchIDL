;+
; NAME:
;   PlotDimmingPaper2Figure5DepthVsHighOrLowMass
;
; PURPOSE:
;   Create two plots showing depth versus high/low mass CMEs
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   PUBLISH_STYLE: Set to increase font and window size of plots
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;
; OUTPUTS:
;   PNG and EPS versions of plot in 2 directories:
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;   Also produces a .sav of everything in
;   Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/IDLSavesets/
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
;   2015/12/21: James Paul Mason: Wrote script.
;   2015/01/08: James Paul Mason: Added PUBLISH_STYLE keyword
;   2016/04/29: James Paul Mason: Edits to change relationships from slope vs speed and depth vs mass to slop/depth vs speed and sqrt(depth) vs mass. Added
;                                 new keyword USE_PHYSICAL_RELATIONSHIPS to do this and still maintain ability to make old plots.
;-
PRO PlotDimmingPaper2Figure5DepthVsHighOrLowMass, PUBLISH_STYLE = PUBLISH_STYLE, DARK_BACKGROUND = DARK_BACKGROUND, USE_PHYSICAL_RELATIONSHIPS = USE_PHYSICAL_RELATIONSHIPS

; Defaults
IF keyword_set(PUBLISH_STYLE) THEN BEGIN
  windowDimension = [800, 700]
  fontSize = 18
  plotTitle = ['', '']
ENDIF ELSE BEGIN
  windowDimension = [600, 500]
  fontSize = 12
  plotTitle = ['High CME Masses vs Dimming Depths', 'Low CME Masses vs Dimming Depths']
ENDELSE
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  barFillColor = 'Azure'
  blueDarkOrLight = 'deep sky blue'
  backgroundColor = 'slate grey' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  barFillColor = 'dark slate grey'
  blueDarkOrLight = 'blue'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/20160115 Mason 2-2 Week Period/Preparation/Figures/'

; This is a very janky way to run, but set p1/p2 = 1 to enable the FIT_HIGH_MASS_ONLY/FIT_LOW_MASS_ONLY flag and then it should be replaced by the plot objects after running
p1 = 1
PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass, FIT_HIGH_MASS_ONLY = p1, PUBLISH_STYLE = PUBLISH_STYLE, DARK_BACKGROUND = DARK_BACKGROUND, USE_PHYSICAL_RELATIONSHIPS = USE_PHYSICAL_RELATIONSHIPS
p1.title = plotTitle[0]

p2 = 1
PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass, FIT_LOW_MASS_ONLY = p2, PUBLISH_STYLE = PUBLISH_STYLE, DARK_BACKGROUND = DARK_BACKGROUND, USE_PHYSICAL_RELATIONSHIPS = USE_PHYSICAL_RELATIONSHIPS
p2.title = plotTitle[1]

IF keyword_set(USE_PHYSICAL_RELATIONSHIPS) THEN BEGIN
  depthType = 'DepthSqrt'
ENDIF ELSE BEGIN
  depthType = 'Depth'
ENDELSE

IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  p1.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/CorrelationsHiMass.png', /TRANSPARENT
  p2.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/CorrelationsLoMass.png', /TRANSPARENT
ENDIF ELSE BEGIN
  p1.save, saveloc1 + depthType + ' Vs High Mass.png'
  p1.save, saveloc2 + 'PNGs/' + depthType + 'VsHighMass.png'
  p1.save, saveloc2 + 'EPSs/' + depthType + 'VsHighMass.eps'
  p2.save, saveloc1 + depthType + ' Vs Low Mass.png'
  p2.save, saveloc2 + 'PNGs/' + depthType + 'VsLowMass.png'
  p2.save, saveloc2 + 'EPSs/' + depthType + 'VsLowMass.eps'
  save, FILENAME = saveloc2 + 'IDLSavesets/Figure5Saveset.sav', /COMPRESS
ENDELSE

END