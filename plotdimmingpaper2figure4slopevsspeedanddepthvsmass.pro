;+
; NAME:
;   PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass
;
; PURPOSE:
;   Create two plots showing slope vs speed and depth vs mass
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
;   2015/10/05: James Paul Mason: Wrote script.
;-
PRO PlotDimmingPaper2Figure4SlopeVsSpeedAndDepthVsMass

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

; Hard coded values pulled drom drive spreadsheet (10 events per row for easier counting)
depths = [0.46, 0.21, 1.64, 0.81, 0.36, 1.06, 2.93, 1.42, 4.41, 1.95, $
          2.67, 1.22, 4.27, 3.64, !VALUES.F_NAN, 4.76, 0.47, 2.68, 5.22, !VALUES.F_NAN, $ 
          0.19, 1.67, !VALUES.F_NAN, !VALUES.F_NAN, 0.99, 0.92, !VALUES.F_NAN, !VALUES.F_NAN, 2.57, !VALUES.F_NAN, $
          2.72, !VALUES.F_NAN, 2.79, 1.07, 1.91, 2.55]
slopes = [0.16, 1.52, 1.37, 0.51, 2.03, 3.27, 2.13, 0.79, 1.63, 1.49, $
          1.65, 1.51, 0.76, 2.29, !VALUES.F_NAN, 1.02, 2.22, 2.87, 3.54, !VALUES.F_NAN, $
          3.29, 1.19, !VALUES.F_NAN, !VALUES.F_NAN, 0.43, 1.38, !VALUES.F_NAN, !VALUES.F_NAN, 2.8, !VALUES.F_NAN, $
          3.7, !VALUES.F_NAN, 1.42, 1.85, 0.82, 0.56] 
speeds = [!VALUES.F_NAN, 338., 175., !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 353., 303., 899., !VALUES.F_NAN, $ 
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 370., !VALUES.F_NAN, 712., !VALUES.F_NAN, 610., 1315., 338., $
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 176., !VALUES.F_NAN, 459., !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 1250., $
          1480., !VALUES.F_NAN, 428., 1150., 346., !VALUES.F_NAN]
masses = [!VALUES.F_NAN, 3.40E+14, 1.40E+14, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 3.20E+15, 1.20E+13, 5.20E+15, !VALUES.F_NAN, $
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 6.50E+14, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, $
          !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 5.10E+14, !VALUES.F_NAN, 7.00E+13, !VALUES.F_NAN, !VALUES.F_NAN, !VALUES.F_NAN, 2.10E+15, $ 
          3.80E+15, !VALUES.F_NAN, 2.40E+14, 2.30E+15, 5.10E+14, !VALUES.F_NAN]

p1 = scatterplot(speeds, slopes, /SYM_FILLED, SYM_SIZE = 2, TITLE = 'CME Speeds vs Dimming Slopes', $
                 XTITLE = 'CME Speed [$km s^{-1}$]', $
                 YTITLE = 'Dimming Slope [$% hour^{-1}$]')
p2 = scatterplot(masses, depths, /SYM_FILLED, SYM_SIZE = 2, TITLE = 'CME Masses vs Dimming Depths', $
                 XTITLE = 'CME Mass [g]', /XLOG, $
                 YTITLE = 'Dimming Depth [%]')

p1.save, saveloc1 + 'Slope Vs Speed.png'
p1.save, saveloc2 + 'PNGs/SlopeVsSpeed.png'
p1.save, saveloc2 + 'EPSs/SlopeVsSpeed.eps'
p2.save, saveloc1 + 'Depth Vs Mass.png'
p2.save, saveloc2 + 'PNGs/DepthVsMass.png'
p2.save, saveloc2 + 'EPSs/DepthVsMass.eps'
save, FILENAME = saveloc2 + 'IDLSavesets/Figure4Saveset.sav', /COMPRESS

END