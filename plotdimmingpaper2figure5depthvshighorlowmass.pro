;+
; NAME:
;   PlotDimmingPaper2Figure5DepthVsHighOrLowMass
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
;   FIT_3D_ONLY: Set this to only analyze the CME data that was derived with 3D methods
;                Needs to be run anytime parameters change to generate the saveset that
;                will be restored for creating the published plot, which includes a red-dashed
;                line for the 3D line fit.
;   FIT_HIGH_MASS_ONLY: Same idea as above except for fitting CME masses above 1E15 g
;   FIT_LOW_MASS_ONLY: Same idea as above except for fitting CME masses below 1E15 g
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
;-
PRO PlotDimmingPaper2Figure5DepthVsHighOrLowMass











END