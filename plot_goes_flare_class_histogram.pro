;+
; NAME:
;   plot_goes_flare_class_histogram
;
; PURPOSE:
;   Create a histogram showing how flares have been classified
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
;   
;
; OPTIONAL OUTPUTS:
;
;
; RESTRICTIONS:
;
;
; EXAMPLE:
;   Just run it! 
;-
PRO plot_goes_flare_class_histogram

; Defaults
dataloc = '~/Dropbox/Research/Data/GOES/events/'
saveloc = '~/Dropbox/Apps/Overleaf/Flare Frequency Distribution (FFD)/figures/'

restore, dataloc + 'GoesEventsGoes15Era.sav'

class = strtrim(goesevents.st$class, 2)
flux = goesevents.flux

class_a = where(flux LT 1e-7 AND flux GE 1e-8, cnt_a)
cnt_a_nums = dblarr(9)
FOR i = 1, 9 DO BEGIN
  class_a_num = where(flux GE i * 10^(-8.) AND flux LT (i+1) * 10^(-8.), cnt_a_num)
  cnt_a_nums[i-1] = cnt_a_num / 1d-7
ENDFOR

class_b = where(flux LT 1e-6 AND flux GE 1e-7, cnt_b)
cnt_b_nums = dblarr(9)
FOR i = 1, 9 DO BEGIN
  class_b_num = where(flux GE i * 10^(-7.) AND flux LT (i+1) * 10^(-7.), cnt_b_num)
  cnt_b_nums[i-1] = cnt_b_num / 1d-7
ENDFOR

class_c = where(flux LT 1e-5 AND flux GE 1e-6, cnt_c)
cnt_c_nums = dblarr(9)
FOR i = 1, 9 DO BEGIN
  class_c_num = where(flux GE i * 10^(-6.) AND flux LT (i+1) * 10^(-6.), cnt_c_num)
  cnt_c_nums[i-1] = cnt_c_num / 1e-6
ENDFOR

class_m = where(flux LT 1e-4 AND flux GE 1e-5, cnt_m)
cnt_m_nums = dblarr(9)
FOR i = 1, 9 DO BEGIN
  class_m_num = where(flux GE i * 10^(-5.) AND flux LT (i+1) * 10^(-5.), cnt_m_num)
  cnt_m_nums[i-1] = cnt_m_num / 1e-5
ENDFOR

class_x = where(flux GE 1e-4, cnt_x)
cnt_x_nums = dblarr(9)
FOR i = 1, 9 DO BEGIN
  class_x_num = where(flux GE i * 10^(-4.), cnt_x_num)
  cnt_x_nums[i-1] = cnt_x_num / 1e-4
ENDFOR

num_base = indgen(9) + 1
blabels = 'B' + strtrim(num_base, 2)
clabels = 'C' + strtrim(num_base, 2)
mlabels = 'M' + strtrim(num_base, 2)
xlabels = 'X' + strtrim(num_base, 2)
labels = [' ', blabels, clabels, mlabels, xlabels, ' ']

p = barplot(indgen(36), [cnt_b_nums, cnt_c_nums, cnt_m_nums, cnt_x_nums], dimensions=[850, 500], $ 
            title='GOES-15 Flare Class Histogram', $
            xmajor=n_elements(labels), xminor=0, xticklen=0, xtitle='Flare Class', $
            ytitle='Frequency [# / (W/m$^2$)]', /YLOG)
p.xtickname = labels
p.save, 'GOES-15 Era Flare Class Histogram.png'
STOP
END