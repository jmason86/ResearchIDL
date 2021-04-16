;+
; NAME:
;   GoesFlareClassHistogram
;
; PURPOSE:
;
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
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
;
;
; MODIFICATION HISTORY:
;   2012-07-05: James Paul Mason: Wrote script.
;-
PRO GoesFlareClassHistogram

restore, '/Users/jmason86/Dropbox/Research/Data/GOES/events/GoesEventsGoes15Era.sav'

class = strtrim(goesevents.st$class, 2)
flux = goesevents.flux

class_c = where(flux LT 1e-5 AND flux GE 1e-6, cnt_c)
cnt_c_nums = lonarr(9)
FOR i = 1, 9 DO BEGIN
  class_c_num = where(flux GE i * 10^(-6.) AND flux LT (i+1) * 10^(-6.), cnt_c_num)
  cnt_c_nums[i-1] = cnt_c_num / 1e-6
ENDFOR

class_m = where(flux LT 1e-4 AND flux GE 1e-5, cnt_m)
cnt_m_nums = lonarr(9)
FOR i = 1, 9 DO BEGIN
  class_m_num = where(flux GE i * 10^(-5.) AND flux LT (i+1) * 10^(-5.), cnt_m_num)
  cnt_m_nums[i-1] = cnt_m_num / 1e-5
ENDFOR

class_x = where(flux GE 1e-4, cnt_x)
cnt_x_nums = lonarr(9)
FOR i = 1, 9 DO BEGIN
  class_x_num = where(flux GE i * 10^(-4.), cnt_x_num)
  cnt_x_nums[i-1] = cnt_x_num / 1e-4
ENDFOR

num_base = indgen(9) + 1
clabels = 'C' + strtrim(num_base, 2)
mlabels = 'M' + strtrim(num_base, 2)
xlabels = 'X' + strtrim(num_base, 2)
labels = [' ', clabels, mlabels, xlabels, ' ']

p = barplot(indgen(27), [cnt_c_nums, cnt_m_nums, cnt_x_nums], $ 
            title='GOES-15 Flare Class Histogram', $
            xmajor=29, xminor=0, xticklen=0, xtitle='Flare Class', $
            ytitle='Frequency [# / (W/m$^2$)]', /YLOG)
p.xtickname = labels
p.save, 'GOES-15 Era Flare Class Histogram.png'
STOP
END