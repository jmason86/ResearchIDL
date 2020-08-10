;+
; NAME:
;   PlotCmeDailyRateHistogram
;
; PURPOSE:
;   Make a histogram of the CME daily rate
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
;   PNG of plot in the save location
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the CDAW catalog file be in the expected directory
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2020-04-30: James Paul Mason: Wrote script.
;-
PRO PlotCmeDailyRateHistogram

  ; Setup
  saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/CDAW/'
  fontSize = 18

  restore, saveloc + 'Historical CME Data CSV Template.sav'
  cdaw = read_ascii(saveloc + 'Historical CME Data.csv', template = myTemplate)
  
  time_iso = cdaw.date + 'T' + cdaw.time + 'Z'
  jd = jpmiso2jd(time_iso)
  days = jd - jd[0]
  months = days/30

  month_index = 0
  FOR month_index = 0, months[-1] - 1 DO BEGIN
    this_month_indices = where(months GE month_index and months LT month_index+1, this_month_rate)
    monthly_rate = (n_elements(monthly_rate) EQ 0) ? this_month_rate : [monthly_rate, this_month_rate]
  ENDFOR
  
  daily_rate = monthly_rate / 30.
  pdf = histogram(daily_rate, locations=xbin)
  pdf_normalized = float(pdf)/max(pdf)
  
  poisson = mean(daily_rate)^xbin * exp(-(mean(daily_rate)))/factorial(xbin)
  poisson_normalized = poisson/max(poisson)
  
  p1 = barplot(xbin, pdf_normalized, FONT_SIZE=fontSize, FILL_COLOR='dark grey', $
               ;title = 'Histogram of Solar CME Daily Rate ($\mu$ = ' + jpmprintnumber(mean(daily_rate)) + ')', $
               xtitle='# CMEs/day', $
               ytitle='probability density')
  p2 = plot(xbin, poisson_normalized, '2--', symbol='o', /SYM_FILLED, /OVERPLOT, $
            name='Poisson distribution')
  l = legend(target = p2, position=[0.92, 0.85], FONT_SIZE=fontSize-4)
  t = text(0.73, 0.73, '$\mu$ = ' + jpmprintnumber(mean(daily_rate)), FONT_SIZE=fontSize)
 
  p1.save, saveloc + 'HistoricalCmeDailyRateHistogramBigFont.png'

END