;+
; NAME:
;   plot_cme_widths
;
; PURPOSE:
;   Make a plot of CME widths
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
;-
PRO plot_cme_widths

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/CDAW/'
fontSize = 18

; Load data
restore, saveloc + 'Historical CME Data CSV Template.sav'
cdaw = read_ascii(saveloc + 'Historical CME Data.csv', template = myTemplate)

; Extract data and toss out those above 180º (those are halos or partial halos)
widths = cdaw.width[where(cdaw.width LT 180)]

; Histogram of distribution
pdf = histogram(widths, locations=xbin)
pdf_normalized = float(pdf)/total(pdf)

; Get some stats
bla = max(pdf, peak_width)

; Plot
p1 = plot(xbin, pdf_normalized, font_size=fontSize, fill_color='dark grey', /FILL_BACKGROUND, dimensions=[800,600], $
          xtitle='solar CME widths [º]', xrange=[0, 180], $
          ytitle='relative probability')
t1 = text(0.6, 0.77, 'peak = ' + jpmprintnumber(peak_width, /NO_DECIMAL) + 'º', font_size=fontSize, alignment=1)
t2 = text(0.6, 0.73, 'median = ' + jpmprintnumber(median(widths), /NO_DECIMAL) + 'º', font_size=fontSize, alignment=1)
t3 = text(0.6, 0.69, '$\sigma$ = ' + jpmprintnumber(stddev(widths), /NO_DECIMAL) + 'º', font_size=fontSize, alignment=1)

p1.save, saveloc + 'CME Widths.png'

END