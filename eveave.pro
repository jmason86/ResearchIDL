; Program to create a daily average for EVE data. 
; 
; James Paul Mason 
; 12/4/10

PRO EveAve, dayave, wave

;read list of files and read into IDL and sum up the intensities
readcol, 'eve_data/list', evelist, format='a',/silent
dataloc = 'eve_data/'
intsum = 0

FOR i=0,n_elements(evelist)-1 DO BEGIN
  data1 = mrdfits( dataloc+evelist(i), 1 ,hdr, /unsigned, /silent )
  data2 = mrdfits( dataloc+evelist(i), 2, hdr, /unsigned, /silent )
  intsum = intsum + data2[0].irradiance
ENDFOR ;i loop

;generate average
dayave = intsum / n_elements(evelist)
wave = data1.wavelength


END