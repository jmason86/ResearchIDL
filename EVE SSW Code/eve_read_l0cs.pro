;docformat = 'rst'
;+
;Reads an ASCII level 0CS file and returns an array of structures
;
;:Categories:
; user
;
;
;:Params:
;  filename: in, required
;    the filename as a string to be read
;  status: out, required
;    0 is good, not 0 is bad
;
;:Returns:
;the array of structures
;
;:Examples:
;::
;  IDL> data = read_l0cs( filename, status )
;
;-
function eve_read_l0cs, filename, status
; MODIFICATION HISTORY:
;  02/19/10 DLW Original file creation
;  08/26/10 DLW Added MP_dark back in.
;
; $Id: read_l0cs.pro,v 3.1 2011/06/03 21:59:53 dlwoodra Exp $
;

status=0

if file_test(filename) eq 0 then begin
   print,'file not found : '+filename
   status = -1
   return,-1
endif

;
; define output record format
;
data_rec = { yyyydoy:0L, mmdd:0L, hhmm:0L, minuteofday:0L, $
             p_xrslong:0., p_xrsshort:0., p_sem304:0., $
             esp_0_7:0., esp_17:0., esp_25:0., esp_30:0., esp_36:0., $
             esp_dark:0., mp_lya:0., mp_dark:0., $
             esp_q0:0., esp_q1:0., esp_q2:0., esp_q3:0., $
             esp_cm_lat:0., esp_cm_lon:0. }

;
; open the file for reading
;
s=';'
openr,lun,/get,filename
cnt=0L
while strmid(s,0,1) eq ';' do begin
   readf,lun,s
   cnt++
endwhile
; now read/retain the date string
datestr = s

n_minutes=file_lines(filename) - cnt
sarr=strarr(n_minutes) ; one for each minute
for i=0,n_elements(sarr)-1 do begin
   readf,lun,s
   sarr[i] = s
endfor
close,lun
free_lun,lun

data = replicate( data_rec, n_minutes )

data.yyyydoy = long(strmid(datestr,0,4))*1000L + $
               long(strmid(datestr,5,3))
data.mmdd = long(strmid(datestr,9,2)) + long(strmid(datestr,12,2))

; parse data into variables
hhmm = long(strmid(sarr,0,4))
data.hhmm = hhmm
hh = long(hhmm/100L)
mm = hhmm mod 100L
;hours = double(hh) + (mm/60.d0)
data.minuteofday = (hh*60L) + mm

c=5L
data.p_xrslong  = float(strmid(sarr, c, 9))
c+=10
data.p_xrsshort = float(strmid(sarr, c, 9))
c+=10
data.p_sem304   = float(strmid(sarr, c, 9))
c+=10
data.esp_0_7    = float(strmid(sarr, c, 9))
c+=10
data.esp_17     = float(strmid(sarr, c, 9))
c+=10
data.esp_25     = float(strmid(sarr, c, 9))
c+=10
data.esp_30     = float(strmid(sarr, c, 9))
c+=10
data.esp_36     = float(strmid(sarr, c, 9))
c+=10
data.esp_dark   = float(strmid(sarr, c, 9))
c+=10
data.mp_lya     = float(strmid(sarr, c, 9))
c+=10
data.mp_dark    = float(strmid(sarr, c, 9))
c+=10
data.esp_q0     = float(strmid(sarr, c, 9))
c+=10
data.esp_q1     = float(strmid(sarr, c, 9))
c+=10
data.esp_q2     = float(strmid(sarr, c, 9))
c+=10
data.esp_q3     = float(strmid(sarr, c, 9))

c+=10
data.esp_cm_lat = float(strmid(sarr, c, 4))
c+=6
data.esp_cm_lon = float(strmid(sarr, c, 4))


return, data
end
