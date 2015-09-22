;+
; NAME:
;   ymd_to_yd.pro
;
; PURPOSE:
;   Convert year,month,day arrays into yyyydoy
;
; CATEGORY:
;   Library function
;
; CALLING SEQUENCE:  
;   ymd_to_yd, year, month, day, yyyydoy, fracday=fracday, SOD=SOD
;
; INPUTS:
;   year = 4 digit year
;   month = 1-12 for Jan-Dec
;   day = 1-31 for day of month 
;         day can be a double (day + fractional day)
;         The fractional part is ignored if SOD is present.
;
; OPTIONAL INPUTS:
;   SOD = seconds of day (overrides fractional part of day)
;
; OUTPUTS:
;   yyyydoy = year + day of year (long or double array)
;             The fraction of day is calculated from SOD if available,
;             otherwise it is calculated from the fractional part of day.
;
; COMMON BLOCKS:
;   ymd_to_yd_lookuptables
;     doy_table : 2d integer array - DOY is doy_table[day,month]
;     ldoy_table: same thing but for leap years
;       These two arrays are 33,13 so that calendar dates can be
;       directly used. Jan 1 or [1,1] is the lowest useable index,
;       while Dec 31 or [31,12] is the highest useable index.
;
; PROCEDURE:
;  1) Check input parameters
;  2) set default output values
;  3) Populate lookup tables if needed
;  4) Retrieve leap_year array
;  5) Use lookup tables to determine DOY
;  6) Add DOY to year*1000L
;  7) Return
;
; EXAMPLES:
;
; fractional day is too small to matter
;  IDL> ymd_to_yd,2002,1,1.00000000000001d,yd
;  IDL> help,yd
;  YD              LONG      =      2002001
;
; fractional day is now large enough to be detected
;  IDL> ymd_to_yd,2002,1,1.0000000000001d,yd
;  IDL> help,yd
;  YD              DOUBLE    =        2002001.0
;
;
; ROUTINES USED:
;  LEAP_YEAR: returns 1 (true) if argument is a leap year, else 0 (false) 
;
; MODIFICATION HISTORY:
;  2/21/03 Don Woodraska File modified from yd2ymd.pro
;  2/24/03 Don Woodraska Added keywords SOD (input seconds of day) and
;  fracday (output fraction of day).
;  2/26/03 Don Woodraska Removed fracday. yyyydoy can now return day
;  fraction instead.
;
; $Log: ymd_to_yd.pro,v $
; Revision 8.0  2005/06/15 18:51:22  see_sw
; commit of version 8.0
;
; Revision 8.0  2004/07/20 20:18:37  turkk
; commit of version 8.0
;
; Revision 7.0  2004/07/08 23:03:03  turkk
; commit of version 7.0
;
; Revision 6.1  2003/03/13 01:48:01  dlwoodra
; initial commit
;
;
;idver='$Id: ymd_to_yd.pro,v 8.0 2005/06/15 18:51:22 see_sw Exp $'
;
;-

pro ymd_to_yd, theYear, theMonth, theDay, yyyydoy, SOD=SOD

; store local lookup tables that only need to be calculated once
common ymd_to_yd_lookuptables,  doy_table, ldoy_table

;
;  1)  Check input parameters
;
if (n_params(0) lt 4) then goto, bailout

n_days = n_elements(theYear)

if n_days ne n_elements(themonth) and $
  n_days ne n_elements(theday) then begin
    print,'ERROR: arrays do not agree in length'
    goto, bailout
endif
x=where((theDay gt 31) or (theDay lt 1),n_x)
if n_x gt 0 then begin
    print,'ERROR: day of month is invalid'
    goto, bailout
endif

;
;  2) set default output values
;
yyyydoy=long(theYear)*1000L
if arg_present(SOD) or size(SOD,/type) ne 0 then begin
        ; then a value for SOD exists, use it
    fracday = SOD / 86400.d0
endif else fracday=double(theDay mod 1L) ; copy day fraction from theDay

; find if any day fractions are gt the precision of a double
w_frac=where(fracday gt 1.d-14,n_w_frac)
;if any, change yyyydoy to a double and add it in
if n_w_frac gt 0 then $
  yyyydoy=yyyydoy + fracday ;convert yyyydoy to double

thelongDay=long(theDay) ;index for array lookup

;
;  3) Populate lookup tables if needed
;
if size(dayofmonthidx,/type) eq 0 then begin

    ;  define doy offset arrays
    doy_offset    = [ 0L, 31, 59, 90,120,151,181,212,243,273,304,334,365 ]
    doy_ly_offset = [ 0L, 31, 60, 91,121,152,182,213,244,274,305,335,366 ]

    m31=indgen(31)+1 ;day of month array

    ;  build the array lookup tables for leap and non-leap years
    doy_table=intarr(32,13) < (-1) ;make a big DOY table by month,dayofmonth
    ldoy_table=doy_table
     ;  january is month 1 (not 0)
     ;  first day of month is 1 (not 0)
    for i=1,12 do begin
        ii = i - 1
         ; non leap years
        lo = i * n_elements(doy_table[*,0]) + 1
         ; lo is scalar start index of 31-element array assignment
        doy_table[lo] =  m31 + doy_offset[ii]
         ; leap years
        llo = i * n_elements(ldoy_table[*,0]) + 1
        ldoy_table[llo] = m31 + doy_ly_offset[ii]
    endfor
endif

;
;  4) Retrieve leap_year array
;
leapYear = leap_year(theYear) ;vector or scalar

if n_days gt 1 then begin
    ; vector part
    leap=where(leapYear eq 1L,n_leap,comp=not_leap,ncomp=n_not_leap)
    doy=lonarr(n_days)
endif else begin
    ; scalar part
    doy=-1L  &  n_not_leap=0L  &  n_leap=0L  &  leap=-1L  &  not_leap=-1L
    if (leapYear eq 1L) then begin
        n_leap=1L      &  leap=0L
    endif else begin
        n_not_leap=1L  &  not_leap=0L
    endelse
endelse

;
;  5) Use lookup tables to determine DOY
;
if n_leap gt 0 then begin
    doy[leap]     = ldoy_table[thelongDay[leap],theMonth[leap]]
endif
if n_not_leap gt 0 then begin
    doy[not_leap] = doy_table[thelongDay[not_leap],theMonth[not_leap]]
endif

;
;  6) Add DOY to year*1000L
;
yyyydoy=yyyydoy+doy

;
;  7) Return
;
return

bailout:
print, ''
print, 'USAGE:  ymd_to_yd, year, month, day, yyyydoy'
print, ''
print, ' inputs'
print, '   year : 4-digit year (ex. 2003)'
print, '   month: 2-digit month number - range is 1-12 (0 is not valid)'
print, '   day  : day of month - range is 1-31 (0 is not valid)'
print, ''
print, ' output'
print, '   yyyydoy : returned value, always a long integer'
print,''
return

end
