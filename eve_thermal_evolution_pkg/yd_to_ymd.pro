;+
; NAME:
;   yd_to_ymd.pro
;
; PURPOSE:
;   Convert yyyydoy to year-month-day
;
; CATEGORY:
;   Library function
;
; CALLING SEQUENCE:  
;   yd_to_ymd, yyyydoy, year, month, day, sod=sod
;
; INPUTS:
;   yyyydoy = year + day of year (should be a long or double)
;
; OUTPUTS:  
;   year = 4 digit year (long)
;   month = 1-12 for Jan-Dec (long)
;   day = 1-31 for day of month (long)
;
; OPTIONAL OUTPUTS:
;   sod = seconds of day (double)
;
; COMMON BLOCKS:
;   yd_to_ymd_lookuptables
;     dayofmonthidx : day index table accessed by DOY 1-366, intarr(367)
;     monthidx      : month index table, intarr(367)
;     ldayofmonthidx: leap year day index table, intarr(367)
;     lmonthidx     : leap year month index table, intarr(367)
;
; PROCEDURE:
;   1) Set default output values
;   2) Check input parameters
;   3) Populate lookup tables if needed
;   4) Make year array
;   5) Get leap_year flags
;   6) Make DOY array
;   7) Assign month and day using leap year/non leap year indices
;   8) Return
;
; EXAMPLES:
;
;  IDL> yd_to_ymd,2002001,y,m,d        
;  IDL> help,y,m,d    
;  Y               LONG      =         2002
;  M               LONG      =            1
;  D               LONG      =            1
;
;  IDL> yd_to_ymd,2002001.1d,y,m,d
;  IDL> help,y,m,d
;  Y               LONG      =         2002
;  M               LONG      =            1
;  D               DOUBLE    =        1.1000000
;
;  IDL> yd_to_ymd,2002001.1d,y,m,d,sod=sod
;  IDL> help,y,m,d,sod
;  Y               LONG      =         2002
;  M               LONG      =            1
;  D               LONG      =            1
;  SOD             DOUBLE    =        8640.0000
;
; ROUTINES_USED:
;  LEAP_YEAR: returns 1 for a leap year, 0 otherwise
;
; MODIFICATION HISTORY:
;  2/21/03 Don Woodraska File modified from yyyydoy2ymd.pro
;  2/24/03 Don Woodraska Modified to convert fractional yyyydoy to sod
;  seconds of day.
;
; $Log: yd_to_ymd.pro,v $
; Revision 8.0  2005/06/15 18:51:22  see_sw
; commit of version 8.0
;
; Revision 8.0  2004/07/20 20:18:37  turkk
; commit of version 8.0
;
; Revision 7.0  2004/07/08 23:03:02  turkk
; commit of version 7.0
;
; Revision 6.1  2003/03/13 01:48:00  dlwoodra
; initial commit
;
;
;idver='$Id: yd_to_ymd.pro,v 8.0 2005/06/15 18:51:22 see_sw Exp $'
;
;-

pro yd_to_ymd, yyyydoy, theYear, theMonth, theDay, sod=sod

; store local lookup tables so they are calculated only once
common yd_to_ymd_lookuptables, dayofmonthidx,monthidx,ldayofmonthidx,lmonthidx

;
;  1) Set default output values
;
theYear = -1L  &  theMonth = -1L  &  theDay = -1L  &  sod = -1.d0

;
;  2)  Check input parameters
;
if (n_params() lt 2 or n_params() gt 4) then goto, bailout

;
;  3) Populate lookup tables if needed
;
if size(dayofmonthidx,/type) eq 0 then begin

    ;  define day arrays for quick calculation
    doy_offset    = [ 0L, 31, 59, 90,120,151,181,212,243,273,304,334,365 ]
    doy_ly_offset = [ 0L, 31, 60, 91,121,152,182,213,244,274,305,335,366 ]

    ;build the array lookup tables
    dayofmonthidx  = intarr(367) < (-1)  &  monthidx  = dayofmonthidx
    ldayofmonthidx = dayofmonthidx       &  lmonthidx = dayofmonthidx
    m31=indgen(31)+1            ;day of month array
    i31=intarr(31)              ;filler of zeroes for month assignment
    for i=1,12 do begin
        ii = i - 1
         ; non leap years
        lo                = 1 + doy_offset[ii]
        dayofmonthidx[lo] = m31
         ; lo is the start array index of the 31-element array assignment
        monthidx[lo]      = i + i31
         ; leap years
        llo                 = 1 + doy_ly_offset[ii]
        ldayofmonthidx[llo] = m31
        lmonthidx[llo]      = i + i31
    endfor
endif

;
;  4) Make year array
;
theYear = long( yyyydoy/1000L )

;
;  5) Get leap_year flags
;
leapYear = leap_year(theYear) ;vector or scalar

;
;  6) Make DOY array
;
DOY    = long( yyyydoy mod 1000L )
n_days = n_elements(DOY)
if arg_present(sod) then flagsod=0 else flagsod=1
sod    = double(yyyydoy mod 1.d0) * 86400.d0

if n_days gt 1 then begin
    ; vector part
    theMonth = lonarr(n_days)  &  theDay = theMonth
    leap=where(leapYear eq 1L,n_leap,comp=not_leap,ncomp=n_not_leap)
endif else begin
    ; scalar part
    n_not_leap=0L  &  n_leap=0L  &  not_leap=-1L  &  leap=-1L
    if (leapYear eq 1L) then begin
        n_leap=1L      &  leap=0L
    endif else begin
        n_not_leap=1L  &  not_leap=0L
    endelse
endelse

;
;  7) Assign month and day using leap year/non leap year indices
;
if n_leap gt 0 then begin
    theMonth[leap] = lmonthidx[DOY[leap]]
    theDay[leap]   = ldayofmonthidx[DOY[leap]]
endif
if n_not_leap gt 0 then begin
    theMonth[not_leap] = monthidx[DOY[not_leap]]
    theDay[not_leap]   = dayofmonthidx[DOY[not_leap]]
endif

; if sod not present on command line and sod is valid, append day
; fraction to theDay (and make it a double)
valid_sod=where(sod gt 1d-14,n_valid_sod)
if flagsod eq 1 and n_valid_sod gt 0 then $
  theDay=theDay+(double(yyyydoy mod 1.d))

;
;  8) Return
;
return

bailout:
print,''
print, 'USAGE:  yd_to_ymd, yyyydoy, year, month, day, sod=sod'
print, ''
print, ' input'
print, '   yyyydoy : 7-digit year and day of year (2002365 is Dec 31,2002)'
print,''
print, ' outputs'
print, '   year : 4-digit year (ex. 2002)'
print, '   month: 2-digit month number - range is 1-12 (0 is not valid)'
print, '   day  : day of month - range is 1-31 (0 is not valid)'
print, '   sod  : seconds of day - range is 0-86399.99999'
print,''
return
end
