; GETTIMEARR
;+
; NAME:
;       GETTIMEARR
; PURPOSE:
;       Return a string array of times corresponding to a set of images
;
; CALLING SEQUENCE:
;       gettimearr, infile, multi=multi,strmd=strmd $
;       begn=begn,lnth=lnth,fits_hdr=fits_hdr,lasco=lasco,ut_times=ut_times,$
;       not_corrected = not_corrected
;
; INPUTS:
;       infile = array of files of nz elements
;
; KEYWORDS:
;       multi - if set, reads in an array of multiple images in a
;       single fits file with a single header
;       strmd - if set, gets the time from the input infile name (the
;       output of the EIT catalog eit_catrd.pro contains not only
;       the data file's name but also other information.
;       begn - if named along with keyword strmd, denotes the location
;       in infile containing the first character in the time of the image
;       lnth - if named along with keyword strmd, gives the length of the
;       time in characters.  Default is 0
;       fits_hdr - placeholder in case someone has a multiple-structured
;       fits header.  Default is 20
;       lasco - set for lasco fits files
;       ut_times - set for format 'yyyy/mm/dd hh:mm UT', otherwise it's
;       'yyyy/mm/dd hh:mm:ss.sss'
;       not_corrected - set to reflect "DATE-OBS" in EIT header and
;       not "CORRECTED DATE_OBS"
;	secchi - to read SECCHI/EUVI files
;
; EXAMPLES:
;       Get the times from an array of files from the EIT catalog
;       IDL> file = eit_catrd('10-jan-1997')
;       IDL> print, file(0)
;       1997-Jan-10 00:00:07 0 s 195::Clear 1x( 9,5) @(33,372/320,531) EFR19970110.000007
;       IDL> timearr = gettimearr(file, /strmd)
;
;       Read in a bunch of fits files and get the times
;       IDL> file = findfile('*.fits')
;       IDL> timearr = gettimearr(file)
;
; RESTRICTIONS:
;       Assumes the fits header contains the variables "date-obs" and
;       "time-obs"
;       Program doesn't think much on its own - relies on keywords from
;       the user
;
; NOTES:
;
; PROCEDURES USED:
;       eit_file2path, eit_fxpar, n_elements, is_fits, headfits
;
; MODIFICATION HISTORY:
;       bjthompson, 28 December, 1996.
;       A. N. Zhukov, Aug. 2004, to accomodate the local filenames.
;	A. N. Zhukov, Feb. 2008, to include SECCHI/EUVI data
;
;
FUNCTION gettimearr2SWAP,infile, dirfiles, multi=multi,strmd=strmd, $
   begn=begn, lnth=lnth,fits_hdr=fits_hdr, lasco=lasco, ut_times=ut_times,$
   not_corrected = not_corrected, secchi = secchi



;eitlz=getenv('eit_lz')

infile = infile(WHERE(infile NE ''))



IF NOT KEYWORD_SET(lasco) THEN lasco = 0
IF NOT KEYWORD_SET(ut_times) THEN ut_times = 0
IF NOT KEYWORD_SET(multi) THEN multi = 0
IF NOT KEYWORD_SET(strmd) THEN strmd = 0 else strmd = 1
IF NOT KEYWORD_SET(begn) THEN  begn = 0
IF NOT KEYWORD_SET(not_corrected) THEN not_corrected=0 else not_corrected=1
IF NOT KEYWORD_SET(lnth) THEN lnth = 20

if not keyword_set(secchi) then begin

print, '-------------------------'
print, ' EIT data '
print, '-------------------------'

   filelist = infile
       sz  = n_elements(filelist) ;& print, sz
       timearr = STRARR(sz)
       FOR ii=0,sz-1 DO BEGIN
           filename = filelist[ii]


           hdr = headfits(strlowcase(filename))

;           hdr = headfits(eitlz+strtrim(strmid(filename, 3, 4), 2)+'\'+strtrim(strmid(filename, 7, 2), 2)+'\'+strlowcase(filename))
;            if (eit_fxpar(hdr,'corrected date_obs') ne 0) then timearr(ii) = $
;                eit_fxpar(hdr,'corrected date_obs') else $
                timearr(ii) = fxpar(hdr,'date-obs')
;            if not (not_corrected) then $
 ;           timearr(ii) = eit_fxpar(hdr, 'corrected date_obs') else $
  ;          timearr(ii) = eit_fxpar(hdr, 'date-obs') + ' '+$
;               eit_fxpar(hdr, 'time-obs')



     ENDFOR



timearr = anytim(timearr,/ecs)
if (ut_times) then timearr = strmid(timearr,0,16) + ' UT'



endif else begin

print, '-------------------------'
print, ' SECCHI/EUVI data '
print, '-------------------------'

   filelist = infile
       sz  = n_elements(filelist) ;& print, sz
       timearr = STRARR(sz)
       	FOR ii=0,sz-1 DO BEGIN

           filename = filelist[ii]
	   ;hdr = headfits(dirfiles+filename)
	   hdr = headfits(filename) ;Changed for SDO
	   timearr(ii) = eit_fxpar(hdr, 'date-obs')
	ENDFOR

print,'here4'
stop


timearr = anytim(timearr,/ecs)
if (ut_times) then timearr = strmid(timearr,0,16) + ' UT'

endelse



RETURN, timearr
END
