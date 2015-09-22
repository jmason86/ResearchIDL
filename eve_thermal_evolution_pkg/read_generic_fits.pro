;+
; NAME:
;  read_generic_fits
;
; PURPOSE:
;  Read all of the HDUs in a FITS file, creating a merged set of
;  structures for the data, and string arrays for the keywords.
;
; CATEGORY:
;  SDO-EVE lib
;
; CALLING SEQUENCE:
;  IDL> data = read_generic_fits( filename [, /verbose][,_extra=_extra] )
;
; INPUTS:
;  filename : a scalar string for a FITS file
;             If comressed with gzip (.gz) spawn is called to
;             decompress into a temporary file which is read and
;             subsequently deleted.
;
; OPTIONAL INPUTS:
;  _extra : additional parameters to pass to mrdfits (refer to mrdfits.pro)
;
; KEYWORD PARAMETERS:
;  /verbose : report information on each HDU read (nothing is reported normally)
;
; OUTPUTS:
;  data : returned data is a structure that contains structures from
;         each HDU and an associated string array for the keywords
;
; OPTIONAL OUTPUTS:
;  none
;
; COMMON BLOCKS:
;  none
;
; SIDE EFFECTS:
;  Substructure names correspond to the keyword "EXTNAME", if absent,
;  then a default naming convention is used.
;
; RESTRICTIONS:
;  Requires fits_info.pro and mrdfits.pro (with it's own dependencies).
;
;  This should be compatible with all OSes, but we only test on Linux
;  and Mac OS X. However, if you encounter problems, try decompressing
;  the FITS files before you call this function.
;
;  You can't get something for nothing. Gzip compressed files
;  need to be decompressed to be properly read if multiple HDUs exist
;  in the FITS file. This takes time and disk space. If the /dev/shm
;  directory exists (linux tmpfs volume) then it is used. This
;  restricts file sizes to about half the size of the available
;  RAM. If that directory does not exist, then the current (.)
;  directory is used, so you need to be in a writeable directory.
;
;  The temporary filename used is based on the system clock. There
;  could be conflicts, so this code is not strictly thread-safe, but
;  most users running several processes will never encounter a
;  conflict. The filename uses 43 decimal digits from the systime(1)
;  function which includes some digits beyond the precision of the
;  clock. It is unikely (but possible) that successive calls would
;  occur on the same clock tick.
;
; EXAMPLE:
;  IDL> data=read_generic_fits('EVS_L2_2010120_00_002_01.fit.gz',/verbose)
;
; MODIFICATION HISTORY:
;  2/21/10 DLW Modified from CJs pre-release version.
;
; $Id: read_generic_fits.pro,v 2.3 2011/02/21 22:45:43 dlwoodra Exp $
;-
function read_generic_fits,infname,verbose=verbose,_extra=extra
  do_del=0
  if strlowcase((reverse(strsplit(infname,'.',/extract)))[0]) eq 'gz' then begin
     id = strtrim(string(systime(1)*1e33,form='(f45.1)'),2) ;random number
     id = strmid(id,0,strlen(id)-1) ; trim off the period
     if file_test('/dev/shm') eq 0 then tmp_path='.'+path_sep() else $
        tmp_path='/dev/shm/'
     infn = tmp_path+'eve_'+id
     file_delete,/allow_nonexistent,infn
     cmd='gzip -dc '+infname+' > '+infn ; plain > is valid for csh- & sh-based shells
     spawn,cmd,result
     do_del=1
  endif else infn = infname
  ; determine the number of HDUs to read
  fits_info,/silent,infn,n_ext=n_hdus ; the only programmatic way to get number of HDUs?
  openr,inf,infn,/get_lun
  status=0
  hdu=0
  this_hdu=mrdfits(inf,0,this_header,/unsigned,silent=~keyword_set(verbose),_extra=extra,status=status)
  while status ge 0 and hdu le n_hdus do begin
    if hdu eq 0 then begin
      result=create_struct('primary',this_hdu,'primary_head',this_header)
    endif else begin
      w=where(strmid(this_header,0,8) eq 'EXTNAME ',count)
      if count eq 1 then begin
        this_hdu_name=strtrim((strsplit(this_header[w],"'",/extract))[1])
      endif else begin
        this_hdu_name=string(format='(%"HDR%03d")',hdu)
      endelse
      this_hdr_name=this_hdu_name+"_HEADER"
      result=create_struct(result,this_hdu_name,this_hdu,this_hdr_name,this_header)
    endelse
    hdu++
    ;0 means skip zero hdus
    this_hdu=mrdfits(inf,0,this_header,/unsigned,silent=~keyword_set(verbose),_extra=extra,status=status)
  endwhile
  free_lun,inf
  if do_del eq 1 then file_delete,infn
  return,result
end
