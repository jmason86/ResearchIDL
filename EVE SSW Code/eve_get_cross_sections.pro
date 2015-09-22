;docformat = 'rst'
;+
;Read molecular cross section table needed to calculate "Stan bands"
;:Returns:
; An array of the following format::
;  col #0 is wavelength (unnits of nm)
;  col #1 is atomic oxygen (0)
;  col #2 is molecular oxygen (O2)
;  col #3 is molecular nitrogen (N2)
;  col #4 is helium (He)
;  col #5 is hydrogen (H)
;  Units are cm^2
; 
;note that the first data row is all zero
; 
;:Keywords:
;  cal_data_path: in, optional
;    Path to cross_sections.dat file, the data file this routine loads. If not
;    set, the program uses environment variable $EVE_CAL_DATA if set, or
;    the path to this .pro file otherwise
;:Categories:
;  internal
;  
;:Private_file:
;-
function eve_get_cross_sections,cal_data_path=cal_data_path

; based on Stan Solomon's get_atmos_corr.pro from TIMED-SEE

common eve_get_cross_sections_static, xsa

if size(xsa,/type) eq 0 then begin
   header=''
   if n_elements(cal_data_path) eq 0 then cal_data_path=getenv('EVE_CAL_DATA')
   if strlen(cal_data_path) eq 0 then cal_data_path=file_dirname(routine_filepath('eve_get_cross_sections',/either))
   openr,lun, cal_data_path+'/cross_sections.dat',/get_lun
   xsa=dblarr(6,802)
   readf,lun,header
   readf,lun,xsa
   close,lun
   free_lun,lun
   xsa[1:5,*] *= 1.e-18 ; convert megabarns to cm^2
endif

; format of returned array xsa
; col #0 is wavelength (unnits of nm)
; col #1 is atomic oxygen (0)
; col #2 is molecular oxygen (O2)
; col #3 is molecular nitrogen (N2)
; col #4 is helium (He)
; col #5 is hydrogen (H)

; note that the first data row is all zero

return,xsa
end
