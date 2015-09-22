; docformat = 'rst'
;
;+
;Get EVE Level 2 or Level 3 data from the EVE website
;
; :Params:
;   level: in, required
;     integer data level you want to get. Current supported levels are 2 and 3.
;   start_yyyydoy: in, required
;     First day to get data for, in yyyydoy format
;   end_yyyydoy: in, optional
;     Last day to get data for, in yyyydoy format. Defaults to start_yyyydoy if not specified
;  
; :Keywords:
;  VERBOSE: in, optional    
;    Set this flag to make the program print more information to stdout, otherwise only prints stuff if there is an error. 
;  OUFN:    out  
;    On output, is set to the local name of the last file retrieved
;  version: in, optional
;    Version of data to retrieve. Gets version 2 by default
;  revision: in, optional
;    Revision of data to retrieve. Gets revision 1 by default 
;  overwrite: in, optional
;    If set, this routine will download the files fresh. By default, if a file to be retrieved is already present on the local system, 
;    that file is not retrieved again
;
;:Description:
; Path of EVE data::
;     ssw/sdo/eve/data/level2/YYYY/DOY/
;     ssw/sdo/eve/data/level3/YYYY/
;         YYYY is the year directory
;         DOY is the Day of Year directory (level 2 only)
;
;Will get both the EVL* and EVS* files
; 
;:Examples:
; ::
; eve_get_data,2,2012001,2012001 ; get EVE line and spectrum level 2 data files for 2012/001 Jan 01
; 
;:categories:
;  user
;-
pro eve_get_data,level,start_yyyydoy,end_yyyydoy,verbose=verbose,oufn=oufn,version=version,revision=revision,overwrite=overwrite

   if n_elements(version) eq 0 then version=2
   if n_elements(revision) eq 0 then revision=1
   
   ; If the url object throws an error it will be caught here
   ;CATCH, errorStatus 

;   IF (errorStatus NE 0) THEN BEGIN
;      CATCH, /CANCEL

      ; Display the error msg in the IDL output log
      ;PRINT, !ERROR_STATE.msg

      ; Get the properties that will tell us more about the error.
      ;oUrl->GetProperty, RESPONSE_CODE=rspCode, RESPONSE_HEADER=rspHdr, RESPONSE_FILENAME=rspFn
      ;PRINT, 'rspCode = ', rspCode
      ;PRINT, 'rspHdr= ', rspHdr
      ;PRINT, 'rspFn= ', rspFn

      ; Destroy the url object
      ;OBJ_DESTROY, oUrl
      ;RETURN
   ;ENDIF
   
  oUrl = OBJ_NEW('IDLnetUrl')   ; create a new IDLnetURL object
  oUrl->SetProperty, VERBOSE = keyword_set(verbose) ; Set verbose to 1 to see more info on the transacton
  oUrl->SetProperty, url_scheme = 'http' ; Set the transfer protocol
  oUrl->SetProperty, URL_HOST = 'lasp.colorado.edu'

  if n_elements(end_yyyydoy) eq 0 then end_yyyydoy=start_yyyydoy  
  yyyydoy=start_yyyydoy
  while yyyydoy le end_yyyydoy do begin
    year=yyyydoy/1000
    doy=yyyydoy mod 1000
    
    if level eq 2 then begin
      local_dir=string(format='(%"%s/level2/%04d/%03d/")',getenv('EVE_DATA'),year,doy)
    
      file_mkdir,local_dir

      type=['EVL','EVS']
      for i_type=0,1 do begin 
        for hour=0,23 do begin
  
          fn=string(format='(%"%s_L2_%07d_%02d_%03d_%02d.fit.gz")',type[i_type],yyyydoy,hour,version,revision)
          f=file_search(local_dir+fn,count=count)
          if count eq 0 or keyword_set(overwrite) then begin
            eve_wget,string(format='(%"http://lasp.colorado.edu/eve/data_access/evewebdataproducts/level2/%04d/%03d/%s")',year, doy,fn),local_dir+fn,verbose=keyword_set(verbose)
          end
          ; Print the path to the file retrieved from the remote server
          if keyword_set(verbose) then PRINT, 'filename returned = ', oufn
        end
      end
    end else if level eq 3 then begin
      local_dir=string(format='(%"%s/level3/%04d/")',getenv('EVE_DATA'),year)
    
      file_mkdir,local_dir

      fn=string(format='(%"%s_L3_%07d_%03d_%02d.fit")','EVE',yyyydoy,version,revision)
      oUrl->SetProperty, URL_PATH = string(format='(%"eve/data_access/evewebdataproducts/level3/%04d/%s")',year, fn)
      
      ; Retrieve a binary image file and write it 
      ; to the local disk's EVE data folder
      oufn = oUrl->Get(FILENAME = local_dir+ fn ) 

      ; Print the path to the file retrieved from the remote server
      if keyword_set(verbose) then PRINT, 'filename returned = ', oufn
    end
    eve_next_yyyydoy,yyyydoy,yyyydoy
  end
  
  ; Destroy the url object
  OBJ_DESTROY, oUrl
end
