;docformat = 'rst'
;
;+
;Callback required by IDLnetURL object. This just prints any info messages
;to the IDL log and tells the object to continue.
;
;:Params:
;  status: in, required
;  progress: in, required
;  data: in, required
;
;:Categories:
;  internal
;:Private:
;-
FUNCTION eve_get_data_Url_Callback, status, progress, data
   ; print the info msgs from the url object
   PRINT, status
   ; return 1 to continue, return 0 to cancel
   RETURN, 1
END

;
;+
;Gets a file from the Internet and writes it in a given location on the local machine. This is a wrapper
;around the IDLnetURL object.
;:Params:
;  url: in, required
;    URL to get from
;  local_fn: in, required
;    filename of the file to write to on the local machine, may contain a relative or absolute path
;:Keywords:
;  verbose: in, optional
;    if set, prints more messages to the IDL log. By default, only error messages are printed.
;    
;:Categories:
;  utility
;-
pro eve_wget,url,local_fn,verbose=verbose
  oUrl = OBJ_NEW('IDLnetUrl')   ; create a new IDLnetURL object
  if keyword_set(verbose) then oUrl->SetProperty, CALLBACK_FUNCTION ='eve_get_data_url_Callback' ; Specify the callback function
  parts=parse_url(url)
  oUrl->SetProperty, VERBOSE = keyword_set(verbose) ; Set verbose to 1 to see more info on the transacton
  oUrl->SetProperty, url_scheme = parts.scheme ; Set the transfer protocol
  oUrl->SetProperty, URL_HOST = parts.host;

  oUrl->SetProperty, URL_PATH = parts.path
      
  ; Retrieve a binary image file and write it 
  ; to the local disk's EVE data folder
  oufn = oUrl->Get(FILENAME = local_fn ) 


end