; docformat = 'rst'

;+
; :Author:
;    Don Woodraska, EVE Science Processing Lead
;
; :Copyright:
;    Copyright 2013 The Regents of the University of Colorado.
;    All rights reserved. This software was developed at the
;    University of Colorado's Laboratory for Atmospheric and
;    Space Physics.
; 
; :Version:
;    $Id: eve_level3_download_merged_data.pro,v 1.2 2013/04/18 00:03:48 dlwoodra Exp $
;
;-

;+
; This function connects to the NASA SDO-EVE webserver and downloads a
; mission-length IDL saveset to the current directory. It then restores
; the EVE dataset and returns it. This data represents DAILY AVERAGES
; from EVE (the level 3 products). For 10-second irradiance data, use the
; EVE level 2 FITS files.
;
; EVE is the EUV Variability Experiment, a NASA instrument on the
; Solar Dynamics Observatory (SDO), built and operated by the
; Laboratory for Atmospheric and Space Physics at the University of
; Colorado, Boulder. 
;
; http://lasp.colorado.edu/home/eve
;
; The download location can be overridden with a keyword, and the local
; file can be deleted with another keyword. 
;
; No arguments are required, and the highest resolution spectra (0.02
; nm sampling) are retrieved by default. You can override this to just
; get a 1-angstrom spectrum, or a 1-nm spectrum with other
; keywords. Note that downloading the high-res data takes a lot
; longer, so you may want to try it on the 1nm data first.
;
; The file that is retrieved has a different name each day (it
; contains the date), so if you want to run this repeatedly for many
; days you should keep an eye on the number of files in your directory. 
;
; This always gets the latest version of EVE data. Since the level 3
; merged daily average files are recreated from scratch each day, any
; older data that gets reprocessed is up-to-date, too.
;
; This was developed for version 003, but should work for all future
; versions as well.
;
; :Keywords:
;    nm : in, optional, type=boolean
;        Set /nm to retrieve the 1-nm binned spectra
;        If neither /nm nor /angstrom are specified, the 0.02 nm
;        sampled spectrum is downloaded.
;    angstrom : in, optional, type=boolean
;        Set /angstrom to retrieve the 1-angstrom binned spectra
;        If neither /nm nor /angstrom are specified, the 0.02 nm
;        sampled spectrum is downloaded.
;    delete_local_copy_after_download : in, optional, type=boolean
;        Set /delete_local_copy_after_download to remove the
;        downloaded file from your computer after it is read. Most
;        people do not want to do this.
;    quiet : in, optional, type=boolean
;        Set /quiet to prevent status messages from being reported
;        about the file download.
;    localdir : in, optional, type=string
;        Provide localdir='/some/directory/' to set a certain location
;        for the downloaded file to reside. By default, the current
;        directory is used.
;
; :Examples:
;    Download the high-res (0.02 nm bin) spectra::
;
;      IDL> eve = eve_level3_download_merged_data() ; get high res data
;
;    Download the 1-angstrom spectra::
;
;      IDL> eve_1a = eve_level3_download_merged_data( /angstrom ) ; get 1angstrom data
;
;    Download the 1-nm spectra::
;
;      IDL> eve_1nm = eve_level3_download_merged_data( /nm ) ; get 1nm data
;
;    Download the 1-nm spectra::
;
;      IDL> eve = eve_level3_download_merged_data( /delete ) ; remove temporary file
;
;    Download the high-res spectra quietly::
;
;      IDL> eve = eve_level3_download_merged_data( /quiet ) ; be quiet
;
;    Download to a different directory from the current one::
;
;      IDL> eve = eve_level3_download_merged_data( localdir='/tmp/' ) ; use dir::
;    The returned result "eve" is an array of structures::
;
;      IDL> help,eve,/struct
;      ** Structure <192f208>, 12 tags, length=2713200, data length=2713192, refs=1:
;      PRIMARY         LONG                 0
;      PRIMARY_HEAD    STRING    Array[5]
;      LINESMETA       STRUCT    -> <Anonymous> Array[30]
;      LINESMETA_HEADER
;                      STRING    Array[46]
;      BANDSMETA       STRUCT    -> <Anonymous> Array[20]
;      BANDSMETA_HEADER
;                      STRING    Array[28]
;      DIODEMETA       STRUCT    -> <Anonymous> Array[6]
;      DIODEMETA_HEADER
;                      STRING    Array[27]
;      SPECTRUMMETA    STRUCT    -> <Anonymous> Array[1]
;      SPECTRUMMETA_HEADER
;                      STRING    Array[47]
;      MERGEDDATA      STRUCT    -> <Anonymous> Array[1079]
;      MERGEDDATA_HEADER
;                      STRING    Array[90]
;
;   The measurements are contained in the MERGEDDATA substructure. The
;   other structures (LINESMETA, BANDSMETA, DIODEMETA, and
;   SPECTRUMMETA) contain useful information for identifying the
;   contents of MERGEDDATA nested structures, and in indices that
;   correspond to the different lines, bands, and diode measurements.::
;
;     IDL> help,eve.mergeddata,/struct
;     ** Structure <203b208>, 19 tags, length=2508, data length=2508, refs=2:
;     YYYYDOY         LONG           2010120
;     CAPTURE         ULONG            86290
;     SP_IRRADIANCE   FLOAT     Array[100]
;     SP_STDEV        FLOAT     Array[100]
;     SP_PRECISION    FLOAT     Array[100]
;     SP_ACCURACY     FLOAT     Array[100]
;     BAND_IRRADIANCE FLOAT     Array[20]
;     BAND_STDEV      FLOAT     Array[20]
;     BAND_PRECISION  FLOAT     Array[20]
;     BAND_ACCURACY   FLOAT     Array[20]
;     DIODE_IRRADIANCE
;                     FLOAT     Array[6]
;     DIODE_STDEV     FLOAT     Array[6]
;     DIODE_PRECISION FLOAT     Array[6]
;     DIODE_ACCURACY  FLOAT     Array[6]
;     LINE_IRRADIANCE FLOAT     Array[30]
;     LINE_STDEV      FLOAT     Array[30]
;     LINE_PRECISION  FLOAT     Array[30]
;     LINE_ACCURACY   FLOAT     Array[30]
;     AU_FACTOR       FLOAT          0.985824
;
;   The wavelength bin centers for the spectra are contained in the
;   eve.spectrummeta.wavelength array.::
;
;     IDL> help,eve.spectrummeta
;     ** Structure <29049d8>, 2 tags, length=416, data length=416, refs=2:
;     WAVELENGTH      FLOAT     Array[100]
;     IRRADIANCE_UNITS
;                     STRING    'W/m^2/nm'
;
;   To plot the irradiance from one (the first) of the many spectra,
;   index the mergeddata structure:: 
;
;     IDL>plot, eve.spectrummeta.wavelength,
;     eve.mergeddata[0].sp_irradiance, /ylog, ps=10, xtitle='Wavelength
;     (nm)', ytitle='Irradiance ' + eve.spectrummeta.irradiance_units 
;
;-
function eve_level3_download_merged_data, nm=nm, angstrom=angstrom, delete_local_copy_after_load=delete_local_copy_after_load, quiet=quiet, localdir=localdir

   ; usage examples:
   ; eve     = eve_level3_download_merged_data() ; get high res data
   ; eve_1nm = eve_level3_download_merged_data( /nm ) ; get 1nm data
   ; eve_1a  = eve_level3_download_merged_data( /angstrom ) ; get 1nm data
   ; eve     = eve_level3_download_merged_data( /delete ) ; remove temporary file
   ; eve     = eve_level3_download_merged_data( /quiet ) ; be quiet
   ; eve     = eve_level3_download_merged_data( localdir='/tmp/' ) ; use dir

   ; note that the website only has the latest version

   ; default is the high res spectrum file
   filepatt='' ;default is '*EVE_L3_merged_???????_???.sav*'

   ; override with keyword to retrieve 1 nm file
   if keyword_set(nm) then filepatt='_1nm' ;'*EVE_L3_merged_1nm_???????_???.sav*'

   ; override with keyword to retrieve 1 angstrom file
   if keyword_set(angstrom) then filepatt='_1a';'*EVE_L3_merged_1a_???????_???.sav*'

   if size(localdir,/type) eq 0 then $
      localdir='.' ; current directory is the default
   localdir=localdir + path_sep()
   
   CATCH, errorStatus
   if (errorStatus ne 0) then begin
      ;CATCH,/CANCEL ; disable future error handling

      ; get whatever info you can about the error
      oUrl->GetProperty, RESPONSE_CODE=response, RESPONSE_HEADER=rspHdr

      ; display the error info
      print,''
      print,'ERROR: code=',strtrim(response,2)
      print,'ERROR: rspHdr=',rspHdr

      ; cleanup
      OBJ_DESTROY, oUrl

      ; ask to try again
      result = dialog_message('The EVE file download did not work. Try again?',/question)
      if strmatch(result,'Yes') eq 0 then begin
         print,'ERROR: eve_level3_download_merged_data aborted by user request'
         return,-1
      endif
   endif

   ; try to connect to URL to get latest merged file

   urlbase = 'http://lasp.colorado.edu/eve/data_access/evewebdataproducts/merged/'
   oUrl = OBJ_NEW('IDLnetUrl')

   ; try to get directory listing
   thehtml = oUrl->Get(url=urlbase,/string_array)

   ; parse the html for the latest save file
   x=where(strmatch(thehtml,'*EVE_L3_merged'+filepatt+'_???????_???.sav*') eq 1,n_x)
   if n_x eq 0 then begin
      print,'ERROR: no save file found on EVE web site'
      stop
   endif

   ; extract the returned filename string buried in the html line
   thefile = stregex(thehtml[x[0]],'EVE_L3_merged'+filepatt+'_[0-9]{7}_[0-9]{3}\.sav',/extract)
   url = urlbase + thefile[0]
   localfile = localdir + thefile

   ; attempt to download the IDL savefile
   if keyword_set(quiet) eq 0 then $
      print,'Downloading latest EVE Level 3 merged file -> ',url
   result = oUrl->Get(URL=url,file=localfile)

   ; cleanup
   oUrl->CloseConnections
   OBJ_DESTROY, oUrl            ; cleanup

   if keyword_set(quiet) eq 0 then $
      print,'Latest EVE Level 3 merged file downloaded '+localfile
   
   ; restore the downloaded savefile, contains a variable "eve"
   restore,localfile ; contains eve structure

   ; we can safely delete the downloaded file now
   ; using this: file_delete,localfile if the user set the keyword
   if keyword_set(delete_local_copy_after_download) then begin
      file_delete, verbose=(keyword_set(quiet) eq 0), localfile
   endif

return, eve
end
