; docformat = 'rst'

;+
; :Author:
;    Don Woodraska
;
; :Copyright:
;    Copyright 2017 The Regents of the University of Colorado.
;    All rights reserved. This software was developed at the
;    University of Colorado's Laboratory for Atmospheric and
;    Space Physics.
;
; :Version:
;    $Id: get_lisird_data.pro 79261 2017-11-06 21:01:35Z dlwoodra $
;
;-

;+
; This function is used internally to retrieve the URL contents
; as a string array. You can provide any HTTP URL and this returns
; the strings that would be sent to a web browser for parsing.
;
; :Params:
;    url: in, required, type=string
;      The restful string to request e.g. 'http://lasp.colorado.edu:80/lisird/latis/sorce_ssi.csv?time,irradiance&time>=2017-01-20&format_time(yyyyDDD)&wavelength=121.5'
;
; :Returns: 
;    This returns a string array of html that you would get from a web
;    browser pointing at the provided URL.
;
;-
function latis_geturlasstring, url

  catch, errorstatus
  if (errorstatus ne 0) then begin
     catch,/cancel
     oURL->getproperty,response_code=respcode, response_header=hdr
     print,'***An error has occurred trying to contact LATIS***'
     print,'*** respose code='+strtrim(respcode,2)+' ***'
     print,'*** response header='+strtrim(hdr,2)+' ***'
     obj_destroy,oURL
     return,''
  endif

  oURL = OBJ_NEW('IDLnetURL')
  print,'calling URL = '+url
  str = oURL->Get(URL=url, /string_array) ; store result in a string array
  oURL->CloseConnections ; need to close the connection in the IDLNetURL object
  OBJ_DESTROY, oURL ; cleanup to prevent memory leaks
  if n_elements(str) lt 2 then begin
     print,'WARNING: latis_geturlasstring - not much returned from url '+url
  endif

return,str
end

;+
; This function extracts the dataset names from the URL string array
; returned by a call to http://lasp.colorado.edu/lisird/latis
;
; :Params:
;   str: in, required, type=strarr
;     This is an array of strings that contain the html from the web
;     site. It has the dataset names embedded in the html code.
;
; :Returns:
;   This returned an aray of string containing all known dataset
;   names.
;
;-
function latis_getdatasetsfromstring, str
  
  ; find the entries that match the dataset pattern
  ; pattern is <a href="oneword">oneword</a></dt>
  matches=stregex(str,'<dt><a href="([_a-zA-Z0-9])+">([_a-zA-Z0-9])+</a></dt>$',/extract)
  midx=where(strlen(matches) gt 2,n_midx)
  datasethtml=matches[midx]

  ; strip off the html (extract element 1 from each string broken by
  ; the double quote character
  datasets=strarr(n_elements(datasethtml))
  for i=0L,n_elements(datasets)-1 do datasets[i] = (strsplit(datasethtml[i],'"',/extract))[1]

return, datasets
end

;+
; This function gets the dataset contents fields and data types.
; It returns the number of fields in the dataset.
;
; :Params:
;    dataset: in, required, type='string'
;       The datset name used to access latis.
;    datatypes: out, optional, type='string array'
;      The IDL data types for the fields.
;    fields: out, optional, type='string array'
;      The data fields in the dataset.
;    record: out, optional, type='structure'
;      The IDL data structure that can hold all of these fields.
;
; :Returns:
;   This returns the number of fields. More useful information is in
;   the fields parameter.
;
;-
function latis_getdatasetfields, dataset, datatypes, fields, record

  url='http://lasp.colorado.edu/lisird/latis/'+strtrim(dataset,2)+'.dds'
  str=latis_geturlasstring(url)
  ; strip off leading/trailing blanks
  str=strtrim(str,2)
  ; find all lines that end in a semicolon that do not have a close bracket
  ; note, only Float64, Int32, and String are the returned types
  strvar=stregex(str,'^([FIS])([a-zA-Z0-9])+ ([a-zA-Z_0-9])+;$',/extract)
  ; remove blank lines
  gd=where((strlen(strvar) gt 0) and (strvar ne 'Int32 index;'),n_gd)
  ; add filter for Int32 index bug in usaf_sunspot datasets
  strvar=strvar[gd]

  ;trim off the semicolon, extract data types and data fields available
  fields=strvar
  datatypes=strvar
  latisdatatypes=['Float64','Int32','String']
  cmd='record={'
  for i=0L,n_elements(strvar)-1 do begin
     typ_var=(strsplit(strvar[i],';',/extract))[0]
     type_vari=strsplit(typ_var,' ',/extract)
     latistype = type_vari[0]
     fields[i] = type_vari[1]
     ; convert latis type to an IDL data type
     case latistype of
        'Float64': idltype='double'
        'Int32': idltype='long'
        'String': idltype='string'
        else: stop,'unknown type '+latistype
     endcase
     datatypes[i] = idltype
     if i ne n_elements(strvar)-1 then $
        cmd += fields[i]+':'+idltype+'(0),' else $
        cmd += fields[i]+':'+idltype+'(0)}'
  endfor
  result=execute(cmd) ; define record

return,n_elements(fields)
end

;+
; This function creates a new data structure with the field
; prepended. This is used to insert the JD field.
;
; :Params:
;   data: in, required, type=structure
;     The initial structure.
;   newfieldtagname: in, required, type=string
;     The new tag name to insert.
;   newfieldtype: in, required, type=string
;     The data type of the new tag.
;   newfield: in, required, type=any
;     The value to assign to the new tag.
; 
; :Returns:
;   This returns a new structure with the new tag inserted into the
;   first location.
;
;-
function latis_prependstructurefield, data, newfieldtagname, newfieldtype, newfield

  tags = tag_names(data[0])
  ; build a string command that constructs one record
  cmd='newrec={'+newfieldtagname+':'+newfieldtype+'(newfield[0]),'
  for i=0L,n_tags(data[0])-1 do begin
     cmd += tags[i]+':data[0].'+tags[i]
     if i eq n_tags(data[0])-1 then cmd+='}' else cmd+=','
  endfor
  ; create the structure newrec by executing the string
  result=execute(cmd)
  ; make the output array of structures match the size of data
  newdata=replicate(newrec,n_elements(data))
  ; assign all of the tags
  newdata.(0)=newfield
  for i=1,n_elements(tag_names(newdata))-1 do newdata.(i) = data.(i-1)
return,newdata
end

;+
; This function creates a new data structure with the field
; inserted into data.data.
;
; :Params:
;   data: in, required, type=structure
;     The initial structure.
;   newfieldtagname: in, required, type=string
;     The new tag name to insert.
;   newfieldtype: in, required, type=string
;     The data type of the new tag.
;   newfield: in, required, type=any
;     The value to assign to the new tag.
; 
; :Returns:
;   This returns a new structure with the new tag inserted into the
;   first location of data.data.
;-
function latis_insertstructurefield, data, newfieldtagname, newfieldtype, newfield

  tmprec = latis_prependstructurefield(data.data, newfieldtagname, newfieldtype, newfield)

  tags = tag_names(data[0])
  ; build a string command that constructs one record
  cmd='newdata={'
  for i=0L,n_tags(data[0])-1 do begin
     if tags[i] eq 'DATA' then cmd += 'data:tmprec' else $
        cmd += tags[i]+':data[0].'+tags[i]
     if i eq n_tags(data[0])-1 then cmd+='}' else cmd+=','
  endfor
  ; create the structure newrec by executing the string
  result=execute(cmd)

return,newdata
end

;+
; This function tries to create a spectrum array based on one
; wavelength array. This will be slow.
;
; :Params:
;    data: in, required, type=structure
;      The data as delievered from LATIS in a flat structure.
;      The structure is required to have a tag named TIME, and 
;      either WAVELENGTH or ENERGY.
;
; :Returns:
;   This returns a new reorganized structure, or if it is not possible
;   to determine the spectrum it returns the input data structure.
;
;-
function latis_createspectra, data

  ; select the tag index to use for sorting and uniqueness
  tags=tag_names(data)
  sorttag=-1

  widx=where(tags eq 'WAVELENGTH',n_widx)
  if n_widx eq 1 then begin
     sorttag=widx[0]
     sorttagname=tags[widx[0]]
  endif

  eidx=where(tags eq 'ENERGY',n_eidx) ; for minxss
  if n_eidx eq 1 then begin
     sorttag=eidx[0]
     sorttagname=tags[eidx[0]]
  endif
  
  if sorttag ne -1 then begin
     ; are there multiple wavelengths/energy
     sortarray=data[uniq(data.(sorttag),sort(data.(sorttag)))].(sorttag)
     if n_elements(sortarray) ge 2 then begin
        ; it is possible to collect data into spectra
        ; recall that time is a string, so it can be tested for uniqueness 
        sorttime=data[uniq(data.time,sort(data.time))].time
        if n_elements(sorttime) eq 1 then return,data ; only one time was found

        ; construct the command to define the record
        cmd='innerrec={'
        for i=0L,n_elements(tags)-1 do begin
           case tags[i] of
              'WAVELENGTH':
              'ENERGY':
              'TIME': begin
                 cmd+=tags[i]+':""'
                 if i ne n_elements(tags)-1 then cmd +=',' else cmd += '}'
              end
              else: begin
                 cmd+=tags[i]+':dblarr('+strtrim(n_elements(sortarray),2)+')'
                 if i ne n_elements(tags)-1 then cmd +=',' else cmd += '}'
              endelse
           endcase
        endfor
        result=execute(cmd)

        cmd='newdatarec={' ; will replicate struct over time
        cmd += sorttagname+':sortarray,'
        cmd += 'data:replicate(innerrec,n_elements(sorttime))}'
        result=execute(cmd)

        innerrectags=tag_names(innerrec)

        newdata=newdatarec
        newdata.data.time=sorttime

        ; newdata is a scalar structure
        ; newdata={sortarray:dblarr(), data:relpicate({},n_elements(sortarray))}

        for iwave=0L,n_elements(sortarray)-1 do begin
           for itime=0L,n_elements(sorttime)-1 do begin
              gd=where(data.time eq sorttime[itime] and data.(sorttag) eq sortarray[iwave],n_gd)
              if n_gd eq 1 then begin
                 for i=0L,n_elements(tag_names(newdatarec.data))-1 do begin
                    if innerrectags[i] ne 'TIME' then begin ; time was already assigned above
                       thistag=where(tags eq innerrectags[i])
                       ; assign one-element at a time
                       newdata.data[itime].(i)[iwave] = data[gd[0]].(thistag)
                    endif
                 endfor
                 ; could speed up if we could do vector assignment
                 ; does latis always return data in wavelength/energy order?
              endif
           endfor
        endfor
     endif else return,data ; only one unique value sortarray, not a spectrum
  endif else return,data ; no sortable field (wavelength or energy) found

return,newdata
end

;+
; This function converts a 7-digit year and day of year into a Julian
; date. This is a very old version from Barry Knapp.
; This could be replaced with the IDL julday function is latis default
; time is changed to yyyymmdd.
;
; :Params:
;    yd: in, required, type=long
;     This is the year and day of year to convert to a julian date.
;
; :Returns:
;   A converted julian date.
;-
FUNCTION latis_barrysydtojd, yd
;
;
; Returns the double-precision Julian Day Number,
; given a Gregorian date of the form yyyyddd.ddd.

; B. G. Knapp, 87/02/04

  y = LONG(yd/1000)+9999L
  d = (ABS(yd) MOD 1000)-1931000.5D0
  RETURN,LONG(y*365.25D0)+y/400-y/100+d

END

;+
; This function downloads data from LATIS, the same one that feeds LISIRD.
; It is an example of how to use LATIS programatically. This function
; is stand-alone, but requires IDL version 6.0 or higher for the
; IDLNetURL object. 
; 
; :Params:
;   wave_nm_in: in, optional, type=float
;     The closest desired wavelength. For the SORCE_SSI dataset,
;     the product behind this has 1nm uniform sampling on 0.5 nm
;     centers, and the valid range for that is 117.5-309.5. 
;     If not provided, all wavelengths are requested (could be much slower).
; 
; :Keywords:
;   dataset: in, optional, type=string
;     The dataset name in LISIRD. See examples for valid
;     datasets. Default is sorce_ssi
;     names.
;   jd: in, optional, type=boolean
;     Set this keyword to convert time to julian days. This is ignored
;     if the timeformat keyword is specified.
;   spectrum: in, optional, type=boolean
;     Set this keyword to let the code try to reorganize the data into
;     arrays of spectra. If successful, this will change the returned
;     data structures to include a nested structure.
;   mintime: in, optional, type=string
;     Set this to a yyyy-mm-dd string to only retrieve data from
;     this date forward in time. This is passed to latis with no 
;     changes.
;   maxtime: in, optional, type=string
;     Set this to a yyyy-mm-dd string to only retrieve data from
;     this date backward in time. This is passed to latis with no
;     changes.
;   timeformat: in, optional, type=string
;      The default time format is yyyyDDD (the most efficient string), 
;      but this can be set to any of the formats suported by latis.
;      This is case sensitive. Valid values do not appear to be
;      documented for lisird3.
;
; :Examples:
;   To list all of the datasets that are available, call with no
;   args or keywords::
;      IDL> data = get_lisird_data()
;
;   The simplest data download is for SORCE (default dataset is sorce_ssi)::
;      IDL> data = get_lisird_data( 121.5 )
;
;   The composite lyman-alpha dataset can be downloaded similarly, and
;   an additional julian date field can be added to any dataset by
;   setting /jd::
;      IDL> data=get_lisird_data( dataset='composite_lyman_alpha', /jd )
;   
;   There are lots of datasets in LATIS. You can specify them using
;   the dataset keyword as follows::
;      IDL> data = get_lisird_data( 30.5, dataset='sdo_eve_ssi_1nm_l3' )
;
;   You can retrieve the whole spectrum instad of just one wavelength
;   using the /spectrum keyword as follows::
;      IDL> data = get_lisird_data( dataset='sdo_eve_ssi_1nm_l3',/spectrum )
;
;   Below is a list of datasets that are accessible through LATIS directly.::
;     american_sunspot_number
;     balmaceda_sunspot_index
;     bremen_composite_mgii
;     bremen_composite_mgii_v4
;     cak
;     composite_lyman_alpha
;     composite_mg_index
;     debrecen_photoheliographic
;     debrecen_photoheliographic_daily
;     debrecen_photoheliographic_group
;     debrecen_photoheliographic_spot
;     fism_daily_files
;     fism_flare_files
;     gome_mgii
;     greenwich_sunspots
;     hathaway_sunspot_area
;     historical_tsi
;     international_sunspot_number
;     kodaikanal_tilt_group
;     kodaikanal_tilt_individual
;     lasp_sunspot_darkening
;     lasp_sunspot_darkening_v02r00
;     lean_spot_fac
;     lemr_datasets
;     log
;     mt_wilson_tilt_group
;     mt_wilson_tilt_individual
;     noaa11_mgii
;     noaa16_mgii
;     noaa17_mgii
;     noaa18_mgii
;     noaa19_mgii
;     noaa7_mgii
;     noaa9_mgii
;     noaa_radio_flux
;     nrl2_facular_brightening
;     nrl2_facular_brightening_cycle
;     nrl2_facular_brightening_cycle_v02r00
;     nrl2_facular_brightening_v02r00
;     nrl2_facular_brightening_v02r01
;     nrl2_historical_ssi
;     nrl2_historical_ssi_cycle_hsrg_v02r01
;     nrl2_historical_ssi_cycle_silso_v02r01
;     nrl2_historical_ssi_hsrg_v02r01
;     nrl2_historical_ssi_silso_v02r01
;     nrl2_historical_ssi_wavelength
;     nrl2_historical_ssi_wavelength_hsrg_v02r01
;     nrl2_historical_ssi_wavelength_silso_v02r01
;     nrl2_historical_tsi
;     nrl2_historical_tsi_cycle
;     nrl2_historical_tsi_cycle_hsrg_v02r01
;     nrl2_historical_tsi_cycle_silso_v02r01
;     nrl2_historical_tsi_hsrg_v02r01
;     nrl2_historical_tsi_silso_v02r01
;     nrl2_observational_composite_tsi
;     nrl2_observational_composite_tsi_v02r01
;     nrl2_ssi_P1D
;     nrl2_ssi_P1D2_bin
;     nrl2_ssi_P1D_files
;     nrl2_ssi_P1M
;     nrl2_ssi_P1M_files
;     nrl2_ssi_P1Y
;     nrl2_sunspot_darkening
;     nrl2_sunspot_darkening_cycle
;     nrl2_sunspot_darkening_cycle_v02r00
;     nrl2_sunspot_darkening_v02r00
;     nrl2_sunspot_darkening_v02r01
;     nrl2_tsi_P1D
;     nrl2_tsi_P1M
;     nrl2_tsi_P1Y
;     nrlssi
;     omi_cah_P1D
;     omi_cah_P1D_alt
;     omi_cak_P1D
;     omi_cak_P1D_alt
;     omi_mgi_P1D
;     omi_mgi_P1D_alt
;     omi_mgii_P1D
;     omi_mgii_P1D_alt
;     omi_solar_indices
;     omi_solar_indices_alt
;     penticton_radio_flux_adjusted
;     penticton_radio_flux_observed
;     properties
;     satire_s_ssi
;     satire_s_ssi_orig
;     satire_s_tsi
;     satire_t_s_ssi
;     satire_t_s_tsi
;     scia_mgii
;     sdo_eve_bands_l3
;     sdo_eve_diodes_l3
;     sdo_eve_lines_l3
;     sdo_eve_ssi_1nm_l3
;     set_mgii
;     sfo_sigma_k
;     sfo_sigma_r
;     sfo_solar_indices
;     sfo_sunspot_indices
;     sfo_sunspot_indices_v2009
;     sme_ssi
;     sorce_mg_index
;     sorce_ssi_l3
;     sorce_tsi_24hr_l3
;     sorce_tsi_6hr_l3
;     stara_hmi_sunspots_P1D
;     stara_mdi_sunspots_P1D
;     tcte_tsi_24hr
;     tcte_tsi_6hr
;     timed_see_egs_l2a_files
;     timed_see_egs_ssi_l2
;     timed_see_lines_l3
;     timed_see_lines_l3a
;     timed_see_ssi_l3
;     timed_see_ssi_l3a
;     timed_see_xps_diodes_l2
;     timed_see_xps_diodes_l2a
;     timed_see_xps_diodes_l3
;     timed_see_xps_diodes_l3a
;     timed_see_xps_l2a_files
;     timed_see_xps_ssi_l4
;     timed_see_xps_ssi_l4a
;     uars_solstice_ssi
;     usaf_sunspot_regions
;     usaf_sunspot_regions_2017
;     usaf_sunspot_regions_files
;     usaf_sunspot_regions_final
;
;-
function get_lisird_data, wave_nm_in, dataset=dataset, jd=jd, spectrum=spectrum, $
  mintime=mintime, maxtime=maxtime, timeformat=timeformat

  ; This is for 1 nm bins only

  ; download sorce solstice 1nm bin at specified wavelength (in nm)
  
  skip_wavelength=0 ; 1 mean ignore wavelength, 0 means use wavelength arg

  if size(wave_nm_in,/type) eq 0 and size(dataset,/type) eq 0 then begin
    print,'Usage: IDL> data = get_lisird_data(wave_nm_in)'
    print,' wave_nm_in is required and must be between 117 and 310nm'
    print,' returning with no data'
    print,''
    print,'Polling LISIRD for datasets...'
    ;
    ; get information about all lisird data sets that are available
    ;
    str=latis_geturlasstring('http://lasp.colorado.edu:80/lisird/latis')
    if n_elements(str) lt 2 then return,-1
    ; find available datasets in the string
    datasets = latis_getdatasetsfromstring(str)

    print,'Available datasets from LISIRD'
    for i=0,n_elements(datasets)-1 do print,'  '+datasets[i]

    return,datasets
 endif else begin
    ; both are not unset, but one of them could be
    if size(dataset,/type) eq 0 then dataset='sorce_ssi'
    if size(wave_nm_in,/type) eq 0 then begin
       wave_nm_in = 0.5
       skip_wavelength=1 ; set to 1 to disregard wavelength
    endif
 endelse


  if n_elements(wave_nm_in) ne 1 then begin
    print,'WARNING: get_lisird_data only works with scalar argments, truncating to the first element'
    print,' Next time, try calling with no argument and supply /spectrum to get the whole spectrum.'
  endif
  wave_nm = wave_nm_in[0]

  if size(dataset,/type) eq 0 then dataset='sorce_ssi'

  ; get datasets as a string array
  str=latis_geturlasstring('http://lasp.colorado.edu:80/lisird/latis')
  valid_datasets = latis_getdatasetsfromstring(str)

  ; is dataset valid?
  gd=where(dataset eq valid_datasets, n_gd)
  if n_gd eq 0 then begin
     for i=0,n_elements(valid_datasets)-1 do print,valid_datasets[i]
     print,''
     print,'get_lisird_data did not get a valid dataset: '+dataset
     print,' please select one from the above list instead and try again'
     return,-1
  endif

  ; the dataset is valid, get the fields that it has
  fields = latis_getdatasetfields(dataset, datatypes, datafields, data_rec)

  ; lisird3 release has a new URL changing from from tss to latis
;  url = 'http://lasp.colorado.edu:80/lisird/latis/'+strtrim(dataset,2)+'.csv?time>=2017-01-20&format_time(yyyyDDD)&wavelength='+strtrim(string(wave_nm,form='(f5.1)'),2)
  
  wavelengthpart=''
  if skip_wavelength ne 1 then $
     wavelengthpart='&wavelength='+strtrim(string(wave_nm,form='(f5.1)'),2)

  mintimepart=''
  if size(mintime,/type) eq size('',/type) then mintimepart='&time>='+mintime
  maxtimepart=''
  if size(maxtime,/type) eq size('',/type) then maxtimepart='&time<='+maxtime
  timeformatpart='&format_time(yyyyDDD)'
  if size(timeformat,/type) eq size('',/type) then timeformatpart='&format_time('+timeformat+')'

  baseurl = 'http://lasp.colorado.edu:80/lisird/latis/'+strtrim(dataset,2)+'.csv?'+strjoin(datafields,',')
  url = baseurl+mintimepart+maxtimepart+timeformatpart+wavelengthpart

  str = latis_geturlasstring(url)
  gd=where(strlen(str) gt 1,n_gd)
  if n_gd lt 2 then begin
     print,'ERROR: not enough data returned, check the minimum date or wavelength'
     ; &first does not work
     ;firsturl=baseurl+timeformatpart+wavelengthpart+'&first'
     ; &limit is a workaround, but there is no workaround for last
     ;firsturl=baseurl+timeformatpart+wavelengthpart+'&limit(1)'
     ;first=latis_geturlasstring(firsturl)
     ; &last does not work
     ;lasturl=baseurl+timeformatpart+wavelengthpart+'&last'
     ;last=latis_geturlasstring(lasturl)

     ; OK revert to getting everything
     fulldataseturl=baseurl+timeformatpart+wavelengthpart
     str=latis_geturlasstring(fulldataseturl)
     print,'First lines in dataset:'
     for i=0,2 do print,str[i]
     print,'Last lines in dataset:'
     for i=-3,-1 do print,str[i]
  endif

  data = replicate(data_rec, n_elements(str)-2) ; skip first line in csv
  for i=0,n_elements(data)-1 do begin
    sarr = strsplit( str[i+1], ',', /extract ) ; parse by commas
    for jfields=0L,n_elements(datafields)-1 do begin
       case datatypes[jfields] of
          'double': var=double(sarr[jfields])
          'long': var=long(sarr[jfields])
          'string': var=string(sarr[jfields])
          else: stop,'ERROR: unknown datatype '+datatypes[jfields]
       endcase
       data[i].(jfields) = var
    endfor ; jfields
  endfor ; i (rows of data)

  ; convert the raw latis structures into spectra if possible
  ; if there is only one spectrum, then wavelengths/energy are already
  ;  unique, so it will not be restructured
  if keyword_set(spectrum) then begin
     newdata = latis_createspectra(data)
     data = temporary(newdata)
  endif

  ; prepend yd converted to jd if keyword was set
  if keyword_set(jd) then begin
     ; which tag is it data.time or data.data.time?
     datatags=tag_names(data)
     gd=where('TIME' eq datatags,n_gd)
     if n_gd eq 1 then begin
        ; time is a top-level tag
        ; not spectra
        thejd = latis_barrysydtojd(double(data.time)+0.5d0)
        newdata = latis_prependstructurefield(data, 'jd', 'double', thejd)
     endif else begin
        ; time is down in data.data.time
        ; assembed spectra requested
        thejd = latis_barrysydtojd(double(data.data.time)+0.5d0)
        newdata = latis_insertstructurefield(data, 'jd', 'double', thejd)
     endelse
     ; need to append a jd field to the data structure
     data = temporary(newdata)
  endif

  return,data
end

; These are some tests to run to check that everything is working.

;+
; This is a useful procedure for testing.
;
; :Params:
;    condition: in, required, type=boolean
;      This is true or false.
;    message: in, required, type=string
;      This is the message to display if the condition is false.
;
; :Keywords:
;   fatal: in, optional, type=boolean
;      If this keyword is set and condition is false execution stops.
;
;-
pro latis_assert, condition, message, fatal=fatal

  if condition then return else begin
     print,'Assert failed - '+message
     if keyword_set(fatal) then stop,' cannot continue'
  endelse

return
end

;+
; This test called get_lisird_data to return all of the known datasets.
;-
function test_return_datasetlist
  status=-1 ; BAD
  datasets=get_lisird_data()
  if n_elements(datasets) gt 100 then status=0 ; OK
return,status
end

;+
; This test checks that SORCE SOLSTICE time-series data can be returned.
;-
function test_return_solstice_timeseries, msg
  msg=''
  status=-1 ; BAD
  data = get_lisird_data(133.5,dataset='sorce_ssi_l3',/jd,min='2004-04-01',max='2004-04-10')
  tags = tag_names(data)
  if tags[0] ne 'JD' then msg='JD tag not in position 0'
  if size(data,/type) ne size({a:0},/type) then msg='Not a structure'
  if n_elements(data) lt 5 then msg='Not enough elements returned'
  if n_elements(data) gt 10 then msg='Too many elements returned'
  ; add more condition tests here
  if strlen(msg) ne 0 then return, status

  ; otherwise it is good
  status=0 ; OK
return,status
end

;+
; This test checks that SORCE SOLSTICE spectral data can be returned.
;-
function test_return_solstice_spectrum, msg
  status=-1 ; BAD
  data = get_lisird_data(dataset='sorce_ssi_l3',/jd,/spectrum,min='2003-10-31',max='2003-11-02')
  tags=tag_names(data)
  junk=where(tags eq 'DATA',n_found)
  if n_found ne 1 then msg=' Spectrum not returned'
  ; add more condition tests here
  if strlen(msg) ne 0 then return, status
  status=0 ; OK
return,status
end

;+
; This is the main testing driver to be called by the user.
; Each test is a function that returns a status to test against.
; These tests are at the bottom so normal usage will not compile them.
; To test your version you can do this::
;    IDL> .run get_lisird_data
;    IDL> test_get_lisird_data
;
; This will print lots of stuff to the screen, but if it does not say
; "Assert failed - FAILED:" anywhere then everything worked.
;-
pro test_get_lisird_data

latis_assert, 0 eq test_return_datasetlist(), 'FAILED: test_return_dataset_list - LATIS is not returning a list of datasets',/fatal

latis_assert, 0 eq test_return_solstice_timeseries(msg), 'FAILED: test_return_solstice_timeseries - LATIS is not returning a time series - '+msg

latis_assert, 0 eq test_return_solstice_spectrum(msg), 'FAILED: test_return_solstice_spectrum - LATIS is not returning a spectrum - '+msg

return
end
