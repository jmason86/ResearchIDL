;+
; SUVI_DOWNLOAD: a procedure to download SUVI files
;
; INPUTS: 
;    date_time: The date/time where you want a file
;               Must be in this format: '2017-09-10T16:06:00'
;               Additional tenths, suffixes (like 'Z') are ignored.
;               You can pass a single date, or a string array of
;               dates. Also possible is a JSOC-style query with a 
;               timespan and cadence, for example:
;     
;               '2020-03-01T00:00:00/2d@8h' would look for data starting at
;                2020-03-01T00:00:00, for a timespan of 2 days, at a cadence
;                of 8 hours. If the cadence argument ('@8h' is omitted, it 
;                defaults to a nominal cadence of 4 minutes. Valid time units
;                are 'm', 'h', and 'd' (minutes, hours, and days). Note that
;                the JSOC-style query is only possible for a single argument,
;                not for string arrays of dates.
;
;    wavelength: the passband of the file, allowed values are:
;                93/94, 131, 171, 195, 284, 303/304
;                (Note that 94, 304 are preferred, but due to some ambiguity in 
;                SUVI passband names, the others will autocorrect
;                to the preferred values.)
; 
;    spacecraft: Which GOES data you want (16 or 17, just enter the number)            
;
; OPTIONAL KEYWORDS:
;    exact_match: Set this to REQUIRE only files with the exact specified time
;    query_only:  Set this for a dry run. Only prints the files that were found
;                 with the current parameters, but does not download any.
;
; OPTIONAL OUTPUTS:
;    return_file: report the name(s) of the file downloaded
;
; HISTORY:
; Created by Daniel B. Seaton (daniel.seaton@noaa.gov) sometime in 2019
;            Univ. of Colorado/NOAA Nat. Centers for Environmental Info.
;            May 2020   Improved handling of experimental files (dbs)
;            July 2020  Added support for lists of dates and JSOC-style
;                       queries (C. Bethge)
;
;-

pro suvi_download, date_time, wavelength, spacecraft, exact_match = exact_match, return_file = return_file, query_only = query_only
  
	;; Author's note: 
	;; there's a bunch of error handling and argument parsing we need to add
	;; to ensure users don't encounter stupid errors that could be avoided

	;; need to add support to filter on exposure time, right now you just have to guess
  
        ;; nominal cadence in seconds to default to if none is given in the JSOC-style query
        nominal_cadence = 240.
  
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; check for JSOC-style range vs specific times ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
        date_size = size(date_time)
        if date_size[n_elements(date_size)-1] eq 1 then begin
           ;; check if there is a JSOC-style range given, or if it is just a single date
           test_split = strsplit(date_time, '/', /extract)
           if size(test_split, /dim) eq 2 then begin
              date_arr = strsplit(date_time, '/@', /extract)
              time_unit = strmid(date_arr[1], strlen(date_arr[1])-1, strlen(date_arr[1]))
              ;; allow minutes, hours, and days as time units, and convert to seconds
              case strlowcase(time_unit) of
                 'm':  time_factor = 60.
                 'h':  time_factor = 60.*60.
                 'd':  time_factor = 60.*60.*24.
                 else: begin
                         message, /cont, 'Not a valid time unit (must be m, h, or d).'
                         return
                       end
              endcase
              ;; timespan in seconds
              timespan = float(strmid(date_arr[1], 0, strlen(date_arr[1])-1)) * time_factor
              ;; Check if there is a cadence given or not. If not, use nominal cadence.
              if size(date_arr, /dim) eq 2 then begin
                 cadence = nominal_cadence
              endif else begin
                 time_unit_c = strmid(date_arr[2], strlen(date_arr[2])-1, strlen(date_arr[2]))
                 ;; again: allow minutes, hours, and days as time units, and convert to seconds
                 case strlowcase(time_unit_c) of
                    'm':  time_factor_c = 60.
                    'h':  time_factor_c = 60.*60.
                    'd':  time_factor_c = 60.*60.*24.
                    else: begin
                            message, /cont, 'Not a valid time unit (must be m, h, or d).'
                            return
                          end
                 endcase
                 cadence = float(strmid(date_arr[2], 0, strlen(date_arr[2])-1)) * time_factor_c
              endelse
              ;; construct a list with the timestamps we are interested in
              date_time_list = list()
              start_date_tai = anytim2tai(date_arr[0])
              end_date_tai   = start_date_tai+timespan
              counter = 0
              while start_date_tai+counter*cadence le end_date_tai do begin
                 date_time_list.add, strjoin(strsplit(strjoin(strsplit(anytim2cal(start_date_tai+counter*cadence,form=11), $
                                                                       '/', /extract), '-'), /extract), 'T')
                 counter++
              endwhile
              ;; convert list to string array
              date_time = date_time_list.ToArray()
           endif
        endif
        
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; parse arguments and keywords ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	spacecraft = strmid(strcompress(string(spacecraft), /remove_all), 0, 2)
	exact_match = keyword_set(exact_match)

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; config server and path ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;; this is constant
	data_server = 'data.ngdc.noaa.gov'

	;; this is constant
	base_path = 'platforms/solar-space-observing-satellites/goes/goes**/l1b/'
	;; inject spacecraft number to base path 
	strput, base_path, spacecraft, strpos(base_path, '**')

	;; deal with wavelength, which might be a float or could be 93/94, 303/304 
	wvln_path_array = ['suvi-l1b-fe094', 'suvi-l1b-fe094', 'suvi-l1b-fe131', 'suvi-l1b-fe171', $
				  'suvi-l1b-fe195', 'suvi-l1b-fe284', 'suvi-l1b-he304', 'suvi-l1b-he304']
	wvln_array = [93, 94, 131, 171, 195, 284, 303, 304]
	wvln_path = (wvln_path_array(where(wvln_array eq wavelength)))[0]

        return_file_list = list()
        for dd=0,n_elements(date_time)-1 do begin
           ;; parse date/time to get the subdirectories we need 
           date_path = strmid(date_time[dd], 0, 4) + '/' + strmid(date_time[dd], 5, 2) + '/' + strmid(date_time[dd], 8, 2) + '/'
        
           full_path = base_path + wvln_path + '/' + date_path
	
           ;;;;;;;;;;;;;;;;;;;;;;;;;;
   	   ;; set up netURL Object ;;
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;

	   ;; create object
           oUrl = OBJ_NEW('IDLnetUrl')

	   ;; use verbose so we can detect problems and errors 
           oUrl->SetProperty, VERBOSE = 1
           
	   ;; protocol is https 
           oUrl->SetProperty, URL_SCHEME = 'https'

           ;; define server and path 
           oUrl->SetProperty, URL_HOST = data_server
           oUrl->SetProperty, URL_PATH = full_path

	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	   ;; fetch and parse file list ;;
	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           ;; check if the URL exists, otherwise skip this date
           if ~url_valid('https://'+data_server+'/'+full_path) then begin
              message, /cont, 'Website for '+strmid(date_time[dd],0,10)+' does not exist.'
              continue
           endif
              
           ;; fetch a list of files and dump to string array called dir_list 
           dir_list = oUrl->Get(/STRING_ARRAY)
           
           ;; there are many lines that don't have files, we don't need them so find just the links to files 
           link_locations = where(strmatch(dir_list, '*href="*.fits*'))

           ;; extract filenames from the link lines 
           filenames = ''
           for n = 0, n_elements(link_locations) - 1 do begin
              link_line = dir_list[link_locations[n]]
              start_pos = strpos(link_line, 'href="') + 6
              if strmatch(link_line, '*fits.gz*') then $
                 end_pos = strpos(link_line, '.fits.gz"') + 8 $
              else $
                 end_pos = strpos(link_line, '.fits"') + 5
              filenames = [filenames, strmid(link_line, start_pos, end_pos - start_pos)]
           endfor
           filenames = filenames[1:*]

           ;; parse filenames into date array
           case filenames[0].indexof('SUVI') of
              ;; operational files 
              3: begin 
                   ;; this sets up the base date from the year and doy 
                   file_date_array = anytim2utc(doy2utc( filenames.substring(27, 29), filenames.substring(23, 26) ), /ccsds)

                   ;; this fills in the file time so we can compare with our desired time 
                   for n = 0, n_elements(filenames) - 1 do begin
                      temp_date = file_date_array[n]
                      strput, temp_date, strmid(filenames[n], 30, 2), 11
                      strput, temp_date, strmid(filenames[n], 32, 2), 14
                      strput, temp_date, strmid(filenames[n], 34, 2), 17
                      strput, temp_date, strmid(filenames[n], 36, 1), 20
                      file_date_array[n] = temp_date
                   endfor
                end

		;; reprocessed and experimental files
                5: file_date_array = anytim2utc( filenames.substring(18, 21) + '-' + $
                                                 filenames.substring(22, 23) + '-' + $
                                                 filenames.substring(24, 25) + 'T' + $
                                                 filenames.substring(27, 28) + ':' + $
                                                 filenames.substring(29, 30) + ':' + $
                                                 filenames.substring(31, 32) + 'Z', /ccsds)

		;; it is possible there are other unparseable filenames
                else: message, 'Encountered a nonstandard file format or another issue with the data directory. Contact NCEI.'
           endcase	

           ;; we use CDS TAI because it provides a convenient framework to compute delta-ts
           file_date_tai = anytim2tai(file_date_array)
           tai_of_interest = anytim2tai(date_time[dd])
           delta_t = abs(tai_of_interest - file_date_tai)

           if exact_match then begin
              matching_file = (where(tai_of_interest eq file_date_tai))[0]
              if matching_file eq -1 then message, 'No matching file. Try running without /exact_match set.'
           endif else $
              matching_file = (where(delta_t eq min(delta_t)))[0]

           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; construct file path & fetch ;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

           file_path = full_path + filenames[matching_file]
           oUrl->SetProperty, URL_PATH = file_path

           return_file_list.add, filenames[matching_file]
           if keyword_set(query_only) then message, /cont, file_path $
           else fn = oUrl->Get(filename = filenames[matching_file])
 
   	   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;; we're all done here, so clean up ;;
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
           obj_destroy, oUrl
       
        endfor
        return_file = return_file_list.ToArray()
end


