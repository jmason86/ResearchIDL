;+
; NAME:
;   JPMReadEveEsp
;
; PURPOSE:
;   Read data from SDO/EVE/ESP and convert time to JD and ISO. 
;
; INPUTS:
;   filenames [strarr]: The path/filenames of the ESP fits files to read in
;   channel [int]: The channel you want to extract. Can be either 18, 26, 30, or 36.
;                  This is a limitation of this code right now because it is only needed for extracting one channel.
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;  espData [structure]: An anonymous structure with: 
;                       jd [dblrarr]:     julian date
;                       espData [fltarr]: ESP data for the diode channel specified by channel input
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires eve_read_whole_fits
;
; EXAMPLE:
;   espDataDiode18 = JPMReadEveEsp('esp_L1_2017247_006.fit', 18)
;
; MODIFICATION HISTORY:
;   2017-10-25: James Paul Mason: Wrote script.
;-
FUNCTION JPMReadEveEsp, filenames, channel

; Loop through each file, read it in, and concatenate into the output array
FOREACH filename, filenames DO BEGIN
  espStructure = eve_read_whole_fits(filename)
  
  ; Convert the time stamp from yyyy, doy, and sod to jd
  goodIndices = where(espStructure.hdr001.year NE 0)
  jdNew = JPMyyyyDoy2JD(long(strtrim(espStructure.hdr001[goodIndices].year, 2) + strtrim(espStructure.hdr001[goodIndices].doy, 2)))
  jdNew += espStructure.hdr001[goodIndices].sod / 86400.
  jd = (jd EQ !NULL) ? jdNew : [jd, jdNew]
  IF total(where(jdNew EQ 0)) NE -1 THEN STOP
  
  ; Grab the data for the relevant channel
  command = 'newData = espStructure.hdr001[goodIndices].CH_' + strtrim(channel, 2)
  void = execute(command)
  espChannelData = (espChannelData EQ !NULL) ? newData : [espChannelData, newData]
  
ENDFOREACH

return, {jd: jd, espData: espChannelData}

END