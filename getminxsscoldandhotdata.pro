;+
; NAME:
;   GetMinxssColdAndHotData
;
; PURPOSE:
;   Grab cold and hot data corresponding to Chloe Downs's determination of when those periods of time are. 
;   Output the min/max temperatures of components for comparison to Thermal Desktop model in both 
;   cold and hot cases. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Console messages of the [min, max] ºC temperatures for various components in cold and hot cases
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires access to MinXSS data and code base
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2016-11-18: James Paul Mason: Wrote script.
;-
PRO GetMinxssColdAndHotData

; Restore the MinXSS level 0C mission length file 
restore, getenv('minxss_data') + '/fm1/level0c/minxss1_l0c_all_mission_length.sav'

; Convert the X123 detector temperature to ºC
hk.X123_DET_TEMP = hk.X123_DET_TEMP - 273.15

; Define the date ranges for cold and hot times based on Chloe's analysis
coldDateRange = ['2016-06-15 10:57:03', '2016-06-15 14:00:16'] ; beta = 1º
coldDateRangeJd = JPMiso2jd(coldDateRange)
hotDateRange = ['2016-05-28 23:30:00', '2016-05-29 02:30:00'] ; beta = 73º 
hotDateRangeJd = JPMiso2jd(hotDateRange)

; Extract just those data corresponding to the date ranges
hkCold = hk[where(hk.time_jd GE coldDateRangeJd[0] AND hk.time_jd LE coldDateRangeJd[1])]
hkHot = hk[where(hk.time_jd GE hotDateRangeJd[0] AND hk.time_jd LE hotDateRangeJd[1])]

; Output relevant temperatures for cold case to console
print, 'ADCS Cold Range: [' + JPMPrintNumber(min(hkCold.XACT_WHEEL2TEMP)) + ', ' + JPMPrintNumber(max(hkCold.XACT_WHEEL2TEMP)) + '] ºC'
print, 'CDH, EPS, MoBo Cold Range: [' + JPMPrintNumber(min([hkCold.CDH_TEMP, hkCold.EPS_TEMP1, hkCold.EPS_TEMP2, hkCold.MB_TEMP1, hkCold.MB_TEMP2])) + $
                                 ', ' + JPMPrintNumber(max([hkCold.CDH_TEMP, hkCold.EPS_TEMP1, hkCold.EPS_TEMP2, hkCold.MB_TEMP1, hkCold.MB_TEMP2])) + '] ºC'
print, 'Batteries Cold Range: [' + JPMPrintNumber(min([hkCold.EPS_BATT_TEMP1, hkCold.EPS_BATT_TEMP2])) + $ 
                            ', ' + JPMPrintNumber(max([hkCold.EPS_BATT_TEMP1, hkCold.EPS_BATT_TEMP2])) + '] ºC'
print, 'COMM Cold Range [' + JPMPrintNumber(min(hkCold.COMM_TEMP)) + ', ' + JPMPrintNumber(max(hkCold.COMM_TEMP)) + '] ºC'
print, 'Solar Panels Cold Range [' + JPMPrintNumber(min([hkCold.EPS_SA1_TEMP, hkCold.EPS_SA2_TEMP, hkCold.EPS_SA3_TEMP])) + $
                              ', ' + JPMPrintNumber(max([hkCold.EPS_SA1_TEMP, hkCold.EPS_SA2_TEMP, hkCold.EPS_SA3_TEMP])) + '] ºC'
print, 'X123 Detector Cold Range: [' + JPMPrintNumber(min(hkCold.X123_DET_TEMP)) + ', ' + JPMPrintNumber(max(hkCold.X123_DET_TEMP)) + '] ºC'
print, 'X123 Board Cold Range: [' + JPMPrintNumber(min(hkCold.X123_BRD_TEMP)) + ', ' + JPMPrintNumber(max(hkCold.X123_BRD_TEMP)) + '] ºC'
print, '--'

; Output relevant temperatures for hot case to console
print, 'ADCS Hot Range: [' + JPMPrintNumber(min(hkHot.XACT_WHEEL2TEMP)) + ', ' + JPMPrintNumber(max(hkHot.XACT_WHEEL2TEMP)) + '] ºC'
print, 'CDH, EPS, MoBo Hot Range: [' + JPMPrintNumber(min([hkHot.CDH_TEMP, hkHot.EPS_TEMP1, hkHot.EPS_TEMP2, hkHot.MB_TEMP1, hkHot.MB_TEMP2])) + $
                                ', ' + JPMPrintNumber(max([hkHot.CDH_TEMP, hkHot.EPS_TEMP1, hkHot.EPS_TEMP2, hkHot.MB_TEMP1, hkHot.MB_TEMP2])) + '] ºC'
print, 'Batteries Hot Range: [' + JPMPrintNumber(min([hkHot.EPS_BATT_TEMP1, hkHot.EPS_BATT_TEMP2])) + $
                           ', ' + JPMPrintNumber(max([hkHot.EPS_BATT_TEMP1, hkHot.EPS_BATT_TEMP2])) + '] ºC'
print, 'COMM Hot Range [' + JPMPrintNumber(min(hkHot.COMM_TEMP)) + ', ' + JPMPrintNumber(max(hkHot.COMM_TEMP)) + '] ºC'
print, 'Solar Panels Hot Range [' + JPMPrintNumber(min([hkHot.EPS_SA1_TEMP, hkHot.EPS_SA2_TEMP, hkHot.EPS_SA3_TEMP])) + $
                             ', ' + JPMPrintNumber(max([hkHot.EPS_SA1_TEMP, hkHot.EPS_SA2_TEMP, hkHot.EPS_SA3_TEMP])) + '] ºC'
print, 'X123 Detector Hot Range: [' + JPMPrintNumber(min(hkHot.X123_DET_TEMP)) + ', ' + JPMPrintNumber(max(hkHot.X123_DET_TEMP)) + '] ºC'
print, 'X123 Board Hot Range: [' + JPMPrintNumber(min(hkHot.X123_BRD_TEMP)) + ', ' + JPMPrintNumber(max(hkHot.X123_BRD_TEMP)) + '] ºC'

END