;+
; NAME: 
;   ComputeSuccessOfMEGSBCampaigns
;   
; PURPOSE:
;   Compare scientist-called MEGS-B flare campaigns and standard daily MEGS-B campaigns
;   to actual flare occurrence. Plot statistics. 
;   
; INPUTS: 
;   EVEDailyOperations.csv
;   GOESEventList.csv
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   None
;   
; OUTPUTS:
;   Plot statistics
;   
; OPTIONAL OUTPUTS: 
;   None
;   
; RESTRICTIONS:
;   None
;   
; EXAMPLE: 
;   
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2012/11/16
;-
PRO ComputeSuccessOfMEGSBCampaigns

dataloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/'

readcol, dataloc+'EVEDailyOperations.csv', date, description, format = 'a, a', DELIMITER=',', SKIPLINE = 9, /SILENT

; Find flare campaigns
twentyFourHourExposureIndices = where(strmatch(description, '* Flare Campaign *', /FOLD_CASE) EQ 1)

; Specify the date of each of the exposures
date5Min = date
date3Hour = date
date24Hour = date(twentyFourHourExposureIndices)

; Define times of standard exposures
time5Min = ['00:50:00', '01:50:00', '02:50:00', '03:50:00', '04:50:00', '05:50:00', '06:50:00', '07:50:00', '08:50:00', '09:50:00', 
            '10:50:00', '11:50:00', '12:50:00', '13:50:00', '14:50:00', '15:50:00', '16:50:00', '17:50:00', '18:50:00', '23:50:00']
time3Hour = ['19:50:00']
            

STOP


END