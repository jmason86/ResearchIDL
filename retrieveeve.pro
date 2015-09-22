; Program to download a range of EVE data at 2 specified times per day
;
; Call sequence: 
;   retrieveEVE, startDOY, endDOY, time1, time2 
; Call example: 
;   retrieveEVE, 135, 195, 9, 18
; 9 and 18 will be the default 2 times to use if no times specified. 
; 
; James Paul Mason
; 7/15/11



;NOTE: AT THIS POINT THE CODE DOESNT WORK. NEED TO FIGURE OUT HOW TO PASS PASSWORD. 

PRO retrieveEVE, startDOY, endDOY, time1, time2

IF n_elements(time1) EQ 0 THEN time1 = '09'
IF n_elements(time2) EQ 0 THEN time2 = '18'

FOR i=startDOY,endDOY DO BEGIN
  
  ;convert dates to strings for call
  str_startDOY = strcompress(string(startDOY),/remove_all)
  str_endDOY = strcompress(string(endDOY),/remove_all)
  i_str = strcompress(string(i),/remove_all)
  
  ;call secure copy
  spawn, 'scp jama6159@evesci2.lasp.colorado.edu:/production/data/level2/2011/'+i_str+'EVS_L2_2011'+i_str+time1+'002_01.fit.gz .'
  spawn, 'scp jama6159@evesci2.lasp.colorado.edu:/production/data/level2/2011/'+i_str+'EVS_L2_2011'+i_str+time2+'002_01.fit.gz .'
  
  

ENDFOR ;i loop




END