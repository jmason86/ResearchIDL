;+
; NAME:
;	open_ps
;
; PURPOSE:
;	set device to ps to make it easier to print
;
; CATEGORY:
;	what level(s) of SEE processing is this procedure used for
;	(valid levels are L0A, L0B, L1, L2, L3)
;
; CALLING SEQUENCE:
;  
;	open_ps,filename,landscape,color
;
; INPUTS:
;	filename = file name
;       landscape and color are keywords set to control printer
;
; OUTPUTS:  
;	none
;
; COMMON BLOCKS:
;	 device_orig, dev_name
;
; PROCEDURE:
;	1. check parameters
;       2. open ps device
;
; MODIFICATION HISTORY:
;	Karen Turk, 5/23/01
;
; CVS Modification Log
;
;   $Log: open_ps.pro,v $
;   Revision 8.0  2005/06/15 18:51:22  see_sw
;   commit of version 8.0
;
;   Revision 8.0  2004/07/20 20:18:28  turkk
;   commit of version 8.0
;
;   Revision 7.0  2004/07/08 23:02:56  turkk
;   commit of version 7.0
;
;   Revision 6.0  2003/03/05 19:32:33  dlwoodra
;   version 6 commit
;
;   Revision 5.20  2002/09/06 23:21:34  see_sw
;   commit of version 5.0
;
;   Revision 4.0  2002/05/29 18:10:01  see_sw
;   Release of version 4.0
;
;   Revision 3.0  2002/02/01 18:55:27  see_sw
;   version_3.0_commit
;
;   Revision 1.1  2001/10/30 02:41:49  dlwoodra
;   initial commit
;
;
;idver='$Id: open_ps.pro,v 8.0 2005/06/15 18:51:22 see_sw Exp $'
;
;+

pro   open_ps,filename,landscape = landscape, color = color

common device_orig, dev_name

; 1 parameter check
if (n_params() eq 0) then begin
    filename = ''
    read,filename, prompt=  'Please choose a name for this file.' 
endif


if (filename eq '') then begin
    filename = 'open_ps_default.ps'
    print, 'You did not choose a file name, the default name is' $
          + ' open_ps_default.ps'
endif

;    set landscape
if (size(landscape,/type) eq 0) then begin
;    landscape not devined    
    landscape = 1
    lands =''
    read,lands, prompt = 'Would you like this printed in landscape? (y/n)'
    if (strlowcase(lands) ne 'y') then begin
        lands = 'n'
        landscape = 0
    endif
endif else begin
;   landscape is defined 
    if (keyword_set(landscape)) then landscape = 1 else landscape = 0
endelse

;set color
if (size(color,/type) eq 0) then begin
;     color was not defined
    color = 1
    clr = ''
    read,clr, prompt = 'Would you like this printed in color? (y/n)'
    if (strlowcase(clr) ne 'y') then begin
        clr = 'n'
        color = 0
    endif
endif else begin
;    color was defined
    if (keyword_set(color)) then color = 1 else color = 0
endelse

;       2. open ps device

dev_name = !d.name
set_plot, 'ps'

device, file = filename, landscape = landscape, color = color, decomposed=0

print, ' device has been set to '+!d.name    
return 

end
