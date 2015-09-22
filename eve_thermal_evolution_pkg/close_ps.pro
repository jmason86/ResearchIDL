;+
; NAME:
;	close_ps
;
; PURPOSE:
;	to close the post script file that was opened in open_ps
;
; CATEGORY:
;	what level(s) of SEE processing is this procedure used for
;	(valid levels are L0A, L0B, L1, L2, L3)
;
; CALLING SEQUENCE:  
;	close_ps
;
; INPUTS:
;	none
;
; OUTPUTS:  
;	none
;
; COMMON BLOCKS:
;	 device_orig, dev_name
;
; PROCEDURE:
;	1. close device
;
; MODIFICATION HISTORY:
;	Karen Turk, 5/23/01
;
; CVS Modification Log
;
;   $Log: close_ps.pro,v $
;   Revision 8.0  2005/06/15 18:51:21  see_sw
;   commit of version 8.0
;
;   Revision 8.0  2004/07/20 20:18:19  turkk
;   commit of version 8.0
;
;   Revision 7.0  2004/07/08 23:02:49  turkk
;   commit of version 7.0
;
;   Revision 6.0  2003/03/05 19:32:27  dlwoodra
;   version 6 commit
;
;   Revision 5.20  2002/09/06 23:21:32  see_sw
;   commit of version 5.0
;
;   Revision 4.0  2002/05/29 18:09:59  see_sw
;   Release of version 4.0
;
;   Revision 3.0  2002/02/01 18:55:25  see_sw
;   version_3.0_commit
;
;   Revision 1.1  2001/10/30 02:41:46  dlwoodra
;   initial commit
;
;
;idver='$Id: close_ps.pro,v 8.0 2005/06/15 18:51:21 see_sw Exp $'
;
;+

pro close_ps

common device_orig, dev_name

;	1. close device

device,/close
 
set_plot, dev_name

print, 'Device has been restored to '+!d.name
return 

end
