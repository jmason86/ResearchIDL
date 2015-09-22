; Program to Un-normalize the Meudon histograms.
; 
; James Paul Mason
; 4/20/11

PRO unnorm

;read in table
readcol,'MeudonK3histog2007.tab',index,mu,median,ave,area,muhi,mulo,$
                                 skipline=3,/silent
STOP
;Median for each ring (from MeudonK3histog.tab)
medians = [2571.0,2424.0,2306.0,2171.0,2038.0,1896.0,1757.0,1612.0,1487.0,1383.0]

;scale the normalized by their median
mu0_unnorm = mu0*medians(0)
mu1_unnorm = mu1*medians(1)
mu2_unnorm = mu2*medians(2)
mu3_unnorm = mu3*medians(3)
mu4_unnorm = mu4*medians(4)
mu5_unnorm = mu5*medians(5)
mu6_unnorm = mu6*medians(6)
mu7_unnorm = mu7*medians(7)
mu8_unnorm = mu8*medians(8)
mu9_unnorm = mu9*medians(9)

;output the file
;Header info
openw,1,'MDONintlevels_unnormalized.tab',width=200
printf,1,'Intensity Table: Meudon_K3'
printf,1,'Filter:?'
printf,1,'Filter offset:0.0'
printf,1,'Filter Units:?, Intensity Units:?'
printf,1,'Models:6 Mus:10'
printf,1,'Mu: 1.0 0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1'

;new intensity output
printf,1,'Model:A', mu0_unnorm(0),mu1_unnorm(0),mu2_unnorm(0),mu3_unnorm(0),mu4_unnorm(0),$
                    mu5_unnorm(0),mu6_unnorm(0),mu7_unnorm(0),mu8_unnorm(0),mu9_unnorm(0)
printf,1,'Model:B', mu0_unnorm(1),mu1_unnorm(1),mu2_unnorm(1),mu3_unnorm(1),mu4_unnorm(1),$
                    mu5_unnorm(1),mu6_unnorm(1),mu7_unnorm(1),mu8_unnorm(1),mu9_unnorm(1)
printf,1,'Model:D', mu0_unnorm(2),mu1_unnorm(2),mu2_unnorm(2),mu3_unnorm(2),mu4_unnorm(2),$
                    mu5_unnorm(2),mu6_unnorm(2),mu7_unnorm(2),mu8_unnorm(2),mu9_unnorm(2)
printf,1,'Model:F', mu0_unnorm(3),mu1_unnorm(3),mu2_unnorm(3),mu3_unnorm(3),mu4_unnorm(3),$
                    mu5_unnorm(3),mu6_unnorm(3),mu7_unnorm(3),mu8_unnorm(3),mu9_unnorm(3)
printf,1,'Model:H', mu0_unnorm(4),mu1_unnorm(4),mu2_unnorm(4),mu3_unnorm(4),mu4_unnorm(4),$
                    mu5_unnorm(4),mu6_unnorm(4),mu7_unnorm(4),mu8_unnorm(4),mu9_unnorm(4)
printf,1,'Model:P', mu0_unnorm(5),mu1_unnorm(5),mu2_unnorm(5),mu3_unnorm(5),mu4_unnorm(5),$
                    mu5_unnorm(5),mu6_unnorm(5),mu7_unnorm(5),mu8_unnorm(5),mu9_unnorm(5)

close,1

END