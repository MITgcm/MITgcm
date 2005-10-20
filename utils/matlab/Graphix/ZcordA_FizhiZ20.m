ZC = [97500.0:-5000:2500.0] 
ZF = [100000:-5000.:0.]
drC = [100000-ZC(1),ZC(1:end-1)-ZC(2:end)];
drF = ZF(1:end-1)-ZF(2:end);
save('ZcordA_FizhiZ20.mat','ZC','ZF','drC','drF');
