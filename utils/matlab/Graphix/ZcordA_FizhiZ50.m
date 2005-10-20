ZC = [99000.0:-2000:1000.0];
ZF = [100000:-2000.:0.];
drC = [100000-ZC(1),ZC(1:end-1)-ZC(2:end)];
drF = ZF(1:end-1)-ZF(2:end);
save('ZcordA_FizhiZ50.mat','ZC','ZF','drC','drF');
