ZC = [98750.0:-2500:1250.0];
ZF = [100000:-2500.:0.];
drC = [100000-ZC(1),ZC(1:end-1)-ZC(2:end)];
drF = ZF(1:end-1)-ZF(2:end);
save('ZcordA_FizhiZ40.mat','ZC','ZF','drC','drF');
