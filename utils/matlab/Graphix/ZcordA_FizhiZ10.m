ZC = [95000:-10000:5000];
ZF = [100000:-10000:0];
drC = [100000-ZC(1),ZC(1:end-1)-ZC(2:end)];
drF = ZF(1:end-1)-ZF(2:end);
save('ZcordA_FizhiZ10.mat','ZC','ZF','drC','drF');
