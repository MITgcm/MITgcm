nklev=30;
ZC=zeros(1,nklev);
ZF=zeros(1,nklev+1);
drC = [3500.*ones(1,28),1000.*ones(1,2)];
ZC(1)=100000.-(0.5*drC(1));
ZF(1)=100000.;
for klev=2:nklev;
 ZC(klev)=ZC(klev-1)-(0.5*(drC(klev)+drC(klev-1)));
 ZF(klev)=ZF(klev-1)-drC(klev-1);
end
ZF(nklev+1)=ZF(nklev)-drC(nklev);
drF = ZF(1:nklev)-ZF(2:nklev+1);
save('ZcordA_FizhiZ30.mat','ZC','ZF','drC','drF');
