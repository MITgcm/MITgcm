%-- after running mitgcmuv, in ../tr_run.thsice:
%  1) extract numerical values from ASCII stat-Diag output files:
%   ( script "extract_StD" & matlab script read_StD.m
%     are located in in MITgcm_contrib/jmc_script )
% > extract_StD instStDiag.0000036000.txt = txt
% > extract_StD surfStDiag.0000036000.txt = txt
%  2) start matlab and execute this script.

rac='../tr_run.thsice/'; nam='check-Conserv'; dBug=0;
nit0=36000; nit=nit0+[1:9:20]; 
i1=1; i2=3; % <- to check half of this period: (i1,i2)=(1,2) or (i1,i2)=(2,3)

arc=rdmds([rac,'RAC']);
drF=squeeze(rdmds([rac,'DRF'])); delR=drF';
[ncx,nc]=size(arc); nPg=ncx*nc; nr=size(drF,1);
cpwater=3994; Lfresh=334000; 
rhosw=1035; rhofw=1000;
rhoIc=900; rhos=330;
salIce=4; salSW=35;
epsAB=0.6; deltaT=86400;

hFac=rdmds([rac,'hFacC']);
msk1=squeeze(hFac(:,:,1)); msk1=ceil(msk1);

ocArea=sum(sum(msk1.*arc));
Depth=rdmds([rac,'Depth']);
var=msk1.*arc; var=var.*Depth; DpAv1=sum(var(:))/ocArea;
rDepth=Depth+msk1-1; rDepth=msk1./rDepth;

var=reshape(arc,nPg,1);
vol=var*delR; var=reshape(vol,ncx,nc,nr);
vol=hFac.*var; Volum=sum(sum(sum(vol))); DpAv2=Volum/ocArea;
fprintf(['Exp: ',nam,' (dir=',rac(1:end-1),'),']);
fprintf(' Check-Cons between iter: %i %i\n',nit(i1),nit(i2));
fprintf('Ocean Area= %g ; Volume= %g mean Depth= %11.6f \n',ocArea,Volum,DpAv1);
fprintf('            mean Depth2= %11.6f (Diff: %10.3e)\n',DpAv2,DpAv2-DpAv1);

%--------------------
 [v2i,iti,M]=rdmds([rac,'surfInst'],nit-1);
if dBug,
 eval(M)
 v4d=reshape(v2i,[dimList nFlds length(iti)]);
 for i=1:length(iti),
  for n=1:nFlds,
   if nDims == 2, var=v4d(:,:,n,i); else var=v4d(:,:,:,n,i); end
   fprintf(['it= %i %2i ',char(fldList(n)),': min,max = %e , %e\n'], ...
                                      iti(i),n,min(var(:)),max(var(:)));
  end
 end
end
 [v2c,iti,M]=rdmds([rac,'etaInst'],nit);
if dBug,
 eval(M)
 v4d=reshape(v2c,[dimList nFlds length(iti)]);
 for i=1:length(iti),
  for n=1:nFlds,
   if nDims == 2, var=v4d(:,:,n,i); else var=v4d(:,:,:,n,i); end
   fprintf(['it= %i %2i ',char(fldList(n)),': min,max = %e , %e\n'], ...
                                      iti(i),n,min(var(:)),max(var(:)));
  end
 end
end

 [v3i,iti,M]=rdmds([rac,'dynInst'],nit-1);
if dBug,
 eval(M)
 v4d=reshape(v3i,[dimList nFlds length(iti)]);
 for i=1:length(iti),
  for n=1:nFlds,
   if nDims == 2, var=v4d(:,:,n,i); else var=v4d(:,:,:,n,i); end
   fprintf(['it= %i %2i ',char(fldList(n)),': min,max = %e , %e\n'], ...
                                      iti(i),n,min(var(:)),max(var(:)));
  end
 end
end

%- vvA: kLev, time_rec, region_rec, [ave,std,min,max,vol], var_rec
namF=[rac,'surfStDiag'];
[nIt,rList,tim1,vvA,listV1]=read_StD(namF,'txt','all_flds');
flxAve=squeeze(vvA);
namF=[rac,'instStDiag'];
[nIt,rList,tim2,vvA,listV2]=read_StD(namF,'txt','all_flds');
dynAve=squeeze(vvA);

delT=(nit(2)-nit(1))*deltaT;

 et1=v2i(:,:,1,i1); et2=v2i(:,:,1,i2);
 pr1=v2i(:,:,2,i1); pr2=v2i(:,:,2,i2);
 et1p=v2c(:,:,i1);  et2p=v2c(:,:,i2);

t1=v3i(:,:,:,1,i1); t2=v3i(:,:,:,1,i2);
s1=v3i(:,:,:,2,i1); s2=v3i(:,:,:,2,i2);

%--- to compute T* & S* (= conserved quantities with AB-2) at it1 & it2 :
dh1=et1p-et1;
dv1=reshape(dh1.*rDepth,[nPg 1])*ones(1,nr); dv1=reshape(dv1,[ncx nc nr]);
dv1(:,:,1)=dv1(:,:,1)-pr1/rhosw*deltaT/delR(1);
dh2=et2p-et2;
dv2=reshape(dh2.*rDepth,[nPg 1])*ones(1,nr); dv2=reshape(dv2,[ncx nc nr]);
dv2(:,:,1)=dv2(:,:,1)-pr2/rhosw*deltaT/delR(1);
%---

H1=dynAve(1,i1,1,2)*dynAve(1,i1,5,2)/ocArea*rhosw*cpwater;
H2=dynAve(1,i2,1,2)*dynAve(1,i2,5,2)/ocArea*rhosw*cpwater;
var=epsAB*t1.*dv1;
H3=sum(sum(sum(var.*vol)))/ocArea*rhosw*cpwater;
var=epsAB*t2.*dv2;
H4=sum(sum(sum(var.*vol)))/ocArea*rhosw*cpwater;
%heating=flxAve(:,1,3)+flxAve(:,1,9); % = tRelax + Qnet (EnPrec missing)
heating=flxAve(:,1,7); % = TFLUX
heating=sum(heating(1+i1:i2))*delT;
fprintf(' H2-H1 = %8.3e ; heating= %8.3e ; Diff: %11.4e (%11.4e)\n', ...
                   H2-H1,heating,H2-H1-heating,(H2-H1-heating)/H2);
fprintf(' h2-h1 = %10.3e (h1=%8.1e, h2=%8.1e): %11.4e (%11.4e)\n', ...
                   H4-H3,H3,H4,H2+H4-H1-H3-heating,(H2+H4-H1-H3-heating)/H2);

hS1=dynAve(1,i1,1,3)*dynAve(1,i1,5,3)/ocArea*rhosw;
hS2=dynAve(1,i2,1,3)*dynAve(1,i2,5,3)/ocArea*rhosw;
var=epsAB*s1.*dv1;
hS3=sum(sum(sum(var.*vol)))/ocArea*rhosw;
var=epsAB*s2.*dv2;
hS4=sum(sum(sum(var.*vol)))/ocArea*rhosw;
saltfx=flxAve(:,1,4)+flxAve(:,1,10); % = sRelax + oceSflux
%saltfx=flxAve(:,1,8); % = SFLUX 
saltfx=sum(saltfx(1+i1:i2))*delT;
fprintf(' S2-S1 = %8.6f ; saltfx = %8.6f ; Diff: %11.4e (%11.4e)\n', ... 
                   hS2-hS1,saltfx,hS2-hS1-saltfx,(hS2-hS1-saltfx)/hS2);
fprintf(' s2-s1 = %10.3e (s1=%8.1e, s2=%8.1e): %11.4e (%11.4e)\n', ...
        hS4-hS3,hS3,hS4,hS2+hS4-hS1-hS3-saltfx,(hS2+hS4-hS1-hS3-saltfx)/hS2);

 etAv1=dynAve(1,i1,1,1)*rhosw;
 etAv2=dynAve(1,i2,1,1)*rhosw;
%- more precised when recomputing average from from full precision 2D field:
%var=et1p; var=msk1.*var; etAv1=sum(sum(var.*arc))/ocArea*rhosw;
%var=et2p; var=msk1.*var; etAv2=sum(sum(var.*arc))/ocArea*rhosw;
fresh=flxAve(:,1,11); fresh=sum(fresh(1+i1:i2))*delT;
fprintf(' Et2-Et1 = %8.6f ; fresh= %8.6f ; Diff: %11.4e (%11.4e)\n', ...
                   etAv2-etAv1,fresh,etAv2-etAv1-fresh,(etAv2-etAv1-fresh)/etAv2);

return
