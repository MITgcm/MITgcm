krd=2; kgr=1; kfac=0;

if krd > 0,
%-- load velocity :
 rac='output_files/';
 nit=72020;
 uu=rdmds([rac,'hUtave'],nit);
 vv=rdmds([rac,'hVtave'],nit);
 if kfac==1,
  hFacW=rdmds([rac,'hFacW']);
  hFacS=rdmds([rac,'hFacS']);
  uu=hFacW.*uu; vv=hFacS.*vv;
 end
 nc=size(uu,2); nPg=nc*nc*6; nr=size(uu,3);
end

if krd > 1,
%Rac='/home/jmc/grid_cs32/' ;
 Rac='grid_files/';
 psiLineF=[Rac,'psiLine_N2S_cs32'];

%- load : 'psi_N','psi_C','psi_P','psiUV','psi_T' :
 load(psiLineF);
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 G=load_grid(Rac,10+ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG;
%- load the grid dx,dy , convert to 10^6 m :
 dxg=G.dXg; dxg=dxg*1.e-6;
 dyg=G.dYg; dyg=dyg*1.e-6;

 fprintf([' load psiLine from file=',psiLineF, ...
          '  AND dxg,dyg files \n']);
end

%- compute detph integrated flow :
delR=[50 70 100 140 190 240 290 340 390 440 490 540 590 640 690]; % = delR in m
ut=reshape(uu,nPg,nr); vt=reshape(vv,nPg,nr);
dxg=reshape(dxg,nPg,1); dyg=reshape(dyg,nPg,1);
var=dyg*delR; ut=var.*ut; ut=sum(ut,2);
var=dxg*delR; vt=var.*vt; vt=sum(vt,2);

%- compute Barotropic Stream Function :
psiNx=size(psi_C,1);psiNy=size(psi_C,2); nPc2=nPg+2;
psiBa=zeros(nPc2,1);
ufac=rem(psi_T,2) ; vfac=fix(psi_T/2) ;
for j=2:psiNy,
 for i=1:psi_N(j),
   i1=psi_C(i,j); i0=psi_P(i,j); i2=psiUV(i,j);
   psiBa(i1)=psiBa(i0)+ufac(i,j)*ut(i2)+vfac(i,j)*vt(i2);
 end
end

if kgr > 0,
%-
shift=-1; ccB=[0 0]; cbV=0; AxBx=[-180 180 -90 90] ;
 ccB=[-140 140];
%AxBx=[-210 210 30 90];
 grph_CSz(psiBa,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx)
end
