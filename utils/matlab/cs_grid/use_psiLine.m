% input: krd, kfac, kgr
% kfac = 0 : read-in 1 diagnostics output file containing both UVELMASS & VVELMASS
% kfac > 0 : read-in 2 Time-ave output files ; kfac=1 with hFac in ; kfac=2 : add hFac here.
krd=2; kfac=0; kgr=1;

if krd > 0,
%-- load velocity :
 gDir='grid_files/'; rDir='output_files/'; nit=72000;
%rDir='../res_s1y/'; nit=72360;
%gDir=rDir;
 titexp=strrep(rDir(1:end-1),'_','\_'); tit0=[';  it=',int2str(nit)];
 if kfac > 0,
  %-- time-ave output:
   uu=rdmds([rDir,'hUtave'],nit);
   vv=rdmds([rDir,'hVtave'],nit);
   if kfac==2,
  %- without included hFac:
    hFacW=rdmds([rDir,'hFacW']);
    hFacS=rdmds([rDir,'hFacS']);
    uu=hFacW.*uu; vv=hFacS.*vv;
   end
 else
  %-- diagnostics output:
  namFil='dynDiag';
  [v4d,dum,M]=rdmds([rDir,namFil],nit);
  eval(M);
  tmp=strcmp(fldList,'UVELMASS'); ju=find(tmp==1);
  tmp=strcmp(fldList,'VVELMASS'); jv=find(tmp==1);
  if isempty(ju) | isempty(jv),
    error(['At least 1 velocity Comp. missing from file: ',namFil]);
  end
  uu=squeeze(v4d(:,:,:,ju));
  vv=squeeze(v4d(:,:,:,jv));
 end
 n1h=size(uu,1); n2h=size(uu,2);
 if n1h == 6*n2h, nc=n2h;
 elseif n1h*6 == n2h, nc=n1h;
 else
  error([' var size: ',int2str(n1h),' x ',int2str(n2h),' does not fit regular cube !']);
 end
 nr=size(uu,3); nPg=nc*nc*6; nPp2=nPg+2;
end

if krd > 1,
 lDir='grid_files/';
 psiLineF=[lDir,'psiLine_N2S_cs32'];

%- load : 'psi_N','psi_C','psi_P','psiUV','psi_T' :
 load(psiLineF);
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 G=load_grid(gDir,ncdf);
 xcs=G.xC; ycs=G.yC; xcg=G.xG; ycg=G.yG;
%- load the grid dx,dy , convert to 10^6 m :
 dxg=G.dXg; dxg=dxg*1.e-6;
 dyg=G.dYg; dyg=dyg*1.e-6;

 fprintf([' load psiLine from file=',psiLineF, ...
          '  AND dxg,dyg files \n']);
end

%- compute detph integrated flow :
%delR=[50 70 100 140 190 240 290 340 390 440 490 540 590 640 690]; % = delR in m
delR=G.dRf';
ut=reshape(uu,nPg,nr); vt=reshape(vv,nPg,nr);
dxg=reshape(dxg,nPg,1); dyg=reshape(dyg,nPg,1);
var=dyg*delR; ut=var.*ut; ut=sum(ut,2);
var=dxg*delR; vt=var.*vt; vt=sum(vt,2);

if n2h == nc,
  %- put long vectors ut,vt in "compact-compatible" format:
  ut=reshape(permute(reshape(ut,[nc 6 nc]),[1 3 2]),[nPg 1]);
  vt=reshape(permute(reshape(vt,[nc 6 nc]),[1 3 2]),[nPg 1]);
end

%- compute Barotropic Stream Function :
psiNx=size(psi_C,1);psiNy=size(psi_C,2);
psiH=zeros(nPp2,1);
ufac=rem(psi_T,2) ; vfac=fix(psi_T/2) ;
for j=2:psiNy,
 for i=1:psi_N(j),
   i1=psi_C(i,j); i0=psi_P(i,j); i2=psiUV(i,j);
   psiH(i1)=psiH(i0)+ufac(i,j)*ut(i2)+vfac(i,j)*vt(i2);
 end
end

if kgr > 0,
%-
 shift=-1; ccB=[0 0]; cbV=1; AxBx=[-180 180 -90 90] ;
 ccB=[-140 140];
%AxBx=[-210 210 30 90];
 grph_CSz(psiH,xcs,ycs,xcg,ycg,ccB(1),ccB(2),shift,cbV,AxBx)
 title(titexp);
end
