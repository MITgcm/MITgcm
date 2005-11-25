function [psi,mskG,ylat]=calc_PsiCube(delM,uu,vv,dxg,dyg,hFacW,hFacS,nBas,dBug);

% [psi,mskG,ylat]=calc_PsiCube(delM,uu,vv,[hFacW,hFacS],[nBas],[dBug]);
%- IMPORTANT: must multiply (u,v) by hFacW,S BEFORE using this script !
%   (so that it can be used in r* coordinate with (h*u,hv)_timeAv in input)
% delM= -delP/g for atmos ; =delZ for ocean (delR)

%- Units: dx,dy /1e6 ; delR /1e3 [hPa] ; psi in 10^9 kg/s
%  Atmos in p : use g=9.81 ; ocean in z : use g=-1;

krd=1; kMsep=1; jprt=0;
nr=length(delM);
Tprt=0;

if (nargin < 9), dBug=0; end
if (nargin < 8), nBas=0; end
if (nargin < 6), kfac=0;
else kfac=1; end;

if Tprt, TimeT0=clock; end

if krd > 0,
%- load: bkl_Ylat, bkl_Npts, bkl_Flg, bkl_IJuv, bkl_Xsg, bkl_Ysg, bkl_Zon
load('isoLat_cs32_59.mat');

%- load the grid dx,dy , convert to 10^6 m :
dxg=dxg*1.e-6;
dyg=dyg*1.e-6;
ncx=size(dxg,1);
nc=size(dxg,2);
dxg=reshape(dxg,ncx*nc,1); dyg=reshape(dyg,ncx*nc,1);
if dBug, fprintf(' AND dxg,dyg'); end
          
if nBas > 0,
%- load Ocean Basin mask (& separation line):
 mskBw=rdda([rac,'maskW_bas.bin'],[ncx*nc 3],1,'real*4','b');
 mskBs=rdda([rac,'maskS_bas.bin'],[ncx*nc 3],1,'real*4','b');
 if nBas==2, 
  mskBw(:,2)=mskBw(:,2)+mskBw(:,3);
  mskBs(:,2)=mskBs(:,2)+mskBs(:,3);
  mskBw=min(1,mskBw); mskBs=min(1,mskBs);
 end
%- load: np_Sep, ij_Sep, tp_Sep:
 sep_lineF=[rac,'sepBas_cs32_60'];
 load(sep_lineF);
 if dBug, fprintf([' + bassin mask & Sep.line:',sep_lineF]); end
end
 if dBug, fprintf('\n'); end
end

if Tprt, TimeT1=clock; end

%- compute the horizontal transport ut,vt :
if length(size(uu)) < 4, Nit=1; else Nit=size(uu,4); end;
uu=reshape(uu,ncx*nc,nr,Nit); vv=reshape(vv,ncx*nc,nr,Nit);
ydim=length(bkl_Ylat); ylat=bkl_Ylat;
psi=zeros(ydim+2,nr+1,1+nBas,Nit); 
mskZ=zeros(ydim+2,nr+1,1+nBas); % = mask of Psi 
mskV=zeros(ydim+2,nr,1+nBas);   % = mask of the Merid.Transport
mskG=zeros(ydim+1,nr,1+nBas);   % = mask of the Ground

%- define ufac,vfac for each bassin:
ufac=zeros([size(bkl_Flg) 1+nBas]);
vfac=zeros([size(bkl_Flg) 1+nBas]);
ufac(:,:,1)=rem(bkl_Flg,2) ; vfac(:,:,1)=fix(bkl_Flg/2) ;
for jl=1:ydim,
  ie=bkl_Npts(jl);
  for b=1:nBas, 
   ufac(1:ie,jl,1+b)=mskBw(bkl_IJuv(1:ie,jl),b).*ufac(1:ie,jl,1);
   vfac(1:ie,jl,1+b)=mskBs(bkl_IJuv(1:ie,jl),b).*vfac(1:ie,jl,1);
 end;
end;

%- compute transport ; integrate folowing broken-lines
for nt=1:Nit, 
 for k=nr:-1:1,

  ut=dyg.*uu(:,k,nt); 
  vt=dxg.*vv(:,k,nt);
  for jl=1:ydim,
   if jl == jprt, fprintf(' jl= %2i , lat= %8.3f , Npts= %3i :\n', ...
                            jl,ylat(jl),bkl_Npts(jl)); end
   ie=bkl_Npts(jl);
   for b=1:1+nBas, 
    vz=sum( ufac(1:ie,jl,b).*ut(bkl_IJuv(1:ie,jl)) ...
           +vfac(1:ie,jl,b).*vt(bkl_IJuv(1:ie,jl)) );
    psi(jl+1,k,b,nt)=psi(jl+1,k+1,b,nt) - delM(k)*vz ;
   end
  end

 end
end
   
if Tprt, TimeT2=clock; end

 %- compute the mask :
if kfac == 1, 
  hFacW=reshape(hFacW,ncx*nc,nr);
  hFacS=reshape(hFacS,ncx*nc,nr);
  ufac=abs(ufac) ; vfac=abs(vfac);
 for jl=1:ydim, 
  ie=bkl_Npts(jl); 
  hw=zeros(ie,nr); hs=zeros(ie,nr);
  hw=hFacW(bkl_IJuv(1:ie,jl),:);
  hs=hFacS(bkl_IJuv(1:ie,jl),:);
  for b=1:1+nBas,
   for k=1:nr,
%   for ii=1:bkl_Npts(jl);
%    ij=bkl_IJuv(ii,jl);
%    mskV(jl+1,k,b)=mskV(jl+1,k,b)+ufac(ii,jl,b)*hFacW(ij,k)+vfac(ii,jl,b)*hFacS(ij,k);
%   end ;
     tmpv=ufac(1:ie,jl,b).*hw(:,k)+vfac(1:ie,jl,b).*hs(:,k);
     mskV(jl+1,k,b)=mskV(jl+1,k,b)+max(tmpv);
   end ;
  end ;
 end
 mskV=ceil(mskV); mskV=min(1,mskV);
%- build the real mask (=mskG, ground) used to draw the continent with "surf":
%   position=centered , dim= ydim+1 x nr
 mskG=mskV(1:ydim+1,:,:)+mskV(2:ydim+2,:,:); mskG=min(1,mskG);

 if Tprt, TimeT3=clock; end

if kMsep & nBas > 0,
 mskW=1+min(1,ceil(hFacW));
 mskS=1+min(1,ceil(hFacS));
 for b=1:nBas,
  bs=b; b1=1+bs; b2=2+rem(bs,nBas);
  if nBas == 2, bs=b+b-1; b1=2; b2=3 ; end
  for j=1:ydim+1,
   for i=1:np_Sep(bs,j),
    ij=ij_Sep(bs,j,i); typ=abs(tp_Sep(bs,j,i));
    if typ == 1, 
     mskG(j,:,b1)=mskG(j,:,b1).*mskW(ij,:);
     mskG(j,:,b2)=mskG(j,:,b2).*mskW(ij,:);
    elseif typ == 2,
     mskG(j,:,b1)=mskG(j,:,b1).*mskS(ij,:);
     mskG(j,:,b2)=mskG(j,:,b2).*mskS(ij,:);
    end
   end
  end
 end
 mskG=min(2,mskG);
else
 if Tprt, TimeT3=clock; end
end

if Tprt, TimeT4=clock; end

%- to keep psi=0 on top & bottom
 mskZ(:,[2:nr+1],:)=mskV; 
 mskZ(:,[1:nr],:)=mskZ(:,[1:nr],:)+mskV;
%- to keep psi=0 on lateral boundaries :
 mskZ([1:ydim],:,:)=mskZ([1:ydim],:,:)+mskZ([2:ydim+1],:,:);
 mskZ([2:ydim+1],:,:)=mskZ([2:ydim+1],:,:)+mskZ([3:ydim+2],:,:);
 mskZ=ceil(mskZ); mskZ=min(1,mskZ);
 if kMsep & nBas > 0,
  mskM=zeros(ydim+2,nr,1+nBas); mskM(2:ydim+2,:,:)=min(2-mskG,1); 
  mskM(1:ydim+1,:,:)=mskM(1:ydim+1,:,:)+mskM(2:ydim+2,:,:);
  mskZ(:,1:nr,:)=min(mskZ(:,1:nr,:),mskM); 
 end
%- apply the mask (and remove dim = 1) :
 if Nit == 1,
  psi=squeeze(psi); mskV=squeeze(mskV); mskZ=squeeze(mskZ);
  psi( find(mskZ==0) )=NaN ;
 else
  for nt=1:Nit,
    psi1=psi(:,:,:,nt); psi1( find(mskZ==0) )=NaN ; psi(:,:,:,nt)=psi1;
  end;
  if nBas < 1, psi=squeeze(psi); mskV=squeeze(mskV); mskZ=squeeze(mskZ); end
 end
else
 if Tprt, TimeT3=TimeT2; TimeT4=TimeT2; end
 if nBas < 1 | Nit == 1, psi=squeeze(psi); end
end;
%----------------- 

if Tprt, TimeT5=clock;
 fprintf([' ---- Load, Comp.1,2,3,4 Total time Psi:', ...
          ' %6.3f %6.3f %6.3f %6.3f %6.3f %9.6f \n'],...
         etime(TimeT1,TimeT0), etime(TimeT2,TimeT1), ...
         etime(TimeT3,TimeT2), etime(TimeT4,TimeT3), ...
         etime(TimeT5,TimeT4), etime(TimeT5,TimeT0) );
end

return
