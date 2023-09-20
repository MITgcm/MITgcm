
% input: krd, kfac, jprt
% kfac = 0 : read-in 1 diagnostics output file containing both UVELMASS & VVELMASS
% kfac > 0 : read-in 2 Time-ave output files ; kfac=1 with hFac in ; kfac=2 : add hFac here.
krd=2; kfac=0; jprt=0;

if krd > 0,
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
 nr=size(uu,3); nPg=nc*nc*6;
end

if krd > 1,
 lDir='grid_files/';
 if krd == 2,
   bk_lineF='isoLat_cs32_59';
%- load broken lines (iso-lat) :
% bkl_Ylat, bkl_Npts, bkl_Flg, bkl_IJuv, bkl_Xsg, bkl_Ysg, bkl_Zon
   load([lDir,bk_lineF]);
 end
 if krd == 3,
   bk_lineF='bkl_AB_cs32';
%- load broken line (great-circle arc AB):
% ab_Npts, ab_Flg, ab_IJuv, ab_Xsg, ab_Ysg
   load([lDir,bk_lineF]);
   bkl_Ylat=0; bkl_Npts=ab_Npts;
   bkl_Flg=ab_Flg'; bkl_IJuv=ab_IJuv'; bkl_Xsg=ab_Xsg'; bkl_Ysg=ab_Ysg';
 end

 ufac=rem(bkl_Flg,2) ; vfac=fix(bkl_Flg/2) ;

%- load the grid dx,dy :
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 G=load_grid(gDir,ncdf);
 dxg=G.dXg; dyg=G.dYg;
 fprintf([' load bk_line description from file=',bk_lineF, ...
          '  AND dxg,dyg files \n']);
 dxg=reshape(dxg,nPg,1); dyg=reshape(dyg,nPg,1);
end

%- Atmospheric example:
%delR=[10.E3, 25.E3, 30.E3, 20.E3, 15.E3]; % = delP in Pa
%psiFac=1.e-9; g=9.81; % <- convert to 10^9 kg/s
%- Oceanic example:
%delR=[50 70 100 140 190 240 290 340 390 440 490 540 590 640 690]; % = delR in m
delR=G.dRf;
psiFac=-1.e-6; g=1; % <- convert to Sv. [10^6 m3/s]

%- compute the horizontal transport ut,vt :
ut=reshape(uu,[nPg nr]); vt=reshape(vv,[nPg nr]);
for k=nr:-1:1,
 ut(:,k)=psiFac*dyg.*ut(:,k);
 vt(:,k)=psiFac*dxg.*vt(:,k);
end
if n2h == 6*nc,
%- to use this (older) bk-line file, long-vector is in "old-format" style, i.e., nc*6*nc:
 ut=reshape(ut,[nc nc 6 nr]); ut=permute(ut,[1 3 2 4]);
 vt=reshape(vt,[nc nc 6 nr]); vt=permute(vt,[1 3 2 4]);
 ut=reshape(ut,[nPg nr]); vt=reshape(vt,[nPg nr]);
end

%- integrate along a broken-line (~ iso Latitude)
ydim=length(bkl_Ylat); ylat=bkl_Ylat;
vz=zeros(ydim,nr);
for jl=1:ydim,
  if jl == jprt, fprintf(' jl= %2i , lat= %8.3f , Npts= %3i :\n', ...
                        jl,ylat(jl),bkl_Npts(jl)); end
 if 1 < 0,
 %-- slower version (with debug print):
  for ii=1:bkl_Npts(jl),
   ij=bkl_IJuv(ii,jl);
   if jl == jprt,
    i = 1+rem(ij-1,6*nc); j=1+fix((ij-1)/nc/6); f=1+fix((i-1)/nc); i=1+rem(i-1,nc);
    fprintf(' no= %3i : Flg,I,J= %2i (%2i,%2i) %3i %3i (nf=%1i)\n', ...
    ii,bkl_Flg(ii,jl),ufac(ii,jl),vfac(ii,jl),i,j,f);
   end
   for k=1:nr,
    vz(jl,k)=vz(jl,k)+ufac(ii,jl)*ut(ij,k)+vfac(ii,jl)*vt(ij,k);
   end
  end
 else
 %-- faster version (no debug print):
 % Note: could reduce memory if permute k & jl loop and compute ut,vt within same k loop
 %       and then can also cumulate stream-function within same single k loop
  ie=bkl_Npts(jl);
  for k=1:nr,
    vz(jl,k)=sum( ufac(1:ie,jl).*ut(bkl_IJuv(1:ie,jl),k) ...
                + vfac(1:ie,jl).*vt(bkl_IJuv(1:ie,jl),k) );
  end
 end

end

%- compute the meridional transport streamfunction:
psi=zeros(ydim+2,nr+1);
for j=1:ydim, for k=nr:-1:1,
   psi(j+1,k)=psi(j+1,k+1) + delR(k)*vz(j,k)/g ;
end ; end

%-- make a graphe :
yax=zeros(ydim+2,1);yax(2:ydim+1)=ylat;
if krd ==3, yax=[-10 0 10];
else yax(1)=2*ylat(1)-ylat(2); yax(2+ydim)=2*ylat(ydim)-ylat(ydim-1); end
zax=[0:nr];
%---
%ccp=[10:20:90]; cc=[-ccp(end:-1:1) 0 ccp];
 ccp=[5:5:20 30:10:90]; cc=[-ccp(end:-1:1) 0 ccp];
[cs,h]=contour(yax,zax,psi',cc);clabel(cs); isoline0(h);
if g==1, set(gca,'YDir','reverse'); end
grid on;
MxV=max(max(abs(psi)));
Msurf=max(squeeze(abs(psi(:,1))));
if g== 1,
 text(0,nr*1.07,sprintf('Max= %9.5g  ;  Surf-Max= %9.5g ', MxV, Msurf))
 title(['Merid. Transport [Sv] ; ',titexp,tit0]);
else
 text(0,-0.5,sprintf('Max= %9.5g  ;  Surf-Max= %9.5g ', MxV, Msurf))
 title(['Merid. Transport [10^9 kg/s] ; ',titexp,tit0]);
end
%-----------------

return
