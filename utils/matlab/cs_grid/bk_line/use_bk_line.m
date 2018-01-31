
% input: krd, kfac, jprt
krd=2; kfac=0; jprt=0;

if krd > 0,
 rac='output_files/';   
 nit=72020;
 titexp=' Global-Ocean-CS '; tit0=[';  it=',int2str(nit)];
 uu=rdmds([rac,'hUtave'],nit);
 vv=rdmds([rac,'hVtave'],nit);
 if kfac==1,
  hFacW=rdmds([rac,'hFacW']);
  hFacS=rdmds([rac,'hFacS']);
  uu=hFacW.*uu; vv=hFacS.*vv;
 end
 nc=size(uu,2); nPg=nc*nc*6;
 nr=size(uu,3);
end

if krd > 1,
 Rac='grid_files/';
 if krd == 2,
   bk_lineF='isoLat_cs32_59';
%- load broken lines (iso-lat) :
% bkl_Ylat, bkl_Npts, bkl_Flg, bkl_IJuv, bkl_Xsg, bkl_Ysg, bkl_Zon
   load([Rac,bk_lineF]);
 end
 if krd == 3,
   bk_lineF='bkl_AB_cs32';
%- load broken line (great-circle arc AB):
% ab_Npts, ab_Flg, ab_IJuv, ab_Xsg, ab_Ysg
   load([Rac,bk_lineF]);
   bkl_Ylat=0; bkl_Npts=ab_Npts; 
   bkl_Flg=ab_Flg'; bkl_IJuv=ab_IJuv'; bkl_Xsg=ab_Xsg'; bkl_Ysg=ab_Ysg';
 end

 ufac=rem(bkl_Flg,2) ; vfac=fix(bkl_Flg/2) ;

%- load the grid dx,dy :
%- set ncdf=1 to load MNC (NetCDF) grid-files ;
%   or ncdf=0 to load MDS (binary) grid-files :
 ncdf=0;
 G=load_grid(Rac,10+ncdf);
 dxg=G.dXg; dyg=G.dYg;
 fprintf([' load bk_line description from file=',bk_lineF, ...
          '  AND dxg,dyg files \n']);
 dxg=reshape(dxg,nPg,1); dyg=reshape(dyg,nPg,1);
end

%- Atmospheric example:
%delR=[10.E3, 25.E3, 30.E3, 20.E3, 15.E3]; % = delP in Pa
%psiFac=1.e-9; g=9.81; % <- convert to 10^9 kg/s
%- Oceanic example:
delR=[50 70 100 140 190 240 290 340 390 440 490 540 590 640 690]; % = delR in m
psiFac=1.e-6; g=1; % <- convert to Sv. [10^6 m3/s]

%- compute the horizontal transport ut,vt :
ut=reshape(uu,[nPg nr]); vt=reshape(vv,[nPg nr]);
for k=nr:-1:1,
 ut(:,k)=psiFac*dyg.*ut(:,k);
 vt(:,k)=psiFac*dxg.*vt(:,k);
end

%- integrate along a broken-line (~ iso Latitude)
ydim=length(bkl_Ylat); ylat=bkl_Ylat;
vz=zeros(ydim,nr);
for jl=1:ydim,
 if jl == jprt, fprintf(' jl= %2i , lat= %8.3f , Npts= %3i :\n', ...
                        jl,ylat(jl),bkl_Npts(jl)); end
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
ccp=[10:20:90]; cc=[-ccp(end:-1:1) 0 ccp];
[cs,h]=contour(yax,zax,psi',cc);clabel(cs); isoline0(h);
if g==1, set(gca,'YDir','reverse'); end
grid on;
MxV=max(max(abs(psi))); 
Msurf=max(squeeze(abs(psi(:,1))));
if g== 1,
 text(0,nr+1,sprintf('Max= %9.5g  ;  Surf-Max= %9.5g ', MxV, Msurf))
 title(['Merid. Transport [Sv] ; ',titexp,tit0]);
else
 text(0,-0.5,sprintf('Max= %9.5g  ;  Surf-Max= %9.5g ', MxV, Msurf))
 title(['Merid. Transport [10^9 kg/s] ; ',titexp,tit0]);
end
%-----------------                           

return
