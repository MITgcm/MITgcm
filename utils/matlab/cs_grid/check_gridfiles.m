
% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/check_gridfiles.m,v 1.1 2008/08/05 20:00:19 jmc Exp $
% $Name:  $

%- for a symetric cubed-sphere grid (nr=ng=nb), check that:
%  1) all the faces have identical grid-spacing & grid-cell area
%     + long & lat match exactly.
%  2) grid-spacing & grid-cell area (+ long & lat) are symetric 
%     regarding the 3 symetry axes of a square.

nc=32; 
%nc=102; 
%nc=510; 

rDir='./';
inpName='tile.mitgrid';
%inpName='onedegcube';
strict=0; %- set to 1 to only check interior [nc,nc] if cell center var.

np=nc+1; nh=nc/2;

for n=1:6,
%-- read :
 p=strfind(inpName,'.');
 if isempty(p),
   namF=sprintf([rDir,inpName,'.face%3.3i.bin'],n);
 else
   namF=sprintf([rDir,inpName(1:4),'%3.3i',inpName(5:end)],n);
 end
 fid=fopen(namF,'r','b');
 vv1=fread(fid,'real*8');
 fclose(fid);
 s=size(vv1,1); k1=s/np/np;
 fprintf(['read: ',namF,' : size: %i (%ix%ix%i)\n'],s,np,np,k1);
 vv1=reshape(vv1,[np np k1]);
%-- do not add angle:
 vv2=vv1; k2=k1;
%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
%------------------------------------------------------------------
%-- save each face:
 if n == 1,
   vvf=zeros(np,np,6,k2);
 end
 vvf(:,:,n,:)=vv2;
end

%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
%   1  2  3   4   5  6  7  8   9  10  11  12  13  14  15  16
gpos=[3 3 3   3   3  0  0  0   0   0   1   2   1   2   2   1];
% gpos: 0 = grid-cell corner ; 1 = U point ; 2 = V point ; 3 = Tracer point

%- do not check the angle (even if in the files, k=17,18)
for k=1:min(k2,16),
 var=vvf(:,:,1,k); avr=mean(var(:));
 if k <  3 | k == 6 | k == 7,
%-- check long & latitude:
  epsil=3.e-14;
  list=[2 4 5];
  for n=list,
    mxd=0; shift=0;
    if k == 1 | k == 6,
      shift=90;
      if n == 5, shift=-90; end
      if n == 4, shift=-180; end
    end
    if n == 2;
     dd=vvf(:,:,n,k)-vvf(:,:,1,k);
     if k < 3, dd=dd(1:nc,1:nc); end
    else
     if gpos(k) == 3,
      var=vvf([1:nc],[1:nc],1,k);
      dd=vvf([nc:-1:1],[1:nc],n,k);
      dd=permute(dd,[2 1])-var;
     else
      var=vvf(:,:,1,k);
      dd=vvf(np:-1:1,:,n,k);
      dd=permute(dd,[2 1])-var;
%     if n == 4, dd(nh+1,:)=0; end %- skip line of day change
     end
    end
     dd=dd-shift;
     mnD=min(dd(:)); MxD=max(dd(:));
     ss=360*round(dd/360); dd=dd-ss;
     dd=abs(dd);
     mxd=max(dd(:));
    if mxd > epsil, 
     fprintf(' k,n= %2i %2i : Max diff f1 = %g (mn,Mx= %g %g)\n',k,n,mxd,mnD,MxD)
    end
  end
  n=6;
    if gpos(k) == 3,
      var=vvf([1:nc],[1:nc],3,k);
      tmp=vvf([1:nc],[1:nc],n,k);
    else
      var=vvf(:,:,3,k);
      tmp=vvf(:,:,n,k);
    end
    dd=tmp(end:-1:1,end:-1:1); dd=permute(dd,[2 1]);
    if k == 2 | k == 7,
     dd=dd+var;
    else
     dd=dd-var;
     if gpos(k) == 0, dd(nh+1,nh+1)=0; end %- skip the pole longitude
    end
     mnD=min(dd(:)); MxD=max(dd(:));
     ss=360*round(dd/360); dd=dd-ss;
     dd=abs(dd); mxd=max(dd(:));
    if mxd > epsil, 
     fprintf(' k,n= %2i %2i : Max diff f3 = %g (mn,Mx= %g %g)\n',k,n,mxd,mnD,MxD)
    end
  %-- check for symetry versus x=Xmid median on face 1
  mxd=0;
  if gpos(k) == 3,
    var=vvf([1:nc],[1:nc],1,k);
    sym=var([nc:-1:1],[1:nc]);
  else
    var=vvf(:,:,1,k);
    sym=var([np:-1:1],:);
  end
  if k == 1 | k == 6,
    dd=sym+var; 
  else
    dd=sym-var; 
  end
  mnD=min(dd(:)); MxD=max(dd(:));
  dd=abs(dd); mxd=max(dd(:));
  if mxd > epsil, 
     fprintf(' k= %2i : Sym1.1 Max diff= %g (mn,Mx= %g %g)\n',k,mxd,mnD,MxD)
  end
  %-- check for symetry versus y=Ymid median on face 1
  mxd=0;
  if gpos(k) == 3,
    var=vvf([1:nc],[1:nc],1,k);
    sym=var([1:nc],[nc:-1:1]);
  else
    var=vvf(:,:,1,k);
    sym=var(:,[np:-1:1]);
  end
  if k == 1 | k == 6,
    dd=sym-var; 
  else
    dd=sym+var; 
  end
  mnD=min(dd(:)); MxD=max(dd(:));
  dd=abs(dd); mxd=max(dd(:));
  if mxd > epsil, 
     fprintf(' k= %2i : Sym1.2 Max diff= %g (mn,Mx= %g %g)\n',k,mxd,mnD,MxD)
  end
  %-- check for symetry versus x=Xmid median on face 3
  %   check only half of the face (enough since check Sym.2 on full face)
  mxd=0;
  if gpos(k) == 3,
    var=vvf([1:nc],[1:nh],3,k);
    sym=var([nc:-1:1],[1:nh]);
  else
   %var=vvf(:,[1:nh+1],3,k);
    var=vvf(:,[1:nh],3,k); %- skip the line of day change
    sym=var([np:-1:1],:);
  end
  if k == 1 | k == 6,
    dd=sym+var;  %- X
    dd=dd-180;
  else
    dd=sym-var;  %- Y
  end
  mnD=min(dd(:)); MxD=max(dd(:));
  dd=abs(dd); mxd=max(dd(:));
  if mxd > epsil, 
     fprintf(' k= %2i : Sym3.1 Max diff= %g (mn,Mx= %g %g)\n',k,mxd,mnD,MxD)
  end
  %-- check for symetry versus y=Ymid median on face 3
  mxd=0;
  if gpos(k) == 3,
    var=vvf([1:nc],[1:nc],3,k);
    sym=var(:,[nc:-1:1]);
  else
    var=vvf(:,:,3,k);
    sym=var(:,[np:-1:1]);
  end
  if k == 1 | k == 6,
    dd=sym+var;  %- X
    if gpos(k) == 0, dd(nh:np,nh+1)=0; end
  else
    dd=sym-var;  %- Y
  end
  mnD=min(dd(:)); MxD=max(dd(:));
  dd=abs(dd); mxd=max(dd(:));
  if mxd > epsil, 
     fprintf(' k= %2i : Sym3.2 Max diff= %g (mn,Mx= %g %g)\n',k,mxd,mnD,MxD)
  end
  %-- check for symetry versus y=x diagonal on face 3
  %   check only 1/4 of the face (enough since already check Sym.1 & Sym.2)
  mxd=0;
  if gpos(k) == 3,
    var=vvf([1:nh],[1:nh],3,k);
    sym=permute(var,[2 1]);
  else
    var=vvf([1:nh+1],[1:nh+1],3,k);
   %var=vvf([1:nh],[1:nh],3,k);
    sym=permute(var,[2 1]);
  end
  if k == 1 | k == 6,
    dd=sym+var;  %- X
    dd=dd-90;
    if gpos(k) == 0, dd(nh+1,nh+1)=0; end %- skip the pole
  else
    dd=sym-var;  %- Y
  end
  mnD=min(dd(:)); MxD=max(dd(:));
  dd=abs(dd); mxd=max(dd(:));
  if mxd > epsil, 
     fprintf(' k= %2i : Sym3.3 Max diff= %g (mn,Mx= %g %g)\n',k,mxd,mnD,MxD)
  end
 end
%-------------------------------------------
%-- check grid-spacing & area:
 if k > 2 & k ~= 6 & k ~= 7,
  %-- check for identical values across faces
  for n=2:6,
    dd=vvf(:,:,n,k)-vvf(:,:,1,k);
    if strict == 1,
      if gpos(k) == 1, dd=dd(:,1:nc); end
      if gpos(k) == 2, dd=dd(1:nc,:); end
      if gpos(k) == 3, dd=dd(1:nc,1:nc); end
    end
    dd=abs(dd); mxd=max(dd(:));
    if mxd > 0, 
     fprintf(' k,n= %2i %2i : Max diff f1 = %g (av= %g)\n',k,n,mxd,avr)
    end
  end
  %-- check for symetry versus x=Xmid median (only on face 1)
  msd=0;
  if gpos(k) >= 2,
    var=vvf([1:nc],:,1,k);
    sym=var([nc:-1:1],:);
  else
    var=vvf(:,:,1,k);
    sym=var([np:-1:1],:);
  end
  dd=sym-var;
  if strict == 1 & rem(gpos(k),2) == 1, dd=dd(:,1:nc); end
  dd=abs(dd); mxd=max(dd(:));
  if mxd > 0, 
     fprintf(' k= %2i : Sym.1 Max diff= %g (av= %g)\n',k,mxd,avr)
  end
  %-- check for symetry versus y=Ymid median (only on face 1)
  msd=0;
  if rem(gpos(k),2) == 1,
    var=vvf(:,[1:nc],1,k);
    sym=var(:,[nc:-1:1]);
  else
    var=vvf(:,:,1,k);
    sym=var(:,[np:-1:1]);
  end
  dd=sym-var; 
  if strict == 1 & gpos(k) >=2, dd=dd(1:nc,:); end
  dd=abs(dd); mxd=max(dd(:));
  if mxd > 0, 
     fprintf(' k= %2i : Sym.2 Max diff= %g (av= %g)\n',k,mxd,avr)
  end
  %-- check for symetry versus x=y diagnonal (only on face 1)
  msd=0;
  if k == 5,
    var=vvf(1:nc,1:nc,1,k);
    sym=permute(var,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if k == 3,
    var=vvf(1:nc,1:nc,1,k);
    sym=vvf(1:nc,1:nc,1,k+1);
    sym=permute(sym,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if k == 10,
    var=vvf(:,:,1,k);
    sym=permute(var,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if k == 8,
    var=vvf(:,:,1,k);
    sym=vvf(:,:,1,k+1);
    sym=permute(sym,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if k == 11 | k == 13,
    var=vvf(:,1:nc,1,k);
    sym=vvf(1:nc,:,1,k+1);
    sym=permute(sym,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if k == 16,
    var=vvf(:,1:nc,1,k);
    sym=vvf(1:nc,:,1,k-1);
    sym=permute(sym,[2 1]);
    dd=sym-var; dd=abs(dd);
    mxd=max(dd(:));
  end
  if mxd > 0, 
     fprintf(' k= %2i : Sym.3 Max diff= %g (av= %g)\n',k,mxd,avr)
  end
 end
end
%  vvf=zeros(np,np,6,k2);
%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
%   1  2  3   4   5  6  7  8   9  10  11  12  13  14  15  16
%pos=[0 0 0   0   0  3  3  3   3   3   1   2   1   2   2   1];

return
shift=-1; ccB=[0 0]; cbV=1; AxBx=[-180 180 -90 90]; kEnv=0;
 AxBx2=[30 60 25 50];

 xc0=vvf(1:nc,1:nc,:,1); xc0=reshape(permute(xc0,[1 3 2]),[nc*6 nc]);
 yc0=vvf(1:nc,1:nc,:,2); yc0=reshape(permute(yc0,[1 3 2]),[nc*6 nc]);
 xg0=vvf(1:nc,1:nc,:,6); xg0=reshape(permute(xg0,[1 3 2]),[nc*6 nc]);
 yg0=vvf(1:nc,1:nc,:,7); yg0=reshape(permute(yg0,[1 3 2]),[nc*6 nc]);

 figure(2);clf;
 xtxt=0;ytxt=-98;
%subplot(311);
 var=vvf(1:nc,1:nc,:,5); var=reshape(permute(var,[1 3 2]),[nc*6 nc]);
 grph_CS(var,xc0,yc0,xg0,yg0,ccB(1),ccB(2),shift,cbV,AxBx,kEnv);
 mnV=min(var(:)); MxV=max(var(:));
 fprintf('min,Max= %9.5g  , %9.5g\n',mnV,MxV);
 text(xtxt,ytxt,sprintf('min,Max= %9.5g  , %9.5g',mnV,MxV));
%title([titexp,'rAc [m^2]'])
