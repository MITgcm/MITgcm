
%-- Add angle (Cos & Sin) to grid files

%nr = 360; ng = 120; nb = 120; tnx=120; tny=120; %- polar-cap grid
rgbDim=[360 120 120];
%- set all 6 faces dimensions
 nr=rgbDim(1); ng=rgbDim(2); nb=rgbDim(3);
 nf=ones(6,2);
 nf(1,:)=[nb nr]; nf(2,:)=[ng nr]; nf(3,:)=[ng nb];
 nf(4,:)=[nr nb]; nf(5,:)=[nr ng]; nf(6,:)=[nb ng];
 fdim=prod(nf,2); fd2= cumsum(fdim); fd1=fd2-fdim+1;

%- compact format:
nx=gcd(gcd(rgbDim(1),rgbDim(2)),rgbDim(3));
ny=sum(fdim)/nx;

%--------
%rDir='../input/';
rDir='./';
inpName='face.mitgrid';
kwr=1; outpName='llc_120x360';

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
 s=size(vv1,1); k1=s/(nf(n,1)+1)/(nf(n,2)+1);
 fprintf(['read: ',namF,' : size: %i (%ix%ix%i)'],s,nf(n,1)+1,nf(n,2)+1,k1);
 vv1=reshape(vv1,[nf(n,1)+1 nf(n,2)+1 k1]);
%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
%------------------------------------------------------------------
%-- save each face:
 nvar=['vvf',int2str(n),'=vv1;']; fprintf(' ; %s \n',nvar);
 eval(nvar);
end
titv={'XC','YC','DXF','DYF','RA','XG','YG','DXV','DYU','RAZ','DXC','DYC', ...
      'RAW','RAS','DXG','DYG'};

jlc=330; ilc=1+nr-jlc;

%- XC YC DXF DYF RA XG YG DXV DYU RAZ DXC DYC RAW RAS DXG DYG:
%   1  2  3   4   5  6  7  8   9  10  11  12  13  14  15  16
gpos=[3 3 3   3   3  0  0  0   0   0   1   2   1   2   2   1];
% gpos: 0 = grid-cell corner ; 1 = U point ; 2 = V point ; 3 = Tracer point

%----------------------------------------------------------------------------

%- compute the analytic grid :
xc1=vvf1(1:120,1:jlc,1); xc=mean(xc1,2);
yc1=vvf1(1:120,1:jlc,2); yc=mean(yc1,1);
xg1=vvf1(1:121,1:jlc+1,6); xg=mean(xg1,2);
yg1=vvf1(1:121,1:jlc+1,7); yg=mean(yg1,1);

%- check symetry:
jeq=find(abs(yg) < 1.e-4);
dxEq=vvf1(1:120,jeq,15); Radius=2*sum(dxEq)/pi;
%fprintf(' Radius= %f ',Radius);
%Radius=round(Radius)*1; dxDeg=0.75;
fprintf(', Radius= %f\n',Radius);
rad=pi/180;

k2=k1+2;
for n=1:6, 
 nvar=['vv1=vvf',int2str(n),';']; %fprintf(' ; %s \n',nvar);
 eval(nvar);
 i2=nf(n,1); j2=nf(n,2); ip=i2+1; jp=j2+1;
 vv2=zeros(ip,jp,k2); vv2(:,:,[1:k1])=vv1;
 if n < 6, 
  yg=vv1(:,:,7);
  dxg=vv1(1:i2,1:jp,15);
  dyg=vv1(1:ip,1:j2,16);
% Build purely zonal flow from a stream function, psi = r_earth * latitude.
  psi=-Radius*yg*rad;
  uZ=psi([1:ip],[1:j2],:)-psi([1:ip],[2:jp],:);
  vZ=psi([2:ip],[1:jp],:)-psi([1:i2],[1:jp],:);
  uZ=uZ./dyg;
  vZ=vZ./dxg;
% Put constructed zonal wind at cell center.
  uZc=(uZ(1:i2,:,:)+uZ(2:ip,:,:))/2;
  vZc=(vZ(:,1:j2,:)+vZ(:,2:jp,:))/2;
% Calculate angles.
  norm=sqrt(uZc.*uZc+vZc.*vZc);
  AngleCS =  uZc./norm;
  AngleSN = -vZc./norm;
  vv2(1:i2,1:j2,k1+1)=AngleCS;
  vv2(1:i2,1:j2,k1+2)=AngleSN;
 else
  AngleCS =  ones(i2,j2);
  AngleSN =  zeros(i2,j2);
 end
%-- save to Structure:
 nvar=sprintf('csF.f%3.3i=AngleCS;',n); eval(nvar)
 nvar=sprintf('snF.f%3.3i=AngleSN;',n); eval(nvar)

%- write to file:
 if kwr == 1,
%-- write :
  namF=sprintf([rDir,outpName,'.face%3.3i.bin'],n);
  fprintf([': write to ',namF,' : size: (%ix%ix%i)\n'],nf(n,1)+1,nf(n,2)+1,k2);
  fid=fopen(namF,'w','b');
  fwrite(fid,vv2,'real*8');
  fclose(fid);
 end

end

nfg=1; k=0; ccB=[-1 1]*1.5;
plot_faces(nfg,csF,k,ccB,rgbDim);
nfg=2;
plot_faces(nfg,snF,k,ccB,rgbDim);
%----------------------------------------------------------------------------
return
