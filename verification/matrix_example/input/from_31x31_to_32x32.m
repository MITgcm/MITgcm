
%- To allow to split the domain in several tiles, change the domain size
%  from 31x31 to 32x32 by adding 1 row (on Southern side) + 1 column (on
%  Western side) of land points.

%- read old input files (31x31) and write new ones:
nx1=31; ny1=31;
nx2=1+nx1;
ny2=1+ny1;
prec='real*4';

fnam1='topog.box';
fnam2='topo_box.bin';
fprintf('converting %s to %s ...',fnam1,fnam2)
vv1=rdda(fnam1,[nx1 ny1],1,prec,'b');
vv2=zeros(nx2,ny2); vv2(2:nx2,2:ny2)=vv1;
fid=fopen(fnam2,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
fprintf(' done\n');
%figure(1);clf; imagesc(vv2');set(gca,'YDir','normal');colorbar

fnam1='windx.cos_y';
fnam2='taux_cosY.bin';
fprintf('converting %s to %s ...',fnam1,fnam2)
vv1=rdda(fnam1,[nx1 ny1],1,prec,'b');
vv2=zeros(nx2,ny2); vv2(2:nx2,2:ny2)=vv1;
fid=fopen(fnam2,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
fprintf(' done\n');

fnam1='trinit1';
fnam2='tr1_ini.bin';
fprintf('converting %s to %s ...',fnam1,fnam2)
vv1=rdda(fnam1,[nx1 ny1],1,prec,'b');
vv2=zeros(nx2,ny2); vv2(2:nx2,2:ny2)=vv1;
fid=fopen(fnam2,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
fprintf(' done\n');

prec='real*8';
fnam1='pickup'; it=200000;
fnam2='pickup.32x32.data';
fprintf('converting %s to %s ...',fnam1,fnam2)
vv1=rdmds(fnam1,it); n3d=size(vv1,3);
vv2=zeros(nx2,ny2,n3d); vv2(2:nx2,2:ny2,:)=vv1;
fid=fopen(fnam2,'w','b'); fwrite(fid,vv2,prec); fclose(fid);
fprintf(' done\n');

