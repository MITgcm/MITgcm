clear all
nx=32;ny=nx;ntx=6;

% Generate initial surface pressure
h=zeros(nx,ntx,ny);
tileno=2;
o=1;
i=round(nx*1/2);
j=round(ny*1/2);
h(i:i+o,tileno,j:j+o)=1;

fid=fopen('square.bin','w','b');
fwrite(fid,h,'real*8');
fclose(fid);

% Read grid info and convert to model input files

% DXC
fid=fopen('DXC.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DXC.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
% DYC
q=q';
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DYC.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% DXG
fid=fopen('DXG.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DXG.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
% DYG
q=q';
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DYG.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% DXF
fid=fopen('DXF.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DXF.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
% DYF
q=q';
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DYF.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% DXV
fid=fopen('DXV.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DXV.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
% DYU
q=q';
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('DYU.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% RAW
fid=fopen('RAW.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('RAW.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
% RAS
q=q';
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('RAS.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% RA
fid=fopen('RA.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('RA.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% RAZ
fid=fopen('RAZ.tile','r','b');q=fread(fid,[nx nx],'real*8');fclose(fid);
Q(1:nx,:)=q;Q(nx+(1:nx),:)=q;Q(2*nx+(1:nx),:)=q;Q(3*nx+(1:nx),:)=q;Q(4*nx+(1:nx),:)=q;Q(5*nx+(1:nx),:)=q;
fid=fopen('RAZ.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% LONC
fid=fopen('LONC.tile','r','b');q=fread(fid,[nx nx*6],'real*8');fclose(fid);
q=reshape(q,[nx nx 6]);
Q(1:nx,:)=q(:,:,1);Q(nx+(1:nx),:)=q(:,:,2);Q(2*nx+(1:nx),:)=q(:,:,3);Q(3*nx+(1:nx),:)=q(:,:,4);Q(4*nx+(1:nx),:)=q(:,:,5);Q(5*nx+(1:nx),:)=q(:,:,6);
fid=fopen('LONC.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% LATC
fid=fopen('LATC.tile','r','b');q=fread(fid,[nx nx*6],'real*8');fclose(fid);
q=reshape(q,[nx nx 6]);
Q(1:nx,:)=q(:,:,1);Q(nx+(1:nx),:)=q(:,:,2);Q(2*nx+(1:nx),:)=q(:,:,3);Q(3*nx+(1:nx),:)=q(:,:,4);Q(4*nx+(1:nx),:)=q(:,:,5);Q(5*nx+(1:nx),:)=q(:,:,6);
fid=fopen('LATC.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

lat=reshape(Q,[nx ntx nx]);
rad=30;
h=100*0.5*(1-cos( max(lat-90+rad,0)/rad*pi ));
fid=fopen('cos.bin','w','b');fwrite(fid,h,'real*8');fclose(fid);
h(:,2,:)=h(:,3,:);h(:,3,:)=0;
fid=fopen('cos_equator.bin','w','b');fwrite(fid,h,'real*8');fclose(fid);

% LONG
fid=fopen('LONG.tile','r','b');q=fread(fid,[nx+1 (nx+1)*6],'real*8');fclose(fid);
q=reshape(q,[nx+1 nx+1 6]);
q=q(1:nx,1:nx,:);
Q(1:nx,:)=q(:,:,1);Q(nx+(1:nx),:)=q(:,:,2);Q(2*nx+(1:nx),:)=q(:,:,3);Q(3*nx+(1:nx),:)=q(:,:,4);Q(4*nx+(1:nx),:)=q(:,:,5);Q(5*nx+(1:nx),:)=q(:,:,6);
fid=fopen('LONG.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);

% LATG
fid=fopen('LATG.tile','r','b');q=fread(fid,[nx+1 (nx+1)*6],'real*8');fclose(fid);
q=reshape(q,[nx+1 nx+1 6]);
q=q(1:nx,1:nx,:);
Q(1:nx,:)=q(:,:,1);Q(nx+(1:nx),:)=q(:,:,2);Q(2*nx+(1:nx),:)=q(:,:,3);Q(3*nx+(1:nx),:)=q(:,:,4);Q(4*nx+(1:nx),:)=q(:,:,5);Q(5*nx+(1:nx),:)=q(:,:,6);
fid=fopen('LATG.bin','w','b');fwrite(fid,Q,'real*8');fclose(fid);
