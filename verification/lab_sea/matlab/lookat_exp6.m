% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp1/';
exp2_path='../input/exp6/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat+1; Blon=lon+1;

% load model output
area1=readbin([exp1_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp1_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice1=readbin([exp1_path 'FUtave.0000000010.data'],[20 16 1],1);
vice1=readbin([exp1_path 'FVtave.0000000010.data'],[20 16 1],1);
area2=readbin([exp2_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff2=readbin([exp2_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice2=readbin([exp2_path 'FUtave.0000000010.data'],[20 16 1],1);
vice2=readbin([exp2_path 'FVtave.0000000010.data'],[20 16 1],1);

% compute ice speed and direction
udir1=0*uice1; vdir1=0*vice1;
udir2=0*uice2; vdir2=0*vice2;
icespeed1=100*sqrt(uice1.^2+vice1.^2);
in=find(icespeed1);
udir1(in)=uice1(in)./icespeed1(in);
vdir1(in)=vice1(in)./icespeed1(in);
icespeed2=100*sqrt(uice2.^2+vice2.^2);
in=find(icespeed2);
udir2(in)=uice2(in)./icespeed2(in);
vdir2(in)=vice2(in)./icespeed2(in);

% plot comparison figures
clf
subplot(321)
mypcolor(lon,lat,area1'); caxis([0 1]), colorbar
title('Sea-ice concentration with dynamics')
subplot(322)
mypcolor(lon,lat,area2'); caxis([0 1]), colorbar
title('Without dynamics')
subplot(323)
mypcolor(lon,lat,heff1'); caxis([0 .3]), colorbar
title('Effective sea-ice thickness with dynamics (m)')
subplot(324)
mypcolor(lon,lat,heff2'); caxis([0 .3]), colorbar
title('Without dynamics')
subplot(325)
mypcolor(Blon,Blat,icespeed1'); caxis([0 10]), colorbar
hold on,myquiver(Blon,Blat,udir1',vdir1','k');
title('Wind stress with dynamics (N/m^2)')
subplot(326)
mypcolor(Blon,Blat,icespeed2'); caxis([0 10]), colorbar
hold on,myquiver(Blon,Blat,udir2',vdir2','k');
title('Without dynamics')
