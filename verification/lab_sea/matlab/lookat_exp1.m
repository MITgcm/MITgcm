% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp1/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat+1; Blon=lon+1;

% load model output
area =readbin('../results/AREAtave.0000000010.data' ,[20 16 1],1);
heff =readbin('../results/HEFFtave.0000000010.data' ,[20 16 1],1);
uice =readbin('../results/UICEtave.0000000010.data' ,[20 16 1],1);
vice =readbin('../results/VICEtave.0000000010.data' ,[20 16 1],1);
area1=readbin([exp1_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp1_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice1=readbin([exp1_path 'UICEtave.0000000010.data'],[20 16 1],1);
vice1=readbin([exp1_path 'VICEtave.0000000010.data'],[20 16 1],1);

% compute ice speed and direction
udir=0*uice; vdir=0*vice;
udir1=0*uice; vdir1=0*vice;
icespeed=100*sqrt(uice.^2+vice.^2);
in=find(icespeed);
udir(in)=uice(in)./icespeed(in);
vdir(in)=vice(in)./icespeed(in);
icespeed1=100*sqrt(uice1.^2+vice1.^2);
in=find(icespeed1);
udir1(in)=uice1(in)./icespeed1(in);
vdir1(in)=vice1(in)./icespeed1(in);

% plot comparison figures

clf, subplot(321)
mypcolor(lon,lat,area1'); caxis([0 1]), colorbar
title('Sea-ice concentration')
set(gca,'xticklabel',[])

subplot(322)
mypcolor(lon,lat,area1'-area'); colorbar
title('Difference with release1 patch10')
set(gca,'xticklabel',[])

subplot(323)
mypcolor(lon,lat,heff1'); caxis([0 .3]), colorbar
title('Effective sea-ice thickness (m)')
set(gca,'xticklabel',[])

subplot(324)
mypcolor(lon,lat,heff1'-heff'); colorbar
title('Difference with release1 patch10')
set(gca,'xticklabel',[])

subplot(325)
mypcolor(Blon,Blat,icespeed1'); caxis([0 10]), colorbar
hold on, myquiver(Blon,Blat,udir1',vdir1','k');
title('Sea-ice velocity (cm/s)')

subplot(326)
mypcolor(Blon,Blat,icespeed1'-icespeed'); colorbar
if mmax(abs(udir1-udir)) | mmax(abs(vdir1-vdir))
  hold on, myquiver(Blon,Blat,udir1'-udir',vdir1'-vdir','k');
end
title('Difference with release1 patch10')
