% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp_path='../build/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat-1; Blon=lon-1;

% load model output
area =readbin('../results/AREAtave.0000000010.data' ,[20 16 1],1);
heff =readbin('../results/HEFFtave.0000000010.data' ,[20 16 1],1);
uice =readbin('../results/UICEtave.0000000010.data' ,[20 16 1],1);
vice =readbin('../results/VICEtave.0000000010.data' ,[20 16 1],1);
area1=readbin([exp_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice1=readbin([exp_path 'UICEtave.0000000010.data'],[20 16 1],1);
vice1=readbin([exp_path 'VICEtave.0000000010.data'],[20 16 1],1);

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
title('Sea-ice concentration, this run')
set(gca,'xticklabel',[])

subplot(322)
mypcolor(lon,lat,area'); caxis([0 1]), colorbar
title('Sea-ice concentration, checkpoint52l')
set(gca,'xticklabel',[])

subplot(323)
mypcolor(lon,lat,heff1'); caxis([0 .6]), colorbar
title('Effective sea-ice thickness (m), this run')
set(gca,'xticklabel',[])

subplot(324)
mypcolor(lon,lat,heff'); caxis([0 .6]), colorbar
title('Effective sea-ice thickness (m), checkpoint52l')
set(gca,'xticklabel',[])

subplot(325)
mypcolor(Blon,Blat,icespeed1'); caxis([0 12]), colorbar
hold on, myquiver(Blon,Blat,udir1',vdir1','k');
title('Sea-ice velocity (cm/s), this run')

subplot(326)
mypcolor(Blon,Blat,icespeed'); caxis([0 12]), colorbar
hold on, myquiver(Blon,Blat,udir',vdir','k');
title('Sea-ice velocity (cm/s), checkpoint52l')
