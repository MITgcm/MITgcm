% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat+1; Blon=lon+1;

% load model output
area=readbin([exp1_path 'AREAtave.0000010950.data'],[20 16 1],1);
heff=readbin([exp1_path 'HEFFtave.0000010950.data'],[20 16 1],1);
uice=readbin([exp1_path 'UICEtave.0000010950.data'],[20 16 1],1);
vice=readbin([exp1_path 'VICEtave.0000010950.data'],[20 16 1],1);

% compute ice speed and direction
udir=0*uice; vdir=0*vice;
icespeed=100*sqrt(uice.^2+vice.^2);
in=find(icespeed);
udir(in)=uice(in)./icespeed(in);
vdir(in)=vice(in)./icespeed(in);

% plot comparison figures
clf
subplot(221)
mypcolor(lon,lat,SSMI(:,:,4)'/100); caxis([0 1]), colorbar
title('Observed March sea-ice concentration')
subplot(222), mypcolor(lon,lat,area'); caxis([0 1]), colorbar
title('Modeled March sea-ice concentration')
subplot(223), mypcolor(lon,lat,heff'); caxis([0 1]), colorbar
title('Effective thickness in m')
subplot(224)
mypcolor(Blon,Blat,icespeed'); caxis([0 10]), colorbar
hold on,myquiver(Blon,Blat,udir',vdir','k');
title('Sea-ice velocity (cm/s)')
