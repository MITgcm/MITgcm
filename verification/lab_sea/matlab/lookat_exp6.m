% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp1/';
exp2_path='../input/exp6/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% load model output
area1=readbin([exp1_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp1_path 'HEFFtave.0000000010.data'],[20 16 1],1);
area2=readbin([exp2_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff2=readbin([exp2_path 'HEFFtave.0000000010.data'],[20 16 1],1);

% plot comparison figures
clf, subplot(221)
mypcolor(lon,lat,area2'); caxis([0 1]), colorbar
title('Sea-ice concentration, no dynamics')
set(gca,'xticklabel',[])

subplot(222)
mypcolor(lon,lat,area2'-area1'); colorbar
title('Difference with LSR solver')
set(gca,'xticklabel',[])

subplot(223)
mypcolor(lon,lat,heff2'); caxis([0 .3]), colorbar
title('Effective sea-ice thickness (m)')
set(gca,'xticklabel',[])

subplot(224)
mypcolor(lon,lat,heff2'-heff1'); colorbar
title('Difference with LSR solver')
set(gca,'xticklabel',[])
