% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 and exp2 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp6/';
exp2_path='../input/exp7/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat-1; Blon=lon-1;

% load model output
area1=readbin([exp1_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp1_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice1=readbin([exp1_path 'uVeltave.0000000010.data'],[20 16 23],1);
vice1=readbin([exp1_path 'vVeltave.0000000010.data'],[20 16 23],1);
area2=readbin([exp2_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff2=readbin([exp2_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice2=readbin([exp2_path 'uVeltave.0000000010.data'],[20 16 23],1);
vice2=readbin([exp2_path 'vVeltave.0000000010.data'],[20 16 23],1);
eta1=readbin([exp1_path 'ETAtave.0000000010.data'],[20 16 1],1);
eta2=readbin([exp2_path 'ETAtave.0000000010.data'],[20 16 1],1);
t1=readbin([exp1_path 'Ttave.0000000010.data'],[20 16 23],1);
t2=readbin([exp2_path 'Ttave.0000000010.data'],[20 16 23],1);
s1=readbin([exp1_path 'Stave.0000000010.data'],[20 16 23],1);
s2=readbin([exp2_path 'Stave.0000000010.data'],[20 16 23],1);

% plot comparison figures
clf, orient tall, wysiwyg, k=1;
subplot(721), mypcolor(lon,lat,area2'); colorbar
title('Ice concentration during first 10 hours'),set(gca,'xticklabel',[])
subplot(722), mypcolor(lon,lat,area2'-area1'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(723), mypcolor(lon,lat,heff2'); colorbar
title('Effective ice thickness'),set(gca,'xticklabel',[])
subplot(724), mypcolor(lon,lat,heff2'-heff1'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(725), mypcolor(lon,lat,uice2(:,:,k)'); colorbar
title('uVel at level 1'),set(gca,'xticklabel',[])
subplot(726), mypcolor(lon,lat,uice2(:,:,k)'-uice1(:,:,k)'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(727), mypcolor(lon,lat,vice2(:,:,k)'); colorbar
title('vVel at level 1'),set(gca,'xticklabel',[])
subplot(728), mypcolor(lon,lat,vice2(:,:,k)'-vice1(:,:,k)'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(7,2,9), mypcolor(lon,lat,eta2'); colorbar
title('SSH'),set(gca,'xticklabel',[])
subplot(7,2,10), mypcolor(lon,lat,eta2'-eta1'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(7,2,11), mypcolor(lon,lat,t2(:,:,k)'); colorbar
title('theta at level 1'),set(gca,'xticklabel',[])
subplot(7,2,12), mypcolor(lon,lat,t2(:,:,k)'-t1(:,:,k)'); colorbar
title('2cpu - 1cpu solution'),set(gca,'xticklabel',[])
subplot(7,2,13), mypcolor(lon,lat,s2(:,:,k)'); colorbar
title('salt at level 1')
subplot(7,2,14), mypcolor(lon,lat,s2(:,:,k)'-s1(:,:,k)'); colorbar
title('2cpu - 1cpu solution')
