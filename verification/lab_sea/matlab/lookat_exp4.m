% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 and exp2 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp1/';
exp2_path='../input/exp4/';

% load monthly-mean SMMR-SSM/I data
load SSMI

% B-grid latitude for uice and vice
Blat=lat-1; Blon=lon-1;

% load model output
area1=readbin([exp1_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff1=readbin([exp1_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice1=readbin([exp1_path 'UICEtave.0000000010.data'],[20 16 1],1);
vice1=readbin([exp1_path 'VICEtave.0000000010.data'],[20 16 1],1);
area2=readbin([exp2_path 'AREAtave.0000000010.data'],[20 16 1],1);
heff2=readbin([exp2_path 'HEFFtave.0000000010.data'],[20 16 1],1);
uice2=readbin([exp2_path 'UICEtave.0000000010.data'],[20 16 1],1);
vice2=readbin([exp2_path 'VICEtave.0000000010.data'],[20 16 1],1);

% plot comparison figures
clf, orient tall, wysiwyg
subplot(421), mypcolor(lon,lat,area2'); colorbar
title('Ice concentration during first 10 hours')
subplot(422), mypcolor(lon,lat,area2'-area1'); colorbar
title('2cpu - 1cpu solution')
subplot(423), mypcolor(lon,lat,heff2'); colorbar
title('Effective ice thickness')
subplot(424), mypcolor(lon,lat,heff2'-heff1'); colorbar
title('2cpu - 1cpu solution')
subplot(425), mypcolor(lon,lat,uice2'); colorbar
title('Uice')
subplot(426), mypcolor(lon,lat,uice2'-uice1'); colorbar
title('2cpu - 1cpu solution')
subplot(427), mypcolor(lon,lat,vice2'); colorbar
title('Vice')
subplot(428), mypcolor(lon,lat,vice2'-vice1'); colorbar
title('2cpu - 1cpu solution')
