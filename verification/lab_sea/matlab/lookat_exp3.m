% need to be in verification/lab_sea/matlab directory
% and to specify location of exp1 output
cd ../../../verification/lab_sea/matlab
exp1_path='../input/exp3a/';
exp2_path='../input/exp3b/';

% load model output
uice1=readbin([exp1_path 'UICEtave.0000000001.data'],[20 16 1],1);
vice1=readbin([exp1_path 'VICEtave.0000000001.data'],[20 16 1],1);
uice2=readbin([exp2_path 'UICEtave.0000000001.data'],[20 16 1],1);
vice2=readbin([exp2_path 'VICEtave.0000000001.data'],[20 16 1],1);

% plot comparison figures
clf, subplot(221), mypcolor(uice1'); colorbar
title('UICE using ADI solver')
subplot(222), mypcolor(vice1'); colorbar
title('VICE using ADI solver')
subplot(223), mypcolor(uice2'); colorbar
title('UICE using LSR solver')
subplot(224), mypcolor(vice2'); colorbar
title('VICE using LSR solver')
