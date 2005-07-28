% compare finite difference and adjoint gradient
% cost function is total sea-ice volume
% control variable is surface air temperature

path('../../lab_sea/matlab',path);
load SSMI
fn1='../../../exe/adxx_atemp.0000000000.001.001.data';
fn2='../../../exe/adxx_atemp.0000000000.002.001.data';
a=zeros(20,16);
a(1:10,:)=readbin(fn1,[10 16],1,'real*8',1);
a(11:20,:)=readbin(fn2,[10 16],1,'real*8',1);

clf reset
cx=[-1 1]; cl=[1 1 1]*.5; sc=1e4;
ax=([min(lon) max(lon) min(lat) max(lat)]);
mypcolor(lon,lat,a'/sc); caxis(cx), colorbar
plotland, axis(ax), grid





for fld={'aqh','atemp','salt','theta','uwind','vwind'}
  fn1=['adxx_' fld{1} '.0000000000.001.001.data'];
  fn2=['adxx_' fld{1} '.0000000000.002.001.data'];
  a=zeros(20,16);
  a(1:10,:)=readbin(fn1,[10 16],1,'real*8',1);
  a(11:20,:)=readbin(fn2,[10 16],1,'real*8',1);
  clf, mypcolor(a'); colorbar, title(fld{1}), pause
end
