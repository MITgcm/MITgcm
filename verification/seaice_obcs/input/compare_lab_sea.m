% Compare MITgcm/verification/seaice_obcs
% results to ../lab_sea/input.salt_plume

% preamble
nx=20; ny=16; nz=18; nt=6; ix=8:17; iy=4:11;
p1='../../lab_sea/tr_run.salt_plume/';
p2='../run/';

% compare U/V/T/S
for f={'T','S','U','V'}
    J=0;
    fn=[p1 f{1} '.' myint2str(nt,10) '.data'];
    tmp=readbin(fn,[nx ny nz]); tmp1=tmp(ix,iy,:);
    fn=[p2 f{1} '.' myint2str(nt,10) '.data'];
    tmp2=readbin(fn,[length(ix) length(iy) nz]);
    for k=1:nz
        clf, subplot(311), mypcolor(tmp1(:,:,k)'); thincolorbar
        title(['lab sea ' f{1} ', level ' int2str(k)]);
        subplot(312), mypcolor(tmp2(:,:,k)'); thincolorbar
        title(['seaice obcs ' f{1} ', level ' int2str(k)]);
        tmp3=tmp2(:,:,k)-tmp1(:,:,k);
        subplot(313), mypcolor(tmp3'); thincolorbar
        title('difference'), pause(.1)
        J=J+sum(sum(tmp3(2:(end-1),2:(end-1)).^2));
    end
    disp([f{1} ': ' num2str(J)])
end

% compare sea ice
for f={'UICE','VICE','AREA','HSNOW','HSALT','HEFF'}
  J=0;
  for t=0:nt
    fn=[p1 f{1} '.' myint2str(t,10) '.data'];
    tmp=readbin(fn,[nx ny]); tmp1=tmp(ix,iy,:);
    fn=[p2 f{1} '.' myint2str(t,10) '.data'];
    tmp2=readbin(fn,[length(ix) length(iy)]);
    clf, subplot(311), mypcolor(tmp1'); cx=caxis; thincolorbar
    title(['lab sea ' f{1} ', time step ' int2str(t)]);
    subplot(312), mypcolor(tmp2'); caxis(cx), thincolorbar
    title(['seaice obcs ' f{1} ', time step ' int2str(t)]);
    tmp3=tmp2-tmp1;
    subplot(313), mypcolor(tmp3'); thincolorbar
    title('difference'), pause(.1)
    J=J+sum(sum(tmp3(2:(end-1),2:(end-1)).^2));
  end
  disp([f{1} ': ' num2str(J)])
end
