clear all;%close all;

addpath(pwd)
cdir = pwd;

% the directory with adjoint output
addir = '/net/ross/raid1/ifenty/MITgcm_20130115/MITgcm/verification/1D_ocean_ice_column/temp/mnc_test_0001/'

% how many vertical cells out output
dep_ind = 5;

% the experiment name
exp_name = 'temp'

% boolean for writing the figure to a png file
do_print = 1 ;

%  cd([dirRoot rdir '/' addir])
  cd(addir)

  add = dir('adstate*');
  ads = dir('adseaice*');
  ade = dir('adexf*');

  adseaice = nc_getall(ads(1).name);
  adstate  = nc_getall(add(1).name);
  adexf = nc_getall(ade(1).name);

  T = adseaice.T.data;
  adheff = permute(adseaice.adheff.data,[ 3 2 1]);
  adarea = permute(adseaice.adarea.data,[ 3 2 1]);

  adqnet = permute(adstate.adQnet.data,[3 2 1]);
  adatemp = permute(adexf.adatemp.data,[3 2 1]);
  advwind = permute(adexf.advwind.data,[3 2 1]);
  adswdown = permute(adexf.adswdown.data,[3 2 1]);
  adT = permute(adstate.adT.data,[4 3 2 1 ]);
  adS = permute(adstate.adS.data,[4 3 2 1 ]);
  
  ipt=1;if(size(adheff,1)==5);ipt=3;end;

  figure(2);clf;

  subplot(423); hold on;
  plot(T,squeeze(adarea(ipt,ipt,:)));grid;title('adarea');
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adarea(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')

  subplot(424); hold on;
  plot(T,squeeze(adheff(ipt,ipt,:)));axis tight;grid;title('adheff')
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adheff(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')

  subplot(425); hold on; 
  plot(T,squeeze(adqnet(ipt,ipt,:)));axis tight;grid;title('adqnet');
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adqnet(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')

  subplot(426); hold on; 
  plot(T,squeeze(advwind(ipt,ipt,:)));axis tight;grid;title('advwind');
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(advwind(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')

  subplot(421); hold on;
  plot(T,squeeze(adT(ipt,ipt,1:dep_ind,:))');axis tight;grid;title('adT')
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adT(ipt,ipt,1,:))));  plot(T(nn),T(nn).*0,'rx')
  h=legend('l1','l2','l3', 'l4','Location','SouthWest');
  set(h,'FontSize',6)

  subplot(422); hold on;
  plot(T,squeeze(adS(ipt,ipt,1:dep_ind,:))');axis tight;grid;title('adS')
  tenPercentAboveBelowLeftRight;

  subplot(427); hold on; 
  plot(T,squeeze(adatemp(ipt,ipt,:)));axis tight;grid;title('adatemp');
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adatemp(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')

  subplot(428); hold on; 
  plot(T,squeeze(adswdown(ipt,ipt,:)));axis tight;grid;title('adswdown');
  tenPercentAboveBelowLeftRight;
  nn = find(isnan(squeeze(adswdown(ipt,ipt,:))));  plot(T(nn),T(nn).*0,'rx')
  
%  cd(cdir)

  set(gcf,'Position',[350 120 600 1000])

  if (do_print)
  set(gcf,'PaperPosition',[0 0 8 12])
  print('-dpng','-r100',['AdjointAnalysis_' exp_name])
  ['printing ' exp_name]
  end

%cd(cdir)
