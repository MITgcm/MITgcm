clear all;

addpath(pwd)
cdir = pwd;

% the directory with netcdf output
fwdd = '/net/ross/raid1/ifenty/MITgcm_20130115/MITgcm/verification/1D_ocean_ice_column/temp/mnc_test_0002/'

% how many vertical cells out output
dep_ind = 5;

% the experiment name
exp_name = 'temp'

% boolean for writing the figure to a png file
do_print = 1 ;


ts_interval = 1*3600/2;

  cd(fwdd)

  gd = dir('grid.t001.nc');
  g  = nc_getall(gd(1).name);


  md = dir('Main_3D*');
  m = nc_getall(md(1).name);

  mT = m.T.data;
 
  interval_number = floor(mT./ts_interval);
 
  nTs(1) = 1;
  cur_interval = 1;
  inds_interval = find(interval_number == cur_interval);
  while(length(inds_interval) > 0)
    nTs(cur_interval+1) = inds_interval(1);
    cur_interval = cur_interval + 1;
    inds_interval = find(interval_number == cur_interval);
  end


  theta = permute(m.THETA.data,[4 3 2 1]);
  salt = permute(m.SALT.data,[4 3 2 1]);

  ipt=1;if(size(theta,1)==5);ipt=3;end;

  id = dir('ICE_avg*');
  if(length(id(1).name)>0);
    ice = nc_getall(id(1).name);
    iceT = ice.T.data;
    area = permute(ice.SIarea.data,[3 2 1]);
    heff = permute(ice.SIheff.data,[3 2 1]);
  end;

  kppd = dir('KPP*');
  if(size(kppd,1)>0);
    kpp = nc_getall(kppd(1).name);
    kppdiffs = permute(kpp.KPPdiffS.data,[4 3 2 1]);
  end;
  
  sp2d = dir('Splume2_*');
  if(size(sp2d,1)>0);
    sp2 = nc_getall(sp2d(1).name);
    plumekb = permute(sp2.PLUMEKB.data, [ 4 3 2 1]);
  end;

  spd = dir('Splume_*');
  if(size(spd,1)>0);
    sp = nc_getall(spd(1).name);
    oceSPDep = permute(sp.oceSPDep.data,[3 2 1]);
    oceSPflx = permute(sp.oceSPflx.data,[3 2 1]);
  end; 

  figure(1);clf;
  subplot(421); hold on;
  plot(iceT,squeeze(area(ipt,ipt,:)),'k');grid;title('area');
  tenPercentAboveBelowLeftRight;

  subplot(422); hold on;
  plot(iceT,squeeze(heff(ipt,ipt,:)),'k');grid;title('heff');
  tenPercentAboveBelowLeftRight;

  subplot(423);hold on;
  plot(mT,squeeze(theta(ipt,ipt,1,:)),'k');;grid;title('T(z=1,t)');
  tenPercentAboveBelowLeftRight;

  subplot(424);hold on;
  plot(mT,squeeze(salt(ipt,ipt,1,:)),'k');;grid;title('S(z=1,t)');
  tenPercentAboveBelowLeftRight;

  if(size(spd,1)>0);
  subplot(427);hold on;
  plot(mT,squeeze(oceSPDep(ipt,ipt,:)),'k');grid;title('oceSPDep');
  tenPercentAboveBelowLeftRight;

  subplot(428);hold on;
  plot(mT,squeeze(oceSPflx(ipt,ipt,:)),'k');grid;title('oceSPflx');
  tenPercentAboveBelowLeftRight;
  end;

  if(size(sp2d,1)>0);
  subplot(425);hold on;
  plot(-g.Z.data(1:dep_ind),squeeze(plumekb(ipt,ipt,1:dep_ind,nTs)),'.-');;grid;title('PlumeKB(z)');
  tenPercentAboveBelowLeftRight;

  subplot(426);hold on;
  plot(-g.Z.data(1:dep_ind),squeeze(kppdiffs(ipt,ipt,1:dep_ind,nTs)),'.-');;grid;title('kpp diff S(Z)');
  tenPercentAboveBelowLeftRight;
  end;
  cd(cdir);

  h=legend('t1','t2','t3', 't4','t5','t6','Location','NorthEast');
  set(h,'FontSize',6)


  set(gcf,'Position',[350 120 600 1000]);

cd(fwdd)
  if (do_print)
  set(gcf,'PaperPosition',[0 0 8 12]);
  print('-dpng','-r100',['ForwardAnalysis_' exp_name]);
  ['printing ' exp_name]
  end

