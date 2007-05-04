% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/latloncap/Attic/plot_mesh.m,v 1.1 2007/05/04 15:16:34 jmc Exp $
% $Name:  $

% Load six facet mesh *.mitgrid files
fn_in='llc90x90x360_lonshift';
fg=load_mitgrid_ct('.',fn_in,90,90,360);
fn_out='llc90x90x360_lonshift_correctedgevals';

% Now fix up facet edges to be correct mesh lengths
mg=fg;
a6=0.;
% o east-west face lhs
% dxV
mg(1).dxV(  1,1:end)=fg(1).dxV(  1,1:end)+fg(5).dyU(end:-1:1,  end)';
mg(1).dxV(end,1:end)=fg(1).dxV(end,1:end)+fg(2).dxV(       1,1:end) ;
mg(2).dxV(  1,1:end)=fg(2).dxV(  1,1:end)+fg(1).dxV(     end,1:end) ;
mg(2).dxV(end,1:end)=fg(2).dxV(end,1:end)+fg(4).dyU(end:-1:1,    1)';
mg(3).dxV(  1,1:end)=fg(3).dxV(  1,1:end)+fg(1).dyU(end:-1:1,    1)';
mg(3).dxV(end,1:end)=fg(3).dxV(end,1:end)+fg(4).dxV(     end,1:end) ;
mg(4).dxV(  1,1:end)=fg(4).dxV(  1,1:end)+fg(3).dxV(     end,1:end) ;
mg(4).dxV(end,1:end)=fg(4).dxV(end,1:end)+fg(6).dyU(end:-1:1,    1)'*a6;
mg(5).dxV(  1,1:end)=fg(5).dxV(  1,1:end)+fg(3).dyU(end:-1:1,  end)';
mg(5).dxV(end,1:end)=fg(5).dxV(end,1:end)+fg(6).dxV(       1,1:end)*a6 ;
mg(6).dxV(  1,1:end)=fg(6).dxV(  1,1:end)+fg(5).dxV(     end,1:end) ;
mg(6).dxV(end,1:end)=fg(6).dxV(end,1:end)+fg(2).dyU(end:-1:1,    1)';
% dxC
mg(1).dxC(  1,1:end-1)=fg(1).dxC(  1,1:end-1)+fg(5).dyC(end-1:-1:1,  end)';
mg(1).dxC(end,1:end-1)=fg(1).dxC(end,1:end-1)+fg(2).dxC(       1,1:end-1) ;
mg(2).dxC(  1,1:end-1)=fg(2).dxC(  1,1:end-1)+fg(1).dxC(     end,1:end-1) ;
mg(2).dxC(end,1:end-1)=fg(2).dxC(end,1:end-1)+fg(4).dyC(end-1:-1:1,    1)';
mg(3).dxC(  1,1:end-1)=fg(3).dxC(  1,1:end-1)+fg(1).dyC(end-1:-1:1,    1)';
mg(3).dxC(end,1:end-1)=fg(3).dxC(end,1:end-1)+fg(4).dxC(     end,1:end-1) ;
mg(4).dxC(  1,1:end-1)=fg(4).dxC(  1,1:end-1)+fg(3).dxC(     end,1:end-1) ;
mg(4).dxC(end,1:end-1)=fg(4).dxC(end,1:end-1)+fg(6).dyC(end-1:-1:1,    1)'*a6;
mg(5).dxC(  1,1:end-1)=fg(5).dxC(  1,1:end-1)+fg(3).dyC(end-1:-1:1,  end)';
mg(5).dxC(end,1:end-1)=fg(5).dxC(end,1:end-1)+fg(6).dxC(       1,1:end-1)*a6 ;
mg(6).dxC(  1,1:end-1)=fg(6).dxC(  1,1:end-1)+fg(5).dxC(     end,1:end-1) ;
mg(6).dxC(end,1:end-1)=fg(6).dxC(end,1:end-1)+fg(2).dyC(end-1:-1:1,    1)';
% rAw
mg(1).rAw(  1,1:end-1)=fg(1).rAw(  1,1:end-1)+fg(5).rAs(end-1:-1:1,  end)';
mg(1).rAw(end,1:end-1)=fg(1).rAw(end,1:end-1)+fg(2).rAw(       1,1:end-1) ;
mg(2).rAw(  1,1:end-1)=fg(2).rAw(  1,1:end-1)+fg(1).rAw(     end,1:end-1) ;
mg(2).rAw(end,1:end-1)=fg(2).rAw(end,1:end-1)+fg(4).rAs(end-1:-1:1,    1)';
mg(3).rAw(  1,1:end-1)=fg(3).rAw(  1,1:end-1)+fg(1).rAs(end-1:-1:1,    1)';
mg(3).rAw(end,1:end-1)=fg(3).rAw(end,1:end-1)+fg(4).rAw(     end,1:end-1) ;
mg(4).rAw(  1,1:end-1)=fg(4).rAw(  1,1:end-1)+fg(3).rAw(     end,1:end-1) ;
mg(4).rAw(end,1:end-1)=fg(4).rAw(end,1:end-1)+fg(6).rAs(end-1:-1:1,    1)'*a6;
mg(5).rAw(  1,1:end-1)=fg(5).rAw(  1,1:end-1)+fg(3).rAs(end-1:-1:1,  end)';
mg(5).rAw(end,1:end-1)=fg(5).rAw(end,1:end-1)+fg(6).rAw(       1,1:end-1)*a6 ;
mg(6).rAw(  1,1:end-1)=fg(6).rAw(  1,1:end-1)+fg(5).rAw(     end,1:end-1) ;
mg(6).rAw(end,1:end-1)=fg(6).rAw(end,1:end-1)+fg(2).rAs(end-1:-1:1,    1)';
% rAz (west-east faces only, not corners or north-south faces)
mg(1).rAz(  1,1:end)=fg(1).rAz(  1,1:end)+fg(5).rAz(end:-1:1,  end)';
mg(1).rAz(end,1:end)=fg(1).rAz(end,1:end)+fg(2).rAz(       1,1:end) ;
mg(2).rAz(  1,1:end)=fg(2).rAz(  1,1:end)+fg(1).rAz(     end,1:end) ;
mg(2).rAz(end,1:end)=fg(2).rAz(end,1:end)+fg(4).rAz(end:-1:1,    1)';
mg(3).rAz(  1,1:end)=fg(3).rAz(  1,1:end)+fg(1).rAz(end:-1:1,    1)';
mg(3).rAz(end,1:end)=fg(3).rAz(end,1:end)+fg(4).rAz(     end,1:end) ;
mg(4).rAz(  1,1:end)=fg(4).rAz(  1,1:end)+fg(3).rAz(     end,1:end) ;
mg(4).rAz(end,1:end)=fg(4).rAz(end,1:end)+fg(6).rAz(end:-1:1,    1)'*a6;
mg(5).rAz(  1,1:end)=fg(5).rAz(  1,1:end)+fg(3).rAz(end:-1:1,  end)';
mg(5).rAz(end,1:end)=fg(5).rAz(end,1:end)+fg(6).rAz(       1,1:end)*a6 ;
mg(6).rAz(  1,1:end)=fg(6).rAz(  1,1:end)+fg(5).rAz(     end,1:end) ;
mg(6).rAz(end,1:end)=fg(6).rAz(end,1:end)+fg(2).rAz(end:-1:1,    1)';
% o north-south face lhs
% dyU
mg(1).dyU(1:end,end)=fg(1).dyU(1:end,end)+fg(3).dxV(    1,end:-1:1)';
mg(1).dyU(1:end,  1)=fg(1).dyU(1:end,  1)+fg(6).dyU(1:end,     end)*a6;
mg(2).dyU(1:end,end)=fg(2).dyU(1:end,end)+fg(3).dyU(1:end,       1);
mg(2).dyU(1:end,  1)=fg(2).dyU(1:end,  1)+fg(6).dxV(  end,end:-1:1)'*a6;
mg(3).dyU(1:end,end)=fg(3).dyU(1:end,end)+fg(5).dxV(    1,end:-1:1)';
mg(3).dyU(1:end,  1)=fg(3).dyU(1:end,  1)+fg(2).dyU(1:end,     end);
mg(4).dyU(1:end,end)=fg(4).dyU(1:end,end)+fg(5).dyU(1:end,       1);
mg(4).dyU(1:end,  1)=fg(4).dyU(1:end,  1)+fg(2).dxV(  end,end:-1:1)';
mg(5).dyU(1:end,end)=fg(5).dyU(1:end,end)+fg(1).dxV(    1,end:-1:1)';
mg(5).dyU(1:end,  1)=fg(5).dyU(1:end,  1)+fg(4).dyU(1:end,     end);
mg(6).dyU(1:end,end)=fg(6).dyU(1:end,end)+fg(1).dyU(1:end,       1);
mg(6).dyU(1:end,  1)=fg(6).dyU(1:end,  1)+fg(4).dxV(  end,end:-1:1)';
% dyC
mg(1).dyC(1:end-1,end)=fg(1).dyC(1:end-1,end)+fg(3).dxC(    1,end-1:-1:1)';
mg(1).dyC(1:end-1,  1)=fg(1).dyC(1:end-1,  1)+fg(6).dyC(1:end-1,     end)*a6;
mg(2).dyC(1:end-1,end)=fg(2).dyC(1:end-1,end)+fg(3).dyC(1:end-1,       1);
mg(2).dyC(1:end-1,  1)=fg(2).dyC(1:end-1,  1)+fg(6).dxC(  end,end-1:-1:1)'*a6;
mg(3).dyC(1:end-1,end)=fg(3).dyC(1:end-1,end)+fg(5).dxC(    1,end-1:-1:1)';
mg(3).dyC(1:end-1,  1)=fg(3).dyC(1:end-1,  1)+fg(2).dyC(1:end-1,     end);
mg(4).dyC(1:end-1,end)=fg(4).dyC(1:end-1,end)+fg(5).dyC(1:end-1,       1);
mg(4).dyC(1:end-1,  1)=fg(4).dyC(1:end-1,  1)+fg(2).dxC(  end,end-1:-1:1)';
mg(5).dyC(1:end-1,end)=fg(5).dyC(1:end-1,end)+fg(1).dxC(    1,end-1:-1:1)';
mg(5).dyC(1:end-1,  1)=fg(5).dyC(1:end-1,  1)+fg(4).dyC(1:end-1,     end);
mg(6).dyC(1:end-1,end)=fg(6).dyC(1:end-1,end)+fg(1).dyC(1:end-1,       1);
mg(6).dyC(1:end-1,  1)=fg(6).dyC(1:end-1,  1)+fg(4).dxC(  end,end-1:-1:1)';
% rAs
mg(1).rAs(1:end-1,end)=fg(1).rAs(1:end-1,end)+fg(3).rAw(    1,end-1:-1:1)';
mg(1).rAs(1:end-1,  1)=fg(1).rAs(1:end-1,  1)+fg(6).rAs(1:end-1,     end)*a6;
mg(2).rAs(1:end-1,end)=fg(2).rAs(1:end-1,end)+fg(3).rAs(1:end-1,       1);
mg(2).rAs(1:end-1,  1)=fg(2).rAs(1:end-1,  1)+fg(6).rAw(  end,end-1:-1:1)'*a6;
mg(3).rAs(1:end-1,end)=fg(3).rAs(1:end-1,end)+fg(5).rAw(    1,end-1:-1:1)';
mg(3).rAs(1:end-1,  1)=fg(3).rAs(1:end-1,  1)+fg(2).rAs(1:end-1,     end);
mg(4).rAs(1:end-1,end)=fg(4).rAs(1:end-1,end)+fg(5).rAs(1:end-1,       1);
mg(4).rAs(1:end-1,  1)=fg(4).rAs(1:end-1,  1)+fg(2).rAw(  end,end-1:-1:1)';
mg(5).rAs(1:end-1,end)=fg(5).rAs(1:end-1,end)+fg(1).rAw(    1,end-1:-1:1)';
mg(5).rAs(1:end-1,  1)=fg(5).rAs(1:end-1,  1)+fg(4).rAs(1:end-1,     end);
mg(6).rAs(1:end-1,end)=fg(6).rAs(1:end-1,end)+fg(1).rAs(1:end-1,       1);
mg(6).rAs(1:end-1,  1)=fg(6).rAs(1:end-1,  1)+fg(4).rAw(  end,end-1:-1:1)';
% rAz
mg(1).rAz(1:end,end)=fg(1).rAz(1:end,end)+fg(3).rAz(    1,end:-1:1)';
mg(1).rAz(1:end,  1)=fg(1).rAz(1:end,  1)+fg(6).rAz(1:end,     end)*a6;
mg(2).rAz(1:end,end)=fg(2).rAz(1:end,end)+fg(3).rAz(1:end,       1);
mg(2).rAz(1:end,  1)=fg(2).rAz(1:end,  1)+fg(6).rAz(  end,end:-1:1)'*a6;
mg(3).rAz(1:end,end)=fg(3).rAz(1:end,end)+fg(5).rAz(    1,end:-1:1)';
mg(3).rAz(1:end,  1)=fg(3).rAz(1:end,  1)+fg(2).rAz(1:end,     end);
mg(4).rAz(1:end,end)=fg(4).rAz(1:end,end)+fg(5).rAz(1:end,       1);
mg(4).rAz(1:end,  1)=fg(4).rAz(1:end,  1)+fg(2).rAz(  end,end:-1:1)';
mg(5).rAz(1:end,end)=fg(5).rAz(1:end,end)+fg(1).rAz(    1,end:-1:1)';
mg(5).rAz(1:end,  1)=fg(5).rAz(1:end,  1)+fg(4).rAz(1:end,     end);
mg(6).rAz(1:end,end)=fg(6).rAz(1:end,end)+fg(1).rAz(1:end,       1);
mg(6).rAz(1:end,  1)=fg(6).rAz(1:end,  1)+fg(4).rAz(  end,end:-1:1)';
% o corner lhs
mg(1).rAz(1  ,  1)=fg(1).rAz(1  ,  1)+fg(5).rAz(end,end)+fg(6).rAz(  1,end)*a6;
mg(1).rAz(end,  1)=fg(1).rAz(end,  1)+fg(2).rAz(  1,  1)+fg(6).rAz(end,end)*a6;
mg(1).rAz(1  ,end)=fg(1).rAz(1  ,end)+fg(3).rAz(  1,end)+fg(5).rAz(  1,end);
mg(1).rAz(end,end)=fg(1).rAz(end,end)+fg(2).rAz(  1,end)+fg(3).rAz(  1,  1);
mg(2).rAz(1  ,  1)=fg(2).rAz(1  ,  1)+fg(1).rAz(  1,end)+fg(6).rAz(end,end)*a6;
mg(2).rAz(end,  1)=fg(2).rAz(end,  1)+fg(4).rAz(end,  1)+fg(6).rAz(end,  1)*a6;
mg(2).rAz(1  ,end)=fg(2).rAz(1  ,end)+fg(1).rAz(end,end)+fg(3).rAz(  1,  1);
mg(2).rAz(end,end)=fg(2).rAz(end,end)+fg(3).rAz(end,  1)+fg(4).rAz(  1,  1);
mg(3).rAz(1  ,  1)=fg(3).rAz(1  ,  1)+fg(1).rAz(end,end)+fg(2).rAz(  1,end);
mg(3).rAz(end,  1)=fg(3).rAz(end,  1)+fg(2).rAz(end,end)+fg(4).rAz(  1,  1);
mg(3).rAz(1  ,end)=fg(3).rAz(1  ,end)+fg(1).rAz(  1,end)+fg(5).rAz(  1,end);
mg(3).rAz(end,end)=fg(3).rAz(end,end)+fg(4).rAz(  1,end)+fg(5).rAz(end,end);
mg(4).rAz(1  ,  1)=fg(4).rAz(1  ,  1)+fg(2).rAz(end,end)+fg(3).rAz(end,  1);
mg(4).rAz(end,  1)=fg(4).rAz(end,  1)+fg(2).rAz(end,  1)+fg(6).rAz(end,  1)*a6;
mg(4).rAz(1  ,end)=fg(4).rAz(1  ,end)+fg(3).rAz(end,end)+fg(5).rAz(  1,  1);
mg(4).rAz(end,end)=fg(4).rAz(end,end)+fg(5).rAz(end,  1)+fg(6).rAz(  1,  1)*a6;
mg(5).rAz(1  ,  1)=fg(5).rAz(1  ,  1)+fg(3).rAz(end,end)+fg(4).rAz(  1,end);
mg(5).rAz(end,  1)=fg(5).rAz(end,  1)+fg(4).rAz(end,end)+fg(6).rAz(  1,  1);
mg(5).rAz(1  ,end)=fg(5).rAz(1  ,end)+fg(1).rAz(  1,end)+fg(3).rAz(  1,end);
mg(5).rAz(end,end)=fg(5).rAz(end,end)+fg(1).rAz(  1,  1)+fg(6).rAz(  1,end)*a6;
mg(6).rAz(1  ,  1)=fg(6).rAz(1  ,  1)+fg(4).rAz(end,end)+fg(5).rAz(  1,end);
mg(6).rAz(end,  1)=fg(6).rAz(end,  1)+fg(2).rAz(end,  1)+fg(4).rAz(end,  1);
mg(6).rAz(1  ,end)=fg(6).rAz(1  ,end)+fg(1).rAz(  1,  1)+fg(5).rAz(end,end);
mg(6).rAz(end,end)=fg(6).rAz(end,end)+fg(1).rAz(end,  1)+fg(2).rAz(  1,  1);

% mg=facets_to_mesh(fg);

clf
spnum=[9 10 6 7 3 4 1];
estr='local';
for i=1:6
 subplot(3,4,spnum(i));imagesc(mg(i).xG');axis xy;colorbar
 label_edges(i,mg,estr);
end

% Set xg and yg on edges of face 6 to their counterparts on faces
% 5, 4, 2, 1.
% This avoids the halo exchange causing problems by making xg and yg wrong in
% 5 and 4 when they are output in mnc.
mg(6).xG(  1,  :)=mg(5).xG(     end,       :);
mg(6).xG(  :,  1)=mg(4).xG(     end,end:-1:1);
mg(6).xG(end,  :)=mg(2).xG(end:-1:1,       1);
mg(6).xG(  :,end)=mg(1).xG(       :,       1);
mg(6).yG(  1,  :)=mg(5).yG(     end,       :);
mg(6).yG(  :,  1)=mg(4).yG(     end,end:-1:1);
mg(6).yG(end,  :)=mg(2).yG(end:-1:1,       1);
mg(6).yG(  :,end)=mg(1).yG(       :,       1);

% Now write out new mesh files with correct values at mesh points along facet edge
% (I hope).
for i=1:6
 fn=sprintf('%s_%d.mitgrid',fn_out,i);
 fid=fopen(fn,'w','ieee-be');
 fwrite(fid,mg(i).xC,'float64');
 fwrite(fid,mg(i).yC,'float64');
 fwrite(fid,mg(i).dxF,'float64');
 fwrite(fid,mg(i).dyF,'float64');
 fwrite(fid,mg(i).rA,'float64');
 fwrite(fid,mg(i).xG,'float64');
 fwrite(fid,mg(i).yG,'float64');
 fwrite(fid,mg(i).dxV,'float64');
 fwrite(fid,mg(i).dyU,'float64');
 fwrite(fid,mg(i).rAz,'float64');
 fwrite(fid,mg(i).dxC,'float64');
 fwrite(fid,mg(i).dyC,'float64');
 fwrite(fid,mg(i).rAw,'float64');
 fwrite(fid,mg(i).rAs,'float64');
 fwrite(fid,mg(i).dxG,'float64');
 fwrite(fid,mg(i).dyG,'float64');
 fwrite(fid,mg(i).AngleCS,'float64');
 fwrite(fid,mg(i).AngleSN,'float64');
 fclose(fid);
end


