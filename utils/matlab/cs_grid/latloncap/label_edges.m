function []=label_edges(nf,mg,r_or_l);

% $Header: /u/gcmpack/MITgcm/utils/matlab/cs_grid/latloncap/Attic/label_edges.m,v 1.1 2007/05/04 15:19:53 jmc Exp $
% $Name:  $

if strcmp(r_or_l,'local')
 for i=1:4
  ls=mg(nf).adj(i).localStart;
  le=mg(nf).adj(i).localEnd;
  xmid=ls(1)+[(le(1)-ls(1)+1)/2];
  ymid=ls(2)+[(le(2)-ls(2)+1)/2];
  ts=sprintf('%d:%d',nf,i);
  text(xmid,ymid,ts);
 end
end

if strcmp(r_or_l,'remote')
 for i=1:length(mg)
  for a=1:length(mg(i).adj)
   if mg(i).adj(a).remoteFacet == nf
    ls=mg(i).adj(a).remoteStart;
    le=mg(i).adj(a).remoteEnd;
    xmid=ls(1)+[(le(1)-ls(1)+1)/2];
    ymid=ls(2)+[(le(2)-ls(2)+1)/2];
    ts=sprintf('%d:%d',i,a);
    text(xmid,ymid,ts);
    fprintf(1,'(%d,%d) to (%d,%d) ==> (%d,%d) %s\n',...
      ls(1),ls(2),...
      le(1),le(2),...
      xmid,ymid,ts);
   end
  end
 end
end

return
