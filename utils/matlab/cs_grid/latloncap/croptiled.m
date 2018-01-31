function t = croptiled(t, cspec )
%
% Crop size of variables listed vlist in a tiled structure t.
%

d=1;
for c=1:length(cspec.crops)
 vlist=cspec.crops(c).fields;
 rlist=cspec.crops(c).ranks;
 ilo=cspec.crops(c).indexlo;
 ihi=cspec.crops(c).indexhi;
 for v=1:length(vlist)
  vn=vlist{v};
  % Figure out range to crop
  ranks=[1:ndims(getfield(t(1).var,vn))];
  rstr='(';
  for r=ranks
   [ri,ii]=intersect(rlist,r);
   if isempty( ri )
    rstr=sprintf('%s:,',rstr);
   else
    rstr=sprintf('%s%d:%d,',rstr,ilo(ii),ihi(ii));
   end
  end
  rstr(end)=')';
  if d > 0
   fprintf(1,'%s=%s%s\n',vn,vn,rstr);
  end
  % Do crop
  for tn=1:length(t)
   phi=getfield(t(tn).var,vn);
   estr=sprintf('phi=phi%s;',rstr);
   eval(estr);
   estr=sprintf('t(%d).var.%s=phi;',tn,vn);
   eval(estr);
  end
 end
end

end

