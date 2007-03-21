function [divlist]=exch2_divider( arglist );
% [divlist]=exch2_divider( arglist );
% return list of prime divider common to all arglist number
 
% $Header: /u/gcmpack/MITgcm/utils/exch2/matlab-topology-generator/Attic/exch2_divider.m,v 1.1 2007/03/21 02:02:12 jmc Exp $
% $Name:  $

divlist=[];
np=length(arglist);
flag=1;
for i=1:np,
 flaf=flag & arglist(i)==round(arglist(i));
end
if ~flag,
 fprintf(' ERROR: not integer number in argument list of exch2_divider !\n',p);
 divlist=0;
 return
end
p=min(arglist(:));
if p < 2,
 fprintf(' ERROR: too small number (%i) in argument list of exch2_divider !\n',p);
 divlist=1;
 return
end

pp=arglist;
nq=0; q=2;
while q <= p,
  xx=pp/q; yy=abs(xx-round(xx));
  if max(yy(:)) == 0,
    nq=nq+1; divlist(nq)=q; p=p/q; pp=xx;
  else
    q=q+1;
  end
end
return
%fprintf(' arg-list:'); fprintf(' %i',arglist); 
%fprintf(' ; res:'); fprintf(' %i',pp); fprintf('\n');
fprintf(' Greater Div: %i ; list:',prod(divlist));
fprintf(' %i,',divlist); fprintf('\n');
