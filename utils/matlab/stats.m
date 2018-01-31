function [varargout] = stats(A)
% stats(A) writes the basic statistics of the A to the terminal which
% are i) the minimum finite value
%    ii) the maximum finite value
%   iii) the mean of the finite values
%    iv) the S.D. of the finite values ( RMS of [A-mean] )
%     v) the fraction of non-finite elements excluded from calculations
%
% e.g.
%  >> stats(topo)
% Min -4555     Max 0         Mean -2331.07  SD 1207.3441   N-Z 1024/1024
%
% [Min Max Mean SD]=stats(topo); returns the statistics in Min, Max,
% Mean and SD and does not write to the terminal.

A=A(:);

%ii=find(A~=0);
ii=find(isfinite(A));
if isempty(ii)
 ii=1;
end
sZ=prod(size(A));
minA=min(A(ii));
maxA=max(A(ii));
meanA=mean(A(ii));
sdA=sqrt( mean( (A(ii)-meanA).^2 ) );
rmsA=sqrt( mean( A(ii).^2 ) );
nZ=sum(~isfinite(A));
switch max(nargout)
 case {0}
% disp( ...
%  sprintf('Min %0.5g  Max %0.5g  Mean %0.5g  SD %0.5g   NaN %i/%i',...
%        minA,maxA,meanA,sdA,nZ,sZ) );
  disp( ...
   sprintf('Min %0.5g  Max %0.5g  Mean %0.5g  RMS %0.5g   NaN %i/%i',...
         minA,maxA,meanA,rmsA,nZ,sZ) );
 case {1}
  varargout(1)={minA};
 case {2}
  varargout(1)={minA};
  varargout(2)={maxA};
 case {3}
  varargout(1)={minA};
  varargout(2)={maxA};
  varargout(3)={meanA};
 case {4}
  varargout(1)={minA};
  varargout(2)={maxA};
  varargout(3)={meanA};
  varargout(4)={sdA};
 otherwise
  error('Too many return arguments requested')
end
