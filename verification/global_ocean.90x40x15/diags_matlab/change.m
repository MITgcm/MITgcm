function new = change(old,relation,flag,value)

% CHANGE     Change values in a matrix
%--------------------------------------------------------------------
% CHANGE   1.3   92/03/25
%
% new = change(old,relation,flag,value)
%
% DESCRIPTION:
%    Changes the 'flag' values in the matrix "old" to the new "value"
%    according to the "relation".
% 
% INPUT:
%    old      = matrix containing values related to "flag"
%               are to be converted to "value"
%    flag     = values related to "flag" then replaced by "value"
%    value    = replacement value
%    relation = string relation e.g. '<', '>', '=='
%
% OUTPUT:
%    new      = matrix "old" with all flagged values changed
%               to "value" (can be returned to same matrix "old")
%
% EXAMPLE:  A = change(A,'==',NaN,0  )
%           B = change(A,'<', -99,NaN) 
%
% CALLER:   general purpose
% CALLEE:   none
%
% AUTHOR:   Phil Morgan 3-02-92

% @(#)change.m   1.3   92/03/25
% @(#)change.m   Martin Losch 02/03/08, 
%                fixed bug that couldn't handle ~= NaN
% 
% Re-created after 2-2-92 hard disk crash.
% Based on flagnan.m - Steve Rintoul  Dec 90
%          alter.m   - Phil  Morgan    Feb 91
%          convert.m - Peter Mcintosh Aug 91
%--------------------------------------------------------------------

% CHECK INPUT ARGUMENTS CALL
if nargin ~= 4
  error('CHANGE.M: Must have 4 input arguments')
end

if (strcmp(relation,'==') | strcmp(relation,'>') | strcmp(relation,'<') | ...
		      strcmp(relation,'~=') | strcmp(relation,'>=') | ...
		      strcmp(relation,'<=')) 
    % valid relation
  else
    error(['CHANGE.M: Relation ''' relation ''' not valid'])
end

% BODY
if isnan(flag)
  if strcmp(relation,'==')
    replace = find(isnan(old));
  elseif strcmp(relation,'~=')
    replace = find(~isnan(old));
  else
    error(['CHANGE.M: Relation ''' relation ''' not valid for flag NaN'])
  end
else
   eval(['replace = find(old',relation,'flag);']);
end
nreplace = length(replace);
new      = old;
if nreplace>0
   new(replace) = value*ones(1,nreplace);
end %if
%--------------------------------------------------------------------


