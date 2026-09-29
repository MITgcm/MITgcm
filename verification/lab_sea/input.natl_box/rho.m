function x=rho(S,T,P);
% RHO    density of seawater (kg/m^3)
%
%        x=rho(S,T,P)
%
%        given the salinity S (ppt), in situ temperature T (deg C)
%        and pressure P (dbars).
%
%        Note: D. Menemenlis (MIT 18 jul 94)
%        This routine is a leaner version of SWSTATE for large matrices,
%        and the user can mix and match scalar, vector, and matrix inputs.
%
%        See also SWSTATE

% check that input arguments are same size, etc.
if length(size(S))>2 | length(size(T))>2 | length(size(P))>2 
  if length(size(S)) ~= length(size(T)) | length(size(S)) ~= length(size(P))
    error('for 3+ dimensioned arrays, S, T, and P must have same dimension')
  end
  if any(size(S)~=size(T)) | any(size(S)~=size(P))
    error('for 3+ dimensioned arrays, S, T, and P must have same size')
  end
else
  [NS,MS]=size(S); [NT,MT]=size(T);
  if ((NS==1 & MS==1) & (NT ~=1 | MT ~=1) ),
    S=S*ones(NT,MT);
    [NS,MS]=size(S);
  elseif ((NT==1 & MT==1) & (NS ~=1 | MS ~=1) ),
    T=T*ones(NS,MS);
    [NT,MT]=size(T);
  end
  
  if (NT+MT) > (NS+MS)
    if (MS==1), S=S*ones(1,MT); 
    elseif (NS==1), S=S'*ones(1,MT);
    end
  elseif (NT+MT) < (NS+MS)
    if (MT==1), T=T*ones(1,MS); 
    elseif (NT==1), T=T'*ones(1,MS);
    end
    [NT,MT]=size(T);
  end
  
  [NP,MP]=size(P);
  if max(NP,MP)>1
    if (MP==1), P=P*ones(1,MT); 
    elseif (NP==1), P=P'*ones(1,MT);
    end
  end
end
  
% CONVERT PRESSURE TO BARS AND TAKE SQUARE ROOT SALINITY.
      P=P/10.;
      SR = sqrt(abs(S));

%  INTERNATIONAL ONE-ATMOSPHERE EQUATION OF STATE OF SEAWATER
      x = (4.8314E-4 * S + ...
          ((-1.6546E-6*T+1.0227E-4).*T-5.72466E-3) .* SR + ...
          (((5.3875E-9*T-8.2467E-7).*T+7.6438E-5).*T-4.0899E-3).*T+8.24493E-1) ...
          .*S + ((((6.536332E-9*T-1.120083E-6).*T+1.001685E-4).*T ...
                        -9.095290E-3).*T+6.793952E-2).*T-28.263737;

% SPECIFIC VOLUME AT ATMOSPHERIC PRESSURE
      V350P = 1.0/1028.1063;
      x = -x*V350P./(1028.1063+x);

% COMPUTE COMPRESSION TERMS
      SR = ((((9.1697E-10*T+2.0816E-8).*T-9.9348E-7) .* S + ...
          (5.2787E-8*T-6.12293E-6).*T+3.47718E-5) .*P + ...
          (1.91075E-4 * SR + (-1.6078E-6*T-1.0981E-5).*T+2.2838E-3) .* ...
          S + ((-5.77905E-7*T+1.16092E-4).*T+1.43713E-3).*T-0.1194975) ...
          .*P + (((-5.3009E-4*T+1.6483E-2).*T+7.944E-2) .* SR + ...
          ((-6.1670E-5*T+1.09987E-2).*T-0.603459).*T+54.6746) .* S + ...
          (((-5.155288E-5*T+1.360477E-2).*T-2.327105).*T+148.4206).*T-1930.06;
       
% EVALUATE PRESSURE POLYNOMIAL
      B  = (5.03217E-5*P+3.359406).*P+21582.27;
      x = x.*(1.0 - P./B) + (V350P+x).*P.*SR./(B.*(B+SR));
      SR = V350P.*(1.0 - P./B);
      x= 1028.106331 + P./B./SR - x ./ (SR.*(SR+x));
