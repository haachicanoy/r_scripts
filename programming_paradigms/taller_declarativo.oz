local X in
   X = 1
   local X in
      X = 2
      {Browse X}
   end
end

local Max C in
   Max = proc {$ X Y Z}
	    if (X >= Y) then Z = X else Z = Y end
	 end
   {Max 3 5 C}
end

declare
fun {Max X Y}
   if X >= Y then X  else Y
   end
end

{Browse {Max 4 19}}

declare C
declare
proc {Max X Y ?Z}
   if X >= Y then Z = X else Z = Y
   end
end

{Browse {Max 1 2}}

{Max 1 2 C}
{Browse C}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Max function without else
declare
fun {Max X Y}
   if X >= Y then X
   end
end

{Browse {Max 1 2}}

% Max function translated to kernel language without else
declare Max in
proc {Max X Y Result1}
   local IfArbiter1 in
      IfArbiter1 = X >= Y
      if IfArbiter1 then
	 X = Result1
      end
   end
end


% Max procedure without else
declare
proc {Max X Y ?Z}
   if X >= Y then Z = X
   end
end

{Browse {Max 1 2}}

% Max procedure translated to kernel language without else
declare Max in
proc {Max X Y Z}
   local IfArbiter1 in
      IfArbiter1 = X >= Y
      if IfArbiter1 then
	 X = Z
      else
	 skip skip
      end
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare
fun {Max L}
   fun {MaxLoop L M}
      case L of nil then M
      [] H|T then
	 if M > H then {MaxLoop T M}
	 else {MaxLoop T H} end
      end
   end
in
   if L == nil then error
   else {MaxLoop L.2 L.1} end
end

% Function expressed in procedural notation
declare
proc {Max L ?ResMax}
   proc {MaxLoop L M ?ResMaxLoop}
      case L of nil then ResMaxLoop = M
      [] H|T then
	 if M > H then ResMaxLoop = {MaxLoop T M}
	 else
	    {MaxLoop T H ResMaxLoop} end
      end
   end
in
   if L == nil then ResMax = error
   else {MaxLoop L.2 L.1 ResMax} end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare
fun {FactList N}
   if N == 1 then [1]
   else [N*(N-1)]
   end
end

{Browse {FactList 3}}