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

   % Definir funcion factorial
   fun {Fact N}
      if N == 0 then 1 else N*{Fact N-1} end
   end

   % Definir funcion que genera la lista
   fun {GenerateList N1}
      if N1 > N then nil else {Fact N1} | {GenerateList N1+1} end
   end
in
   % Parametro de llamada/inicio de la funcion
   {GenerateList 1}
end

{Browse {FactList 4}}

% for(i in 1:length(x))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare
fun {Sum N}
   if N == 0 then 0
   else N+{Sum N-1}
   end
end

{Browse {Sum 10}}

%%%%%%%

declare
fun {Suma N}
   fun {Iterador S Isdone Transform}
      if {Isdone S.2} then S
      else S1 = {Transform S.1}
      in
	 {Iterador r(S1 S.2+1) Isdone Transform}
      end
   end
in
   {Iterador r(0 1)%0 1
    fun {$ N1} N1 > N end
    fun {$ S} S+1 end
   }
end


declare
fun {Suma N}
   fun {Iterador S N1 Isdone Transform}
      if {Isdone N1} then S
      else S1 in
	 S1 = {Transform S N1}
	 {Iterador S1 N1+1 Isdone Transform}
      end
   end
in
   {Iterador 0 1
    fun {$ N1} N1 > N end
    fun {$ S N1} S+N1 end
   }
end

{Browse {Suma 5}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare
proc {ForAllTail Xs P ?Z}
   case Xs of nil then {P Xs}
   [] X|Xr then {P Xs} {ForAllTail Xr P Z}
   end
end

{Browse {ForAllTail [1 2 3 4] Browse}}

%%%%

declare
proc {ForAllTail Xs P ?Z}
   if Xs.2 ==  nil then {P Xs}
   else {P Xs} {ForAllTail Xs.2 P Z}
   end
end

{Browse {ForAllTail [1 2 3 4] Browse}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Punto 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declare
A = [1 2 3 4]
B = [4 3 2 1]

{Browse A.1#B.1}

declare
proc {Convolucion X Y ?Z}
   Zini Zfin
   proc {GenerateList Xl ?Zh ?Zt ?R}
      case Xl of nil then
	 case Y of nil then R=nil
	    [] Y1|Yt then Zh = Y1 Zt = Yt R = nil end
      [] Xh|Xt then
	 local VyHtemp VyTtemp in
	    R = Xh#VyHtemp|{GenerateList Xt ?VyHtemp ?VyTtemp}
	    if VyTtemp == nil then skip
	       else
	    Zh = VyTtemp.1
	       Zt = VyTtemp.2
	       end
	 end
      end
   end
in
   {GenerateList X Zini Zfin Z}
end

{Browse {Convolucion [1 2 3 4] [a b c d]}}