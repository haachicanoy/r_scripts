%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERAL CONCEPTS

%%%%% Simple calcs

{Browse 9999*999}
{Browse 999*999}

%%%%% Declarative variables

declare
V = 9999*9999
{Browse V*V}
V = 5

%%%%% Functions

% Factorial function
declare
fun {Fact N}
   if N==0 then 1 else N*{Fact N-1} end
end
{Browse Fact}
{Browse {Fact 5}}
{Browse {Fact 10}}
{Browse {Fact 100}}

% Combinatorial function
declare
fun {Comb N K}
   {Fact N} div ({Fact K}*{Fact N-K})
end
{Browse Comb}
{Browse {Comb 10 5}}

%%%%% Lists

% Basics

declare
H = 5
T = [6 7 8]

{Browse H|T}
{Browse 5|6|7|nil}

declare
L = [5 6 7 8]
{Browse L}

{Browse L.1}
{Browse L.2}

% Pattern recognition
declare
L = [5 6 7 8]
case L of H|T then {Browse H} {Browse T} end

% Create a Pascal's triangle function
declare Pascal SumListas DesplIzq DesplDer
fun {Pascal N}
   if N == 1 then [1]
   else
      {SumListas
       {DesplIzq {Pascal N-1}}
       {DesplDer {Pascal N-1}}}
   end
end

% Declare auxiliar functions
fun {DesplIzq L}
   case L of H|T then H|{DesplIzq T}
   else [0] end
end

fun {DesplDer L} 0|L end

fun {SumListas L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
	 H1 + H2|{SumListas T1 T2}
      end
   else nil end
end

{Browse {Pascal 10}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SPECIFIC CONCEPTS

%%%%% Lazy evaluation
declare
fun lazy {Ents N}
   N|{Ents N+1}
end

declare L
{Browse L}
L = {Ents 0}

case L of A|B|C|_ then {Browse A+B+C} end
{Browse L}
{Browse L.2.2.2.2.2.2.2.2.2.2.1}

% Lazy Pascal triangle function
declare ListaPascal SumListas DesplIzq DesplDer
fun lazy {ListaPascal Fila}
   Fila|{ListaPascal{SumListas {DesplIzq Fila}{DesplDer Fila}}}
end
fun {DesplIzq L}
   case L of H|T then H|{DesplIzq T}
   else [0] end
end

fun {DesplDer L} 0|L end

fun {SumListas L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
	 H1 + H2|{SumListas T1 T2}
      end
   else nil end
end
LP = {ListaPascal [1]}
{Browse LP.1}
{Browse LP.2.1}
{Browse LP.2.2.2.2.1}
{Browse {List.nth LP 28}}

%%%%% High order programming

declare
fun {PascalGenerico Op N}
   if N==1 then [1]
   else L in L={PascalGenerico Op N-1}
      {OpListas Op {DesplIzq L} {DesplDer L}}
   end
end
fun {OpListas Op L1 L2}
   case L1 of H1|T1 then
      case L2 of H2|T2 then
	 {Op H1 H2}|{OpListas Op T1 T2}
      end
   else nil end
end

declare
fun {Sum X Y} X + Y end
fun {Xor X Y} if X == Y then 0 else 1 end end
declare
fun {PascalXor N} {PascalGenerico Xor N} end
fun {PascalRapido N} {PascalGenerico Sum N} end
{Browse {PascalRapido 10}}
{Browse {PascalXor 10}}
{Browse {PascalGenerico Number.'*' 10}}

%%%%% Concurrency and dataflow

% Concurrency: threads
declare
thread
   P in
   P={Pascal 25}
   {Browse P}
end
{Browse 99*99}

% Data flow
declare X in
thread
   {Delay 10000} X=99
end
{Browse ini} {Browse X*X}
% vs
declare X in
thread
   {Browse ini} {Browse X*X}
end
{Delay 10000} X=99

%%%%% State, objects and classes

% State

declare
C = {NewCell _}
{Browse C}
% C := 0
% {Browse @C}
C := @C + 1
{Browse @C}
C = {NewCell 2}

declare
C = {NewCell 0}
fun {PascalRapido N}
   C := @C + 1
   {PascalGenerico Sum N}
end
{Browse @C}

{Browse {PascalRapido 5}}
{Browse @C}

{Browse {PascalRapido 5}}
{Browse @C}

% Objects

declare
local C in
   C = {NewCell 0}
   fun {Incr}
      C := @C + 1 @C
   end
   fun {Leer}
      @C
   end
end
{Browse @C}
{Browse {Incr}}
{Browse {Leer}}

% Classes

declare
fun {ContadorNuevo}
   C Incr Leer in
   C = {NewCell 0}
   fun {Incr}
      C := @C + 1 @C
   end
   fun {Leer}
      @C
   end
   contador(incr:Incr
	    leer:Leer)
end

declare
Ctr1 = {ContadorNuevo}
Ctr2 = {ContadorNuevo}
{Browse {Ctr2.incr}}

{Browse {Ctr1.leer}}
{Browse Ctr1}
{Browse Ctr2}


