declare
fun {Size Ls}
   case Ls of nil then 0
   [] _|Lr then 1+{Size Lr}
   end
end

{Browse {Size [a b c]}}

% Invertir una lista
declare
fun {Concat Ls Ms}
   case Ls of nil then Ms
   [] X|Lr then X|{Concat Lr Ms}
   end
end


declare
fun {Invertir Xs}
   case Xs of nil then nil
   [] X|Xr then
      {Concat {Invertir Xr}
       [X]}
   end
end

{Browse {Invertir [2 3 4 5 6 7 8 9 10]}}

% Version declarativa
declare
fun {Gen L H}
   {Delay 100}
   if L > H then nil
   else L|{Gen L+1 H} end
end
Xs = {Gen 1 10}
{Browse Xs}

declare
Ys = {Map Xs fun {$ X} X*X end}
{Browse Ys}

% Version concurrente declarativa

declare
thread Xs = {Gen 1 10} end
thread Ys = {Map Xs fun {$ X} X*X end} end
{Browse Ys}

declare X0 X1 X2 X3 in
thread
   Y0 Y1 Y2 Y3 in
   {Browse [Y0 Y1 Y2 Y3]}
   Y0 = X0 + 1
   Y1 = X1 + Y0
   Y2 = X2 + Y1
   Y3 = X3 + Y2
   {Browse listo}
end
{Browse [X0 X1 X2 X3]}

X0 = 0
{Delay 2000}

X1 = 1
{Delay 2000}

X2 = 2
{Delay 2000}

X3 = 3

% Version concurrente de Map

declare
fun {MapC Xs F}
   case Xs of nil then nil
   [] X|Xr then
      thread {F X} end | {Map Xr F}
   end
end

{Browse mapC#{MapC [1 2 3] fun {$ A} {Delay 15000} A*A end}}

% Fibonacci

declare
fun {FibD X}
   if X =< 2 then 1
   else {FibD X-1} + {FibD X-2}
   end
end
fun {FibC X}
   if X =< 2 then 1
   else thread {FibC X-1} end + {FibC X-2}
   end
end

{Browse {FibC 35}}

% Flujos

declare Xs Xs2
Xs = 0|1|2|3|4|Xs2
{Browse Xs}
declare Xs3 in
Xs2 = 5|6|7|Xs3

Xs3 = nil

% Productor-Consumidor ejemplo

% Productor
declare
fun {Generar N Limite}
   if N < Limite then N|{Generar N+1 Limite}
   else nil end
end
% Consumidor
declare
fun {Suma Xs A} % Xs: lista; A: acumulado de la suma
   case Xs of X|Xr then {Suma Xr A+X}
   [] nil then A
   end
end

local Xs S in
   thread {Browse Xs} Xs = {Generar 0 150} end
   thread S = {Suma Xs 0} end
   {Browse S}
end

% Multiples lectores
local Xs S1 S2 S3 in
   thread Xs = {Generar 0 150000} end
   thread S1 = {Suma Xs 0} end
   thread S2 = {Suma Xs 0} end
   thread S3 = {Suma Xs 0} end
   {Browse s1#S1#s2#S2#s3#S3}
end

% Filtros
declare EsImpar
local Xs Ys S in
   thread Xs = {Generar 0 15} end
   thread Ys = {Filter Xs EsImpar} end
   thread S = {Suma Ys 0} end
   {Browse S}
end
fun {EsImpar X}
   X mod 2 \= 0
end

% Criba de Eratostenes para numeros primos
declare
fun {Generar N Limite}
   if N < Limite then N|{Generar N+1 Limite}
   else nil end
end
fun {Criba Xs}
   case Xs of nil then nil
   [] X|Xr then Ys in
      thread Ys = {Filter Xr fun {$ Y} Y mod X \= 0 end} end
      X|{Criba Ys}
   end
end

% Testing
local Xs Ys in
   thread Xs = {Generar 2 10000} end
   thread Ys = {Criba Xs} end
   {Browse Ys}
end

%%% Evaluacion perezosa intro
declare
fun lazy {F1 X} 1+X*(3+X*(3+X)) end
fun lazy {F2 X} Y=X*X in Y*Y end
fun lazy {F3 X} (X+1)*(X+1) end
A = {F1 10}
B = {F2 20}
C = {F3 30}
{Browse A#B#C}
D = A+B
{Browse D}

% Disparador By-need
declare Y
{ByNeed
 proc {$ W}
    A B C in
    a(A B C)=W
    A=111*111
    B=A*A
    C=B*B
 end
 Y}
{Browse Y}
{Browse Y.1}

% Flujos perezosos
% Productor/Consumidor revisitado
declare
fun lazy {Generar N} N|{Generar N+1} end
fun {Suma Xs A Limite}
   if Limite > 0 then
      case Xs of X|Xr then {Suma Xr A+X Limite-1} end
   else A end
end

local Xs S in
   thread Xs = {Generar 0} end
   S = {Suma Xs 0 150}
   {Browse S}
   {Browse Xs}
end

% Problema de Hamming
declare
fun lazy {MultEscalar N H}
   case H of X|H2 then N*X|{MultEscalar N H2} end
end
fun lazy {Mezclar Xs Ys}
   case Xs#Ys of (X|Xr)#(Y|Yr) then
      if X<Y then X|{Mezclar Xr Ys}
      elseif X>Y then Y|{Mezclar Xs Yr}
      else X|{Mezclar Xr Yr}
      end
   end
end
H = 1|{Mezclar {MultEscalar 2 H}
       {Mezclar {MultEscalar 3 H}
	{MultEscalar 5 H}}}
{Browse H}

declare
proc {Tocar N H}
   if N>0 then {Tocar N-1 H.2} else skip end
end
{Tocar 30 H}