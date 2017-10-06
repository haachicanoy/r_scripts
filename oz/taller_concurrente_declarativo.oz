% Taller programacion concurrente declarativa

% Punto 1
declare A B C D in
thread D = C + 1 end
thread C = B + 1 end
thread A = 1 end
thread B = A + 1 end
{Browse D|C|A|B}
{Browse D}

declare A B C D in
thread {Delay 2000} D = C + 1 end
thread {Delay 1000} C = B + 1 end
thread {Delay 200} A = 1 end
thread {Delay 500} B = A + 1 end
{Browse D|C|A|B}
{Browse D}

declare A B C D in
thread {Delay 4000} D = C + 1 end
thread {Delay 3000} C = B + 1 end
thread {Delay 1000} A = 1 end
thread {Delay 2000} B = A + 1 end
{Browse D|C|A|B}
{Browse D}

% En que orden son creados los hilos?
% Los hilos son creados de manera simultanea.

% En que orden son evaluados los hilos?
% El primer hilo evaluado corresponde al que toma la declaracion A = 1 (tercer hilo), posteriormente para anular la suspension del siguiente hilo se ejecuta el cuato que liga la variable B, luego el segundo hilo (ligadura variable C) y finalmente el primer hilo ligando la variable D. Esto se da, debido a que el programa fue construido de forma lineal.

% Punto 2

declare
fun {Filter List F}
   case List of X|List2 then
      if {F X} then X|{Filter List2 F}
      else {Filter List2 F} end
   else
      nil
   end
end
{Show {Filter [5 1 2 4 0] fun {$ X} X>2 end}}

% a.
declare A
{Show {Filter [5 1 A 4 0] fun {$ X} X>2 end}}
A = 15
{Show {Filter [5 1 A 4 0] fun {$ X} X>2 end}}
% Se suspende la ejecucion de Show hasta que A sea ligado con un valor

% b.
declare Sal A
thread Sal={Filter [5 1 A 4 0] fun {$ X} X>2 end} end
{Show Sal}
A = 15
{Show Sal}
% 

% c.
declare Sal A
thread Sal = {Filter [5 1 A 4 0] fun {$ X} X>2 end} end
{Delay 1000}
{Show Sal}
% Solamente se ejecuta el resultado de un hilo

% d.
declare Sal A
thread Sal = {Filter [5 1 A 4 0] fun {$ X} X>2 end} end
thread A = 6 end
{Delay 3000}
{Show Sal}


% Punto 3
% Productor
declare
fun {Productor Start End}
   if Start < End then
      Start|{Productor Start+1 End}
   else nil end
end
{Browse {Productor 0 10000}}
declare
fun {Consumer Xs A}
   case Xs of X|Xr then {Consumer Xr A+X}
   [] nil then A
   end
end
{Browse {Consumer Xs 0}}

declare
{ByNeed proc {$ A}
	   A = 111*111
	end
 y}
{Browse Y}