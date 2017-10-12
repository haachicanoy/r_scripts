% Taller programacion concurrente declarativa

% Punto 1
declare A B C D in
thread D = C + 1 end
thread C = B + 1 end
thread A = 1 end
thread B = A + 1 end
{Browse D|C|A|B}
{Browse D}


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

% b.
declare Sal A
thread Sal={Filter [5 1 A 4 0] fun {$ X} X>2 end} end
{Show Sal}

% c.
declare Sal A
thread Sal = {Filter [5 1 A 4 0] fun {$ X} X>2 end} end
{Delay 1000} {Show Sal}

% d.
declare Sal A
thread Sal = {Filter [5 1 A 4 0] fun {$ X} X>2 end} end
thread A = 6 end
{Delay 1000}
{Show Sal}

% Punto 9