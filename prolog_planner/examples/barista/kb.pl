%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                       HIGH-LEVEL KB                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous coffee/1.

coffee(espresso).
coffee(cappuccino).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        LOW-LEVEL KB                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous pos/1.
:- discontiguous wheeled/1.
:- discontiguous arm/1.
:- discontiguous object/1.

pos(register).
pos(service_area).
pos(ingredient_area).
pos(coffee_machine).
pos(coffee_dispenser).
pos(milk_dispenser).
pos(cup_dispender).
pos(sink).

wheeled(barista).
arm(barista).

object(paper_cup).
object(china_cup).
object(coffee_chamber).