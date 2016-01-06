# PISKa

PISKa is a Parallel Implementation of Spatial Kappa, based on 
KaSim, a stochastic simulator for rule-based models. With PISKa you can create 'compartments' connected
by 'links', and create 'transports' instructions among 'compartments'. This is useful when you have a non
homogeneous space. PISKa is highly scalable due to it MPI implementation, what allow you to create many compartments and run your simulation in many cores.

## Installation

To install follow the nexts steps:

As root
 
* Install Opam (in Ubuntu 14.04)

`add-apt-repository ppa:avsm/ppa`

`apt-get update`

`apt-get install ocaml ocaml-native-compilers camlp4-extra opam m4`

* aptitude install libopenmpi1.6

* aptitude install openmpi-bin

* aptitude install libopenmpi-dev

Now, NOT as root
*  Install OCaml, with opam init --comp 4.02.1

* opam config env

* opam install mpi

* `make` in PISKa directory

This should produce PISKa binaries. You will need your own plotting program (like gnuplot) to visualize curves.

If step 5 doesn't works (Error: Unbound module Mpi), you should change
the path in the Makefile to aims to OCaml mpi, ie,

`OCAMLOPT = $(OCAMLBINPATH)ocamlopt.opt -I PATH_TO_YOUR_OPAM_MPI/.opam/SOMETHING/lib/mpi  unix.cmxa mpi.cmxa str.cmxa #-p #-g -ccopt -g -ccopt -pg` 

and run `make` again.

## Usage

In order to run a simulation of 1000 rule applications of 2 compartment, type

`mpirun -n 2 PISKa -i piskaFile.cka -t 1000 -p 1000 -sync-t 1`

This will produce a data file of 1000 point (-p option) containing the trajectory of 1000 time step (-t) with synchronization time among compartments of 1 time step (-sync-t). The number of process (-n) must be equal to the number of compartments. 

Type:

`PISKa --help` 

for a complete list of options.

##New functionality regard KaSim:

###Compartment: 
To create a compartment of name 'compartmentA' and volume 1 we write:
`%compartment: 'compartmentA' 1`

and a matrix of 4x4 and each compartment of volume 1.5

`%compartment: 'compartmentA' [4][4] 1.5`


###Link:
The link is the connection among compartment, it can be unidirectional or bidirectional. To set an unidirectional link among two compartments we write

`%link: 'link_name' 'compartmentA' -> 'compartmentB' $1`

where only the agents presents in 'compartmentA' can travel to 'compartmentB' with a delay time of 1. We can assume the delay time among compartment as the travel time. To set a bidirectional link we write

`%link: 'link_name' 'compartmentA' <-> 'compartmentB' $1`

where agents in both compartment can move in any direction.

###Transport:
The transport defines a rate of agents per unit of time traveling in a link. To set a rate of 1.3 to agent 'agentA()' in a link called 'link_name' we write

`%transport: 'link_name' agentA() @1.3`

and if we want to move joints or separated agents we write

`%transport: <move-join> 'link_name' agentA() @ 0.2`

`%transport: <move-free> 'link_name' agentB() @ 0.7`

where agentA will move with any agent join to it and agentB will separate and move alone.

###Use:
The use instruction is to define the compartment that will be used, and set the initial conditions of it. If we want to initialize a compartment we write

`%use: 'compartmentA'`

`%init: 100 agentA() #define the number of agents` 

where we initialize the compartmentA with 100 agents of name 'agentA()'. The instruction must go immediately after the instruction %use.

*The unit of time is defined in the model by the rates, it can be seconds, weeks, years, etc.

###Fixed rates in rules
If you want to fix a rate, ie, you don't want it depends on volume of a compartment, you use `@*`, for instance

`'ruleName' agentA(),agentB() -> agentC() @* 1.5`

will fix the rate of this rule at 1.5, independently of the size of compartment.

##Kappa Syntax
The next information is available in KaSim/Kappa documentation, but we put here a brief resume to a fast implementation of a model in PISKa.

###Agent
We define an agent in the following way

`%agent: AgentName(site1~STATE1~STATE2, site2~STATE1, site3)`

that mean the agent 'AgentName' has 3 sites, the first site is called 'site1' and has 2 possibles states, STATE1 and STATE2. Equal to site2. The third site 'site3' has no states.

###Agents binding
We can joint two agents, binding by a site, in the following way

`agentA(s~T, c!1), agentB(s~U, d!1)`

what means that agentA and agentB are binding each other by the c and d site respectively. The sign ! and it consecutive number makes the binding.
  
###Rules
The rules are what governs the agents interactions. A rules syntaxs is given by

`'ruleName' agentA(),agentB() -> agentC() @ 1.5`

in this case agentA and agentB joint and create the agentC with a rate of 1.5. In an other example we have

`'ruleName' agentA(c~A), agentB(c~B) -> agentA(c~B), agentB(c~B) @ 0.5`

where the interaction among agentA and agentB change the state of site c (from A to B) of agentA with a rate of 0.5. If the rate is replaced with an '[inf]', the reaction will always occur.

###Variables
We can define some variables in the following way

`%var: 'varName' 50`

what is the same to varName=50. We can also do something like

`%var: 'agentNumberA' agentA()`

where we are defining agentNumberA equals to the number of agents with name agentA.

###Observable
We can define the output of our simulation in the following way

`%obs: 'varOutName' agentA(c~S)`

this will show in a column the numbers of agents with name agentA and site c in state S. This can be done with many agents

`%obs: 'varOutNameA' agentA(c~S)`

`%obs: 'varOutNameB' agentB(c~R)`

and it will display two columns with the respective number of agents in its respective states.

###Modifications (kind of scripting)

`%mod:` 


You can find a more detailed description about kappa language here:

http://www.kappalanguage.org/syntax.html

##Example
Here we present a very simple model

##ERRORS
ExceptionDefn.Semantics_Error(_, "Rule name Transport 1 superman() at 1.393503 (2,1) is already used") 
lo que pasa es que los  transportes que tienen tiempo distinto de 0 (fijado en el link con $x) generan perturbaciones y cada una lleva un nombre, y no debieran poder repetirse
pero al parecer ocurren tantos transportes por unidad de tiempo que algunos están ocurriendo incluso en el mismo instante