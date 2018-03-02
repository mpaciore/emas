%% @author jstypka <jasieek@student.agh.edu.pl>
%% @version 1.0
%% @doc The module contains abstract genetic operators

-module(emas_genetic).
-export([solution/1, evaluation/2, reproduction/2, reproduction/3]).

-include ("emas.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc Generates a random solution.
-spec solution(sim_params()) -> solution().
solution(SP) ->
    Ops = SP#sim_params.genetic_ops,
    Ops:solution(SP).

%% @doc Evaluates a given solution. Higher is better.
-spec evaluation(solution(), sim_params()) -> float().
evaluation(S, SP) ->
    Ops = SP#sim_params.genetic_ops,
    Ops:evaluation(S, SP).

%% @doc Reproduction function for a single agent (mutation only).
-spec reproduction(solution(), sim_params()) -> solution().
reproduction(S, SP) ->
    Ops = SP#sim_params.genetic_ops,
    case rand:uniform() < SP#sim_params.mutation_chance of
        true -> Ops:mutation(S, SP);
        false -> S
    end.

%% @doc Reproduction function for a pair of agents (crossover and mutation).
-spec reproduction(solution(), solution(), sim_params()) -> [solution()].
reproduction(S1, S2, SP) ->
    Ops = SP#sim_params.genetic_ops,
    {R1, R2} = case rand:uniform() < SP#sim_params.recombination_chance of
                   true -> Ops:recombination(S1, S2, SP);
                   false -> {S1, S2}
               end,
    M1 = case rand:uniform() < SP#sim_params.mutation_chance of
             true -> Ops:mutation(R1, SP);
             false -> R1
         end,
    M2 = case rand:uniform() < SP#sim_params.mutation_chance of
             true -> Ops:mutation(R2, SP);
             false -> R2
         end,
    [M1, M2].
