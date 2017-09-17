%  ann.erl
-module(ann).
-compile([export_all]).

-define(ANN_TWISTING_TERM, 0.001).

sigmoid(X) when X < -100.0 -> 0.0;
sigmoid(X) when X >  100.0 -> 1;
sigmoid(X) -> 1.0 / (1.0 + math:exp(-X)).
sigmoid_grad(Y) -> Y * (1.0 - Y).

relu(X) -> max(0.0, X).
relu_grad(Y) when Y =< 0 -> 0.0;
relu_grad(_Y) -> 1.0.

tanh(X) -> math:tanh(X).
tanh_grad(Y) -> 1 - Y * Y.

tanh1(X) -> 1.7159 * math:tanh(0.6666666666666666*X) + ?ANN_TWISTING_TERM * X.    % a*tanh(bx)+cx 
tanh1_grad(Y) when Y >= 1.7159 -> ?ANN_TWISTING_TERM;
tanh1_grad(Y) when Y =< -1.7159 -> ?ANN_TWISTING_TERM;
tanh1_grad(Y) -> ?ANN_TWISTING_TERM + 1.1439333333333332 - 0.3885230297025856*Y*Y.  % c+ab-b*y*y/a (approximation)

activation(tanh1, X) -> tanh1(X);
activation(tanh, X) -> tanh(X);
activation(sigmoid, X) -> sigmoid(X);
activation(relu, X) -> relu(X).

activation_gradient(tanh1, Y) -> tanh1_grad(Y);
activation_gradient(tanh, Y) -> tanh_grad(Y);
activation_gradient(sigmoid, Y) -> sigmoid_grad(Y);
activation_gradient(relu, Y) -> relu_grad(Y).

output_activation(X) -> X.

output_activation_gradient(_) -> 1.0.

run_neuron(Activation) ->  % spawn an uninitialized neuron (including BiasNeurons)
  spawn_link(ann, run_neuron, [none, [], [], 0, Activation]).

run_neuron(Main, Inputs, Outputs, Token, Activation) ->           % neuron event loop
  receive
    {main_pid, Pid} ->
      run_neuron(Pid, Inputs, Outputs, Token, Activation);

    {connect_to_output, Pid} ->
      run_neuron(Main, Inputs, [Pid | Outputs], Token, Activation);

    {connect_to_input, PidWeight} ->
      run_neuron(Main, [PidWeight | Inputs], Outputs, Token, Activation);

    {status, Token} ->
      status_neuron(Inputs, Outputs, Token),
      run_neuron(Main, Inputs, Outputs, Token + 1, Activation);
    
    {layer_weights, ReplyPid} ->
      W = layer_weights(lists:reverse(Outputs)),
      ReplyPid ! {response_layer_weights, W},
      run_neuron(Main, Inputs, Outputs, Token, Activation);
    
    {weights, ReplyPid} ->
      ReplyPid ! {response_weight, [W || {_I, W} <- lists:reverse(Inputs)]},
      run_neuron(Main, Inputs, Outputs, Token, Activation);

    {feed_forward, Token} ->
      fire_neuron(Inputs, Outputs, {feed_forward, Token}, Main, Token, Activation),
      run_neuron(Main, Inputs, Outputs, Token + 1, Activation);

    {back_prop, LearningRate, Token} ->
      NewInputs = learn_neuron(Inputs, Outputs, LearningRate, Main, Token, Activation),
      run_neuron(Main, NewInputs, Outputs, Token + 1, Activation)
  end.

%% -----
% operations on neuron
%% -----
status_neuron(Inputs, Outputs, Token) ->
  io:format("~p -> ~p --> ~p ~n", [self(), Inputs, Outputs]),
  resend_status_neuron(Inputs, Outputs, Token).

resend_status_neuron([], Outputs, Token) ->
  lists:foreach(fun(Pid) ->
                  Pid ! {status, Token}
                end, Outputs);
resend_status_neuron(_, _, _) -> ok.

layer_weights([]) -> [];
layer_weights([Out | Outputs]) ->     % collect input weights of next layer's neurons
  Out ! {weights, self()},
  Weight = receive
    {response_weight, W} -> W
  end,
  [Weight | layer_weights(Outputs)].

% -- learning
learn_neuron([], Outputs, LearningRate, Main, Token, Activation) ->
  % -- bias neuron
  fire_neuron([], Outputs, {back_prop, LearningRate, Token}, Main, Token, Activation),
  [receive {delta, Pid, _, Token} -> ignore end || Pid <- Outputs],
  [];
learn_neuron(Inputs, [], LearningRate, Main, Token, Activation) ->
  % -- output neuron
  {Output, Outs} = fire_neuron(Inputs, [], output_layer_fire, Main, Token, Activation),
  receive {target, Target, Token} -> Target end,
  Delta = (Output - Target) * output_activation_gradient(Output),
  update_weights(Inputs, Outs, Delta, LearningRate, Token);
learn_neuron([{Pid, Weight}], Outputs, _, Main, Token, Activation) when Pid == Main ->
  % -- input layer neuron
  fire_neuron([{Pid, Weight}], Outputs, input_layer_fire, Main, Token, Activation),
  [receive {delta, N, InDelta, Token} -> InDelta end || N <- Outputs],
  [{Pid, Weight}];
learn_neuron(Inputs, Outputs, LearningRate, Main, Token, Activation) ->
  % -- hidden layer neuron
  {Output, Outs} = fire_neuron(Inputs, Outputs, hidden_layer_fire, Main, Token, Activation),
  Delta = lists:sum([receive {delta, Pid, InDelta, Token} -> InDelta end || Pid <- Outputs]) * activation_gradient(Activation, Output),
  update_weights(Inputs, Outs, Delta, LearningRate, Token).

update_weights(Inputs, Outs, Delta, LearningRate, Token) ->
  Self = self(),
  lists:map(fun({{Pid, Weight}, Out}) ->
              Pid ! {delta, Self, Delta * Weight, Token},
              {Pid, Weight - LearningRate * Delta * Out}
            end, lists:zip(Inputs, Outs)).

% -- predicting
fire_neuron([], Outputs, Opt, _, Token, _Activation) ->
  % -- bias neuron
  Self = self(),
  lists:foreach(fun(Pid) ->
                  Pid ! Opt, 
                  Pid ! {fire, Self, 1.0, Token} 
                end, Outputs),
  {1.0, []};
fire_neuron(Inputs, [], _, Main, Token, _Activation) ->
  % -- output neuron
  Self = self(),
  Input = [receive {fire, Pid, In, Token} -> {Weight * In, In} end || {Pid, Weight} <- Inputs],
  Output = output_activation(sum_tuples(Input)),
  Main ! {output, Self, Output, Token},
  {Output, snd(Input)};
fire_neuron([{Pid, _}], Outputs, _, Main, Token, _Activation) when Pid == Main ->
  % -- input layer
  Self = self(),
  Output = receive {fire, Pid, In, Token} -> In end,
  lists:foreach(fun(N) ->
                  N ! {fire, Self, Output, Token}
                end, Outputs),
  {Output, [Output]};
fire_neuron(Inputs, Outputs, _, _, Token, Activation) ->
  % -- hidden layer neuron
  Self = self(),
  Input = [receive {fire, Pid, In, Token} -> {Weight * In, In} end || {Pid, Weight} <- Inputs],
  Output = activation(Activation, sum_tuples(Input)),
  lists:foreach(fun(Pid) ->
                  Pid ! {fire, Self, Output, Token}
                end, Outputs),
  {Output, snd(Input)}.

create_neural_network(Layers) ->
  Weights = random_weights(compute_neurons(Layers)),
  create_neural_network(Layers, Weights).

create_neural_network(Layers, Weights) when is_list(Weights) ->
  create_neural_network(Layers, Weights, sigmoid);
create_neural_network(Layers, Activation) when is_atom(Activation) ->
  Weights = random_weights(compute_neurons(Layers)),
  create_neural_network(Layers, Weights, Activation).

create_neural_network(Layers, Weights, Activation) when length(Layers) > 1 ->
  io:format("create_neural_network...~n"),
  Neurons = [[run_neuron(Activation) || _ <- lists:seq(1, InLayer)] || InLayer <- modify_layers(Layers)],
  full_mesh_connect(Neurons, Weights),
  NN = spawn_link(ann, neural_network, [
                    tl(hd(Neurons)),                                          % InputLayer
                    lists:last(Neurons),                                      % OutputLayer
                    lists:map(fun(X) -> hd(X) end, lists:droplast(Neurons)),  % BiasNeurons
                    0]),                                                      % Token
  lists:foreach(fun(Pid) -> 
                  Pid ! {connect_to_input, {NN, 1.0}} 
                end, tl(hd(Neurons))),                  % connect InputLayer to NN process
  lists:foreach(fun(Pid) ->
                  Pid ! {main_pid, NN}
                end, lists:concat(Neurons)),            % connect InputLayer to NN process
  io:format("create_neural_network ~p succeeded~n",[NN]),
  NN;
create_neural_network(_, _, _) ->
  io:format("Not enought layers.~n"), 
  fail.

neural_network(InputLayer, OutputLayer, BiasNeurons, Token) -> % Network process event loop
  erlang:garbage_collect(),
  receive
    {predict, Input} ->
      Output = forward_pass(InputLayer, OutputLayer, BiasNeurons, Token, Input),
      io:format("Output: ~p~n", [Output]),
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + 1);
    {predict, Input, ReplyPid} ->
      Output = forward_pass(InputLayer, OutputLayer, BiasNeurons, Token, Input),
      ReplyPid ! {predicted, Output},
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + 1);
    {compute_error, TrainingSet} ->
      Examples = length(TrainingSet),
      Rss =  forward_pass_examples(InputLayer, OutputLayer, BiasNeurons, Token, TrainingSet, Examples),
      io:format("RSS: ~p~n", [Rss]),
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + Examples);
    {compute_error, TrainingSet, ReplyPid} ->
      Examples = length(TrainingSet),
      Rss =  forward_pass_examples(InputLayer, OutputLayer, BiasNeurons, Token, TrainingSet, Examples),
      ReplyPid ! {computed_error, Rss},
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + Examples);
    {learn_epochs, Epochs, LearningRate, TrainingSet} ->
      % io:format("Start ann Training.~n"),
      % io:format("ann training set ~p ~n", [TrainingSet]),
      TokenShift = learn_epochs(InputLayer, OutputLayer, BiasNeurons, Token, Epochs, LearningRate, TrainingSet),
      io:format("Done.~n"),
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + TokenShift);
    {learn_epochs, Epochs, LearningRate, TrainingSet, ReplyPid} ->
      TokenShift = learn_epochs(InputLayer, OutputLayer, BiasNeurons, Token, Epochs, LearningRate, TrainingSet),
      ReplyPid ! training_done,
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + TokenShift);
    {learn_until, RssEps, LearningRate, TrainingSet} ->
      TokenShift = learn_until(InputLayer, OutputLayer, BiasNeurons, Token, RssEps, LearningRate, TrainingSet),
      io:format("Done.~n"),
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + TokenShift);
    {learn_until, RssEps, LearningRate, TrainingSet, ReplyPid} ->
      TokenShift = learn_until(InputLayer, OutputLayer, BiasNeurons, Token, RssEps, LearningRate, TrainingSet),
      ReplyPid ! training_done,
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + TokenShift);
    {train, Epochs, _ResErr, LearningRate, TrainingSet, ReplyPid} ->
      % io:format("Start ann Training.~n"),
      % io:format("ann training set ~p at rate ~p~n", [TrainingSet, LearningRate]),
      TokenShift = learn_epochs(InputLayer, OutputLayer, BiasNeurons, Token, Epochs, LearningRate, TrainingSet),
      ReplyPid ! training_done,
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + TokenShift);
    {status} ->
      lists:foreach(fun(N) -> 
                      N ! {status, Token}
                    end, BiasNeurons),
      lists:foreach(fun(N) -> 
                      N ! {status, Token}
                    end, InputLayer),
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token + 1);
    {weights, ReplyPid} ->
      Weights = lists:map(fun(N) -> 
                      N ! {layer_weights, self()},
                      receive {response_layer_weights, W} -> W end
                    end, BiasNeurons),
      ReplyPid ! {response_weights, Weights},
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token);
    {finish} ->
      exit(neural_network_shutdown);
    _ ->
      neural_network(InputLayer, OutputLayer, BiasNeurons, Token)
  end.

predict(NN, [N|_]=Input) when is_number(N) ->
    NN ! {predict, Input, self()},
    receive {predicted, Val} -> Val end;
predict(NN, Set) ->
  Self = self(),
  lists:map(fun(Ex) ->
              NN ! {predict, Ex, Self},
              receive {predicted, Val} -> Val end,
              Val
            end, Set).

get_weights(NN) ->
    NN ! {weights, self()},
    receive {response_weights, Weights} -> Weights end.

train(NN, Epochs, ResErr, LearningRate, TrainingSet) ->
    NN ! {train, Epochs, ResErr, LearningRate, TrainingSet, self()},
    receive training_done -> ok end.

compute_error(NN, TrainingSet) ->
    NN ! {compute_error, TrainingSet, self()},
    receive {computed_error, Rss} -> Rss end.

learn_epochs(InputLayer, OutputLayer, BiasNeurons, Token, Epochs, LearningRate, TrainingSet) ->
  Examples = length(TrainingSet),
  lists:foreach(fun(Epoch) ->
      learn_pass(InputLayer, OutputLayer, BiasNeurons, Token + Examples * Epoch * 2, LearningRate, TrainingSet, Examples),
      Rss = forward_pass_examples(InputLayer, OutputLayer, BiasNeurons, Token + Examples * (Epoch * 2 + 1), TrainingSet, Examples),
      % io:format("Epoch ~p, loss: ~p, examples: ~p~n", [Epoch, Rss, Examples])
      io:format("Epoch ~p, loss: ~p, average: ~p~n", [Epoch, Rss, Rss / Examples])
    end, lists:seq(0, Epochs - 1)),
  Examples * Epochs * 2.

learn_until(InputLayer, OutputLayer, BiasNeurons, Token, RssEps, LearningRate, TrainingSet) ->
  Examples = length(TrainingSet),
  {_, Epochs} = until(
    fun({X, _}) -> X < RssEps end,
    fun({_, Epoch}) ->
      learn_pass(InputLayer, OutputLayer, BiasNeurons, Token + Examples * Epoch, LearningRate, TrainingSet, Examples),
      Rss = forward_pass_examples(InputLayer, OutputLayer, BiasNeurons, Token + Examples * (Epoch + 1), TrainingSet, Examples),
      io:format("Epoch ~p, loss: ~p, average: ~p~n", [Epoch div 2, Rss, Rss / Examples]),
      {Rss, Epoch + 2}
    end, {RssEps * 2, 0}),
  Examples * Epochs.

learn_pass(InputLayer, OutputLayer, BiasNeurons, Token, LearningRate, TrainingSet, Examples) ->
  Self = self(),
  lists:foreach(fun({Shift, {Input, Output}}) ->
                  TokenShift = Token + Shift,
                  lists:foreach(fun(N) -> 
                                  N ! {back_prop, LearningRate, TokenShift} 
                                end, BiasNeurons),
                  lists:foreach(fun({N, In}) ->
                                  N ! {back_prop, LearningRate, TokenShift},
                                  N ! {fire, Self, In, TokenShift}
                                end, lists:zip(InputLayer, Input)),
                  lists:foreach(fun({N, Out}) ->
                                  N ! {target, Out, TokenShift}
                                end, lists:zip(OutputLayer, Output)),
                  [receive {output, Pid, Out, TokenShift} -> Out end || Pid <- OutputLayer]
                end, lists:zip(lists:seq(0, Examples - 1), TrainingSet)).

forward_pass_examples(InputLayer, OutputLayer, BiasNeurons, Token, TrainingSet, Examples) ->
  Rss = lists:map(fun({Shift, {ExIn, ExOut}}) ->
                    TokenShift = Token + Shift,
                    Output = forward_pass(InputLayer, OutputLayer, BiasNeurons, TokenShift, ExIn),
                    compute_rss(Output, ExOut)
                  end, lists:zip(lists:seq(0, Examples - 1), TrainingSet)),
  lists:sum(Rss).

forward_pass(InputLayer, OutputLayer, BiasNeurons, Token, Example) ->
  Self = self(),
  lists:foreach(fun(Pid) -> 
                  Pid ! {feed_forward, Token} 
                end, BiasNeurons),
  lists:foreach(fun({Pid, In}) -> 
                  Pid ! {feed_forward, Token}, 
                  Pid ! {fire, Self, In, Token} 
                end, lists:zip(InputLayer, Example)),
  [receive {output, Pid, Out, Token} -> Out end || Pid <- OutputLayer].

compute_rss(Xs, Ys) -> compute_rss(Xs, Ys, 0.0).
compute_rss([], [], R) -> R;
compute_rss([X | Xs], [Y | Ys], R) -> 
  D = X - Y,
  compute_rss(Xs, Ys, R + D * D).

connect(InPidWeight, OutPid) ->
  OutPid ! {connect_to_input, InPidWeight},
  {InPid, _W} = InPidWeight,
  % io:format("connecting ~p to ~p weight ~p~n", [InPid, OutPid, _W]),
  InPid ! {connect_to_output, OutPid},
  ok.

full_mesh_connect(_, []) -> ok;
full_mesh_connect([N1, N2], W) -> 
  full_mesh_connect_layers(N1, N2, W);
full_mesh_connect([N1, N2 | Ns], W) ->
  Ws = full_mesh_connect_layers(N1, tl(N2), W),
  full_mesh_connect([N2 | Ns], Ws).

full_mesh_connect_layers(_, [], W) -> W;
full_mesh_connect_layers(N1, [N | Ns], W) ->
  Ws = full_mesh_connect_layer(N1, N, W),
  full_mesh_connect_layers(N1, Ns, Ws).

full_mesh_connect_layer([], _, W) -> W;
full_mesh_connect_layer([N1 | Ns], N, [W | Ws]) ->
  connect({N1, W}, N),
  full_mesh_connect_layer(Ns, N, Ws).


%% -----
% utility functions
%% -----
sum_tuples(L) -> sum_tuples(L, 0).
sum_tuples([], Acc) -> Acc;
sum_tuples([{X, _} | Xs], Acc) -> sum_tuples(Xs, Acc + X).

snd([]) -> [];
snd([{_, X} | Xs]) -> [X | snd(Xs)].

modify_layers([]) -> [];
modify_layers([L]) -> [L];
modify_layers([L | Ls]) -> [L + 1 | modify_layers(Ls)].

compute_neurons([]) -> 0;             % brutto Layers (with BiasNeurons) from netto Layers
compute_neurons([_L]) -> 0;
compute_neurons([L1, L2 | Ls]) -> (L1 + 1) * L2 + compute_neurons([L2 | Ls]).

random_weights(N) -> [rand:uniform() * 0.5 - 0.25 || _ <- lists:seq(1, N)].
% random_weights(N) -> lists:seq(1, N).

random_shuffle(L) ->
  [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- L])].

until(P, F, X) ->
  case P(X) of
    false -> until(P, F, F(X));
    true -> X
  end.

%% -----
% read/write functions
%% -----
read_train_set(FileName, NumOfFeatures) ->
  Lines = read_lines(FileName),
  Numbers = lists:map(fun(L) -> lists:map(fun(X) -> parse_float(X) end, string:tokens(L, " ")) end, Lines),
  lists:map(fun(L) -> lists:split(NumOfFeatures, L) end, Numbers).

write_data(FileName, Data) ->
  Out = string:join([string:join([io_lib:format("~p", [X]) || X <- Y], " ") || Y <- Data], "\n"),
  file:write_file(FileName, io_lib:fwrite(Out, [])).

read_lines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_lines(Device)
      after file:close(Device)
    end.

get_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> [Line | get_lines(Device)]
    end.

parse_float(String) ->
  T = string:strip(String, both, $\n),
  S = string:strip(T, both),
  case string:to_float(S) of
    {error, no_float} -> float(list_to_integer(S));
    {F, _} -> F
  end.
