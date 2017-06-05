%% Ch. 9 - Hierarchical Models
%% Example 6: The Shape Bias

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module(library(apply)).

:- set_options(default),set_inference(backward(lw)).

:- initialization(init).

% Auxiliary
map_multiply(_, [], []) := true. %% !!
map_multiply(N, [H1|T1], [H2|T2]) := %% !!
    H2 is N*H1,
    map_multiply(N, T1, T2).

% Parameters
shapes([0,1,2,3,4,5,6,7,8,9,10]).
colors([0,1,2,3,4,5,6,7,8,9,10]).
textures([0,1,2,3,4,5,6,7,8,9,10]).
sizes([0,1,2,3,4,5,6,7,8,9,10]).

phi_shapes ~ dirichlet([1,1,1,1,1,1,1,1,1,1,1]).
phi_colors ~ dirichlet([1,1,1,1,1,1,1,1,1,1,1]).
phi_textures ~ dirichlet([1,1,1,1,1,1,1,1,1,1,1]).
phi_sizes ~ dirichlet([1,1,1,1,1,1,1,1,1,1,1]).

% regularity parameters: how strongly we expect the global prototype 
% to project (ie. determine the local prototypes):
alpha_shapes   ~ gamma(1, 1). % exponential distribution as	     
alpha_colors   ~ gamma(1, 1). % a special case of gamma distribution
alpha_textures ~ gamma(1, 1). % gamma(1, beta) = exp(1/beta)
alpha_sizes    ~ gamma(1, 1).

prototype_shapes ~ val(Proto) :=
    phi_shapes ~= Ps,
    alpha_shapes ~= A,
    map_multiply(A, Ps, Proto).

prototype_colors ~ val(Proto) :=
    phi_colors ~= Ps,
    alpha_colors ~= A,
    map_multiply(A, Ps, Proto).

prototype_textures ~ val(Proto) :=
    phi_textures ~= Ps,
    alpha_textures ~= A,
    map_multiply(A, Ps, Proto).

prototype_sizes ~ val(Proto) :=
    phi_sizes ~= Ps,
    alpha_sizes ~= A,
    map_multiply(A, Ps, Proto).

cat_shape(Cat) ~ dirichlet(Ps) :=
	prototype_shapes ~= Ps.
cat_color(Cat) ~ dirichlet(Ps) :=
	prototype_colors ~= Ps.
cat_texture(Cat) ~ dirichlet(Ps) :=
	prototype_textures ~= Ps.
cat_size(Cat) ~ dirichlet(Ps) :=
	prototype_sizes ~= Ps.
category_prototype(Category) ~ val(proto(Shape, Color, Texture, Size)) :=
	cat_shape(Category) ~= Shape,
	cat_color(Category) ~= Color,
	cat_texture(Category) ~= Texture,
	cat_size(Category) ~= Size.

draw_object(Category, N) ~ val(object(Shape, Color, Texture, Size)) :=
	draw_shape(Category, N) ~= Shape,
	draw_color(Category, N) ~= Color,
	draw_texture(Category, N) ~= Texture,
	draw_size(Category, N) ~= Size.

draw_shape(Cat, N) ~ finite([P0:0, P1:1, P2:2, P3:3, P4:4, P5:5,
			    P6:6, P7:7, P8:8, P9:9, P10:10]) :=
	category_prototype(Cat) ~= proto(Shape,_,_,_),
	Shape = [P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10].

draw_color(Cat, N) ~ finite([P0:0, P1:1, P2:2, P3:3, P4:4, P5:5,
			    P6:6, P7:7, P8:8, P9:9, P10:10]) :=
	category_prototype(Cat) ~= proto(_,Color,_,_),
	Color = [P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10].

draw_texture(Cat, N) ~ finite([P0:0, P1:1, P2:2, P3:3, P4:4, P5:5,
			       P6:6, P7:7, P8:8, P9:9, P10:10]) :=
	category_prototype(Cat) ~= proto(_,_,Texture,_),
	Texture = [P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10].

draw_size(Cat, N) ~ finite([P0:0, P1:1, P2:2, P3:3, P4:4, P5:5,
			    P6:6, P7:7, P8:8, P9:9, P10:10]) :=
	category_prototype(Cat) ~= proto(_,_,_,Size),
	Size = [P0,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10].

evidence([draw_object(cat1, 1) ~= object(1,1,1,1), 
	  draw_object(cat1, 2) ~= object(1,2,2,2),
	  draw_object(cat2, 1) ~= object(2,3,3,1),
	  draw_object(cat2, 2) ~= object(2,4,4,2),
	  draw_object(cat3, 1) ~= object(3,5,5,1),
	  draw_object(cat3, 2) ~= object(3,6,6,2),
	  draw_object(cat4, 1) ~= object(4,7,7,1),
	  draw_object(cat4, 2) ~= object(4,8,8,2),
	  draw_object(cat5, 1) ~= object(5,9,9,1)]).

test(N) :-
    init,
    evidence(Evidence),
    eval_query_distribution_eval(X, Evidence, [], 
				 draw_object(cat5, 2) ~= object(X,_,_,_), N, LP, _, _),
    writeln(LP).

:- initialization(test(50000)).
