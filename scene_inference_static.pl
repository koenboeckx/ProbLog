% Scene Inference

:-use_module(library(lists)).

% object(ID, x_location, y_location, vertical_size, color).
% image = [[_,_,_,_], [_,_,_,_]].

make_object(ID, object(ID, XLoc, YLoc, VertSize, Color)) :-
    select_uniform(xloc(ID), [0,1,2,3], XLoc, _),
    select_uniform(yloc(ID), [0,1], YLoc, _),
    select_uniform(vertsize(ID), [1,2], VertSize, _),
    select_uniform(color(ID), [1,2], Color, _).

%object_appearance(Object, Image).
object_appearance(Object, [[A,B,C,D], [E,F,G,H]]) :-
    set_pixel(Object,0,0,A), set_pixel(Object,1,0,B),
    set_pixel(Object,2,0,C), set_pixel(Object,3,0,D),
    set_pixel(Object,0,1,E), set_pixel(Object,1,1,F),
    set_pixel(Object,2,1,G), set_pixel(Object,3,1,H).

%set_pixel(Object, XCoord, YCoord, Pixel).
set_pixel(object(_, XLoc, _, _, _), XCoord, _, 0) :- 
    XCoord < XLoc.
set_pixel(object(_, XLoc, _, _, _), XCoord, _, 0) :-
    XLoc1 is XLoc + 1,
    XLoc1 =< XCoord.
set_pixel(object(_, _, YLoc, _, _), _, YCoord, 0) :- YCoord < YLoc.
set_pixel(object(_, _, YLoc, VertSize, _), _, YCoord, 0) :-
    YLoc1 is YLoc + VertSize,
    YLoc1 =< YCoord.
set_pixel(object(_, XLoc, YLoc, VertSize, Color), XCoord, YCoord, Color) :-
    XLoc = XCoord,
    YLoc =< YCoord,
    YLoc1 is YLoc + VertSize,  YLoc1 > YCoord.

% layerr(ObjectImage, BackgroundImage, CompoundImage)
layer([ORow1, ORow2], [IRow1, IRow2], [CRow1, CRow2]) :-
    layer_row(ORow1, IRow1, CRow1),
    layer_row(ORow2, IRow2, CRow2).
layer_row([],[],[]).
layer_row([0|ORow], [IPix|IRow], [IPix|CRow]) :-
    layer_row(ORow, IRow, CRow).
layer_row([C|ORow], [IPix|IRow], [C|CRow]) :-
    C>0,
    layer_row(ORow, IRow, CRow).
    
observed_image([[0,1,0,0], [0,1,0,0]]).

0.5::num_objects(1); 0.5::num_objects(2).

image(Image) :-
    num_objects(1),
    make_object(1,Object1), 
    object_appearance(Object1, Image).
image(Image) :-
    num_objects(2),
    make_object(1, Object1),
    make_object(2, Object2),
    object_appearance(Object1, Image1), 
    object_appearance(Object2, Image2),
    layer(Image1, Image2, Image).

evidence(image(Image)) :-
    observed_image(Image).

query(num_objects(_)).
