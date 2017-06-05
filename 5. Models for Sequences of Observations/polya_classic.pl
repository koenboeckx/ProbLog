 % Polya Urn - Classic implementation

P1::pick(Nsample, w, Urn); P2::pick(Nsample, b, Urn) :-
    Urn = urn(Nw, Nb),
    P1 is Nw/(Nw + Nb),
    P2 is Nb/(Nw + Nb).

% samples(Urn, Nreplace, Nsamples, Samples)
samples(_,_,0,[]).
samples(urn(Nw,Nb), Nreplace, Nsamples, [Sample|OtherSamples]) :-
    pick(Nsamples, Sample, urn(Nw, Nb)),
    (Sample = w,
        Add_white is Nreplace-1, Add_black is 0;
    Sample = b,
        Add_white is 0, Add_black is Nreplace-1
    ),
    NewWhite is Nw + Add_white,
    NewBlack is Nb + Add_black,
    NewSamples is Nsamples-1,
    samples(urn(NewWhite,NewBlack), Nreplace, NewSamples, OtherSamples).

query(samples(urn(1,2), 4, 3, [_,_,_])).
