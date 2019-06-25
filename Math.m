(* ::Package:: *)

(* ::Subtitle:: *)
(*Miscellaneous Functions*)


BeginPackage["Miscellaneous`"];


(* ::Section:: *)
(*Public*)


PresentationStyle::usage="PresentationStyle[graphic_Graphics] sets some options of a graphcis such that it looks nicer in presentation."


LinSpace::usage="LinSpace[a,b,n] returns an array of `n` equidistantly spaced numbers between `a` and `b`";
LogSpace::usage="LogSpace[a,b,n] returns an array of `n` logarithmically spaced numbers between `a` and `b`";
realSphericalHarmonicY::usage="realSphericalHarmonicY[l, m, \[Theta], \[Phi]] returns spherical harmonics with only real values";


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


PresentationStyle[graphic_Graphics]:=Show[graphic,BaseStyle->{Thickness[0.01],Medium},AxesStyle->Thickness[0.005]];


LinSpace[a_:0,b_:1,n_:20]:=Range[0,n-1]/(n-1)*(b-a)+a;
LogSpace[a_:1,b_:10,n_:20]:=Exp[LinSpace[Log[a],Log[b],n]];


realSphericalHarmonicY[l_Integer, m_Integer, \[Theta]_, \[Phi]_] := 
 Which[
  m == 0,
  SphericalHarmonicY[l, 0, \[Theta], \[Phi]],
  m > 0,
  Chop[(SphericalHarmonicY[l, m, \[Theta], \[Phi]] + (-1)^m*
       SphericalHarmonicY[l, -m, \[Theta], \[Phi]])/Sqrt[2]],
  True,
  Chop[(SphericalHarmonicY[l, -m, \[Theta], \[Phi]] - (-1)^m*
       SphericalHarmonicY[l, m, \[Theta], \[Phi]])/(I*Sqrt[2])]];

(* AllTrue[array_,func_:Identity]:=(Union[Map[func,Flatten[array]]]==={True}); *)


End[(* Private *)];
EndPackage[];
