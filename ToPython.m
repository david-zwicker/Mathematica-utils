(* ::Package:: *)

BeginPackage["ToPython`"]

ToPython::usage = "ToPython[expression, numpyprefix] converts Mathematica expression to
    a Numpy compatible expression. Because Numpy can be imported in several ways,
    numpystring is a string that will be added to appended to function names, e.g.,
    Cos->np.cos"
 
ToPythonEquation::usage = "ToPythonEquation[equation, numpyprefix] converts a
    Mathematica equation to a Numpy compatible express"
 
Begin["Private`"]

ToPython[expression_, numpyprefix_:"np", copy_:True]:=Module[{result,greekrule,PythonForm,np,lp,rp,a,b},
(*FUNCTION TO CONVERT MATHEMATICA EXPRESSION TO NUMPY;
----------------------------------------------------;
INPUT ARGUMENTS;
expression: your mathematica expression, it can be numbers, literals, complexes or lists;
numpy\[LetterSpace]prefix: string defining your Numpy import prefix, e.g.:
if your used "import numpy as np", your prefix should be the string "np"
if your used "from numpy import *", your prefix should be the empty string ""
;
OUTPUT;
the Numpy python-ready expression (to be copied as a string);
!The formatted expression will be copied ot your clipboard, ready to paste on Python!;
------------------------------------------------------;
Not tested for every possible combination; use at your risk, by Gustavo Wiederhecker*)

If[numpyprefix=="", np=numpyprefix, np=numpyprefix<>"."];
(*if no prefix is included, the "." separator is not used*)

lp="(";
rp=")";
PythonForm[Times[a_,Power[b_,-1]]]:=lp<>PythonForm[a]<>rp<>"/"<>lp<>PythonForm[b]<>rp;
PythonForm[Rational[a_,b_]]:=PythonForm[a]<>"/"<>PythonForm[b];
PythonForm[Complex[a_,b_]]:="complex"<>lp<>PythonForm[a]<>", "<>PythonForm[b]<>rp;
PythonForm[Times[a_,b_]]:=PythonForm[a]<>" * "<>PythonForm[b];
PythonForm[Plus[a_,b_]]:=PythonForm[a]<>" + "<>PythonForm[b];
PythonForm[Power[a_,b_]]:=lp<>PythonForm[a]<>rp<>"**"<>lp<>PythonForm[b]<>rp;
PythonForm[Arg]=np<>"angle";
PythonForm[Exp[a_]]:=np<>"exp"<>lp<>PythonForm[a]<>rp;

(*Some functions that are not defined in numpy*)
PythonForm[Csc[a_]]:="1/"<>np<>"sin"<>lp<>PythonForm[a]<>rp;
PythonForm[Sec[a_]]:="1/"<>np<>"cos"<>lp<>PythonForm[a]<>rp;
PythonForm[Cot[a_]]:="1/"<>np<>"tan"<>lp<>PythonForm[a]<>rp;
PythonForm[Csch[a_]]:="1/"<>np<>"sinh"<>lp<>PythonForm[a]<>rp;
PythonForm[Sech[a_]]:="1/"<>np<>"cosh"<>lp<>PythonForm[a]<>rp;
PythonForm[Coth[a_]]:="1/"<>np<>"tanh"<>lp<>PythonForm[a]<>rp;

(*Handling arrays*)
PythonForm[List[args__]]:=np<>"array"<>lp<>"["<>StringRiffle[PythonForm/@List[args], ", "]<>"]"<>rp;

(*Pi and E*)
PythonForm[\[Pi]]=np<>"pi";
PythonForm[E]=np<>"e";

(*real numbers, engineering notation*)
PythonForm[r_Real]:=Block[{a=MantissaExponent[r]},
    If[r>=0,ToString[N[a[[1]],6]]<>"e"<>ToString[a[[2]]],"("<>ToString[N[a[[1]],6]]<>"e"<>ToString[a[[2]]]<>")"]
];

(*Greek characters*)
greekrule={
    "\[Alpha]"->"alpha","\[Beta]"->"beta","\[Gamma]"->"gamma","\[Delta]"->"delta",
    "\[CurlyEpsilon]"->"curlyepsilon","\[Zeta]"->"zeta","\[Eta]"->"eta",
    "\[Theta]"->"theta","\[Iota]"->"iota","\[Kappa]"->"kappa","\[Lambda]"->"lambda",
    "\[Mu]"->"mu","\[Nu]"->"nu","\[Xi]"->"xi","\[Omicron]"->"omicron","\[Pi]"->"pi",
    "\[Rho]"->"rho","\[FinalSigma]"->"finalsigma","\[Sigma]"->"sigma","\[Tau]"->"tau",
    "\[Upsilon]"->"upsilon","\[CurlyPhi]"->"curlyphi","\[Chi]"->"chi","\[Phi]" -> "phi",
    "\[Psi]"->"psi",
    "\[Omega]"->"omega","\[CapitalAlpha]"->"Alpha","\[CapitalBeta]"->"Beta",
    "\[CapitalGamma]"->"Gamma","\[CapitalDelta]"->"Delta",
    "\[CapitalEpsilon]"->"CurlyEpsilon","\[CapitalZeta]"->"Zeta",
    "\[CapitalEta]"->"Eta","\[CapitalTheta]"->"Theta","\[CapitalIota]"->"Iota",
    "\[CapitalKappa]"->"Kappa","\[CapitalLambda]"->"Lambda","\[CapitalMu]"->"Mu",
    "\[CapitalNu]"->"Nu","\[CapitalXi]"->"Xi","\[CapitalOmicron]"->"Omicron",
    "\[CapitalPi]"->"Pi","\[CapitalRho]"->"Rho","\[CapitalSigma]"->"Sigma",
    "\[CapitalTau]"->"Tau","\[CapitalUpsilon]"->"Upsilon","\[CapitalPhi]"->"CurlyPhi",
    "\[CapitalChi]"->"Chi","\[CapitalPsi]"->"Psi","\[CapitalOmega]"->"Omega"};

(*Everything else*)
PythonForm[h_[args__]]:=np<>ToLowerCase[PythonForm[h]]<>lp<>PythonForm[args]<>rp;
PythonForm[a_ListQ]:=np<>"array"<>StringReplace[ToString[a],{"{"-> "[","}"-> "]"}];
PythonForm[allOther_]:=StringReplace[ToString[allOther,FortranForm],greekrule];

(*Copy results to clipboard*)
result = StringReplace[PythonForm[expression], greekrule];
If[copy,CopyToClipboard[result]];
result
]


ToPythonEquation[Equal[a_, b_], numpyprefix_:"np", copy_:True]:=ToPython[a - b, numpyprefix, copy]


End[]
EndPackage[]