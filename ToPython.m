(* ::Package:: *)

(*This package provides a function to convert a Mathematica expression to numpy
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
Not tested for every possible combination; use at your risk, by Gustavo Wiederhecker with
modifications by David Zwicker
*)


BeginPackage["ToPython`"]

ToPython::usage = "ToPython[expression, numpyprefix] converts Mathematica expression to
    a Numpy compatible expression. Because Numpy can be imported in several ways,
    numpystring is a string that will be added to appended to function names, e.g.,
    Cos->np.cos"
 
ToPythonEquation::usage = "ToPythonEquation[equation, numpyprefix] converts a
    Mathematica equation to a Numpy compatible express"
 
Begin["Private`"]

ToPython[expression_, numpyprefix_:"np", copy_:True] := 
    Module[{result, greekrule, PythonForm, np, a, b, l, m, args},

(* determine the correct numpy prefix *)
If[numpyprefix=="", np=numpyprefix, np=numpyprefix<>"."];

(* general function for formating output *)
format[pattern_String, args__] := ToString @ StringForm[pattern, Sequence @@ PythonForm /@ List[args]];

(* special forms that need to be recognized *)
PythonForm[Times[-1, a_]] := format["-(``)", a];
PythonForm[Power[a_, Rational[1, 2]]] := format[np<>"sqrt(``)", a];
PythonForm[Times[a_, Power[b_, -1]]] := format["(``) / (``)", a, b];

(* Simple math *)
PythonForm[Rational[a_,b_]] := format["(``) / (``)", a, b];
PythonForm[Complex[a_,b_]] := format["complex(``, ``)", a, b];
PythonForm[Times[a_,b_]] := format["(``) * (``)", a, b];
PythonForm[Plus[a_,b_]] := format["`` + ``", a, b];
PythonForm[Power[a_,b_]] := format["(``) ** (``)", a, b];
PythonForm[Exp[a_]] := format[np<>"exp(``)", a];

(* Some special functions *)
PythonForm[Arg[a_]] := format[np<>"angle(``)", a];
PythonForm[SphericalHarmonicY[l_, m_, a_, b_]] := format[
    "special.sph_harm(``, ``, (``) % (2 * "<>np<>"pi), (``) % "<>np<>"pi)",
    m, l, b, a];

(* Some functions that are not defined in numpy *)
PythonForm[Csc[a_]] := format["1 / "<>np<>"sin(``)", a];
PythonForm[Sec[a_]] := format["1 / "<>np<>"cos(``)", a];
PythonForm[Cot[a_]] := format["1 / "<>np<>"tan(``)", a];
PythonForm[Csch[a_]] := format["1 / "<>np<>"sinh(``)", a];
PythonForm[Sech[a_]] := format["1 / "<>np<>"cosh(``)", a];
PythonForm[Coth[a_]] := format["1 / "<>np<>"tanh(``)", a];

(* Handling arrays *)
PythonForm[List[args__]] := np<>"array(["<>StringRiffle[PythonForm/@List[args], ", "]<>"])";
PythonForm[a_ListQ] := np<>"array"<>StringReplace[ToString[a], {"{"-> "[","}"-> "]"}];

(* Constants *)
PythonForm[\[Pi]] = np<>"pi";
PythonForm[E] = np<>"e";

(* real numbers, engineering notation *)
PythonForm[r_Real] := Block[{a=MantissaExponent[r]},
    If[r>=0,ToString[N[a[[1]],6]]<>"e"<>ToString[a[[2]]],"("<>ToString[N[a[[1]],6]]<>"e"<>ToString[a[[2]]]<>")"]
];

(* Greek characters *)
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

(* Everything else *)
PythonForm[h_[args__]] := np<>ToLowerCase[PythonForm[h]]<>"("<>PythonForm[args]<>")";
PythonForm[allOther_] := StringReplace[ToString[allOther, FortranForm], greekrule];

result = StringReplace[PythonForm[expression], greekrule];
(* Copy results to clipboard *)
If[copy,CopyToClipboard[result]];
result
]


ToPythonEquation[Equal[a_, b_], numpyprefix_:"np", copy_:True] := ToPython[a - b, numpyprefix, copy]


End[]
EndPackage[]