BeginPackage["Utils`"]


LogNormalMeanStdDistribution::usage = 
    "LogNormal distribution parameterized by its mean and standard deviation"
LogNormalMeanVarDistribution::usage = 
    "LogNormal distribution parameterized by its mean and variance deviation"
only::usage = 
    "Return only element from a list. Raise error if length of the list is not 1"


Begin["`Private`"]

LogNormalMeanStdDistribution[mu_,sigma_]:=
    LogNormalDistribution[
        Log[mu^2/Sqrt[mu^2+sigma^2]],
        Sqrt@Log[1+sigma^2/mu^2]
    ]


LogNormalMeanVarDistribution[mu_,var_]:=
    LogNormalDistribution[
        Log[mu^2/Sqrt[mu^2+var]],
        Sqrt@Log[1+var/mu^2]
    ]


only[l_List]:=If[
        Length@l===1,
        First@l,
        Throw["List did not contain single item."]
    ];


End[]
EndPackage[]
