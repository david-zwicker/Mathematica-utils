BeginPackage["Utils`"]


LogNormalMeanStdDistribution::usage = 
    "LogNormal distribution parameterized by its mean and standard deviation"
LogNormalMeanVarDistribution::usage = 
    "LogNormal distribution parameterized by its mean and variance deviation"
LogUniformDistributionMeanWidth::usage = 
    "LogUniform distribution parameterized by its mean and standard deviation"
only::usage = 
    "Return only element from a list. Raise error if length of the list is not 1"


Begin["`Private`"]

LogNormalMeanStdDistribution[mean_, std_]:=
    LogNormalDistribution[
        Log[mean^2 / Sqrt[mean^2+std^2]],
        Sqrt@Log[1 + std^2/mean^2]
    ]


LogNormalMeanVarDistribution[mean_, var_]:=
    LogNormalDistribution[
        Log[mean^2 / Sqrt[mean^2+var]],
        Sqrt@Log[1 + var/mean^2]
    ]


LogUniformDistributionMeanWidth[mean_, width_] := Module[{m, u},

    m = 2*mean*width*Log[width]/(width^2 - 1);

    TransformedDistribution[Exp[u], 
        u \[Distributed] 
        UniformDistribution[{Log[m] - Log[width], Log[m] + Log[width]}]]
    ]


only[l_List]:=If[
        Length@l===1,
        First@l,
        Throw["List did not contain single item."]
    ];


End[]
EndPackage[]
