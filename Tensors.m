(* ::Package:: *)

(* ::Subtitle:: *)
(*Tensors` Package*)


(* ::Text:: *)
(*This package has been written by David Zwicker <zwicker@pks.mpg.de> *)


(* ::Section:: *)
(*Documentation*)


(* ::Text:: *)
(*This package provides routines for working with tensor fields in curvilinear coordinate systems. It does not provide functions for vector analysis in those systems. Use the Mathematica built in VectorAnalysis` package for this purpose. The package has been tested with Mathematica version 7 and 8 only.*)


(* ::Text:: *)
(*Tensor fields of rank r are internally defined using the custom head "Tensor" and two arguments:*)
(*	Tensor[character,data]*)
(*where 'character' is an array of length r containing the index position of the dimensions describing the tensor. It contains boolean values, where True denotes upper (contravariant) and False lower (covariant) indices. The 'data' is then a multidimensional array of depth r where each dimension is dim, the dimension of the underlying vector space (usually 3, in our case). The number of items in the array is thus dim^r. To construct tensors, it is convenient to use the supplied functions ContravariantTensor[data] and CovariantTensor[data], where the components 'data' must be specified as a multidimensional array which defines both the rank and the dimensions. There is also a function PhysicalTensor[data], which assumes that all supplied components have the same unit and returns a properly scaled tensor.*)


(* ::Text:: *)
(*The used coordinate system is the same as the one used in the VectorAnalysis` package and may be changed using its SetCoordinates[] function. Additionally, many functions in the Tensors` package support an optional last argument defining the coordinate system to be used in the same way as many function in the VectorAnalysis` package themself do. There is an important exception to the rule when it comes to binary operators, like '==', the addition '+', etc.: there is no way of specifying a coordinate system in these cases and the global, default one is used instead for raising and lowering indices.*)


(* ::Section:: *)
(*Public*)


BeginPackage["Tensors`",{"VectorAnalysis`"}];


(* ::Subsubsection:: *)
(*Coordinate System*)


GetCoordinates::usage="Returns the variables of the coordinate system";
GetMetricTensor::usage="Returns the metric tensor of the coordiante system";
GetScaleFactors::usage="Returns the scale factors of the corrdinate system";
GetChristoffelSymbol::usage="Returns Christoffel symbols of the corrdiante system";
GetCoordinatesData::usage="Returns information about the coordiante system"


(* ::Subsubsection:: *)
(*Basic Tensor Functions*)


Tensor::usage="Indicates a tensor field which assignes a tensor to any point in space given in the current coordinate system. The tensor is defined in terms of components in the local basis constructed from the current coordiante system.";
MakeTensor::usage="Converts an array into a tensor";
ContravariantTensor::usage="Builds a tensor assuming the supplied array contains contravariant components.";
CovariantTensor::usage="Builds a tensor assuming the supplied array contains covariant components.";
PhysicalTensor::usage="Builds a physical tensor assuming that all supplied components have the same units and must thus be scaled by the scale factors.";
Rank::usage="Returns the rank of the tensor";
Physical::usage="Returns the tensor components scaled as physcial components";
ContravariantQ::usage="Checks whether a given tensor is purely contravariant. The function may also check, whether a given index is contravariant.";
CovariantQ::usage="Checks whether a given tensor is purely covariant. The function may also check, whether a given index is covariant.";
MixedQ::usage="Checks whether a given tensor is of mixed type";
ScalarQ::usage="Checks whether a given tensor is a scalar";


(* ::Subsubsection:: *)
(*Basic Tensor Operations*)


RaiseIndex::usage="Raises the given index";
RaiseIndices::usage="Raises the given indices. All indices are raised, if no list is supplied.";
LowerIndex::usage="Lowers the given index";
LowerIndices::usage="Lowers the given indices. All indices are lowered, if no list is supplied.";


(* ::Subsubsection:: *)
(*Advanced Tensor Functions*)


Contract::usage="Contracts a tensor along the two given indices";
TensorProduct::usage="Returns the tensor product of two tensors";
ScalarProduct::usage="Calculates the scalar product of two tensors";
SymmetricQ::usage="Checks whether the tensor is symmetric in the indices id1 and id2";
DCovariant::usage="Calculates the covariante derivative of the tensor";
Divergence::usage="Calculates the divergence of the tensor field";
LaplaceOperator::usage="Calculates the Laplacian of the tensor field";


(* ::Subsubsection:: *)
(*Auxiliary Functions*)


TestTensor::usage="Creates a test tensor with the given rank consisting of either random numbers of simple symbolic expressions";


(* ::Section:: *)
(*Private*)


Begin["Private`"]


(* ::Subsection:: *)
(*Messages*)


Tensors::ranklow = "The rank of the Tensor is too low";
Tensors::rankhigh = "The rank of the Tensor is too high";
Tensors::rankdiff = "The ranks of the tensors do not match";
Tensors::idxsame = "The indices are the same";


(* ::Subsection:: *)
(*Coordinate System*)


CalculateCoordinatesData[system_,var_]:=Module[
		{coords,assum,trans,basis,metric,m,k,i,j,MetricUU,MetricDD,dim},

	(* extract basic data *)
	coords=VectorAnalysis`Coordinates[system];
	var["Variables"]=coords;
	assum=Apply[And,VectorAnalysis`CoordinateRanges[system]];
	var["Assumptions"]=assum;
	dim=Length[coords];
	var["Dimension"]=dim;
	
	(* calculate the metric tensor *)
	trans=VectorAnalysis`CoordinatesToCartesian[coords,system];
	basis=Table[D[trans[[l]],j],{j,coords},{l,Length[coords]}];
	MetricDD=Simplify[
		Table[basis[[l]].basis[[j]],{l,Length[coords]},{j,Length[coords]}]
		,assum
	];
	var["MetricDD"]=MetricDD;
	MetricUU=Inverse[MetricDD];
	var["MetricUU"]=MetricUU;
	var["ScaleFactors"]=FullSimplify[Sqrt[Diagonal[var["MetricDD"]]],assum];

	(* calculate the Christoffel symbols *)
	var["ChristoffelValues"]=Table[
		FullSimplify[Sum[
			MetricUU[[k,m]]/2*(
				D[MetricDD[[m,i]],coords[[j]]]
				+ D[MetricDD[[j,m]],coords[[i]]]
				- D[MetricDD[[i,j]],coords[[m]]]
			)
		,{m,dim}],assum]
		,{k,dim},{i,dim},{j,dim}
	];
	
	(* return construct *)
	var
];


GetCoordinatesData[coordsys_:Automatic]:=Module[{system},

	(* determine coordinate system to use *)
	system=If[coordsys===Automatic,
		VectorAnalysis`CoordinateSystem
	,(* else *)
		coordsys
	];
	

	(* add the variables to the coordsys *)
	If[Head[system]===Symbol,
		system=system[Apply[Sequence,VectorAnalysis`Coordinates[system]]];
	];

	(* get its properties either from cache or calculate them *)
	If[VectorQ[Tensors`Private`Cache[system]["Variables"]],
		Tensors`Private`Cache[system]
	,(* else: calculate the values *)
		CalculateCoordinatesData[system,Tensors`Private`Cache[system]]
	]
];


(*GetBaseVector[id_Integer,coordsys_:Automatic]*)


GetCoordinates[coordsys_:Automatic]:=GetCoordinatesData[coordsys]["Variables"];
GetMetricTensor[coordsys_:Automatic]:=Tensor[{False,False},GetCoordinatesData[coordsys]["MetricDD"]];
GetScaleFactors[coordsys_:Automatic]:=GetCoordinatesData[coordsys]["ScaleFactors"];
GetChristoffelSymbol[ids_,coordsys_:Automatic]:=GetCoordinatesData[coordsys]["ChristoffelValues"][[ids]];


(* ::Subsection:: *)
(*Basic functions*)


MakeTensor[data_,contravariant_:True]:=
	If[Head[data]===Tensor,
		data
	, (* else: must be converted to Tensor *)
		If[ArrayQ[data],
			Tensor[
				ConstantArray[contravariant,ArrayDepth[data]],
				data
			]
		,(* else: must be scalar *)
			Tensor[{},data]
		]
	];


ContravariantTensor[data_]:=MakeTensor[data,True];
CovariantTensor[data_]:=MakeTensor[data,False];


PhysicalTensor[data_,coordsys_:Automatic]:=Module[{sf,scale},
	If[ArrayQ[data],
		(* Scale the components *)
		sf=GetCoordinatesData[coordsys]["ScaleFactors"];
		scale=Outer[
			Times,
			Apply[Sequence,ConstantArray[sf,ArrayDepth[data]]]
		];
		MakeTensor[data/scale]
	,(* else *)
		MakeTensor[data]
	]
];


Rank[tensor_Tensor]:=Length[tensor[[1]]];


Unprotect[System`Dimensions];
System`Dimensions[tensor_Tensor]:=
	If[Head[tensor[[2]]]===List,
		Dimensions[tensor[[2]]]
	, (* else *)
		{}
	];
Protect[System`Dimensions];


ContravariantQ[tensor_Tensor,id_:All]:=If[
	id===All,
	Apply[And,tensor[[1]]],
	tensor[[1,id]]
];
CovariantQ[tensor_Tensor,id_:All]:=If[
	id===All,
	Apply[Nor,tensor[[1]]],
	Not[tensor[[1,id]]]
];
MixedQ[tensor_Tensor]:=Nor[ContravariantQ[tensor],CovariantQ[tensor]];


ScalarQ[tensor_Tensor]:=Rank[tensor]===0;


Unprotect[System`VectorQ];
System`VectorQ[tensor_Tensor]:=Rank[tensor]===1;
Protect[System`VectorQ];


Unprotect[System`MatrixQ];
System`MatrixQ[tensor_Tensor]:=Rank[tensor]===2;
Protect[System`MatrixQ];


(* ::Subsection:: *)
(*Index operations*)


TensorMetricMultiplication[tensorData_,metric_,id_,coordsys_:Automatic]:=Module[{k,l,coords},
	coords=GetCoordinatesData[coordsys];
	If[id==1,
		Table[
			Sum[tensorData[[k]]*metric[[k,l]],{k,coords["Dimension"]}]
		,{l,coords["Dimension"]}]
	,(* else: id>1 *)
		Map[
			Table[
				Sum[#[[k]]*metric[[k,l]],{k,coords["Dimension"]}]
			,{l,coords["Dimension"]}]&
		,tensorData,id-1]
	]
];


RaiseIndex[tensor_Tensor,id_Integer,coordsys_:Automatic]:=Module[{metric},
	If[ContravariantQ[tensor,id],
		tensor
	, (* else: raise the index *)
		metric=GetCoordinatesData[coordsys]["MetricUU"];
		Tensor[
			ReplacePart[tensor[[1]],id->True],
			TensorMetricMultiplication[tensor[[2]],metric,id]
		]
	]
];
RaiseIndices[tensor_Tensor,ids_List,coordsys_:Automatic]:=
	Fold[RaiseIndex[#1,#2,coordsys]&,tensor,ids];
RaiseIndices[tensor_Tensor,coordsys_:Automatic]:=
	RaiseIndices[tensor,Range[Rank[tensor]],coordsys]/;Not[Head[coordsys]===List];


LowerIndex[tensor_Tensor,id_Integer,coordsys_:Automatic]:=Module[{metric},
	If[CovariantQ[tensor,id],
		tensor
	,(* else: lower the index *)
		metric=GetCoordinatesData[coordsys]["MetricDD"];
		Tensor[
			ReplacePart[tensor[[1]],id->False],
			TensorMetricMultiplication[tensor[[2]],metric,id]
		]
	]
];
LowerIndices[tensor_Tensor,ids_List,coordsys_:Automatic]:=
	Fold[LowerIndex[#1,#2,coordsys]&,tensor,ids];
LowerIndices[tensor_Tensor,coordsys_:Automatic]:=
	LowerIndices[tensor,Range[Rank[tensor]],coordsys]/;Not[Head[coordsys]===List];


(* Returns the components of the contravariant version of the tensor *)
Unprotect[System`Normal];
System`Normal[tensor_Tensor,coordsys_:Automatic]:=RaiseIndices[tensor,coordsys][[2]];
Protect[System`Normal];


Physical[tensor_Tensor,coordsys_:Automatic]:=Module[{scale,sf},
	If[ScalarQ[tensor],
		tensor[[2]]
	,(* else *)
		(* Scale the components *)
		sf=GetCoordinatesData[coordsys]["ScaleFactors"];
		scale=Outer[
			Times,
			Apply[Sequence,ConstantArray[sf,Rank[tensor]]]
		];
		RaiseIndices[tensor,coordsys][[2]]*scale
	]
];


(* ::Subsection:: *)
(*Arithmetic functions*)


Unprotect[System`Equal];
Equal[t1_Tensor,t2_Tensor]:=Module[{t=t1,k},
	If[Rank[t1]===Rank[t2],
		Table[
			t=If[ContravariantQ[t2,k],RaiseIndex[t,k],LowerIndex[t,k]];
			,{k,Rank[t2]}
		];
		Simplify[t[[2]]==t2[[2]],GetCoordinatesData[]["Assumptions"]]===True
	,(* else *)
		False
	]
];
Protect[System`Equal];


Unprotect[System`Unequal];
Unequal[t1_Tensor,t2_Tensor]:=Module[{t=t1,k},
	If[Rank[t1]===Rank[t2],
		Table[
			t=If[ContravariantQ[t2,k],RaiseIndex[t,k],LowerIndex[t,k]];
			,{k,Rank[t2]}
		];
		Not[Simplify[t[[2]]==t2[[2]],GetCoordinatesData[]["Assumptions"]]]
	,(* else *)
		True
	]
];
Protect[System`Unequal];


OnlyScalarsQ[list__]:=Module[{expr},
	expr=Map[
		ArrayQ[#]||(Head[#]===Tensor)&
	,List[list]];
	Fold[Nor,False,expr]	
];
Unprotect[System`Times];
System`Times[numbers__/;OnlyScalarsQ[numbers],tensor_Tensor]:=
	Tensor[tensor[[1]],Times[numbers]*tensor[[2]]];
Protect[System`Times];


PlusBinary[a_Tensor,b_Tensor]:=
	If[a[[1]]=!=b[[1]],
		Message[Tensors::rankdiff];
		Throw[Indeterminate];
	,(* else *)
		Tensor[a[[1]],a[[2]]+b[[2]]]
	];

Unprotect[System`Plus];
System`Plus[tensors__Tensor]:=Module[{res,length=Length[{tensors}]},

	res=Catch[Which[
		length==1,Identity[tensors],
		length==2,PlusBinary[tensors],
		True,Fold[PlusBinary,First[{tensors}],Rest[{tensors}]]
	]];
	If[res===Indeterminate,
		HoldForm[Plus[tensors]]
	,
		res
	]
];
Protect[System`Plus];


Unprotect[System`Minus];
System`Minus[tensor_Tensor]:=-1*tensor;
Protect[System`Minus];


Contract[tensor_Tensor,id1_Integer,id2_Integer,coordsys_:Automatic]:=Module[{ids,data},
	
	(* Check input *)
	If[id1==id2,
		Message[Tensors::idxsame];
		Return[HoldForm[Contract[tensor,id1,id2,coordsys]]];
	];
	If[Max[id1,id2]>Rank[tensor],
		Message[Tensors::ranklow];
		Return[HoldForm[Contract[tensor,id1,id2,coordsys]]];
	];

	(* Adjust type of indices *)
	data=Which[
		ContravariantQ[tensor,id1]&&ContravariantQ[tensor,id2],
		LowerIndex[tensor,id2,coordsys][[2]],

		CovariantQ[tensor,id1]&&CovariantQ[tensor,id2],
		RaiseIndex[tensor,id2,coordsys][[2]],

		True,tensor[[2]]
	];

	(* Reorder the indices, such that id1 and id2 are the first *)
	ids=Range[Rank[tensor]];
	ids=Join[{id1,id2},Delete[ids,{{id1},{id2}}]];
	(* Apply inverse permutation *)
	data=Transpose[data,Ordering[ids]];
	(* Do the contraction *)
	data=Tr[data,Plus,2];
	(* Return the contracted tensor *)
	Tensor[Delete[tensor[[1]],{{id1},{id2}}],data]
];


TensorProductBinary[tensor1_Tensor,tensor2_Tensor]:=
	Which[
		ScalarQ[tensor1],tensor1[[2]]*tensor2,
		ScalarQ[tensor2],tensor2[[2]]*tensor1,
		True,Tensor[
			Join[tensor1[[1]],tensor2[[1]]],
			Outer[Times,tensor1[[2]],tensor2[[2]]]
		]
	];

Unprotect[NonCommutativeMultiply];
NonCommutativeMultiply[tensors___Tensor]:=
	Which[
		Length[{tensors}]==0,Tensor[{},1],
		Length[{tensors}]==1,Identity[tensors],
		Length[{tensors}]==2,TensorProductBinary[tensors],
		True,Fold[TensorProductBinary,Tensor[{},1],{tensors}]
	];
TensorProduct=NonCommutativeMultiply;
Protect[NonCommutativeMultiply];


ScalarProduct[vector1_Tensor?VectorQ,vector2_Tensor?VectorQ,coordsys_:Automatic]:=Module[{data},
	data=If[
		ContravariantQ[vector1]
		,LowerIndex[vector2,1,coordsys][[2]]
		,RaiseIndex[vector2,1,coordsys][[2]]
	];
	Tensor[{},Dot[vector1[[2]],data]]
];


Unprotect[System`Tr];
System`Tr[tensor_Tensor,coordsys_:Automatic]:=Module[{r=Rank[tensor]},
	If[r<2,
		Message[Tensors::ranklow];
		HoldForm[Tr[tensor,coordsys]]
	, (* else *)
		Contract[tensor,r-1,r,coordsys]
	]
];
Protect[System`Tr];


SymmetricQ[tensor_Tensor,id1_Integer,id2_Integer]:=Module[{data},
	If[Max[id1,id2]>Rank[tensor],
		Message[Tensors::ranklow];
		Return[HoldForm[SymmetricQ[tensor,id1,id2]]];
	];
	data=Transpose[tensor[[2]],ReplacePart[Range[Rank[tensor]],{id1->id2,id2->id1}]];
	tensor[[2]]===data
];


Unprotect[System`SymmetricMatrixQ];
System`SymmetricMatrixQ[matrix_Tensor?MatrixQ]:=
	SymmetricMatrixQ[matrix[[2]]];
Protect[System`SymmetricMatrixQ];


Unprotect[System`Norm];
System`Norm[vector_Tensor?VectorQ,coordsys_:Automatic]:=
	Sqrt[ScalarProduct[vector,vector,coordsys][[2]]];
Protect[System`Norm];


Unprotect[System`Normalize];
System`Normalize[vector_Tensor?VectorQ,coordsys_:Automatic]:=
	vector/Norm[vector,coordsys];
Protect[System`Normalize];


(* ::Subsection:: *)
(*Calculus*)


ContractArray[array_List,id1_Integer,id2_Integer]:=Module[{ids,data},
	ids=Range[ArrayDepth[array]];
	ids=Join[{id1,id2},Delete[ids,{{id1},{id2}}]];
	(* Apply inverse permutation *)
	data=Transpose[array,Ordering[ids]];
	(* Do the contraction *)
	Tr[data,Plus,2]
]	


MultiplyArrays[array1_List,array2_List,id1_Integer,id2_Integer]:=
	ContractArray[Outer[Times,array1,array2],id1,ArrayDepth[array1]+id2];


DCovariant[tensor_Tensor,coordsys_:Automatic]:=Module[
		{coords,data,rank=Rank[tensor],values=tensor[[2]],c,r},

	coords=GetCoordinatesData[coordsys];

	(* Calculate the components of the covariant derivative *)
	data=Table[
		D[values,coords["Variables"][[c]]]
		+Sum[
			If[ContravariantQ[tensor,r],
				Transpose[
					MultiplyArrays[coords["ChristoffelValues"][[All,c,All]],values,2,r]
					,Join[{r},Range[1,r-1],Range[r+1,rank]]
				]
			, (* else: covariant *)
				-Transpose[
					MultiplyArrays[coords["ChristoffelValues"][[All,c,All]],values,1,r]
					,Join[{r},Range[1,r-1],Range[r+1,rank]]
				]
			]
			,{r,rank}
		]
		,{c,coords["Dimension"]}
	];
	data=Simplify[data,coords["Assumptions"]];

	(* Move dimension of the derivative to the end *)
	data=Transpose[data,Prepend[Range[rank],rank+1]];
	Tensor[Append[tensor[[1]],False],data]
];


Unprotect[Gradient];
Gradient[tensor_Tensor,coordsys_:Automatic]:=
	RaiseIndex[DCovariant[tensor,coordsys],Rank[tensor]+1]
Protect[Gradient];


Divergence[tensor_Tensor,coordsys_:Automatic]:=Module[{r=Rank[tensor]},
	Contract[DCovariant[tensor,coordsys],r,r+1,coordsys]
];


LaplaceOperator[tensor_Tensor,coordsys_:Automatic]:=
	Divergence[Gradient[tensor,coordsys],coordsys];


(* ::Subsection:: *)
(*Test Cases*)


TestScalar[depth_:0,coordsys_:Automatic]:=Module[{p},
	p=depth*RandomReal[];
	FullSimplify[Which[
		p<.1,RandomChoice[{#1+#2&,#1*#2&}]@@{TestScalar[depth+1],TestScalar[depth+1]},
		p<.2,RandomChoice[{Exp[#]&,#^2&,Sqrt[#]&}]@TestScalar[depth+1],
		True,RandomChoice[Join[{1,2},GetCoordinatesData[coordsys]["Variables"]]]
	],GetCoordinatesData[coordsys]["Assumptions"]]
];


TestTensorData[rank_Integer,depth_:0,coordsys_:Automatic]:=Module[{k},
	If[rank==0,
		TestScalar[depth,coordsys]
	,(* else: rank > 0 *)
		Table[TestTensorData[rank-1,depth,coordsys],{k,GetCoordinatesData[coordsys]["Dimension"]}]
	]
];


TestTensor[rank_Integer:0,Numerical_:True,depth_:0,coordsys_:Automatic]:=
	If[Numerical,
		Tensor[
			ConstantArray[True,rank],
			RandomReal[{0,1},ConstantArray[GetCoordinatesData[coordsys]["Dimension"],rank]]
		]
	,(* else: not numerical *)
		Tensor[ConstantArray[True,rank],TestTensorData[rank,depth,coordsys]]
	];


(* ::Section::Closed:: *)
(*End Package*)


End[(* Private *)];
EndPackage[];
