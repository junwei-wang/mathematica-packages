(* ::Package:: *)

(* ::Input:: *)
(*(* How to write a Mathematica package https://www.12000.org/my_notes/how_to_write_package_in_mathematica/index.htm *)*)


(* ::Input::Initialization:: *)
BeginPackage["bf`"]
Unprotect @@Names["bf`*"]
ClearAll @@Names["bf`*"]

BooleanFunctionQ::usage="BooleanFunctionQ[f] is a list a Boolean function"
BooleanFunctionNumberOfVariable::usage="BooleanFunctionNumberOfVariable[f]: return the number of variables"
BooleanFunctionWalshTransform::usage="BooleanFunctionWalshTransform[f,w]: Walsh transform of f on point w"
BooleanFunctionNonlinearity::usage="BooleanFunctionNonlinearity[f]: Nonlinearity of f"

Begin["`Priavte`"]

BooleanFunctionQ[f_]:=(IntegerQ[Log2[Length[f]]]&&(Sort[DeleteDuplicates[f]]=={0,1}));
BooleanFunctionNumberOfVariable[f_]:=Log2[Length[f]];
BooleanFunctionWalshTransform[f_,w_]:=Module[{size,exps},
size=Length[f];
exps=BitXor[f[[#+1]],Mod[Count[IntegerDigits[BitAnd[w,#],2],1],2]]&/@Range[0,size-1];
2*Count[exps,0]-size
];
BooleanFunctionNonlinearity[f_]:=(Length[f]-Max@Abs[BooleanFunctionWalshTransform[f,#]&/@Range[0,Length[f]-1]])/2;

End[]
Protect@@Names["bf`*"]
EndPackage[]
