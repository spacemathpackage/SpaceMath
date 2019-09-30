(* ::Package:: *)

(* ::Input:: *)
(*THIS PROGRAM EVALUATE THE \[Kappa]-FACTOR,from (*https://arxiv.org/abs/1809.10733*);*)


(* ::Input:: *)
(**)


\[Tau]fi::usage = "xcvbnt";
\[Tau]fj::usage = "xcvbn";
WidthHff::usage = "xcvbn";
ft::usage = "xcvbn";
fb::usage = "xcvbn";
gt::usage = "xcvbn";
gb::usage = "xcvbn";
At::usage = "xcvbn";
Ab::usage = "xcvbn";
Ft::usage = "xcvbn";
Fb::usage = "xcvbn";
AHgg::usage = "xcvbn";
WidthHgg::usage = "xcvbn";
Aht::usage = "xcvbn";
Ahb::usage = "xcvbn";
Af::usage = "xcvbn";
fW::usage = "xcvbn";
gW::usage = "xcvbn";
AW::usage = "xcvbn";
FW::usage = "xcvbn";
AhW::usage = "xcvbn";
fH::usage = "xcvbn";
gH::usage = "xcvbn";
AH::usage = "xcvbn";
FH::usage = "xcvbn";
AHc::usage = "xcvbn";
Ahgaga::usage = "xcvbn";
WidthHgaga::usage = "xcvbn";
RTW::usage = "xcvbn";
RTZ::usage = "xcvbn";
WidthHWW::usage = "xcvbn";
WidthHZZ::usage = "xcvbn";
kb::usage = "xcvbn";
ktau::usage = "xcvbn";
ktop::usage = "xcvbn";
kW::usage = "xcvbn";
kZ::usage = "xcvbn";
kgaga::usage= "kgaga";
kgluglu::usage = "xcvbn"
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


Begin["`Package`"]

End[]

Begin["`KappaXX`Private`"]



(* ::Input:: *)
(*Scalar boson decays into fermion pair;*)


(* ::Input:: *)
(*Definitions;*)


\[Tau]fi[mi_,mS_] := (2 mi/mS)^2
\[Tau]fj[mj_,mS_] := (2 mj/mS)^2


(* ::Input:: *)
(*Decay width of the Scalar boson into fermion pair;*)


(* ::Input:: *)
(**)


WidthHff[ghfifj_, Nc_, mi_, mj_,mS_] := (((ghfifj^2) Nc mS)/(128 \[Pi]))*((4-(Sqrt[\[Tau]fi[mi,mS]]+Sqrt[\[Tau]fj[mj,mS]])^2)^(3/2)) (Sqrt[(4-(Sqrt[\[Tau]fi[mi,mS]]-Sqrt[\[Tau]fj[mj,mS]])^2)])


(* ::Input:: *)
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


(* ::Input:: *)
(*Scalar boson decay into gluon pair at one-loop level;*)


(* ::Input:: *)
(*Definitions;*)


(* ::Input:: *)
(**)


ft[mS_]:=-(1/4) (Log[(1+Sqrt[1-(4mt^2)/mS^2])/(1-Sqrt[1-(4mt^2)/mS^2])]-I \[Pi])^2;
fb[mS_]:=-(1/4) (Log[(1+Sqrt[1-(4mb^2)/mS^2])/(1-Sqrt[1-(4mb^2)/mS^2])]-I \[Pi])^2;
gt[mS_]:=ArcSin[1/Sqrt[(4mt^2)/mS^2]]^2;
gb[mS_]:=ArcSin[1/Sqrt[(4mb^2)/mS^2]]^2;


(* ::Input:: *)
(**)


At[mS_]:=If[((4mt^2)/mS^2)>=1,gt[mS],ft[mS]];
Ab[mS_]:=If[((4mb^2)/mS^2)>=1,gb[mS],fb[mS]];


(* ::Input:: *)
(**)


Ft[mS_]:=-2 ((4 mt^2)/mS^2) (1+(1-(4mt^2)/mS^2)*At[mS]);
Fb[mS_]:=-2 ((4 mb^2)/mS^2) (1+(1-(4mb^2)/mS^2)*Ab[mS]);


(* ::Input:: *)
(**)


AHgg[ghtt_,ghbb_,mS_]:=2*mW ((ghtt/mt Ft[mS])+(ghbb/mb Fb[mS]))(*We consider the contribution of bottom and top quarks inside the loop*)


(* ::Input:: *)
(*Decay width of the Scalar boson into gluon pair;*)


(* ::Input:: *)
(**)


WidthHgg[ghtt_,ghbb_,mS_]:=((\[Alpha]s^2 mS^3)/(512 \[Pi]^3 mW^2))*Abs[AHgg[ghtt,ghbb,mS]]^2


(* ::Input:: *)
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


(* ::Input:: *)
(*Higgs boson decay into photon pair;*)


(* ::Input:: *)
(**)


{Qt=2/3,Qb=-1/3};


(* ::Input:: *)
(*(*Main fermion contribution come from top and bottom quark*)*)


Aht[ghtt_,mS_]:=6 mW  Qt^2 (ghtt/mt) Ft[mS]
Ahb[ghbb_,mS_]:=6 mW  Qb^2 (ghbb/mb) Fb[mS]
Af[ghtt_,ghbb_,mS_]:=Aht[ghtt,mS]+Ahb[ghbb,mS]
(**)


(* ::Input:: *)
(*(*W contribution*)*)


fW[mS_]:=-(1/4) (Log[(1+Sqrt[1-(4mW^2)/mS^2])/(1-Sqrt[1-(4mW^2)/mS^2])]-I \[Pi])^2;
gW[mS_]:=ArcSin[1/Sqrt[(4mW^2)/mS^2]]^2;
AW[mS_]:=If[((4mW^2)/mS^2)>=1,gW[mS],fW[mS]];
FW[mS_]:=2+(3((4mW^2)/mS^2))+(3((4mW^2)/mS^2)(2-((4mW^2)/mS^2)))AW[mS];
AhW[ghWW_,mS_]:=(ghWW/mW) FW[mS]


(* ::Input:: *)
(*Charged scalar contribution*)


fH[mCH_,mS_]:=-(1/4) (Log[(1+Sqrt[1-(4mCH^2)/mS^2])/(1-Sqrt[1-(4mCH^2)/mS^2])]-I \[Pi])^2;
gH[mCH_,mS_]:=ArcSin[1/Sqrt[(4mCH^2)/mS^2]]^2;
AH[mCH_,mS_]:=If[((4mCH^2)/mS^2)>=1,gH[mCH,mS],fH[mCH,mS]];
(**)
FH[mCH_,mS_]:=((4mCH^2)/mS^2)(1-((4mCH^2)/mS^2)*AH[mCH,mS])
AHc[gCH_,mCH_,mS_]:=mW*gCH/mCH^2 FH[mCH,mS]


(* ::Input:: *)
(**)


Ahgaga[ghtt_,ghbb_,ghWW_,mS_,gCH_,mCH_]:=Af[ghtt,ghbb,mS]+AhW[ghWW,mS]+AHc[gCH,mCH,mS]


(* ::Input:: *)
(*Decay width of Scalar boson into photon-photon*)


WidthHgaga[ghtt_,ghbb_,ghWW_,gCH_,mCH_,mS_]:=(((\[Alpha]em^2) (mS^3))/(1024 \[Pi]^3 mW^2))*Abs[Ahgaga[ghtt,ghbb,ghWW,mS,gCH,mCH]]^2


(* ::Input:: *)
(**)


(* ::Input:: *)
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


(* ::Input:: *)
(*Scalar boson decay into vector pair;*)


(* ::Input:: *)
(*Definitions;*)


(* ::Input:: *)
(**)


RTW[mS_]:=-(((1-mW^2/mS^2) (47 (mW^4/mS^4)-(13 mW^2/mS^2)+2))/(2 mW^2/mS^2))-3/2 (4 (mW^4/mS^4)-6 (mW^2/mS^2)+1)*(Log[mW^2/mS^2])+
(3 (20 (mW^4/mS^4)-8 (mW^2/mS^2)+1))/Sqrt[4 (mW^2/mS^2)-1]*ArcCos[(3 (mW^2/mS^2)-1)/(2 (mW^3/mS^3))];


(* ::Input:: *)
(**)


RTZ[mS_]:=RTW[mS]/.mW-> mZ;


(* ::Input:: *)
(**)


\[Delta]Z=7-(40/(3 SW^2))+160/(9 SW^4);


(* ::Input:: *)
(*Decay width of Higgs boson into WW pair;*)


(* ::Input:: *)
(**)


WidthHWW[ghWW_,mS_]:=((ghWW^2) mS)/(512 (\[Pi]^3) (mW^4)) RTW[mS]
(**)


(* ::Input:: *)
(*Decay width of Scalar boson into ZZ pair;*)


(* ::Input:: *)
(**)


WidthHZZ[ghZZ_,mS_]:=((ghZZ^2) mS)/(2048 (\[Pi]^3) mZ^4) \[Delta]Z RTZ[mS]


(* ::Input:: *)
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


(* ::Input:: *)
(*\[Kappa]-factors;*)


(* ::Input:: *)
(*Kappab;*)


kb[ghbb_]:=Sqrt[WidthHff[ghbb, 3, mb, mb,125]/WidthHff[g mb/(2 mW), 3, mb, mb,125]]


(* ::Input:: *)
(*Kappatau;*)


ktau[ghtautau_]:=Sqrt[WidthHff[ghtautau, 1, mtau, mtau,125]/WidthHff[g mtau/(2 mW), 1, mtau, mtau,125]]


(* ::Input:: *)
(*Kappatop;*)


ktop[ghtt_]:=Sqrt[WidthHff[ghtt, 3, mt, mt,125]/WidthHff[g mt/(2 mW), 3, mt, mt,125]]


(* ::Input:: *)
(*KappaW;*)


kW[ghWW_]:=Sqrt[WidthHWW[ghWW,125]/WidthHWW[gw*mW,125]]


(* ::Input:: *)
(*KappaZ;*)


kZ[ghZZ_]:=Sqrt[WidthHZZ[ghZZ,125]/WidthHZZ[gz*mZ,125]]


(* ::Input:: *)
(*Kappa\[Gamma];*)


kgaga[ghtt_,ghbb_,ghWW_,gCH_,mCH_]:=Sqrt[WidthHgaga[ghtt,ghbb,ghWW,gCH,mCH,125]/WidthHgaga[g mt/(2 mW),g mb/(2 mW),gw*mW,0,mCH,125]]


(* ::Input:: *)
(*Kappag;*)


kgluglu[ghtt_,ghbb_]:=Sqrt[WidthHgg[ghtt,ghbb,125]/WidthHgg[g mt/(2 mW),g mb/(2 mW),125]]


(* ::Input:: *)
(*(************************************************************************************************************************************************************************************************************************************************************************************************************************)*)


End[]
