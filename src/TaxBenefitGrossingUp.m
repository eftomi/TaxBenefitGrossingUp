(* ::Package:: *)

(* :Title: TaxBenefitGrossingUp -- package for personal income tax calculations, i.e. gross <===> net *)

(* :Author: Toma\[ZHacek] Turk, Mitja \[CapitalCHacek]ok *)

(* :Summary: Package allows for gross personal income estimation (GrossIncome[]) for individual on the basis of net personal income 
             and relevant personal income tax (PIT) parameters such as tax allowances, PIT schedule, tax credit system and 
             social security contributions. Besides this, package exposes NetIncome[] function as a GrossIncome[] reverse.
             Primary goal of the package is to enable data imputation of gross personal income in microdatabases used in 
             macroeconomic simulations where gross personal income computation is not trivial. *)

(* :Package Version: 1.0.0 *)

(* :Mathematica Version: 8.0.1.0 *)

(* :Keywords: Personal Income Tax, Grossing Up, Net Tax, Data Imputation, Microsimulations  *)


BeginPackage["TaxBenefitGrossingUp`"]

NetIncome::usage="NetIncome[\*StyleBox[\"GrossIncome\",
FontSlant->Italic],\*StyleBox[\"TaxAllowancesAmount\",
FontSlant->Italic],\*StyleBox[\"PersonalIncomeTaxSchedule\",
FontSlant->Italic], \*StyleBox[\"opts\",
FontSlant->Italic]] gives net income estimated from a given gross income, amount of tax allowances and tax schedule. 
Tax schedule should be provided as a list of tax brackets {{\*StyleBox[\"lo\",
FontSlant->Italic], \*StyleBox[\"up\",
FontSlant->Italic], \*StyleBox[\"marginal rate\",
FontSlant->Italic]}, ...} where each tax bracket is defined by its lower and upper margins and tax rate.

There are twelve possible combinations of \*StyleBox[\"opts\",
FontSlant->Italic] depending on social security contribution rules and tax credits system.
Social security contributions can be given by:
- schedule for social security contributions: SocialSecuritySchedule->{{\*StyleBox[\"lo\",
FontSlant->Italic], \*StyleBox[\"up\",
FontSlant->Italic], \*StyleBox[\"marginal rate\",
FontSlant->Italic]}, ...}
- social security contributions as gross income proportion: SocialSecurityProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- social security contributions in absolute amount: SocialSecurityAbsoluteAmount->\*StyleBox[\"a\",
FontSlant->Italic]
Tax credit system can be given as:
- proportion of initial tax: TaxCreditInitialPITProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- proportion of gross income: TaxCreditGrossIncomeProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- in absolute amount: TaxCreditAbsoluteAmount->\*StyleBox[\"a\",
FontSlant->Italic]
Tax credit can be ommited from the options if the taxation system does not allow for it."

GrossIncome::usage="GrossIncome[\*StyleBox[\"NetIncome\",
FontSlant->Italic],\*StyleBox[\"TaxAllowancesAmount\",
FontSlant->Italic],\*StyleBox[\"PersonalIncomeTaxSchedule\",
FontSlant->Italic], \*StyleBox[\"opts\",
FontSlant->Italic]] gives gross income estimated from a given net income, amount of tax allowances and tax schedule. 
Tax schedule should be provided as a list of tax brackets {{\*StyleBox[\"lo\",
FontSlant->Italic], \*StyleBox[\"up\",
FontSlant->Italic], \*StyleBox[\"marginal rate\",
FontSlant->Italic]}, ...} where each tax bracket is defined by its lower and upper margins and tax rate.

There are twelve possible combinations of \*StyleBox[\"opts\",
FontSlant->Italic] depending on social security contribution rules and tax credits system.
Social security contributions can be given by:
- schedule for social security contributions: SocialSecuritySchedule->{{\*StyleBox[\"lo\",
FontSlant->Italic], \*StyleBox[\"up\",
FontSlant->Italic], \*StyleBox[\"marginal rate\",
FontSlant->Italic]}, ...}
- social security contributions as gross income proportion: SocialSecurityProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- social security contributions in absolute amount: SocialSecurityAbsoluteAmount->\*StyleBox[\"a\",
FontSlant->Italic]
Tax credit system can be given as:
- proportion of initial tax: TaxCreditInitialPITProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- proportion of gross income: TaxCreditGrossIncomeProportion->\*StyleBox[\"p\",
FontSlant->Italic]
- in absolute amount: TaxCreditAbsoluteAmount->\*StyleBox[\"a\",
FontSlant->Italic]
Tax credit can be ommited from the options if the taxation system does not allow for it."

NetIncome::arg = "Improper argument `1`."
NetIncome::opt = "Improper option `1`."
NetIncome::args = "Improper options configuration."
NetIncome::fail = "Function called improperly, check arguments and options."

GrossIncome::arg = "Improper argument `1`"
GrossIncome::opt = "Improper option `1`."
GrossIncome::args = "Improper options configuration."
GrossIncome::fail = "Function called improperly, check arguments."

Begin["`Private`"]

(*************************************************************************************************************)
(*** MAIN FUNCTIONS ******************************************************************************************)
(*************************************************************************************************************)

NetIncome[
GrossIncome_,
TaxAllowancesAmount_,
PersonalIncomeTaxSchedule_,
opts:OptionsPattern[]
]:=(
Check[
Catch[
CheckArguments[GrossIncome,TaxAllowancesAmount];
CheckSchedulePIT[PersonalIncomeTaxSchedule];
TaxAllowances=TaxAllowancesAmount;
PITScheduleData=Prepend[PersonalIncomeTaxSchedule,{"Lo","Hi","Tr"}];
CheckOptionsNetIncome[opts];
If[Length[SocSecScheduleData]!=0,SocSecScheduleData=Prepend[SocSecScheduleData,{"Lo","Hi","Sr"}]];
Switch[GetCombination[],
0,Throw[Message[NetIncome::args]],
1,NetIncomeI[GrossIncome, TaxAllowances],
2,NetIncomeII[GrossIncome, TaxAllowances],
3,NetIncomeIII[GrossIncome, TaxAllowances],
4,NetIncomeIV[GrossIncome, TaxAllowances],
5,NetIncomeV[GrossIncome, TaxAllowances],
6,NetIncomeVI[GrossIncome, TaxAllowances],
7,NetIncomeVII[GrossIncome, TaxAllowances],
8,NetIncomeVIII[GrossIncome, TaxAllowances],
9,NetIncomeIX[GrossIncome, TaxAllowances],
10,NetIncomeX[GrossIncome, TaxAllowances],
11,NetIncomeXI[GrossIncome, TaxAllowances],
12,NetIncomeXII[GrossIncome, TaxAllowances]]
],
Message[NetIncome::fail]]
)

GrossIncome[
NetIncome_,
TaxAllowancesAmount_,
PersonalIncomeTaxSchedule_,
opts:OptionsPattern[]
]:=(
Check[
Catch[
CheckArgumentsGross[NetIncome,TaxAllowancesAmount];
CheckSchedulePITGross[PersonalIncomeTaxSchedule];
TaxAllowances=TaxAllowancesAmount;
PITScheduleData=Prepend[PersonalIncomeTaxSchedule,{"Lo","Hi","Tr"}];
CheckOptionsGrossIncome[opts];
NumberOfPITscheduleBrackets=Length[PITScheduleData]-1;
NumberOfSocSecBrackets = Length[SocSecScheduleData];
If[NumberOfSocSecBrackets<=0,
NumberOfSocSecBrackets=1, (* there's no schedule *)
SocSecScheduleData=Prepend[SocSecScheduleData,{"Lo","Hi","Sr"}]]; (* if there's schedule, prepend it *)
Switch[GetCombination[],
0,Throw[Message[GrossIncome::args]],
1,GrossIncomeI[NetIncome, TaxAllowances],
2,GrossIncomeII[NetIncome, TaxAllowances],
3,GrossIncomeIII[NetIncome, TaxAllowances],
4,GrossIncomeIV[NetIncome, TaxAllowances],
5,GrossIncomeV[NetIncome, TaxAllowances],
6,GrossIncomeVI[NetIncome, TaxAllowances],
7,GrossIncomeVII[NetIncome, TaxAllowances],
8,GrossIncomeVIII[NetIncome, TaxAllowances],
9,GrossIncomeIX[NetIncome, TaxAllowances],
10,GrossIncomeX[NetIncome, TaxAllowances],
11,GrossIncomeXI[NetIncome, TaxAllowances],
12,GrossIncomeXII[NetIncome, TaxAllowances]]
],
Message[GrossIncome::fail]]
)

CheckArgumentsNet[GrossIncome_,TaxAllowancesAmount_,PersonalIncomeTaxSchedule_]:=(
If[GrossIncome<0,Throw[Message[NetIncome::arg,"GrossIncome"]]];
If[TaxAllowancesAmount<0,Throw[Message[NetIncome::arg,"TaxAllowancesAmount"]]];
)

CheckSchedulePITNet[Schedule_]:=
If[ListQ[Schedule],
If[Length[DeleteCases[Schedule,{_Integer|_Real|_Rational,_Integer|_Real|_Rational|"-",_Integer|_Real|_Rational},1]]>0,
Throw[Message[NetIncome::arg,"PersonalIncomeTaxSchedule"]]],Throw[Message[NetIncome::arg,"PersonalIncomeTaxSchedule"]]]

CheckScheduleSocSecNet[Schedule_]:=
If[ListQ[Schedule],
If[Length[DeleteCases[Schedule,{_Integer|_Real|_Rational,_Integer|_Real|_Rational|"-",_Integer|_Real|_Rational},1]]>0,
Throw[Message[NetIncome::opt,"SocialSecuritySchedule"]]],Throw[Message[NetIncome::opt,"SocialSecuritySchedule"]]]

CheckArgumentsGross[NetIncome_,TaxAllowancesAmount_,PersonalIncomeTaxSchedule_]:=(
If[GrossIncome<0,Throw[Message[GrossIncome::arg,"GrossIncome"]]];
If[TaxAllowancesAmount<0,Throw[Message[GrossIncome::arg,"TaxAllowancesAmount"]]];
)

CheckSchedulePITGross[Schedule_]:=
If[ListQ[Schedule],
If[Length[DeleteCases[Schedule,{_Integer|_Real|_Rational,_Integer|_Real|_Rational|"-",_Integer|_Real|_Rational},1]]>0,
Throw[Message[GrossIncome::arg,"PersonalIncomeTaxSchedule"]]],Throw[Message[GrossIncome::arg,"PersonalIncomeTaxSchedule"]]]

CheckScheduleSocSecGross[Schedule_]:=
If[ListQ[Schedule],
If[Length[DeleteCases[Schedule,{_Integer|_Real|_Rational,_Integer|_Real|_Rational|"-",_Integer|_Real|_Rational},1]]>0,
Throw[Message[GrossIncome::opt,"SocialSecuritySchedule"]]],Throw[Message[GrossIncome::opt,"SocialSecuritySchedule"]]]

CheckOptionsNetIncome[opts:OptionsPattern[]] := (
SocSecScheduleData=OptionValue[SocialSecuritySchedule];
CheckScheduleSocSecNet[SocSecScheduleData];
If[Not[ListQ[SocSecScheduleData]],Throw[Message[GrossIncome::args]]];
SPropG=OptionValue[SocialSecurityProportion];
SPropG=OptionValue[SocialSecurityProportion];
SAbs=OptionValue[SocialSecurityAbsoluteAmount];
CPropPIT=OptionValue[TaxCreditInitialPITProportion];
CPropG=OptionValue[TaxCreditGrossIncomeProportion];
CAbs=OptionValue[TaxCreditAbsoluteAmount];)

CheckOptionsGrossIncome[opts:OptionsPattern[]] := (
SocSecScheduleData=OptionValue[SocialSecuritySchedule];
CheckScheduleSocSecGross[SocSecScheduleData];
SPropG=OptionValue[SocialSecurityProportion];
SAbs=OptionValue[SocialSecurityAbsoluteAmount];
CPropPIT=OptionValue[TaxCreditInitialPITProportion];
CPropG=OptionValue[TaxCreditGrossIncomeProportion];
CAbs=OptionValue[TaxCreditAbsoluteAmount];)

Options[GrossIncome]=Options[NetIncome]=Options[GetCombination]=Options[CheckOptionsNetIncome]=Options[CheckOptionsGrossIncome]={
SocialSecuritySchedule->{},
SocialSecurityProportion->0,
SocialSecurityAbsoluteAmount->0,
TaxCreditInitialPITProportion->0,
TaxCreditGrossIncomeProportion->0,
TaxCreditAbsoluteAmount->0
};

GetCombination[]:=( 
CWithout=If[CPropPIT+CPropG+CAbs!=0,0,1];
SocSecScheduleDataPresent=If[SocSecScheduleData=={},0,1];
Which[
CWithout*SocSecScheduleDataPresent!=0,1,
CWithout*SPropG!=0,2,
CWithout*SAbs!=0,3,
CPropPIT*SocSecScheduleDataPresent!=0,4,
CPropPIT*SPropG!=0,5,
CPropPIT*SAbs!=0,6,
CPropG*SocSecScheduleDataPresent!=0,7,
CPropG*SPropG!=0,8,
CPropG*SAbs!=0,9,
CAbs*SocSecScheduleDataPresent!=0,10,
CAbs*SPropG!=0,11,
CAbs*SAbs!=0,12,
True, 0 (* no taxation data available *)
]
)

(*************************************************************************************************************)
(*** HELPERS *************************************************************************************************)
(*************************************************************************************************************)
PITSchedule[bracket_,param_]:= (* returns parameters from PIT schedule *)
PITScheduleData[[bracket+1,(Position[PITScheduleData[[1]],param][[1,1]])]];

SocSecSchedule[bracket_,param_]:= (* returns parameters from soc. sec. contr. schedule *)
SocSecScheduleData[[bracket+1,(Position[SocSecScheduleData[[1]],param][[1,1]])]];

PITSchBracket[taxbase_]:=Which[  (* corresponding bracket for a given taxbase *)
taxbase<PITSchedule[1,"Hi"],1,
PITSchedule[2,"Lo"]<=taxbase &&taxbase<PITSchedule[2,"Hi"],2,
PITSchedule[3,"Lo"]<=taxbase,3];

SocSecSchBracket[income_]:=Which[  (* corresponding bracket for a given gross income *)
income<SocSecSchedule[1,"Hi"],1,
SocSecSchedule[2,"Lo"]<=income &&income<SocSecSchedule[2,"Hi"],2,
SocSecSchedule[3,"Lo"]<=income,3];

PITSchTr[taxbase_]:=Which[  (* PIT rate for a given taxbase *)
taxbase<PITSchedule[1,"Hi"],PITSchedule[1,"Tr"],
PITSchedule[2,"Lo"]<=taxbase &&taxbase<PITSchedule[2,"Hi"],PITSchedule[2,"Tr"],
PITSchedule[3,"Lo"]<=taxbase,PITSchedule[3,"Tr"]];

SocSecSchSr[income_]:=Which[  (* soc. sec. contr. rate for a given gross income *)
income<SocSecSchedule[1,"Hi"],SocSecSchedule[1,"Sr"],
SocSecSchedule[2,"Lo"]<=income &&income<SocSecSchedule[2,"Hi"],SocSecSchedule[2,"Sr"],
SocSecSchedule[3,"Lo"]<=income,SocSecSchedule[3,"Sr"]];

PITSchLower[bracket_]:= (* PIT collected for all "lower" brackets *)
Sum [(PITSchedule[i,"Hi"]-PITSchedule[i,"Lo"])PITSchedule[i,"Tr"],{i,1,bracket-1}];
SocSecSchLower[bracket_]:= (* social security contributions for all "lower" brackets *)
Sum [(SocSecSchedule[i,"Hi"]-SocSecSchedule[i,"Lo"])SocSecSchedule[i,"Sr"],{i,1,bracket-1}]
AppendNewData[DB_,Data_]:=Table[Flatten[{DB[[i]],Data}],{i,1,Length[DB]}];

(*************************************************************************************************************)
(*** TAX EQUATIONS ***********************************************************************************)
(*************************************************************************************************************)

(*** calculation of social security contributions under various systems ***)
(* social security contributions defined by schedule *)
(* applicable in I, IV, VII, X *)

CalcSSCSch[GrossIncome_]:=SocSecSchSr[GrossIncome] (GrossIncome-SocSecSchedule[SocSecSchBracket[GrossIncome],"Lo"])+
SocSecSchLower[SocSecSchBracket[GrossIncome]];

(* social security contributions as a proportion of gross income *)
(* applicable in II, V, VIII, XI *)

CalcSSCProp[GrossIncome_]:=SPropG GrossIncome;

(* social security contributions as absolute amount *)
(* applicable in III, VI, IX, XII *)

CalcSSCAbs[]:=SAbs;

(*** calculation of PIT ***)
(* general, without tax credit *)
(* applicable in I, II, III *)

CalcPIT[GrossIncome_,SSC_,TA_]:=(
TaxBase=GrossIncome-SSC-TA;
PITSchTr[TaxBase](TaxBase- PITSchedule[PITSchBracket[TaxBase],"Lo"])+PITSchLower[PITSchBracket[TaxBase]]);

(* tax credit in % of initial PIT *)
(* applicable in IV, V, VI *)

CalcPITCPropPIT[GrossIncome_,SSC_,TA_,CPropPIT_]:=(
TaxBase=GrossIncome-SSC-TA;
InitialPIT=PITSchTr[TaxBase](TaxBase- PITSchedule[PITSchBracket[TaxBase],"Lo"])+PITSchLower[PITSchBracket[TaxBase]];
InitialPIT-CPropPIT InitialPIT);

(* tax credit in % of gross income *)
(* applicable in VII, VIII, IX *)

CalcPITCPropG[GrossIncome_,SSC_,TA_,CPropG_]:=(
TaxBase=GrossIncome-SSC-TA;
InitialPIT=PITSchTr[TaxBase](TaxBase- PITSchedule[PITSchBracket[TaxBase],"Lo"])+PITSchLower[PITSchBracket[TaxBase]];
If[CPropG GrossIncome<InitialPIT,FinalPIT=InitialPIT-CPropG GrossIncome,FinalPIT=0];
FinalPIT);

(* tax credit in absolute amount *)
(* applicable in X, XI, XII *)

CalcPITCAbs[GrossIncome_,SSC_,TA_,CAbs_]:=(
TaxBase=GrossIncome-SSC-TA;
InitialPIT=PITSchTr[TaxBase](TaxBase- PITSchedule[PITSchBracket[TaxBase],"Lo"])+PITSchLower[PITSchBracket[TaxBase]];
If[CAbs<InitialPIT,FinalPIT=InitialPIT-CAbs,FinalPIT=0];
FinalPIT);

(*** functions to calculate net income ***)
(*** type I ***)

NetIncomeI[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCSch[GrossIncome]-CalcPIT[GrossIncome,CalcSSCSch[GrossIncome],TA]);

(*** type II ***)

NetIncomeII[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCProp[GrossIncome]-CalcPIT[GrossIncome,CalcSSCProp[GrossIncome],TA]);

(*** type III ***)

NetIncomeIII[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCAbs[]-CalcPIT[GrossIncome,CalcSSCAbs[],TA]);

(*** type IV ***)

NetIncomeIV[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCSch[GrossIncome]-CalcPITCPropPIT[GrossIncome,CalcSSCSch[GrossIncome],TA,CPropPIT]);

(*** type V ***)

NetIncomeV[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCProp[GrossIncome]-CalcPITCPropPIT[GrossIncome,CalcSSCProp[GrossIncome],TA,CPropPIT]);

(*** type VI ***)

NetIncomeVI[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCAbs[]-CalcPITCPropPIT[GrossIncome,CalcSSCAbs[],TA,CPropPIT]);

(*** type VII ***)

NetIncomeVII[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCSch[GrossIncome]-CalcPITCPropG[GrossIncome,CalcSSCSch[GrossIncome],TA,CPropG]);

(*** type VIII ***)

NetIncomeVIII[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCProp[GrossIncome]-CalcPITCPropG[GrossIncome,CalcSSCProp[GrossIncome],TA,CPropG]);

(*** type IX ***)

NetIncomeIX[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCAbs[]-CalcPITCPropG[GrossIncome,CalcSSCAbs[],TA,CPropG]);

(*** type X ***)

NetIncomeX[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCSch[GrossIncome]-CalcPITCAbs[GrossIncome,CalcSSCSch[GrossIncome],TA,CAbs]);

(*** type XI ***)

NetIncomeXI[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCProp[GrossIncome]-CalcPITCAbs[GrossIncome,CalcSSCProp[GrossIncome],TA,CAbs]);

(*** type XII ***)

NetIncomeXII[GrossIncome_,TA_]:=(
GrossIncome-CalcSSCAbs[]-CalcPITCAbs[GrossIncome,CalcSSCAbs[],TA,CAbs]);

(*** functions to calculate gross income ***)
(*** main functions ***)

GrossIncomeI[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeICore, NetIncomeI, NetIncome, TA];
GrossIncomeII[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeIICore, NetIncomeII, NetIncome, TA];
GrossIncomeIII[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeIIICore, NetIncomeIII, NetIncome, TA];
GrossIncomeIV[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeIVCore, NetIncomeIV, NetIncome, TA];
GrossIncomeV[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeVCore, NetIncomeV, NetIncome, TA];
GrossIncomeVI[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeVICore, NetIncomeVI, NetIncome, TA];
GrossIncomeVII[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeVIICore, NetIncomeVII, NetIncome, TA];
GrossIncomeVIII[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeVIIICore, NetIncomeVIII, NetIncome, TA];
GrossIncomeIX[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeIXCore, NetIncomeIX, NetIncome, TA];
GrossIncomeX[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeXCore, NetIncomeX, NetIncome, TA];
GrossIncomeXI[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeXICore, NetIncomeXI, NetIncome, TA];
GrossIncomeXII[NetIncome_, TA_] := GrossIncomeSearch[GrossIncomeXIICore, NetIncomeXII, NetIncome, TA];

GrossIncomeSearch[GrossIncomeCoreFunction_,NetIncomeFunction_,NetIncome_, TA_] := (
GrossIncomeCandidates=Table[GrossIncomeCoreFunction[NetIncome,TA,pb,sb],
{sb,1,NumberOfSocSecBrackets},
{pb,1,NumberOfPITscheduleBrackets}] ;
NetIncomeCandidates = Table[NetIncomeFunction[GrossIncomeCandidates[[sb,pb]],TA],
{sb,1,NumberOfSocSecBrackets},
{pb,1,NumberOfPITscheduleBrackets}] ;
Max[Table[If[NetIncomeCandidates[[sb,pb]]==NetIncome,GrossIncomeCandidates[[sb,pb]],0],
{sb,1,NumberOfSocSecBrackets},
{pb,1,NumberOfPITscheduleBrackets}
]]
)

(*** core functions with basic formulas ***)
(*** type I ***)

GrossIncomeICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Sr=SocSecSchedule[SSCBracket,"Sr"];
Pr=PITSchedule[PITBracket,"Tr"];
Los=SocSecSchedule[SSCBracket,"Lo"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
SumSSCLo=SocSecSchLower[SSCBracket];
(NetIncome-Lot Pr-Los Sr+Los Pr Sr+SumPITLo+SumSSCLo-Pr SumSSCLo-Pr TA)/((Sr-1) (Pr-1)));

(*** type II ***)

GrossIncomeIICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
(NetIncome-Pr TA-Pr Lot+SumPITLo)/((1-SPropG)-Pr(1-SPropG)));

(*** type III ***)

GrossIncomeIIICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
(NetIncome+SAbs-Pr SAbs-Pr TA-Pr Lot+SumPITLo)/(1-Pr));

(*** type IV ***)

GrossIncomeIVCore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Sr=SocSecSchedule[SSCBracket,"Sr"];
Pr=PITSchedule[PITBracket,"Tr"];
Los=SocSecSchedule[SSCBracket,"Lo"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
SumSSCLo=SocSecSchLower[SSCBracket];
(NetIncome-Sr Los-Pr Lot+CPropPIT Pr Lot+Pr Sr Los-CPropPIT Pr Sr Los-Pr TA+
CPropPIT Pr TA-SumSSCLo (Pr(1-CPropPIT)-1)-(CPropPIT-1) SumPITLo)/ ((1-Sr)(Pr(CPropPIT-1)+1)));

(*** type V ***)

GrossIncomeVCore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
( NetIncome-(1-CPropPIT)(Pr Lot+Pr TA-SumPITLo))/((1- SPropG)(1-Pr+CPropPIT Pr)));

(*** type VI ***)

GrossIncomeVICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
(NetIncome+SAbs+(CPropPIT-1)(Pr Lot+Pr SAbs+Pr TA-SumPITLo))/(1-Pr+CPropPIT Pr));

(*** type VII ***)

GrossIncomeVIICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Sr=SocSecSchedule[SSCBracket,"Sr"];
Pr=PITSchedule[PITBracket,"Tr"];
Los=SocSecSchedule[SSCBracket,"Lo"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
SumSSCLo=SocSecSchLower[SSCBracket];
g1=(NetIncome-Lot Pr-Los Sr+Los Pr Sr+SumPITLo+SumSSCLo-Pr SumSSCLo-Pr TA)/(1+CPropG-Pr-Sr+Pr Sr);
g2=(NetIncome-Sr Los +SumSSCLo)/(1-Sr);
Max[g1,g2]);

(*** type VIII ***)

GrossIncomeVIIICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
g1=(NetIncome-Pr Lot-Pr TA+SumPITLo)/(1+CPropG-SPropG-Pr+SPropG Pr);
g2=NetIncome/(1-SPropG);
Max[g1,g2]);

(*** type IX ***)

GrossIncomeIXCore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
g1=(NetIncome+SAbs-Pr Lot-Pr SAbs-Pr TA+SumPITLo)/(1+CPropG-Pr);
g2=NetIncome+SAbs;
Max[g1,g2]);

(*** type X ***)

GrossIncomeXCore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Sr=SocSecSchedule[SSCBracket,"Sr"];
Pr=PITSchedule[PITBracket,"Tr"];
Los=SocSecSchedule[SSCBracket,"Lo"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
SumSSCLo=SocSecSchLower[SSCBracket];
g1=(-CAbs+NetIncome-Los Sr+SumPITLo+SumSSCLo-Lot Pr+Los Sr Pr-SumSSCLo Pr-TA Pr)/((-1+Sr) (-1+Pr));
g2=(NetIncome-Sr Los +SumSSCLo)/(1-Sr);
Max[g1,g2]);

(*** type XI ***)

GrossIncomeXICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
g1=(NetIncome-CAbs-Pr Lot-Pr TA+SumPITLo)/((SPropG-1)(Pr-1));
g2=NetIncome/(1-SPropG);
Max[g1,g2]);

(*** type XII ***)

GrossIncomeXIICore[NetIncome_,TA_,PITBracket_,SSCBracket_]:=(
Pr=PITSchedule[PITBracket,"Tr"];
Lot=PITSchedule[PITBracket,"Lo"];
SumPITLo=PITSchLower[PITBracket];
g1=(NetIncome-CAbs+SAbs-Pr Lot-Pr SAbs-Pr TA+SumPITLo)/(1-Pr);
g2=NetIncome+SAbs;
Max[g1,g2]);


End[]

EndPackage[]
