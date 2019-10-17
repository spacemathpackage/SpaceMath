(* ::Package:: *)

If[ MemberQ[$Packages,"SpaceMath`"],
	Print["SpaceMath is already loaded! To reload it, please restart the kernel."];
	Abort[]
];

If[ ($VersionNumber < 10.0),
	Print["You need at least Mathematica 10.0 to run SpaceMath. Quitting the Mathematica kernel."];
	Abort[]
];

(*    Find out where SpaceMath is installed    *)
If[ !ValueQ[SpaceMath`$SpaceMathDirectory],
	SpaceMath`$SpaceMathDirectory =
	DirectoryName[$InputFileName]
];

If[ FileNames["*",{SpaceMath`$SpaceMathDirectory}] === {},
	Print["Could not find a SpaceMath installation. Quitting the Mathematica kernel."];
	Clear[SpaceMath`$SpaceMathDirectory];
	Abort[];
];

(*    Set the version number    *)
SpaceMath`$SpaceMathVersion = "1.0";

(*    Set defaults here, not in the config file    *)
If[ !ValueQ[Global`$SpaceMathStartupMessages],
	Global`$SpaceMathStartupMessages = True
];

If[ Global`$SpaceMathStartupMessages=!=False,
	PrintTemporary[Style["Loading SpaceMath from "<>
	SpaceMath`$SpaceMathDirectory, "Text"]]
];

BeginPackage["SpaceMath`"];

SpaceMath::usage =
"For installation notes visit www.feyncalc.org\n
For a list of availabe objects type $FeynCalcStuff, \
which contains a list of all functions and options in StringForm. \
You can get on-line information by ?function, e.g. ?Contract.\n
There are several useful functions for short input, type $SMS for a list of \
short commands. Then type, e.g., ?GA.\n\n
To enable/disable start-up messages, put the line\n
$SpaceMathStartupMessages = True;\n
or\n
$SpaceMathStartupMessages = False;\n
into your \"init.m\" file or into your \"SMConfig.m\" file."

$SpaceMathStuff::usage =
"$FeynCalcStuff is the list of availabe stuff in FeynCalc.";

$SpaceMathVersion::usage =
"$FeynCalcVersion is a string that represents the version of FeynCalc.";

MakeSpaceMathPrivateContext::usage =
"MakeFeynCalcPrivateContext[val] constructs
SpaceMath`Private`val.";

OptionsSelect::usage =
"OptionsSelect[function,opts] returns the option settings of opts \
accepted by function.  When an option occurs several times in opts, the first \
setting is selected";

UseWriteString::usage =
"UseWriteString is an option for SMPrint. If set to True,
the expression is printed via WriteString instead of Print.";

WriteStringOutput::usage =
"UseWriteStringOutput an option for SMPrint. It specifies, to which
stream WriteString should output the expression";

SpaceMath::faerror =
"FeynArts not found or damaged. Please download the FeynArts \
tarball from www.feynarts.de, unpack it to `1` and restart FeynCalc.";

SpaceMath::taerror =
"TARCER*.mx file not found or damaged. Please evaluate the command \
GenerateTarcerMX to create it.";

SpaceMath::phierror =
"PHI failed to load. Please try resintalling FeynCalc.";

SpaceMath::tfadvice =
"You are not using TraditionalForm as the default format type of new \
output cells. Without TraditionalForm FeynCalc cannot use built-in \
typeseting rules that make various objects like Lorentz vectors or \
Dirac matrices look nicer. To change the format type go to \
Edit->Preferences->Evaluation.";

SMMonitor::usage =
"SMMonitor is a simple function that activates Monitor if there
is a notebook interface available and disables it otherwise.";

SMMonitorStub::usage =
"SMMonitorStub is a stub for Monitor when the notebook interface
is not available";

SMDoControl::usage =
"SMDoControl is an option for SMPrint that specifies which variable
is used to control the debugging output of SMPrint. The default value
is $VeryVerbose.";

SMDeclareHeader::usage =
"SMDeclareHeader is an internal FeynCalc function to declare
objects inside an .m file in the same manner as it is done in
the JLink package. It may be used by FeynCalc addons."

Begin["`Private`"]

SetAttributes[SMPrint, HoldRest];

Options[SMPrint] = {
		SMDoControl :> $VeryVerbose,
		UseWriteString -> False,
		WriteStringOutput ->"stdout"
}

SMPrint[level_, fcprintx__ /;!OptionQ[{fcprintx}] , OptionsPattern[]] :=
	Block[{flowcontrol=OptionValue[SMDoControl]},
		If[ flowcontrol >= level,
			If[ OptionValue[UseWriteString],
				WriteString[OptionValue[WriteStringOutput],fcprintx],
				Print[fcprintx]
			]
		]
	];

SMMonitor:=
	If[$Notebooks && $SMCheckProgress, Monitor, SMMonitorStub];

SMMonitorStub[x_,__]:=
	x;

SMDeclareHeader[file_] :=
	Module[ {strm, einput, moreLines = True},
		strm = OpenRead[file];
		If[ Head[strm] =!= InputStream,
			Return[$Failed]
		];
		While[
			moreLines,
			einput = Read[strm, Hold[Expression]];
			ReleaseHold[einput];
			If[ einput === $Failed || MatchQ[einput, Hold[_End]],
				moreLines = False
			]
		];
		Close[file]
	];



End[];

listMisc = FileNames[{"*.m"},ToFileName[{$SpaceMathDirectory,"HiggsData"}]];

AppendTo[$ContextPath, "SpaceMath`Package`"];

SMDeclareHeader/@listMisc;

Get/@listMisc;

EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Print FeynCalc's startup message *)
If[ Global`$SpaceMathStartupMessages =!= False,
Print [Style["M. A. Arroyo-Ure\[NTilde]a","Text"]];
Print [Style["Facultad de Estudios Superiores-Cuautitl\[AAcute]n, Universidad Nacional Aut\[OAcute]noma de M\[EAcute]xico","Text"]];

Print [Style["E. A. Herrera-Chac\[OAcute]n","Text"]];
Print [Style["T. A. Valencia-P\[EAcute]rez","Text"]];
Print [Style["Facultad de Ciencias F\[IAcute]sico Matem\[AAcute]ticas, Benem\[EAcute]rita Universidad Aut\[OAcute]noma de Puebla","Text"]];

	];

BeginPackage["SpaceMath`"];
If[ Global`$LoadAddOns=!={},
	SMDeclareHeader/@Map[ToFileName[{$SpaceMathDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns];
	Get/@Map[ToFileName[{$SpaceMathDirectory,  "AddOns",#},#<>".m"] &, Global`$LoadAddOns]
];
EndPackage[];


