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

BeginPackage["SpaceMath`"];

SpaceMath::usage =
"SpaceMath package"

SMDeclareHeader::usage =
"SMDeclareHeader is an internal FeynCalc function to declare
objects inside an .m file in the same manner as it is done in
the JLink package. It may be used by FeynCalc addons."

Begin["`Private`"]


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


