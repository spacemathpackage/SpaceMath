(* ::Package:: *)

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: SpaceMath															*)

(*
	This software is covered by the GNU General Public License 3.
*)

(* :Summary:	We present a Mathematica application, so-called SpaceMath, 
				for the search of Beyond Standard Model (BSM) parameter spaces 
				which be agree with the most up-to-date experimental measurements.			*)

(* ------------------------------------------------------------------------ *)


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
SpaceMath`$SpaceMathVersion = "10.0";

(*    Set defaults here, not in the config file    *)
If[ !ValueQ[Global`$SpaceMathStartupMessages],
	Global`$SpaceMathStartupMessages = True
];


(*    Load configuration file    *)
If[ FileExistsQ[FileNameJoin[{SpaceMath`$SpaceMathDirectory,"SMConfig.m"}]],
	Get[FileNameJoin[{SpaceMath`$SpaceMathDirectory,"SMConfig.m"}]]
];

If[ Global`$SpaceMathStartupMessages=!=False,
	PrintTemporary[Style["Loading SpaceMath from "<>
	SpaceMath`$SpaceMathDirectory, "Text"]]
];


BeginPackage["SpaceMath`"];

Begin["`Private`"]

End[];

listKappaXX = FileNames[{"*.m"},ToFileName[{$SpaceMathDirectory,"HiggsData"}]];
listPlotsKappaXX = FileNames[{"*.m"},ToFileName[{$SpaceMathDirectory,"HiggsData"}]];
listParticleData = FileNames[{"*.m"},ToFileName[{$SpaceMathDirectory,"HiggsData"}]];

AppendTo[$ContextPath, "SpaceMath`Package`"];

SMDeclareHeader/@listKappaXX;
SMDeclareHeader/@listPlotsKappaXX;
SMDeclareHeader/@listParticleData;

Get/@listKappaXX;
Get/@listPlotsKappaXX;
Get/@listParticleData;

EndPackage[];

(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* Print SpaceMath's startup message *)
If[ Global`$SpaceMathStartupMessages =!= False,
	Print[	Style["SpaceMath ", "Text", Bold], Style[$SpaceMathVersion <> ". For help, use the ",
				"Text"],
			Style[DisplayForm@ButtonBox["documentation center", BaseStyle->"Link", ButtonData :> "paclet:SpaceMath/",
				ButtonNote -> "paclet:SpaceMath/"], "Text"],
			Style[", check out the ", "Text"],
			Style[DisplayForm@ButtonBox["wiki",ButtonData :> {URL["https://github.com/spacemathpackage/SpaceMath/wiki/SpaceMath"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "https://github.com/spacemathpackage/SpaceMath/wiki/SpaceMath"],"Text"],
			Style[" or write to the ", "Text"],
			Style[DisplayForm@ButtonBox["mailing list.",ButtonData :> {URL["http://www.SpaceMath.org/forum/"], None},BaseStyle -> "Hyperlink",
				ButtonNote -> "http://www.SpaceMath.org/forum/"],"Text"]];
	Print[ Style["See also the supplied ","Text"],

	Style[DisplayForm@ButtonBox["examples.", BaseStyle -> "Hyperlink",	ButtonFunction :>
							SystemOpen[FileNameJoin[{$SpaceMathDirectory, "Examples"}]],
							Evaluator -> Automatic, Method -> "Preemptive"], "Text"],
	Style[" If you use SpaceMath in your research, please cite","Text"]];
Print [Style["M. A. Arroyo-Ure\[NTilde]a","Text"]];
Print [Style["Facultad de Estudios Superiores-Cuautitl\[AAcute]n, Universidad Nacional Aut\[OAcute]noma de M\[EAcute]xico","Text"]];
Print [Style["E. A. Herrera-Chac\[OAcute]n","Text"]];
Print [Style["T. A. Valencia-P\[EAcute]rez","Text"]];
Print [Style["Facultad de Ciencias F\[IAcute]sico Matem\[AAcute]ticas, Benem\[EAcute]rita Universidad Aut\[OAcute]noma de Puebla","Text"]];
	];

BeginPackage["SpaceMath`"];

EndPackage[];
