(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: Install SpaceMath package															*)

(*
This software is covered by the GNU General Public License 3.
Copyright (C) 2019-2119 Marco Antonio Arroyo Ureña
Copyright (C) 2019-2119 Tomás Antonio Valencia Pérez
*)

(* :Summary:  Installs SpaceMath *)

(* ------------------------------------------------------------------------ *)

InstallSpaceMath::notcomp =
"Your Mathematica version is too old. SpaceMath requires at least Mathematica 11. Installation aborted!";

InstallSpaceMath::failed =
"Download of `1` failed. Installation aborted!";

InstallSpaceMathQuiet::usage="InstallSpaceMathQuiet is the silent mode of installing SpaceMath, where the 
installer does not ask you any questions but silently overwrites any existing SpaceMath installation and 
modifies Mathematica's options accordingly. The main purpose of this mode is 
to facilitate the installation of SpaceMath on Mathematica Online.";

AutoEnableTraditionalForm::usage="AutoEnableTraditionalForm is an option of InstallSpaceMath. If 
set to True, the format type of new output cells will be set to TraditionalForm. False means that 
the current value will not be changed.";

AutoOverwriteSpaceMathDirectory::usage="AutoOverwriteSpaceMathDirectory is an option of InstallSpaceMath. If 
set to True, the existing SpaceMath directory will be deleted without any further notice. The default
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

AutoDisableInsufficientVersionWarning::usage="AutoDisableInsufficientVersionWarning is an option of InstallSpaceMath. If 
set to True, warning messages for notebooks that were created with a newer Mathematica version will be silently disabled. 
This is needed to use SpaceMath documentation in Mathematica 11+, since otherwise the warning message will appear every 
time one opens a help page for a SpaceMath function. The default value None means that the user will be asked by a dialog. 
False means that the warning will not be disabled.";

SpaceMathDevelopmentVersionLink::usage="SpaceMathDevelopmentVersionLink is an option of InstallSpaceMath. It specifies the url 
to the main repository of SpaceMath. This repository is used to install the development version of SpaceMath.";

SpaceMathStableVersionLink::usage="SpaceMathStableVersionLink is an option of InstallSpaceMath. It specifies the url 
to the latest stable release of SpaceMath.";

InstallSpaceMathDevelopmentVersion::usage="InstallSpaceMathDevelopmentVersion is an option of InstallSpaceMath. If 
set to True, the installer will download the latest development version of SpaceMath from the git repository. 
Otherwise it will install the latest stable version.";

InstallSpaceMathTo::usage="InstallSpaceMathTo is an option of InstallSpaceMath. It specifies, the full path 
to the directory where SpaceMath will be installed.";

$PathToSMArc::usage="$PathToSMArc specifies where the installer should look for the zipped SpaceMath version. 
If the value is not empty, the installer will use the specified file instead of downloading it from the official 
website."

If[ !ValueQ[$PathToSMArc],
	$PathToSMArc = ""
];

If[  $VersionNumber == 9,
(*To use FetchURL in MMA8 we need to load URLTools first *)
Needs["Utilities`URLTools`"];
];

Options[InstallSpaceMath]={
	AutoDisableInsufficientVersionWarning-> None,
	AutoEnableTraditionalForm -> None,
	AutoOverwriteSpaceMathDirectory-> None,
	SpaceMathDevelopmentVersionLink->"https://github.com/spacemathpackage/SpaceMath/archive/development.zip",
	SpaceMathStableVersionLink->"https://github.com/spacemathpackage/SpaceMath/archive/master.zip",
	InstallSpaceMathDevelopmentVersion->False,
	InstallSpaceMathTo->FileNameJoin[{$UserBaseDirectory, "Applications","SpaceMath"}]
};

Options[InstallSpaceMathQuiet]=
	Options[InstallSpaceMath];

InstallSpaceMathQuiet[]:=
	InstallSpaceMath[
		AutoDisableInsufficientVersionWarning-> True,
		AutoEnableTraditionalForm -> True,
	    AutoOverwriteSpaceMathDirectory-> True
	];

InstallSpaceMath[OptionsPattern[]]:=
	Module[{	unzipDir, tmpzip, gitzip, packageName, packageDir, fullPath,
				strDisableWarning,SMGetUrl, configFileProlog,
				strOverwriteSMdit, faInstalled, zipDir, strEnableTraditionalForm,
				useTraditionalForm, configFile},

	If[OptionValue[InstallSpaceMathDevelopmentVersion],
		gitzip = OptionValue[SpaceMathDevelopmentVersionLink],
		gitzip = OptionValue[SpaceMathStableVersionLink]
	];

	faInstalled=False;
	useTraditionalForm=False;

	packageName = "SpaceMath";
	packageDir = OptionValue[InstallSpaceMathTo];

strDisableWarning="To make the documentation work, we need to disable the warning that appears 
when you open a notebook that was created with a newer Mathematica version. Otherwise this 
warning will pop up every time you use the Documentation Center to read info on SpaceMath functions 
in Mathematica 11+. This setting is harmless and can be always undone via 
\"SetOptions[$FrontEnd, MessageOptions -> {\"InsufficientVersionWarning\" -> True}]\". Should we do this now?";

strEnableTraditionalForm="SpaceMath makes an extensive use of Mathematica's typesetting capabilities to 
format the output in a nice and easily readable manner. However, the built-in typesetting is available 
only if the format type of new output cells is set to TraditionalForm. The default value is StandardForm. 
Do you want to allow SpaceMath to change the default output format to TraditionalForm whenever it is loaded? 
This will only affect the current SpaceMath front end session and will not influence any subsequent Mathematica 
sessions, i.e. the changes are not persistent.";

strOverwriteSMdit="Looks like SpaceMath is already installed. Do you want to replace the content 
of " <> packageDir <> " with the downloaded version of SpaceMath? If you are using any custom configuration 
files or add-ons that are located in that directory, please backup them in advance.";

configFileProlog ="(*Here you can put some commands and settings to be evaluated on every start of SpaceMath. \n
This allows you to customize your SpaceMath installation to fit your needs best.*)";

	If[$VersionNumber < 9,
		Message[InstallSpaceMath::notcomp];
		Abort[]
	];

	If[$VersionNumber == 9,
		(*To use FetchURL in MMA8 we need to load URLTools first *)
		SMGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
		SMGetUrl[x_]:= URLSave[x,CreateTemporary[]]
	];


	(* If the package directory already exists, ask the user about overwriting *)
	If[ DirectoryQ[packageDir],

		If[ OptionValue[AutoOverwriteSpaceMathDirectory],

			Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

			Null,
			If[ ChoiceDialog[strOverwriteSMdit,{"Yes, overwrite the " <> packageName <>" directory"->True,
				"No, I need to do a backup first. Abort installation."->False}, WindowFloating->True, WindowTitle->"Existing SpaceMath Installation detected"],
				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
				Abort[]
			]
		]
	];

	(* Download SpaceMath tarball	*)
	If[ $PathToSMArc=!="",
		tmpzip = $PathToSMArc;
		WriteString["stdout", "Installing SpaceMath from ", tmpzip," ..."],
		WriteString["stdout", "Downloading SpaceMath from ", gitzip," ..."];
		tmpzip=SMGetUrl[gitzip];
	];

	If[tmpzip===$Failed,
		WriteString["stdout", "\nFailed to download SpaceMath. Please check your interent connection.\nInstallation aborted!"];
		Abort[],

		unzipDir= tmpzip<>".dir";
		WriteString["stdout", "done! \n"];
	];

	(* Extract to the content	*)
	WriteString["stdout", "SpaceMath zip file was saved to ", tmpzip,".\n"];
	WriteString["stdout", "Extracting SpaceMath zip file to ", unzipDir, " ..."];

	If[	ExtractArchive[tmpzip, unzipDir]===$Failed,
		WriteString["stdout", "\nFailed to extract the SpaceMath zip. The file might be corrupted.\nInstallation aborted!"];
		Abort[],
		WriteString["stdout", "done! \n"];
		(* Delete the downloaded file	*)
		If[ $PathToSMArc==="",
			Quiet@DeleteFile[tmpzip];
		]
	];

	WriteString["stdout", "Recognizing the directory structure..."];
	zipDir = FileNames["SpaceMath.m", unzipDir, Infinity];
	If[ Length[zipDir]===1,
		fullPath = DirectoryName[zipDir[[1]]];
		zipDir = Last[FileNameSplit[DirectoryName[zipDir[[1]]]]];
		WriteString["stdout", "done! \n"],
		WriteString["stdout", "\nFailed to recognize the directory structure of the downloaded zip file. \nInstallation aborted!"];
		Abort[]
	];

	(* Move the files to the final destination	*)
	WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];

	If[	CopyDirectory[fullPath,packageDir]===$Failed,
		WriteString["stdout", "\nFailed to copy "  <>fullPath<>" to ", packageDir <>". \nInstallation aborted!"];
		Abort[],
		WriteString["stdout", "done! \n"];
		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];
	];

	(* Activate TraditionalForm	*)
	WriteString["stdout", "Setting up the format type of new output cells ... "];
	If[ OptionValue[AutoEnableTraditionalForm],

		useTraditionalForm = True,
		Null,
		If[ ChoiceDialog[strEnableTraditionalForm, WindowFloating->True, WindowTitle->"TraditionalForm output"],
			useTraditionalForm = True
		]
	];

	WriteString["stdout", "done! \n"];

	(* To have the documentation available immediately after installing SpaceMath *)
	RebuildPacletData[];

	(* Generate SMConfig.m	*)
	WriteString["stdout", "Creating the configuration file ... "];
	configFile = StringJoin[configFileProlog, "\n\n(* Activate TraditionalForm output for each SpaceMath session *) \n$SMTraditionalFormOutput="<>ToString[useTraditionalForm]<>";"];
	Export[FileNameJoin[{packageDir,"SMConfig.m"}], configFile, "Text"];
	WriteString["stdout", "done! \n"];

	WriteString["stdout", "\nInstallation complete! Loading SpaceMath ... \n"];
	
	Get["SpaceMath`"];

];