(* Wolfram Language Package *)

mtau::usage = "tau mass";
mmu::usage="muon mass"; 
me::usage="electron mass";
mu::usage="up quark mass";
md::usage="down quark mass"; 
mc::usage="charm quark mass";
ms::usage="strange quark mass"; 
mt::usage="top quark mass";
mb::usage="bottom quark";
mh::usage="Higgs boson mass";
mW::usage="W gauge boson mass"; 
mZ::usage="gauge boson mass";
vev::usage="vacumm expectation value";
\[Alpha]em::usage="structure fine constant";
ee::usage="electric charge";
g::usage="2 mW/vev";
\[Alpha]s::usage="strong alpha";
CW::usage="cosine of Weinberg angle";
SW::usage="sine of Weinberg angle";
gw::usage="Weak constant coupling (W)";
gz::usage="Weak constant coupling (Z)";
ge::usage="electric charge";
GF::usage="Fermi constant";
EpstopSUP::usage="0.01";
EpstopINF::usage="-0.43";
EpsbotSUP::usage="-0.19+0.28";
EpsbotINF::usage="-0.19-0.28";
EpstauSUP::usage="-0.03+0.17";
EpstauINF::usage="-0.03-0.17";
EpsZSUP::usage="0+0.1";
EpsZINF::usage="0-0.1";
EpsWSUP::usage="-0.2+0.13";
EpsWINF::usage="-0.2-0.13";
Rbb::usage="1.12";
Rtautau::usage="1.02";
Rww::usage="1.35";
Rzz::usage="1.22";
Rgammagamma::usage="1.16";
RbbSUP2sig::usage="6.95661669137334";
RbbINF2sig::usage="-1.9366166913733407";
RtautauSUP2sig::usage="2.050599820107919";
RtautauINF2sig::usage="0.04940017989208112";
RwwSUP2sig::usage="1.7501666319589044";
RwwINF2sig::usage="0.9498333680410957";
RzzSUP2sig::usage="1.660151489073175";
RzzINF2sig::usage="0.779848510926825";
RgammagammaINF2sig::usage="0.7696155740811373";
RgammagammaSUP2sig::usage="1.5503844259188626";
RbbSUP1sig::usage="4.73330834568667";
RbbINF1sig::usage="0.28669165431332955";
RtautauSUP1sig::usage="1.5502999100539596";
RtautauINF1sig::usage="0.5497000899460406";
RwwSUP1sig::usage="1.5500833159794523";
RwwINF1sig::usage="1.149916684020548";
RzzSUP1sig::usage="1.4400757445365875";
RzzINF1sig::usage="0.9999242554634125";
RgammagammaINF1sig::usage="0.9648077870405687";
RgammagammaSUP1sig::usage="1.3551922129594312";
kappaZ::usage="0.99";
kappaW::usage="1.10";
kappaTop::usage="1.11";
kappaTau1::usage="1.01";
kappaBot::usage="-1.10";
kappaGluon::usage="1.18";
kappaGamma::usage="1.07";
kappaZSUP2sig::usage="1.22";
kappaZINF2sig::usage="0.78";
kappaWSUP2sig::usage="\!\(TraditionalForm\`1.45\)";
kappaWINF2sig::usage="0.81";
kappaTopSUP2sig::usage="1.26";
kappaTopINF2sig::usage="0.7";
kappaTauSUP2sig::usage="1.36";
kappaTauINF2sig::usage="0.68";
kappaBotSUP2sig::usage="1.75046";
kappaBotINF2sig::usage="0.58954";
kappaGluonSUP2sig::usage="1.48022";
kappaGluonINF2sig::usage="0.879778";
kappaGammaSUP2sig::usage="1.36006";
kappaGammaINF2sig::usage="0.779943";
kappaZSUP1sig::usage="1.00+0.11";
kappaZINF1sig::usage="1.00-0.11";
kappaWSUP1sig::usage="1.13+0.16";
kappaWINF1sig::usage="1.13-0.16";
kappaTopSUP1sig::usage="0.98+0.14";
kappaTopINF1sig::usage="0.98-0.14";
kappaTauSUP1sig::usage="1.02+0.17";
kappaTauINF1sig::usage="1.02-0.17";
kappaBotSUP1sig::usage="1.17+0.27";
kappaBotINF1sig::usage="1.17-0.31";
kappaGluonSUP1sig::usage="1.18+0.16";
kappaGluonINF1sig::usage="1.18-0.14";
kappaGammaSUP1sig::usage="1.07+0.14";
kappaGammaINF1sig::usage="1.07-0.15";
BRMUtoEgamma::usage="Upper bound of the mu\[Rule] e gamma decay";
BRMUtoEEE::usage="Upper bound of the mu\[Rule] 3e decay";
BRTAUtoMUgamma::usage="Upper bound of the tau\[Rule] mu gamma decay";
BRTAUtoEgamma::usage="Upper bound of the tau\[Rule] e gamma decay";
BRTAUtoEEE::usage="Upper bound of the tau\[Rule] 3e decay";
BRTAUtoMUMUMU::usage="Upper bound of the tau\[Rule] 3\[Mu] decay";
BRHtoTAUMU::usage="Upper bound of the h\[Rule] tau mu decay";
GF::usage="Fermi constant";
Ttau::usage="tau lifetime";
TotWidh::usage="Total width of the Higgs boson";
aMUInf::usage="lower limit of the discrepancy interval of the muon anomalous magnetic dipole moment";
aMUSup::usage="upper limit of the discrepancy interval of the muon anomalous magnetic dipole moment";
aSM::usage="Theoretical prediction of the SM for the muon anomalous magnetic dipole moment";
aEXP::usage="Experimental value for the muon anomalous magnetic dipole moment";
BRTAUtolnunu::usage="Branching ratio of the tau \[Rule] l nu nu decay";
dmuINF::usage="lower limit of the muon alectric dipole moment";
dmuSUP::usage="upper limit of the muon alectric dipole moment";
bsgammaINF3sigma::usage="0.000259";
bsgammaSUP3sigma::usage="\!\(TraditionalForm\`0.000427\)";

Begin["`Package`"]

End[]

Begin["`ParticleData`Private`"]

(*value of masses(units given in GeV )*)

(*Reference: M. Tanabashi et al. (Particle Data Group), Phys. Rev. D 98, 030001 (2018).*)

mtau = 1.77686; (*tau mass*)
mmu = 0.1056583745; (*muon mass*) 
me = 0.0005109989461; (*electron mass*)
mu = 0.0022; (*up quark mass*)
md = 0.0047; (*down quark mass*) 
mc = 1.275; (*charm quark mass*)
ms = 0.095; (*strange quark mass*) 
mt = 173.21; (*top quark mass*)
mb = 4.18; (*bottom quark*)
mh = 125.18; (*Higgs boson mass*)
mW = 80.379; (*W gauge boson mass*) 
mZ = 91.1876 (*Z gauge boson mass*)
xpi = 6*Pi;

(*value of constants*)

vev = 246; (*vacumm expectation value*)
\[Alpha]em =1/137; (*structure fine constant*)
ee =Sqrt[4*\[Pi]*\[Alpha]em ]; (*electric charge*)
g = 2 mW/vev;
\[Alpha]s =0.11; (*strong alpha*)
CW =mW/mZ; (*cosine of Weinberg angle*)
SW =Sqrt[1-(CW^2)]; (*sine of Weinberg angle*)
gw =ge/SW; (*Weak constant coupling (W)*)
gz =gw/CW; (*Weak constant coupling (Z)*)
ge =Sqrt[4 \[Pi] \[Alpha]em]; (*electric charge*)
GF =1.16637^-5; (*Fermi constant*)

(*value of bounds*)
Higgs data
(*Epsilon to 2\[Sigma]*)
(*Reference: P. P. Giardino, K. Kannike, I. Masina, M. Raidal, and A. Strumia, J. High Energy Phys. 05 (2014) 046.*)
EpstopSUP =0.01;
EpstopINF =-0.43;
EpsbotSUP =-0.19+0.28;
EpsbotINF =-0.19-0.28;
EpstauSUP =-0.03+0.17;
EpstauINF =-0.03-0.17;
EpsZSUP =0+0.1;
EpsZINF =0-0.1;
EpsWSUP =-0.2+0.13;
EpsWINF =-0.2-0.13;
(*Signal Strengths*)
(*Reference: ARXIV:1809.10733*)
(*central values for gluon production*)

Rbb =1.12;
Rtautau =1.02;
Rww =1.35;
Rzz =1.22;
Rgammagamma =1.16;

(*Signal Strengths to 2\[Sigma]*)
(*Reference: ARXIV:1809.10733*)

RbbSUP2sig =6.95661669137334;
RbbINF2sig =-1.9366166913733407;
RtautauSUP2sig =2.050599820107919;
RtautauINF2sig =0.04940017989208112;
RwwSUP2sig =1.7501666319589044;
RwwINF2sig =0.9498333680410957;
RzzSUP2sig =1.660151489073175;
RzzINF2sig =0.779848510926825;
RgammagammaINF2sig =0.7696155740811373;
RgammagammaSUP2sig =1.5503844259188626;
(*Signal Strengths to 1\[Sigma]*)
(*Reference: ARXIV:1809.10733*)
RbbSUP1sig=4.73330834568667;
RbbINF1sig=0.28669165431332955;
RtautauSUP1sig=1.5502999100539596;
RtautauINF1sig=0.5497000899460406;
RwwSUP1sig=1.5500833159794523;
RwwINF1sig=1.149916684020548;
RzzSUP1sig=1.4400757445365875;
RzzINF1sig=0.9999242554634125;
RgammagammaINF1sig=0.9648077870405687;
RgammagammaSUP1sig=1.3551922129594312;

(*kappa-parametrization*)
(*central values*)
kappaZ=0.99;
kappaW=1.10;
kappaTop=1.11;
kappaTau1=1.01;
kappaBot=-1.10;
kappaGluon=1.18;
kappaGamma=1.07;

(*kappaX to 2\[Sigma]*)
(*Reference: ARXIV:1809.10733*)

kappaZSUP2sig=1.22;
kappaZINF2sig=0.78;
kappaWSUP2sig=\!\(TraditionalForm\`1.45\);
kappaWINF2sig=0.81;
kappaTopSUP2sig=1.26;
kappaTopINF2sig=0.7;
kappaTauSUP2sig=1.36;
kappaTauINF2sig=0.68;
kappaBotSUP2sig=1.75046;
kappaBotINF2sig=0.58954;
kappaGluonSUP2sig=1.48022;
kappaGluonINF2sig=0.879778;
kappaGammaSUP2sig=1.36006;
kappaGammaINF2sig=0.779943;
(*{
kappaZSUP2sig:=1.22007;
kappaZINF2sig:=0.759928;
kappaWSUP2sig:=1.391433239925259;
kappaWINF2sig:=0.808567;
kappaTopSUP2sig:=1.3303;
kappaTopINF2sig:=0.889697;
kappaTauSUP2sig:=1.37074;
kappaTauINF2sig:=0.64926;
kappaBotSUP2sig:=1.66297;
kappaBotINF2sig:=0.537032;
kappaGluonSUP2sig:=1.47134;
kappaGluonINF2sig:=0.848659;
kappaGammaSUP2sig:=1.14;
kappaGammaINF2sig:=0.78
};*)
(*kappaX to 1\[Sigma]*)
(*Reference: ARXIV:1809.10733*)

kappaZSUP1sig=1.00+0.11;
kappaZINF1sig=1.00-0.11;
kappaWSUP1sig=1.13+0.16;
kappaWINF1sig=1.13-0.16;
kappaTopSUP1sig=0.98+0.14;
kappaTopINF1sig=0.98-0.14;
kappaTauSUP1sig=1.02+0.17;
kappaTauINF1sig=1.02-0.17;
kappaBotSUP1sig=1.17+0.27;
kappaBotINF1sig=1.17-0.31;
kappaGluonSUP1sig=1.18+0.16;
kappaGluonINF1sig=1.18-0.14;
kappaGammaSUP1sig=1.07+0.14;
kappaGammaINF1sig=1.07-0.15;
LFV processes
(*Reference: M. Tanabashi et al. (Particle Data Group), Phys. Rev. D 98, 030001 (2018)*)

BRMUtoEgamma=4.2*(10^(-13)); (*Upper bound of the mu\[Rule] e gamma decay*)
BRMUtoEEE=1*(10^(-12)); (*Upper bound of the mu\[Rule] 3e decay*)
BRTAUtoMUgamma=4.4*(10^(-8));(*Upper bound of the tau\[Rule] mu gamma decay*)
BRTAUtoEgamma=3.3*(10^(-8));(*Upper bound of the tau\[Rule] e gamma decay*)
BRTAUtoEEE=2.7*(10^(-8)); (*Upper bound of the tau\[Rule] 3e decay*)
BRTAUtoMUMUMU=2.7*(10^(-8)); (*Upper bound of the tau\[Rule] 3\[Mu] decay*)
BRHtoTAUMU=0.0025; (*Upper bound of the h\[Rule] tau mu decay*)
GF=1.1663787*(10^-5); (*Fermi constant*)
Ttau=(2.906*10^-13) ((1/6.582)*10^25);(*tau lifetime*)
TotWidh=0.0041; (*Total width of the Higgs boson*)
aMUInf=1.32*10^-9; (*lower limit of the discrepancy interval of the muon anomalous magnetic dipole moment*)
aMUSup=4.44*10^-9; (*upper limit of the discrepancy interval of the muon anomalous magnetic dipole moment*)
aSM=11659179*10^-10; (*Theoretical prediction of the SM for the muon anomalous magnetic dipole moment*)
aEXP=116592091*10^-11; (*Experimental value for the muon anomalous magnetic dipole moment*)
BRTAUtolnunu=0.17; (*Branching ratio of the tau \[Rule] l nu nu decay*)
dmuINF=-10*(10^-20); (*lower limit of the muon alectric dipole moment*)
dmuSUP=8*(10^-20); (*upper limit of the muon alectric dipole moment*)
B-physics
(*{
TwoSigBmumuSUP:=4.301110699893027*(10^-9) (*experimental
bounds satisfing two standard deviations for B0s into 2mu decay*),
TwoSigBmumuINF:=1.2988893001069727*(10^-9) (*experimental
bounds satisfing two standard deviations for B0s into 2mu decay*),
mB0s := 5.36689 (*B0s meson mass*),
TB0s:=0.0227*(10^13) (*Lifetime of the B0s meson*),
fB0s:=0.242,  (*B0s decay constant*)
BRBmesonTOmumuSM:=3.66*(10^-9) (*Branching ratio of the B to mumu decay*)
BRexpBdTOmumu:=9.4*(10^(-10))
};*)
b-s gamma
bsgammaINF3sigma =0.000259;
bsgammaSUP3sigma =\!\(TraditionalForm\`0.000427\);

End[]



