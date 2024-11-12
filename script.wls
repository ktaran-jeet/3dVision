#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Title:: *)
(*TP2: Partie 1: Camera simulee*)


(* ::Subtitle:: *)
(*3D World:*)


(*Placement of object in 3d world: returns transformation function for given transformations*)
Clear[Place3D];
Place3D[position_, rotationAngle_, rotationAxis_: {1, 0, 0}, scale_ : {1, 1, 1}] := 
 Module[{scaleFactors},
  scaleFactors = If[Length[scale] == 3, scale, {scale, scale, scale}];
  Composition[TranslationTransform[position],
              RotationTransform[rotationAngle, rotationAxis],
              ScalingTransform[scaleFactors]]
];

Place3D[position_] := Place3D[position, 0, {1, 0, 0}, {1, 1, 1}];
Place3D[position_, rotationAngle_, rotationAxis_] := Place3D[position, rotationAngle, rotationAxis, {1, 1, 1}];
Place3D[position_, scale_] := Place3D[position,  0, {1, 0, 0}, scale];

(* Function to apply transformations to a 3D object *)
TransformedObject[object_, transformationfn_] :=  GeometricTransformation[object, transformationfn];


(* ::Input::Initialization:: *)
(*3D primitives: Cuboid*)
unCuboid[dx_,dy_,dz_]:={{-dx,-dy,-dz},(*Bottom-front-left*)
{dx,-dy,-dz},(*Bottom-front-right*)
{dx,dy,-dz},(*Bottom-back-right*)
{-dx,dy,-dz},(*Bottom-back-left*)
{-dx,-dy,dz},(*Top-front-left*)
{dx,-dy,dz},(*Top-front-right*)
{dx,dy,dz},(*Top-back-right*)
{-dx,dy,dz}     (*Top-back-left*)}

getFaces[transformedCorners_]:={{transformedCorners[[1]],transformedCorners[[2]],transformedCorners[[6]],transformedCorners[[5]]},(*Front face*){transformedCorners[[2]],transformedCorners[[3]],transformedCorners[[7]],transformedCorners[[6]]},(*Right face*){transformedCorners[[3]],transformedCorners[[4]],transformedCorners[[8]],transformedCorners[[7]]},(*Back face*){transformedCorners[[4]],transformedCorners[[1]],transformedCorners[[5]],transformedCorners[[8]]},(*Left face*){transformedCorners[[1]],transformedCorners[[2]],transformedCorners[[3]],transformedCorners[[4]]},(*Bottom face*){transformedCorners[[5]],transformedCorners[[6]],transformedCorners[[7]],transformedCorners[[8]]}  (*Top face*)}


(* Helper function to apply color, opacity, and graphics for each object *)
see[e_] := {
	If[KeyExistsQ[e, "color"], e[["color"]], Black],
	If[KeyExistsQ[e, "opacity"], Opacity[e[["opacity"]]], Opacity[1]], 
	If[KeyExistsQ[e, "Checkerpoints"], Point[e[["Checkerpoints"]]]],
	If[KeyExistsQ[e, "shape"], e[["shape"]]],
	If[KeyExistsQ[e, "cornerpoints"],Polygon/@(getFaces[e[["cornerpoints"]]])]
};
(* Main function to display objects in 3D with an optional bounding box *)
seeMonde3D[monde3d_, {{xmin_, xmax_}, {ymin_, ymax_}, {zmin_, zmax_}}] :=
  Graphics3D[{
    GrayLevel[0.8], Opacity[0.2],Cuboid[{xmin, ymin, zmin}, {xmax, ymax, zmax}],
    Red, Thick,Line[{{0, ymin, 0}, {0, ymax, 0}}], Line[{{xmin, 0, 0}, {xmax, 0, 0}}],Line[{{0, 0, zmin}, {0, 0, zmax}}],
    Text[Style["X", Bold, Thick, Black],{xmax,0,0}],
    Text[Style["Y", Bold, Thick, Black],{0,ymax,0}],
    Text[Style["Z", Bold, Thick, Black],{0,0,zmax}],
    see /@ monde3d
  }, 
  PlotRange -> {{xmin, xmax}, {ymin, ymax}, {zmin, zmax}},
  Boxed -> False
];


(* ::Input::Initialization:: *)
monde3d= {<|"cornerpoints" -> Place3D[{0,0,2},Pi/4,{1,0,1},1] @unCuboid[2,1,3],
 "color" -> Green,
 "opacity"->0.5|>,
 <|"cornerpoints" -> Place3D[{1,2,4}, Pi/4,{0,1,1},1] @unCuboid[2,2,2],
 "color" -> Red,
 "opacity"->0.6|> 
  };
seeMonde3D[monde3d, {{-10, 10}, {-10, 10}, {-10, 10}}]


(* ::Subtitle:: *)
(*Camera 2D:*)


(* ::Input::Initialization:: *)
(*Function to calculate the intrinsic matrix of camera*)
Clear[InstrinsicMatrix];
InstrinsicMatrix[angleOfView_,imageDimensions_:{1920,1080}]:=Module[{W,H,fX,fY,cX,cY, interne},
{W,H}=imageDimensions;
fX=(W/2)/Tan[angleOfView/2];
fY=(H/2)/Tan[angleOfView/2];
cX=W/2;
cY=H/2;
interne = {{fX,0,cX},{0,fY,cY},{0,0,1}}
];

World2ImageMatrix[c2wMatrix_,internMatrix_:{1920,1080}]:=Module[{w2cMatrix, interne3x4, w2iMatrix},
w2cMatrix = Inverse[c2wMatrix];
interne3x4=Append[#,0]&/@internMatrix;
w2iMatrix=interne3x4 . w2cMatrix;
N[w2iMatrix]
];


(*Euclidean to prespective and vice-versa*)
e2p[v_]:=Append[v,1];
p2e[v_]:=Delete[v,-1]/v[[-1]];

(* Function to draw Camera*)
DrawCam[camera2World_,interneMatrix_,focalLength_,imageDimensions_ :{1920,1080}]:=Module[{W,H,imagePlaneCorners,cameracorners,worldcorners,camera},
{W,H}=imageDimensions;
(*camera centre and image plane corners in pixels*)
imagePlaneCorners = {{0, 0, 0},{-W/2, -H/2, 1},{W/2, -H/2, 1},{W/2, H/2, 1},{-W/2, H/2, 1}};
cameracorners=Inverse[interneMatrix] . #&/@imagePlaneCorners;
cameracorners=cameracorners*focalLength;
worldcorners = camera2World . #&/@(e2p/@cameracorners);
worldcorners = p2e /@worldcorners;

camera = Graphics3D[{LightBrown, Polygon[worldcorners[[2 ;; 5]]],
    Black, Sphere[worldcorners[[1]], 0.5],
Thickness[0.001],
 Table[
    Line[{worldcorners[[1]], worldcorners[[i]]}], 
    {i, 2, Length[worldcorners]}]
  }];
camera
];


(* ::Input:: *)
(*(*Camera placement in the world*)*)
(*angleOfView=90 Degree; *)
(*imageDimensions={1920,1080};*)
(*focalLength =10;*)
(*c2w = TransformationMatrix[Place3D[{6, 6, -4}, Pi/8,{1,0,0}, 1]];*)
(*w2i = World2ImageMatrix[c2w,InstrinsicMatrix[angleOfView,imageDimensions]];*)
(**)
(*Show[*)
(*seeMonde3D[monde3d, {{-20, 20}, {-20, 20}, {-20, 20}}],*)
(*DrawCam[c2w, InstrinsicMatrix[angleOfView, imageDimensions],focalLength, imageDimensions]*)
(*]*)


(* ::Subtitle:: *)
(*Image Capture:*)


(* ::Input::Initialization:: *)
(*Function to capture image: perspective transform*)
photo[w2i_,segment_]:=Block[{p,q},(
p=(w2i . e2p[#])&/@segment;
q=p/p[[All,3]];
q[[All,{1,2}]]; 
{#[[1]],#[[2]]}&/@  q(*pixel position u,v in x-y space*)
)]


(* ::Input:: *)
(*(*Plot image*)*)
(*vg={#[["color" ]],Polygon[getFaces[photo[w2i,#[["cornerpoints" ]]]]]}&/@monde3d*)
(*Graphics[{{#[[1]],#[[2]]}&/@vg},Axes->True,AspectRatio->Automatic]*)


(* ::Title:: *)
(*Partie 2: Calibrage direct:*)


(* ::Input::Initialization:: *)
mondecalib= {<|"cornerpoints" -> Place3D[{0,0,2},Pi/4,{1,1,0},1] @unCuboid[2,1,3],
 "color" -> Green,
 "opacity"->0.5|>
  };


(* ::Input:: *)
(*Show[*)
(*seeMonde3D[mondecalib, {{-20, 20}, {-20, 20}, {-20, 20}}],*)
(*DrawCam[c2w, InstrinsicMatrix[angleOfView, imageDimensions],focalLength, imageDimensions]*)
(*]*)


(* ::Input:: *)
(*points3d = N[mondecalib[[1, "cornerpoints"]]];*)
(*points2d = photo[w2i,points3d];*)


(* ::Input::Initialization:: *)
ProjectionMatrix[points3d_List,points2d_List]:=Module[{ligne3D,ma,u,w,v, M},
ligne3D[{x_,y_,z_},{xi_,yi_}]:={{0,0,0,0,x,y,z,1,-x yi,-y yi,-yi z,-yi},{-x,-y,-z,-1,0,0,0,0,x xi,xi y,xi z,xi}};

ma=Table[ligne3D[points3d[[i]],points2d[[i]]],{i,1,Length[points3d]}];
ma=Flatten[ma,{{1,2},{3}}];
{u,w,v}=SingularValueDecomposition[ma];
M = Partition[v[[All,-1]],4];
M
]


(* ::Input:: *)
(*(*Verify the SVD returns correct matrix:*)*)
(*M=ProjectionMatrix[points3d,points2d];*)
(*M = M*(w2i[[-1,-1]]/M[[-1,-1]])//Chop;*)
(*M//MatrixForm*)
(*w2i //MatrixForm*)


(* ::Input::Initialization:: *)
(*Factorization to Intrinsic and extrinsic matrices*)
diagonalePositive[{r_,q_}]:=Block[{s},(
s=DiagonalMatrix[Sign[Diagonal[r]]];
{r . s,s . q}
)];
RQDecompV1[a_]:=Block[{r1,q1},(
{q1,r1}=QRDecomposition[Transpose[Reverse[a]]];
q1=Transpose[q1]; (* a cause de mathematica *)
r1=Reverse[Transpose[Reverse[r1]]];
q1=Reverse[Transpose[q1]];
diagonalePositive[{r1,q1}]
)];

FactorizeTransformationMatrix[M_]:=Module[{K,R, t, H},
t= Inverse[M[[1;;3,1;;3]]] . M[[All,-1]];
H = M[[1;;3,1;;3]];
{K,R} = RQDecompV1[H];
{K,R,t}];


(* ::Input:: *)
(*{K,R,t}= FactorizeTransformationMatrix[M];*)
(*K//Chop//MatrixForm*)
(*InstrinsicMatrix[angleOfView,imageDimensions]//MatrixForm*)
(*R//Chop//MatrixForm*)
(*t//Chop//MatrixForm*)


(* ::Title:: *)
(*Partie3 : Calibrage Planaire*)


(* ::Input::Initialization:: *)
(*Draw checkerboard in 3D: For simplicity oly points are drawn*)
Checkerboard[numSquares_,squareSize_]:=Flatten[Table[{i*squareSize,j*squareSize,0},{i,0,numSquares-1},{j,0,numSquares-1}],1];


(* ::Input:: *)
(*(*Test*)*)
(*Graphics3D[{PointSize[Medium],GrayLevel[0.4],Point[Checkerboard[5,1]]}]*)


(* ::Input::Initialization:: *)
(*checkerboeard in different orientations*)
mondeplanar= {<|"Checkerpoints" -> Place3D[{0,0,0},1] @Checkerboard[5,1],
 "color" -> Black,
 "opacity"->0.8|>,
<|"Checkerpoints" -> Place3D[{1,-2,0},Pi/8, {0,0,1},1] @Checkerboard[5,1],
 "color" -> Red,
 "opacity"->0.8|>,
<|"Checkerpoints" -> Place3D[{0,0,4},Pi/8, {1,0,0},1]@Checkerboard[5,1],
 "color" -> Gray,
 "opacity"->0.8|>,
<|"Checkerpoints" ->Place3D[{-2,1,2},1] @Checkerboard[5,1],
 "color" -> Green,
 "opacity"->0.8|>
  };


(* ::Input:: *)
(*Show[*)
(*seeMonde3D[mondeplanar, {{-20, 20}, {-20, 20}, {-20, 20}}],*)
(*DrawCam[c2w, InstrinsicMatrix[angleOfView, imageDimensions],focalLength, imageDimensions]*)
(*]*)


(* ::Input:: *)
(*points3dplanar = N[#[[ "Checkerpoints"]]]&/@mondeplanar;*)
(*points3dplanar=Flatten[points3dplanar,1];*)
(*points2dplanar = photo[w2i,points3dplanar];*)
(*M=ProjectionMatrix[points3dplanar,points2dplanar];*)
(*M = M*(w2i[[-1,-1]]/M[[-1,-1]])//Chop;*)
(*M//MatrixForm*)
(*w2i //MatrixForm*)


(* ::Input:: *)
(**)
