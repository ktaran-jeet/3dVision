(* ::Package:: *)

(* ::Input:: *)
(*Manipulate[*)
(* Module[{scaling, rotation, translation, transform},*)
(*  *)
(*  (* Define transformations based on slider inputs *)*)
(*  scaling = ScalingTransform[{scale, scale, scale}];*)
(*  rotation = RotationTransform[rotationAngle, {rx, ry, rz}, {0, 0, 0}];*)
(*  translation = TranslationTransform[{tx, ty, tz}];*)
(*  *)
(*  (* Combine transformations: scale -> rotate -> translate *)*)
(*  transform = translation@*rotation@*scaling;*)
(*  *)
(*  axes = {*)
(*    Red, Line[{{-10, 0, 0}, {10, 0, 0}}], Text[Style["X", Red, Bold], {10, 0, 0.5}],*)
(*    Green, Line[{{0, -10, 0}, {0, 10, 0}}], Text[Style["Y", Green, Bold], {0, 10, 0.5}],*)
(*    Blue, Line[{{0, 0, -10}, {0, 0, 10}}], Text[Style["Z", Blue, Bold], {0.5, 0, 10}]*)
(*  };*)
(*  (* Display transformed object *)*)
(*  Graphics3D[{axes, *)
(*    GeometricTransformation[obj, transform]}, *)
(*   PlotRange -> {{-10, 10}, {-10, 10}, {-10, 10}}, *)
(*   Boxed -> True]*)
(*  ],*)
(* *)
(* (* Parameters with sliders *)*)
(* {{scale, 1, "Scale"}, 0.1, 5, 0.1},*)
(* {{rotationAngle, 0, "Rotation Angle (radians)"}, 0, 2 \[Pi]},*)
(* {{rx, 0, "Rotation Axis X"}, -1, 1, 0.1},*)
(* {{ry, 0, "Rotation Axis Y"}, -1, 1, 0.1},*)
(* {{rz, 1, "Rotation Axis Z"}, -1, 1, 0.1},*)
(* {{tx, 0, "Translate X"}, -10, 10, 0.1},*)
(* {{ty, 0, "Translate Y"}, -10, 10, 0.1},*)
(* {{tz, 0, "Translate Z"}, -10, 10, 0.1},*)
(* *)
(* (* 3D object to transform *)*)
(* Initialization :> (obj = Cylinder[])*)
(*]*)


(* ::Input:: *)
(**)