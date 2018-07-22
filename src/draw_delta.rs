// use rug::ops::{DivRounding, Pow};
// use rug::Integer;
// use std::ops::{Add, Neg, Sub};
// use std::collections::VecDeque;

// use euclid::*;

// use defs::*;
// use math;
// use delta::*;

// // A "vector" in the world, possibly going up or down levels
// // TODO: Do we need a `Dyadic`?

// #[derive(Clone, Debug, PartialEq)]
// enum Step {
//     Whole { delta: Delta },
//     PartialZero { transform: TypedTransform2D<f32, UniverseSpace, UniverseSpace> },
//     PartialUp { transform: TypedTransform2D<f32, UniverseSpace, ParentSpace> },
//     PartialDown { transform: TypedTransform2D<f32, UniverseSpace, ChildSpace> }
// }

// #[derive(Clone, Debug, PartialEq)] // MUST TODO: this Eq is dangerous!
// pub struct DrawDelta {
//     steps: VecDeque<Step>,
// }
