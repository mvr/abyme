use std::ops::{Add, Neg, Sub};

use euclid::*;

use num::Integer as IntegerTrait;
use rug::Integer;
use rug;
use rug::ops::{DivRounding, Pow};

use defs::*;

#[inline]
pub fn coerce_up(v: ChildVec) -> UVec {
    UVec::new(v.x, v.y)
}

#[inline]
pub fn truncate_up(v: ChildVec) -> UVec {
    UVec::new(
        v.x.div_floor(&(ZOOM_SCALE as i32)),
        v.y.div_floor(&(ZOOM_SCALE as i32)),
    )
}

#[inline]
pub fn mod_up(v: ChildVec) -> ChildVec {
    ChildVec::new(v.x % ZOOM_SCALE as i32, v.y % ZOOM_SCALE as i32)
}

#[inline]
pub fn split_up(v: ChildVec) -> (UVec, ChildVec) {
    (truncate_up(v), mod_up(v))
}

#[inline]
pub fn shift_down(v: UVec) -> ChildVec {
    ChildVec::new(v.x * (ZOOM_SCALE as i32), v.y * (ZOOM_SCALE as i32))
}

#[inline]
pub fn coerce_down(v: UVec) -> ChildVec {
    ChildVec::new(v.x, v.y)
}

#[inline]
pub fn add_vec<T: Add<T, Output = T>, U>(
    v: TypedVector2D<T, U>,
    u: TypedVector2D<T, U>,
) -> TypedVector2D<T, U> {
    TypedVector2D::new(v.x + u.x, v.y + u.y)
}

#[inline]
pub fn sub_vec<T: Sub<T, Output = T>, U>(
    v: TypedVector2D<T, U>,
    u: TypedVector2D<T, U>,
) -> TypedVector2D<T, U> {
    TypedVector2D::new(v.x - u.x, v.y - u.y)
}

#[inline]
pub fn scale_vec<U>(v: &TypedVector2D<Integer, U>, amount: Integer) -> TypedVector2D<Integer, U> {
    TypedVector2D::new(amount.clone() * &v.x, amount.clone() * &v.y)
}

#[inline]
pub fn div_vec<U>(v: &TypedVector2D<Integer, U>, amount: &Integer) -> TypedVector2D<Integer, U> {
    TypedVector2D::new(v.x.clone().div_floor(amount), v.y.clone().div_floor(amount))
}

pub fn scaled_bigint_to_float(int: &rug::Integer, scale: i16) -> f32 {
    if scale < 0 {
        (int * rug::Integer::from(ZOOM_SCALE).pow(-scale as u32)).to_f32()
    } else if scale == 0 {
        int.to_f32()
    } else {
        // self.zdelta > 0
        let denom = rug::Integer::from(ZOOM_SCALE).pow(scale as u32);
        rug::Rational::from((int, denom)).to_f32()
    }
}

// TODO: Could be more generic
pub fn smoothstep(x: f32) -> f32 {
    if x < 0.0 {
        0.0
    } else if x > 1.0 {
        1.0
    } else {
        x * x * x * (x * (x * 6.0 - 15.0) + 10.0)
    }
}

// TODO: turn the following into a pub trait TypedRectExt
// pub trait TypedRectExt<T, U> {
//     fn center(&self) -> TypedPoint2D<T, U>;
//     fn transform_to<U2>(&self, other: TypedRect<T, U2>) -> TypedTransform2D<f32, U, U2>;
//     fn transform_to_fit<U2>(&self, other: TypedRect<T, U2>) -> TypedTransform2D<f32, U, U2>;
// }

pub mod transform {
    use euclid::*;
    use defs::*;

    #[inline]
    pub fn screen_to_gl(
        resolution: TypedSize2D<u32, ScreenSpace>,
    ) -> TypedTransform2D<f32, ScreenSpace, GLSpace> {
        let scalex = 1.0 / (resolution.width as f32);
        let scaley = 1.0 / (resolution.height as f32);

        TypedTransform2D::create_scale(scalex, scaley)
    }

    #[inline]
    pub fn rect_to_rect<U, U2>(
        source: &TypedRect<f32, U>,
        target: &TypedRect<f32, U2>,
    ) -> TypedTransform2D<f32, U, U2> {
        let x_scale = target.size.width / source.size.width;
        let y_scale = target.size.height / source.size.height;
        TypedTransform2D::create_translation(-source.origin.x, -source.origin.y)
            .post_scale(x_scale, y_scale)
            .post_translate(TypedVector2D::from_untyped(&target
                .origin
                .to_untyped()
                .to_vector()))
    }

    // TODO: doesn't have to be f32
    // #[inline]
    pub fn fit_rect_in<U, U2>(
        source: &TypedRect<f32, U>,
        target: &TypedRect<f32, U2>,
    ) -> TypedTransform2D<f32, U, U2> {
        let source_ratio = source.size.width / source.size.height;
        let target_ratio = target.size.width / target.size.height;

        if source_ratio > target_ratio {
            // source is wider than target, so fills target width
            let image_height = target.size.width / source_ratio;
            let image_origin_y =
                (target.origin.y + target.size.height / 2.0) - (image_height / 2.0);

            let image = TypedRect::new(
                TypedPoint2D::new(target.origin.x, image_origin_y),
                TypedSize2D::new(target.size.width, image_height),
            );

            rect_to_rect(source, &image)
        } else {
            // source is thinner than target
            let image_width = target.size.height * source_ratio;
            let image_origin_x = (target.origin.x + target.size.width / 2.0) - (image_width / 2.0);

            let image = TypedRect::new(
                TypedPoint2D::new(image_origin_x, target.origin.y),
                TypedSize2D::new(image_width, target.size.height),
            );

            rect_to_rect(source, &image)
        }
    }

    pub fn transform_to_pair<U, U2>(
        transform: &TypedTransform2D<f32, U, U2>,
    ) -> (TypedVector2D<f32, U2>, f32) {
        let offset = transform.transform_point(&TypedPoint2D::new(0.0, 0.0));
        let scale = transform.transform_point(&TypedPoint2D::new(1.0, 0.0)) - offset;

        (offset.to_vector(), scale.x)
    }

    pub fn transform_from_pair<U, U2>(
        offset: TypedVector2D<f32, U2>,
        scale: f32,
    ) -> TypedTransform2D<f32, U, U2> {
        TypedTransform2D::identity()
            .post_scale(scale, scale)
            .post_translate(offset)
    }

    // pub fn transform_scale<U, U2>(
    //     transform: &TypedTransform2D<f32, U, U2>,
    //     amount: f32,
    // ) -> TypedTransform2D<f32, U, U2> {
    //     let (offset, scale) = transform_to_pair(transform);

    //     transform_from_pair(offset * amount, 1.0 + (scale - 1.0) * amount)
    // }

    pub fn lerp<U, U2>(
        transform1: &TypedTransform2D<f32, U, U2>,
        transform2: &TypedTransform2D<f32, U, U2>,
        amount: f32,
    ) -> TypedTransform2D<f32, U, U2> {
        let (offset1, scale1) = transform_to_pair(transform1);
        let (offset2, scale2) = transform_to_pair(transform2);

        transform_from_pair(
            offset1 + (offset2 - offset1) * amount,
            scale1 + (scale2 - scale1) * amount,
        )
    }
}

pub trait To2dGlTransform {
    fn to_gl_mat3(&self) -> [[f32; 3]; 3];
}

impl<U> To2dGlTransform for TypedTransform2D<f32, U, GLSpace> {
    fn to_gl_mat3(&self) -> [[f32; 3]; 3] {
        let [m11, m12, m21, m22, m31, m32] = self.to_row_major_array();

        #[rustfmt_skip]
        [[m11, m12, 0.0],
         [m21, m22, 0.0],
         [m31, m32, 1.0]]
    }
}

pub mod time {
    use std::time;

    // TODO: f64?
    pub fn duration_to_secs(time_delta: time::Duration) -> f32 {
        time_delta.as_secs() as f32 + time_delta.subsec_nanos() as f32 * 1e-9
    }
}
