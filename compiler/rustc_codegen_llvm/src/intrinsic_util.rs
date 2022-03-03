use rustc_codegen_ssa::mir::operand::OperandRef;
use rustc_codegen_ssa::mir::place::PlaceRef;
use rustc_codegen_ssa::traits::{BaseTypeMethods, BuilderMethods, MiscMethods};
use rustc_hir::LangItem;
use rustc_middle::ty;
use rustc_middle::ty::subst::GenericArg;
use rustc_middle::ty::subst::{Subst, SubstsRef};
use rustc_span::DUMMY_SP;

use crate::builder::Builder;
use crate::value::Value;

pub(crate) fn codegen_collectable_calls<'ll, 'tcx>(
    builder: &mut Builder<'_, 'll, 'tcx>,
    op: OperandRef<'tcx, &'ll Value>,
    substs: SubstsRef<'tcx>,
) {
    let place = op.deref(builder.cx());
    codegen_collectable_calls_for_fields(builder, place, substs);
}

fn codegen_collectable_calls_for_fields<'ll, 'tcx>(
    builder: &mut Builder<'_, 'll, 'tcx>,
    place: PlaceRef<'tcx, &'ll Value>,
    substs: SubstsRef<'tcx>,
) {
    let ty = place.layout.ty.subst(builder.tcx, substs);
    match ty.kind() {
        ty::Infer(ty::FreshIntTy(_))
            | ty::Infer(ty::FreshFloatTy(_))
            | ty::Bool
            | ty::Int(_)
            | ty::Uint(_)
            | ty::Float(_)
            | ty::Never
            | ty::FnDef(..)
            | ty::FnPtr(_)
            | ty::Char
            | ty::GeneratorWitness(..)
            // We can't call `Collectable` trait methods behind a raw
            // pointer since we can't be sure the pointer is valid. If such
            // a case is important, then it must be handled in the
            // `Collectable::set_managed` implementor.
            | ty::RawPtr(..)
            | ty::Str => return,

            ty::Ref(_, _refty, ..) => {
                let place = builder.load_operand(place).deref(builder.cx());
                codegen_collectable_calls_for_fields(builder, place, substs)
            },
            ty::Adt(_adt_def, adt_substs) => {
                let set_col_did = match builder.tcx.lang_items().require(LangItem::SetCollectable) {
                    Ok(id) => id,
                    Err(err) => builder.tcx.sess.fatal(&err),
                };

                // First, add a call to the current ADT's collectable trait if
                // it exists.
                if ty.is_collectable(builder.tcx.at(DUMMY_SP), ty::ParamEnv::reveal_all()) {
                    let subs = builder.tcx.mk_substs(std::iter::once::<GenericArg<'tcx>>(ty.into()));
                    let inst = ty::Instance::resolve(builder.tcx, ty::ParamEnv::reveal_all(), set_col_did, subs).unwrap().unwrap();
                    let val = place.llval;
                    let f = builder.cx().get_fn_addr(inst);
                    let fn_ty = builder.type_func(&[builder.val_ty(val)], builder.type_void());
                    builder.call(fn_ty, f, &[val], None);
                }

                // Iterate over its fields to ensure that if their values
                // contain Collectable implementations, they are also called.
                for i in 0..place.layout.fields.count() {
                    let field = place.project_field(builder, i);
                    codegen_collectable_calls_for_fields(builder, field, adt_substs);
                }
            }
        _ => todo!(),
    }
}
