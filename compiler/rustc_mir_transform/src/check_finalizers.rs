#![allow(rustc::untranslatable_diagnostic)]
#![allow(rustc::diagnostic_outside_of_impl)]
use rustc_hir::lang_items::LangItem;
use rustc_middle::mir::visit::PlaceContext;
use rustc_middle::mir::visit::Visitor;
use rustc_middle::mir::*;
use rustc_middle::ty::{self, ParamEnv, Ty, TyCtxt};
use rustc_span::symbol::sym;
use rustc_span::Span;
use rustc_trait_selection::infer::InferCtxtExt as _;
use rustc_trait_selection::infer::TyCtxtInferExt;

#[derive(PartialEq)]
pub struct CheckFinalizers;

#[derive(Debug)]
enum FinalizerErrorKind<'tcx> {
    /// Does not implement `Send` + `Sync`
    NotSendAndSync(Span),
    /// Does not implement `FinalizerSafe`
    NotFinalizerSafe(Ty<'tcx>, Span),
    /// Does not implement `ReferenceFree`
    NotReferenceFree,
    /// Uses a trait object whose concrete type is unknown
    UnknownTraitObject,
    /// Calls a function whose definition is unavailable, so we can't be certain it's safe.
    MissingFnDef,
}

impl<'tcx> FinalizerErrorKind<'tcx> {
    fn emit(&self, cx: &FinalizationCtxt<'tcx>) {
        let arg_sp = cx.tcx.sess.source_map().span_to_snippet(cx.arg).unwrap();
        let mut err = cx.tcx.sess.psess.dcx.struct_span_err(
            cx.arg,
            format!("`{arg_sp}` has a drop method which cannot be safely finalized."),
        );
        match self {
            Self::NotSendAndSync(span) => {
                err.span_label(*span, "caused by the expression in `fn drop(&mut)` here because");
                err.span_label(*span, "it uses a type which is not safe to use in a finalizer.");
                err.help("`Gc` runs finalizers on a separate thread, so drop methods\nmust only use values whose types implement `Send` + `Sync`.");
            }
            Self::NotFinalizerSafe(ty, span) => {
                // Special-case `Gc` types for more friendly errors
                if cx.is_gc(*ty) {
                    err.span_label(
                        *span,
                        "caused by the expression here in `fn drop(&mut)` because",
                    );
                    err.span_label(*span, "it uses another `Gc` type.");
                    err.span_label(
                        cx.ctor,
                        format!("Finalizers cannot safely dereference other `Gc`s, because they might have already been finalised."),
                    );
                } else {
                    err.span_label(
                        *span,
                        "caused by the expression in `fn drop(&mut)` here because",
                    );
                    err.span_label(
                        *span,
                        "it uses a type which is not safe to use in a finalizer.",
                    );
                    err.help("`Gc` runs finalizers on a separate thread, so drop methods\nmust only use values whose types implement `FinalizerSafe`.");
                    err.span_label(
                        cx.ctor,
                        format!("`Gc::new` requires that it implements the `FinalizeSafe` trait.",),
                    );
                }
            }
            Self::NotReferenceFree => {
                err.span_label(
                    cx.arg,
                    "contains a reference (&) which is not safe to be used in a finalizer.",
                );
                err.span_label(
                    cx.ctor,
                    format!("`Gc::new` requires finalizable types to be reference free.",),
                );
            }
            Self::MissingFnDef => {
                err.span_label(cx.arg, "contains a function call which may be unsafe.");
            }
            Self::UnknownTraitObject => {
                err.span_label(cx.arg, "contains a trait object whose implementation is unknown.");
            }
        }
        err.emit();
    }
}

impl<'tcx> MirPass<'tcx> for CheckFinalizers {
    fn run_pass(&self, tcx: TyCtxt<'tcx>, body: &mut Body<'tcx>) {
        let ctor = tcx.get_diagnostic_item(sym::gc_ctor);

        if ctor.is_none() {
            return;
        }

        let ctor_did = ctor.unwrap();
        let param_env = tcx.param_env(body.source.def_id());

        for block in body.basic_blocks.iter() {
            match &block.terminator {
                Some(Terminator { kind: TerminatorKind::Call { func, args, .. }, source_info }) => {
                    let func_ty = func.ty(body, tcx);
                    if let ty::FnDef(fn_did, ..) = func_ty.kind() {
                        if *fn_did == ctor_did {
                            let arg = match &args[0].node {
                                Operand::Copy(place) | Operand::Move(place) => {
                                    body.local_decls()[place.local].source_info.span
                                }
                                Operand::Constant(con) => con.span,
                            };
                            let arg_ty = args[0].node.ty(body, tcx);

                            let mut finalizer_cx =
                                FinalizationCtxt { ctor: source_info.span, arg, tcx, param_env };
                            finalizer_cx.check_for_dangling_refs(arg_ty);
                            finalizer_cx.check(arg_ty);
                        }
                    }
                }
                _ => {}
            }
        }
    }
}

struct FinalizationCtxt<'tcx> {
    ctor: Span,
    arg: Span,
    tcx: TyCtxt<'tcx>,
    param_env: ParamEnv<'tcx>,
}

impl<'tcx> FinalizationCtxt<'tcx> {
    fn check_for_dangling_refs(&mut self, ty: Ty<'tcx>) {
        if !self.is_reference_free(ty) && self.tcx.needs_finalizer_raw(self.param_env.and(ty)) {
            let err = FinalizerErrorKind::NotReferenceFree;
            err.emit(self);
        }
    }

    fn check(&mut self, ty: Ty<'tcx>) {
        if !self.tcx.needs_finalizer_raw(self.param_env.and(ty)) {
            return;
        }

        if self.is_finalize_unchecked(ty) {
            return;
        }

        if self.is_send(ty) && self.is_sync(ty) && self.is_finalizer_safe(ty) {
            return;
        }

        // We must now recurse through the `Ty`'s component types and search for
        // all the `Drop` impls. If we find any, we have to check that there are
        // no unsound projections into fields in their drop method body. More
        // specifically: if one of the drop methods dereferences a field which
        // is !FinalizerSafe, we must throw an error.
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
            | ty::RawPtr(..)
            | ty::Ref(..)
            | ty::Str
            | ty::Error(..)
            | ty::Foreign(..) => {
                // None of these types can implement Drop.
                return;
            }
            ty::Dynamic(..) => {
                // Dropping a trait object uses a virtual call, so we can't
                // work out which drop method to look at compile-time. This
                // means we must be more conservative and bail with an error
                // here, even if the drop impl itself would have been safe.
                let err = FinalizerErrorKind::UnknownTraitObject;
                err.emit(self);
            }
            ty::Slice(ty) => self.check(*ty),
            ty::Array(elem_ty, ..) => {
                self.check(*elem_ty);
            }
            ty::Tuple(fields) => {
                // Each tuple field must be individually checked for a `Drop`
                // impl.
                fields.iter().for_each(|f_ty| self.check(f_ty));
            }
            ty::Adt(def, substs) if !self.is_copy(ty) => {
                if def.has_dtor(self.tcx) {
                    if def.is_box() {
                        // This is a special case because Box has an empty drop
                        // method which is filled in later by the compiler.
                        let err = FinalizerErrorKind::MissingFnDef;
                        err.emit(self);
                    }

                    let drop_trait = self.tcx.require_lang_item(LangItem::Drop, None);
                    let drop_fn = self.tcx.associated_item_def_ids(drop_trait)[0];
                    let substs = self.tcx.mk_args_trait(ty, substs.into_iter());
                    let instance = ty::Instance::resolve(self.tcx, self.param_env, drop_fn, substs)
                        .unwrap()
                        .unwrap();
                    let mir = self.tcx.instance_mir(instance.def);
                    let mut checker = ProjectionChecker { cx: self, body: mir };
                    checker.visit_body(&mir);
                }

                for field in def.all_fields() {
                    let field_ty = self.tcx.type_of(field.did).instantiate(self.tcx, substs);
                    self.check(field_ty);
                }
            }
            _ => (),
        }
    }

    fn is_finalizer_safe(&self, ty: Ty<'tcx>) -> bool {
        let t = self.tcx.get_diagnostic_item(sym::FinalizerSafe).unwrap();
        return self
            .tcx
            .infer_ctxt()
            .build()
            .type_implements_trait(t, [ty], self.param_env)
            .must_apply_modulo_regions();
    }

    fn is_reference_free(&self, ty: Ty<'tcx>) -> bool {
        let t = self.tcx.get_diagnostic_item(sym::ReferenceFree).unwrap();
        return self
            .tcx
            .infer_ctxt()
            .build()
            .type_implements_trait(t, [ty], self.param_env)
            .must_apply_modulo_regions();
    }

    fn is_copy(&self, ty: Ty<'tcx>) -> bool {
        ty.is_copy_modulo_regions(self.tcx, self.param_env)
    }

    fn is_send(&self, ty: Ty<'tcx>) -> bool {
        let t = self.tcx.get_diagnostic_item(sym::Send).unwrap();
        return self
            .tcx
            .infer_ctxt()
            .build()
            .type_implements_trait(t, [ty], self.param_env)
            .must_apply_modulo_regions();
    }

    fn is_sync(&self, ty: Ty<'tcx>) -> bool {
        let t = self.tcx.get_diagnostic_item(sym::Sync).unwrap();
        return self
            .tcx
            .infer_ctxt()
            .build()
            .type_implements_trait(t, [ty], self.param_env)
            .must_apply_modulo_regions();
    }

    fn is_gc(&self, ty: Ty<'tcx>) -> bool {
        if let ty::Adt(def, ..) = ty.kind() {
            if def.did() == self.tcx.get_diagnostic_item(sym::gc).unwrap() {
                return true;
            }
        }
        return false;
    }

    fn is_finalize_unchecked(&self, ty: Ty<'tcx>) -> bool {
        if let ty::Adt(def, ..) = ty.kind() {
            if def.did() == self.tcx.get_diagnostic_item(sym::FinalizeUnchecked).unwrap() {
                return true;
            }
        }
        return false;
    }
}

struct ProjectionChecker<'a, 'tcx> {
    cx: &'a FinalizationCtxt<'tcx>,
    body: &'a Body<'tcx>,
}

impl<'a, 'tcx> Visitor<'tcx> for ProjectionChecker<'a, 'tcx> {
    fn visit_projection(
        &mut self,
        place_ref: PlaceRef<'tcx>,
        context: PlaceContext,
        location: Location,
    ) {
        for (_, proj) in place_ref.iter_projections() {
            match proj {
                ProjectionElem::Field(_, ty) => {
                    let span = self.body.source_info(location).span;
                    if !self.cx.is_send(ty) || !self.cx.is_sync(ty) {
                        let err = FinalizerErrorKind::NotSendAndSync(span);
                        err.emit(self.cx);
                    }
                    if !self.cx.is_finalizer_safe(ty) {
                        let err = FinalizerErrorKind::NotFinalizerSafe(ty, span);
                        err.emit(self.cx);
                    }
                }
                _ => (),
            }
        }
        self.super_projection(place_ref, context, location);
    }

    fn visit_terminator(&mut self, terminator: &Terminator<'tcx>, _: Location) {
        if let TerminatorKind::Call { ref args, .. } = terminator.kind {
            for caller_arg in self.body.args_iter() {
                let recv_ty = self.body.local_decls()[caller_arg].ty;
                for arg in args.iter() {
                    let arg_ty = arg.node.ty(self.body, self.cx.tcx);
                    if arg_ty == recv_ty {
                        // Currently, we do not recurse into function calls
                        // to see whether they access `!FinalizerSafe`
                        // fields, so we must throw an error in `drop`
                        // methods which call other functions and pass
                        // `self` as an argument.
                        //
                        // Here, we throw an error if `drop(&mut self)`
                        // calls a function with an argument that has the
                        // same type as the drop receiver (e.g. foo(x:
                        // &Self)). This approximation will always prevent
                        // unsound `drop` methods, however, it is overly
                        // conservative and will prevent correct examples
                        // like below from compiling:
                        //
                        // ```
                        // fn drop(&mut self) {
                        //   let x = Self { ... };
                        //   x.foo();
                        // }
                        // ```
                        //
                        // This example is sound, because `x` is a local
                        // that was instantiated on the finalizer thread, so
                        // its fields are always safe to access from inside
                        // this drop method.
                        //
                        // However, this will not compile, because the
                        // receiver for `x.foo()` is the same type as the
                        // `self` reference. To fix this, we would need to
                        // do a def-use analysis on the self reference to
                        // find every MIR local which refers to it that ends
                        // up being passed to a call terminator. This is not
                        // trivial to do at the moment.
                        let err = FinalizerErrorKind::MissingFnDef;
                        err.emit(self.cx);
                    }
                }
            }
        }
    }
}
