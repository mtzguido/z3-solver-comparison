(set-option :global-decls false)
(set-option :smt.mbqi false)
(set-option :auto_config false)
(set-option :produce-unsat-cores true)
(set-option :model true)
(set-option :smt.case_split 3)
(set-option :rewriter.enable_der false)
(set-option :rewriter.sort_disjunctions false)
(set-option :pi.decompose_patterns false)
(set-option :smt.relevancy 2)
(declare-sort FString)
(declare-fun FString_constr_id (FString) Int)

(declare-sort Term)
(declare-fun Term_constr_id (Term) Int)
(declare-sort Dummy_sort)
(declare-fun Dummy_value () Dummy_sort)
(declare-datatypes () ((Fuel 
(ZFuel) 
(SFuel (prec Fuel)))))
(declare-fun MaxIFuel () Fuel)
(declare-fun MaxFuel () Fuel)
(declare-fun PreType (Term) Term)
(declare-fun Valid (Term) Bool)
(declare-fun HasTypeFuel (Fuel Term Term) Bool)
(define-fun HasTypeZ ((x Term) (t Term)) Bool
(HasTypeFuel ZFuel x t))
(define-fun HasType ((x Term) (t Term)) Bool
(HasTypeFuel MaxIFuel x t))
(declare-fun IsTotFun (Term) Bool)

                ;;fuel irrelevance
(assert (forall ((f Fuel) (x Term) (t Term))
(! (= (HasTypeFuel (SFuel f) x t)
(HasTypeZ x t))
:pattern ((HasTypeFuel (SFuel f) x t)))))
(declare-fun NoHoist (Term Bool) Bool)
;;no-hoist
(assert (forall ((dummy Term) (b Bool))
(! (= (NoHoist dummy b)
b)
:pattern ((NoHoist dummy b)))))
(define-fun  IsTyped ((x Term)) Bool
(exists ((t Term)) (HasTypeZ x t)))
(declare-fun ApplyTF (Term Fuel) Term)
(declare-fun ApplyTT (Term Term) Term)
(declare-fun Prec (Term Term) Bool)
(assert (forall ((x Term) (y Term) (z Term))
(! (implies (and (Prec x y) (Prec y z))
(Prec x z))
                                   :pattern ((Prec x z) (Prec x y)))))
(assert (forall ((x Term) (y Term))
(implies (Prec x y)
(not (Prec y x)))))
(declare-fun Closure (Term) Term)
(declare-fun ConsTerm (Term Term) Term)
(declare-fun ConsFuel (Fuel Term) Term)
(declare-fun Tm_uvar (Int) Term)
(define-fun Reify ((x Term)) Term x)
(declare-fun Prims.precedes (Term Term Term Term) Term)
(declare-fun Range_const (Int) Term)
(declare-fun _mul (Int Int) Int)
(declare-fun _div (Int Int) Int)
(declare-fun _mod (Int Int) Int)
(declare-fun __uu__PartialApp () Term)
(assert (forall ((x Int) (y Int)) (! (= (_mul x y) (* x y)) :pattern ((_mul x y)))))
(assert (forall ((x Int) (y Int)) (! (= (_div x y) (div x y)) :pattern ((_div x y)))))
(assert (forall ((x Int) (y Int)) (! (= (_mod x y) (mod x y)) :pattern ((_mod x y)))))
(declare-fun _rmul (Real Real) Real)
(declare-fun _rdiv (Real Real) Real)
(assert (forall ((x Real) (y Real)) (! (= (_rmul x y) (* x y)) :pattern ((_rmul x y)))))
(assert (forall ((x Real) (y Real)) (! (= (_rdiv x y) (/ x y)) :pattern ((_rdiv x y)))))
(define-fun Unreachable () Bool false)
; <start constructor FString_const>

;;;;;;;;;;;;;;;;Constructor
(declare-fun FString_const (Int) FString)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= 0
(FString_constr_id (FString_const @u0)))
 

:pattern ((FString_const @u0))
:qid constructor_distinct_FString_const))
:named constructor_distinct_FString_const))
;;;;;;;;;;;;;;;;Projector
(declare-fun FString_const_proj_0 (FString) Int)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= (FString_const_proj_0 (FString_const @u0))
@u0)
 

:pattern ((FString_const @u0))
:qid projection_inverse_FString_const_proj_0))
:named projection_inverse_FString_const_proj_0))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FString_const ((__@u0 FString)) Bool
 (and (= (FString_constr_id __@u0)
0)
(= __@u0
(FString_const (FString_const_proj_0 __@u0)))))

; </end constructor FString_const>


; <start constructor Tm_type>

;;;;;;;;;;;;;;;;Constructor
(declare-fun Tm_type () Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (= 2
(Term_constr_id Tm_type))
:named constructor_distinct_Tm_type))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Tm_type ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
2)
(= __@x0
Tm_type)))

; </end constructor Tm_type>


; <start constructor Tm_arrow>

;;;;;;;;;;;;;;;;Constructor
(declare-fun Tm_arrow (Int) Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= 3
(Term_constr_id (Tm_arrow @u0)))
 

:pattern ((Tm_arrow @u0))
:qid constructor_distinct_Tm_arrow))
:named constructor_distinct_Tm_arrow))
;;;;;;;;;;;;;;;;Projector
(declare-fun Tm_arrow_id (Term) Int)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= (Tm_arrow_id (Tm_arrow @u0))
@u0)
 

:pattern ((Tm_arrow @u0))
:qid projection_inverse_Tm_arrow_id))
:named projection_inverse_Tm_arrow_id))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Tm_arrow ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
3)
(= __@x0
(Tm_arrow (Tm_arrow_id __@x0)))))

; </end constructor Tm_arrow>


; <start constructor Tm_unit>

;;;;;;;;;;;;;;;;Constructor
(declare-fun Tm_unit () Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (= 6
(Term_constr_id Tm_unit))
:named constructor_distinct_Tm_unit))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Tm_unit ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
6)
(= __@x0
Tm_unit)))

; </end constructor Tm_unit>


; <start constructor BoxInt>

;;;;;;;;;;;;;;;;Constructor
(declare-fun BoxInt (Int) Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= 7
(Term_constr_id (BoxInt @u0)))
 

:pattern ((BoxInt @u0))
:qid constructor_distinct_BoxInt))
:named constructor_distinct_BoxInt))
;;;;;;;;;;;;;;;;Projector
(declare-fun BoxInt_proj_0 (Term) Int)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 Int))
 (! (= (BoxInt_proj_0 (BoxInt @u0))
@u0)
 

:pattern ((BoxInt @u0))
:qid projection_inverse_BoxInt_proj_0))
:named projection_inverse_BoxInt_proj_0))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-BoxInt ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
7)
(= __@x0
(BoxInt (BoxInt_proj_0 __@x0)))))

; </end constructor BoxInt>


; <start constructor BoxBool>

;;;;;;;;;;;;;;;;Constructor
(declare-fun BoxBool (Bool) Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 Bool))
 (! (= 8
(Term_constr_id (BoxBool @u0)))
 

:pattern ((BoxBool @u0))
:qid constructor_distinct_BoxBool))
:named constructor_distinct_BoxBool))
;;;;;;;;;;;;;;;;Projector
(declare-fun BoxBool_proj_0 (Term) Bool)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 Bool))
 (! (= (BoxBool_proj_0 (BoxBool @u0))
@u0)
 

:pattern ((BoxBool @u0))
:qid projection_inverse_BoxBool_proj_0))
:named projection_inverse_BoxBool_proj_0))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-BoxBool ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
8)
(= __@x0
(BoxBool (BoxBool_proj_0 __@x0)))))

; </end constructor BoxBool>


; <start constructor BoxString>

;;;;;;;;;;;;;;;;Constructor
(declare-fun BoxString (FString) Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 FString))
 (! (= 9
(Term_constr_id (BoxString @u0)))
 

:pattern ((BoxString @u0))
:qid constructor_distinct_BoxString))
:named constructor_distinct_BoxString))
;;;;;;;;;;;;;;;;Projector
(declare-fun BoxString_proj_0 (Term) FString)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 FString))
 (! (= (BoxString_proj_0 (BoxString @u0))
@u0)
 

:pattern ((BoxString @u0))
:qid projection_inverse_BoxString_proj_0))
:named projection_inverse_BoxString_proj_0))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-BoxString ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
9)
(= __@x0
(BoxString (BoxString_proj_0 __@x0)))))

; </end constructor BoxString>


; <start constructor BoxReal>

;;;;;;;;;;;;;;;;Constructor
(declare-fun BoxReal (Real) Term)
;;;;;;;;;;;;;;;;Constructor distinct
;;; Fact-ids: 
(assert (! (forall ((@u0 Real))
 (! (= 10
(Term_constr_id (BoxReal @u0)))
 

:pattern ((BoxReal @u0))
:qid constructor_distinct_BoxReal))
:named constructor_distinct_BoxReal))
;;;;;;;;;;;;;;;;Projector
(declare-fun BoxReal_proj_0 (Term) Real)
;;;;;;;;;;;;;;;;Projection inverse
;;; Fact-ids: 
(assert (! (forall ((@u0 Real))
 (! (= (BoxReal_proj_0 (BoxReal @u0))
@u0)
 

:pattern ((BoxReal @u0))
:qid projection_inverse_BoxReal_proj_0))
:named projection_inverse_BoxReal_proj_0))
;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-BoxReal ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
10)
(= __@x0
(BoxReal (BoxReal_proj_0 __@x0)))))

; </end constructor BoxReal>

(declare-fun Prims.precedes@tok () Term)
(assert
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
(! (= (ApplyTT (ApplyTT (ApplyTT (ApplyTT Prims.precedes@tok
@x0)
@x1)
@x2)
@x3)
(Prims.precedes @x0 @x1 @x2 @x3))

:pattern ((ApplyTT (ApplyTT (ApplyTT (ApplyTT Prims.precedes@tok
@x0)
@x1)
@x2)
@x3)))))

(declare-fun Prims.lex_t () Term)
(assert (forall ((t1 Term) (t2 Term) (e1 Term) (e2 Term))
(! (iff (Valid (Prims.precedes t1 t2 e1 e2))
(Valid (Prims.precedes Prims.lex_t Prims.lex_t e1 e2)))
:pattern (Prims.precedes t1 t2 e1 e2))))
(assert (forall ((t1 Term) (t2 Term))
(! (iff (Valid (Prims.precedes Prims.lex_t Prims.lex_t t1 t2)) 
(Prec t1 t2))
:pattern ((Prims.precedes Prims.lex_t Prims.lex_t t1 t2)))))
(assert (forall ((e Term) (t Term))
(! (implies (HasType e t)
(Valid t))
:pattern ((HasType e t)
(Valid t))
:qid __prelude_valid_intro)))


;;; Start module Prims

; Externals for module Prims


; <Start encoding Prims.attribute>

(declare-fun Prims.attribute () Term)

; </end encoding Prims.attribute>


; <Start encoding Prims.cps>

(declare-fun Prims.cps () Term)

; </end encoding Prims.cps>


; <Start encoding Prims.hasEq>

(declare-fun Prims.hasEq (Term) Term)
;;;;;;;;;;;;;;;;_: Type -> Prims.GTot Type
(declare-fun Tm_arrow_ef9cb512a25ee351fa5536d617490497 () Term)
(declare-fun Prims.hasEq@tok () Term)

; </end encoding Prims.hasEq>


; <Start encoding Prims.eqtype>

(declare-fun Prims.eqtype () Term)
(declare-fun Tm_refine_414d0a9f578ab0048252f8c8f552b99f () Term)
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name Prims.eqtype; Namespace Prims
(assert (! 
;; def=prims.fst(52,14-52,31); use=prims.fst(52,14-52,31)
(forall ((@u0 Fuel) (@x1 Term))
 (! (iff (HasTypeFuel @u0
@x1
Tm_refine_414d0a9f578ab0048252f8c8f552b99f)
(and (HasTypeFuel @u0
@x1
Tm_type)

;; def=prims.fst(52,23-52,30); use=prims.fst(52,23-52,30)
(Valid 
;; def=prims.fst(52,23-52,30); use=prims.fst(52,23-52,30)
(Prims.hasEq @x1)
)
))
 

:pattern ((HasTypeFuel @u0
@x1
Tm_refine_414d0a9f578ab0048252f8c8f552b99f))
:qid refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f))

:named refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f))
;;;;;;;;;;;;;;;;Equation for Prims.eqtype
;;; Fact-ids: Name Prims.eqtype; Namespace Prims
(assert (! (= Prims.eqtype
Tm_refine_414d0a9f578ab0048252f8c8f552b99f)
:named equation_Prims.eqtype))

; </end encoding Prims.eqtype>


; <Start encoding Prims.bool>

(declare-fun Prims.bool () Term)
;;;;;;;;;;;;;;;;function token typing
;;; Fact-ids: Name Prims.bool; Namespace Prims
(assert (! (HasType Prims.bool
Prims.eqtype)
:named function_token_typing_Prims.bool))
;;;;;;;;;;;;;;;;pretyping
;;; Fact-ids: Name Prims.bool; Namespace Prims
(assert (! 
;; def=prims.fst(59,5-59,9); use=prims.fst(59,5-59,9)
(forall ((@x0 Term) (@u1 Fuel))
 (! (implies (HasTypeFuel @u1
@x0
Prims.bool)
(= Prims.bool
(PreType @x0)))
 

:pattern ((HasTypeFuel @u1
@x0
Prims.bool))
:qid Prims_pretyping_f537159ed795b314b4e58c260361ae86))

:named Prims_pretyping_f537159ed795b314b4e58c260361ae86))
;;;;;;;;;;;;;;;;bool typing
;;; Fact-ids: Name Prims.bool; Namespace Prims
(assert (! (forall ((@u0 Bool))
 (! (HasType (BoxBool @u0)
Prims.bool)
 

:pattern ((BoxBool @u0))
:qid bool_typing))
:named bool_typing))

; </end encoding Prims.bool>


; <Start encoding Prims.empty>

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.empty () Term)

; <Start encoding Prims.empty>


; <start constructor Prims.empty>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.empty ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
111)
(= __@x0
Prims.empty)))

; </end constructor Prims.empty>


; </end encoding Prims.empty>


; </end encoding Prims.empty>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.trivial () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.T () Term)
;;;;;;;;;;;;;;;;data constructor proxy: T
(declare-fun Prims.T@tok () Term)

; <Start encoding Prims.trivial>


; <start constructor Prims.trivial>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.trivial ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
116)
(= __@x0
Prims.trivial)))

; </end constructor Prims.trivial>


; </end encoding Prims.trivial>


; <Start encoding Prims.T>


; <start constructor Prims.T>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.T ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
122)
(= __@x0
Prims.T)))

; </end constructor Prims.T>


; </end encoding Prims.T>


; </end encoding >


; <Start encoding Prims.uu___is_T>

(declare-fun Prims.uu___is_T (Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.trivial -> Prims.bool
(declare-fun Tm_arrow_053f01f066524059a49c5dc621e6494a () Term)
(declare-fun Prims.uu___is_T@tok () Term)

; </end encoding Prims.uu___is_T>


; <Start encoding Prims.unit>

(declare-fun Prims.unit () Term)

; </end encoding Prims.unit>


; <Start encoding Prims.squash>

(declare-fun Prims.squash (Term) Term)

(declare-fun Prims.squash@tok () Term)
(declare-fun Tm_refine_2de20c066034c13bf76e9c0b94f4806c (Term) Term)

; </end encoding Prims.squash>


; <Start encoding Prims.auto_squash>

(declare-fun Prims.auto_squash (Term) Term)

(declare-fun Prims.auto_squash@tok () Term)

; </end encoding Prims.auto_squash>


; <Start encoding Prims.logical>

(declare-fun Prims.logical () Term)

; </end encoding Prims.logical>


; <Start encoding Prims.smt_theory_symbol>

(declare-fun Prims.smt_theory_symbol () Term)

; </end encoding Prims.smt_theory_symbol>


; <Start encoding Prims.l_True>

(declare-fun Prims.l_True () Term)

; </end encoding Prims.l_True>


; <Start encoding Prims.l_False>

(declare-fun Prims.l_False () Term)

; </end encoding Prims.l_False>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.equals (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.equals@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.equals@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.equals@x2 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun Prims.equals@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Refl (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Refl_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Refl_x (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Refl
(declare-fun Prims.Refl@tok () Term)
;;;;;;;;;;;;;;;;Prims.equals x x
(declare-fun Tm_arrow_8e00c6263684633abbc1d1a87608e391 () Term)

; <Start encoding Prims.equals>


; <start constructor Prims.equals>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.equals ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
134)
(exists ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= __@x0
(Prims.equals @x0
@x1
@x2))
 
;;no pats
:qid is-Prims.equals))))

; </end constructor Prims.equals>


; </end encoding Prims.equals>


; <Start encoding Prims.Refl>


; <start constructor Prims.Refl>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Refl ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
141)
(= __@x0
(Prims.Refl (Prims.Refl_a __@x0)
(Prims.Refl_x __@x0)))))

; </end constructor Prims.Refl>


; </end encoding Prims.Refl>


; </end encoding >


; <Start encoding Prims.uu___is_Refl>

(declare-fun Prims.uu___is_Refl (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.equals x _ -> Prims.bool
(declare-fun Tm_arrow_2a4540f76c8969717ea911077d7b4d15 () Term)
(declare-fun Prims.uu___is_Refl@tok () Term)

; </end encoding Prims.uu___is_Refl>


; <Start encoding Prims.eq2>

(declare-fun Prims.eq2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> y: a -> Prims.logical
(declare-fun Tm_arrow_1ec40cec1da281b45a559c74dd57f3b7 () Term)
(declare-fun Prims.eq2@tok () Term)

; </end encoding Prims.eq2>


; <Start encoding Prims.b2t>

(declare-fun Prims.b2t (Term) Term)

; </end encoding Prims.b2t>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.pair (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.pair@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.pair@x1 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun Prims.pair@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Pair (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Pair_p (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Pair_q (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Pair__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Pair__2 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Pair
(declare-fun Prims.Pair@tok () Term)
;;;;;;;;;;;;;;;;_1: p -> _2: q -> Prims.pair p q
(declare-fun Tm_arrow_e2b0096073073582c70f249d40f91c5d () Term)

; <Start encoding Prims.pair>


; <start constructor Prims.pair>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.pair ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
150)
(exists ((@x0 Term) (@x1 Term))
 (! (= __@x0
(Prims.pair @x0
@x1))
 
;;no pats
:qid is-Prims.pair))))

; </end constructor Prims.pair>


; </end encoding Prims.pair>


; <Start encoding Prims.Pair>


; <start constructor Prims.Pair>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Pair ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
157)
(= __@x0
(Prims.Pair (Prims.Pair_p __@x0)
(Prims.Pair_q __@x0)
(Prims.Pair__1 __@x0)
(Prims.Pair__2 __@x0)))))

; </end constructor Prims.Pair>


; </end encoding Prims.Pair>


; </end encoding >


; <Start encoding Prims.uu___is_Pair>

(declare-fun Prims.uu___is_Pair (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.pair p q -> Prims.bool
(declare-fun Tm_arrow_0a519c999e1325381ee4c9b1d93a06b2 () Term)
(declare-fun Prims.uu___is_Pair@tok () Term)

; </end encoding Prims.uu___is_Pair>


; <Start encoding Prims.__proj__Pair__item___1>

(declare-fun Prims.__proj__Pair__item___1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.pair p q -> p
(declare-fun Tm_arrow_214b45775d1504fb2699ff0d156b6857 () Term)
(declare-fun Prims.__proj__Pair__item___1@tok () Term)

; </end encoding Prims.__proj__Pair__item___1>


; <Start encoding Prims.__proj__Pair__item___2>

(declare-fun Prims.__proj__Pair__item___2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.pair p q -> q
(declare-fun Tm_arrow_1b97dbe8f5eb289f51cc2556690371e3 () Term)
(declare-fun Prims.__proj__Pair__item___2@tok () Term)

; </end encoding Prims.__proj__Pair__item___2>


; <Start encoding Prims.l_and>

(declare-fun Prims.l_and (Term Term) Term)
;;;;;;;;;;;;;;;;p: Prims.logical -> q: Prims.logical -> Prims.logical
(declare-fun Tm_arrow_289ee2cc5874944bf725b9e3db8c0fd6 () Term)
(declare-fun Prims.l_and@tok () Term)

; </end encoding Prims.l_and>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.sum (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.sum@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.sum@x1 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun Prims.sum@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Left_p (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Left_q (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Left_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Left
(declare-fun Prims.Left@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Right_p (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Right_q (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Right_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Right
(declare-fun Prims.Right@tok () Term)
;;;;;;;;;;;;;;;;v: p -> Prims.sum p q
(declare-fun Tm_arrow_4ef073c03ed003774fe6ccb4064aeebd () Term)
;;;;;;;;;;;;;;;;v: q -> Prims.sum p q
(declare-fun Tm_arrow_c537ccd7fef2183d55f1a6960ee793b0 () Term)

; <Start encoding Prims.sum>


; <start constructor Prims.sum>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.sum ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
169)
(exists ((@x0 Term) (@x1 Term))
 (! (= __@x0
(Prims.sum @x0
@x1))
 
;;no pats
:qid is-Prims.sum))))

; </end constructor Prims.sum>


; </end encoding Prims.sum>


; <Start encoding Prims.Left>


; <start constructor Prims.Left>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Left ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
176)
(= __@x0
(Prims.Left (Prims.Left_p __@x0)
(Prims.Left_q __@x0)
(Prims.Left_v __@x0)))))

; </end constructor Prims.Left>


; </end encoding Prims.Left>


; <Start encoding Prims.Right>


; <start constructor Prims.Right>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Right ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
181)
(= __@x0
(Prims.Right (Prims.Right_p __@x0)
(Prims.Right_q __@x0)
(Prims.Right_v __@x0)))))

; </end constructor Prims.Right>


; </end encoding Prims.Right>


; </end encoding >


; <Start encoding Prims.uu___is_Left>

(declare-fun Prims.uu___is_Left (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.sum p q -> Prims.bool
(declare-fun Tm_arrow_3c254c463840bdea2ca63f23cf7c2f0c () Term)
(declare-fun Prims.uu___is_Left@tok () Term)

; </end encoding Prims.uu___is_Left>


; <Start encoding Prims.__proj__Left__item__v>

(declare-fun Tm_refine_6140be01a70b18051829f178aaf0270b (Term Term) Term)
(declare-fun Prims.__proj__Left__item__v (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: Prims.sum p q {Left? _} -> p
(declare-fun Tm_arrow_aaf070885513892396497eef726adce1 () Term)
(declare-fun Prims.__proj__Left__item__v@tok () Term)

; </end encoding Prims.__proj__Left__item__v>


; <Start encoding Prims.uu___is_Right>

(declare-fun Prims.uu___is_Right (Term Term Term) Term)

(declare-fun Prims.uu___is_Right@tok () Term)

; </end encoding Prims.uu___is_Right>


; <Start encoding Prims.__proj__Right__item__v>

(declare-fun Tm_refine_43ea5cf89e866ce271f97bd6ce102588 (Term Term) Term)
(declare-fun Prims.__proj__Right__item__v (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: Prims.sum p q {Right? _} -> q
(declare-fun Tm_arrow_4ae0e7dd85e55aec7e8190dea6e3997f () Term)
(declare-fun Prims.__proj__Right__item__v@tok () Term)

; </end encoding Prims.__proj__Right__item__v>


; <Start encoding Prims.l_or>

(declare-fun Prims.l_or (Term Term) Term)

(declare-fun Prims.l_or@tok () Term)

; </end encoding Prims.l_or>


; <Start encoding Prims.l_imp>

(declare-fun Prims.l_imp (Term Term) Term)

(declare-fun Prims.l_imp@tok () Term)
;;;;;;;;;;;;;;;;_: p -> Prims.GTot q
(declare-fun Tm_ghost_arrow_0283b8a2a36bbec52abac4e3d837674a (Term Term) Term)

; </end encoding Prims.l_imp>


; <Start encoding Prims.l_iff>

(declare-fun Prims.l_iff (Term Term) Term)

(declare-fun Prims.l_iff@tok () Term)

; </end encoding Prims.l_iff>


; <Start encoding Prims.l_not>

(declare-fun Prims.l_not (Term) Term)
;;;;;;;;;;;;;;;;p: Prims.logical -> Prims.logical
(declare-fun Tm_arrow_8178e3b6934aa50ea45bb0ccea2d9711 () Term)
(declare-fun Prims.l_not@tok () Term)

; </end encoding Prims.l_not>


; <Skipped Prims.l_ITE/>


; <Skipped Prims.precedes/>


; <Start encoding Prims.string>

(declare-fun Prims.string () Term)

; </end encoding Prims.string>


; <Start encoding Prims.warn_on_use>

(declare-fun Prims.warn_on_use (Term) Term)
;;;;;;;;;;;;;;;;msg: Prims.string -> Prims.unit
(declare-fun Tm_arrow_2863eb88d7490a9c3cf347c16ca04740 () Term)
(declare-fun Prims.warn_on_use@tok () Term)

; </end encoding Prims.warn_on_use>


; <Start encoding Prims.deprecated>

(declare-fun Prims.deprecated (Term) Term)

(declare-fun Prims.deprecated@tok () Term)

; </end encoding Prims.deprecated>


; <Start encoding Prims.has_type>

(declare-fun Prims.has_type (Term Term Term) Term)
;;;;;;;;;;;;;;;;_: a -> _: Type -> Type
(declare-fun Tm_arrow_b5d8ed0243b8c7c893f2b329de57c62b () Term)
(declare-fun Prims.has_type@tok () Term)

; </end encoding Prims.has_type>


; <Start encoding Prims.l_Forall>

;;;;;;;;;;;;;;;;_: a -> Prims.GTot Type
(declare-fun Tm_arrow_2eaa01e78f73e9bab5d0955fc1a662da (Term) Term)
(declare-fun Prims.l_Forall (Term Term) Term)

;;;;;;;;;;;;;;;;p: (_: a -> Prims.GTot Type) -> Prims.logical
(declare-fun Tm_arrow_977ec6901669a051ac66211b8e72666a () Term)
(declare-fun Prims.l_Forall@tok () Term)

;;;;;;;;;;;;;;;;x: a -> Prims.GTot (p x)
(declare-fun Tm_ghost_arrow_3aa447697277bb40c9738c9125c3e80f (Term Term) Term)

; </end encoding Prims.l_Forall>


; <Skipped />


; <Start encoding Prims.subtype_of>

(declare-fun Prims.subtype_of (Term Term) Term)
;;;;;;;;;;;;;;;;p1: Type -> p2: Type -> Prims.logical
(declare-fun Tm_arrow_28becc0427b69ebf63ea956148504d97 () Term)
(declare-fun Prims.subtype_of@tok () Term)

(declare-fun Tm_abs_2319c8dded71dc14c3f65c301c18a7ca (Term Term) Term)

; </end encoding Prims.subtype_of>


; <Skipped />


; <Start encoding Prims.prop>

(declare-fun Prims.prop () Term)
(declare-fun Tm_refine_73f210ca6e0061ed4a3150f69b8f33bf () Term)

; </end encoding Prims.prop>


; <Start encoding Prims.pure_pre>

(declare-fun Prims.pure_pre () Term)

; </end encoding Prims.pure_pre>


; <Start encoding Prims.pure_post'>

(declare-fun Prims.pure_post_ (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> pre: Type -> Type
(declare-fun Tm_arrow_e4cf09589736facd1137944a1f5a00a6 () Term)
(declare-fun Prims.pure_post_@tok () Term)
(declare-fun Tm_refine_8d65e998a07dd53ec478e27017d9dba5 (Term Term) Term)
;;;;;;;;;;;;;;;;_: a{pre} -> Prims.GTot Type
(declare-fun Tm_arrow_92458cff82f9ffee1f6e26a1c0c579f3 (Term Term) Term)

; </end encoding Prims.pure_post'>


; <Start encoding Prims.pure_post>

(declare-fun Prims.pure_post (Term) Term)

(declare-fun Prims.pure_post@tok () Term)

; </end encoding Prims.pure_post>


; <Start encoding Prims.pure_wp'>

(declare-fun Prims.pure_wp_ (Term) Term)

(declare-fun Prims.pure_wp_@tok () Term)
;;;;;;;;;;;;;;;;_: Prims.pure_post a -> Prims.GTot Prims.pure_pre
(declare-fun Tm_arrow_e5c03abbf8b0946a9aa7ee31bb7999a4 (Term) Term)

; </end encoding Prims.pure_wp'>


; <Start encoding Prims.pure_wp_monotonic0>

(declare-fun Prims.pure_wp_monotonic0 (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp: Prims.pure_wp' a -> Prims.logical
(declare-fun Tm_arrow_85436e2c1c64a4dd0159a737ef5b212e () Term)
(declare-fun Prims.pure_wp_monotonic0@tok () Term)

(declare-fun Tm_abs_ea1703a99385b474600aa7a3bdb045c5 (Term Term Term) Term)
;;;;;;;;;;;;;;;;q: Prims.pure_post a -> Prims.GTot Type
(declare-fun Tm_arrow_b275e247a67e3e77d7c5997d864d1247 (Term) Term)
(declare-fun Tm_abs_fd5475515272d0f1ce55fa6741fae069 (Term Term Term) Term)

(declare-fun Tm_abs_a590ae03fb5fe4509368585c8f8c00cd (Term Term) Term)

; </end encoding Prims.pure_wp_monotonic0>


; <Start encoding Prims.pure_wp_monotonic>

(declare-fun Prims.pure_wp_monotonic (Term Term) Term)

(declare-fun Prims.pure_wp_monotonic@tok () Term)

; </end encoding Prims.pure_wp_monotonic>


; <Start encoding Prims.pure_wp>

(declare-fun Prims.pure_wp (Term) Term)

(declare-fun Prims.pure_wp@tok () Term)
(declare-fun Tm_refine_15e0beb75f7033bad5fae236999feebe (Term) Term)

; </end encoding Prims.pure_wp>


; <Start encoding Prims.guard_free>

(declare-fun Prims.guard_free (Term) Term)

(declare-fun Prims.guard_free@tok () Term)

; </end encoding Prims.guard_free>


; <Skipped Prims.pure_return0/>


; <Skipped Prims.pure_bind_wp0/>


; <Skipped Prims.pure_if_then_else0/>


; <Skipped Prims.pure_ite_wp0/>


; <Skipped Prims.pure_stronger/>


; <Skipped Prims.pure_close_wp0/>


; <Skipped Prims.pure_trivial/>


; <Skipped Prims.PURE/>


; <Skipped Prims.Pure/>


; <Skipped Prims.Admit/>


; <Skipped Prims.pure_null_wp0/>


; <Skipped Prims.Tot/>


; <Start encoding Prims.pure_assert_wp0>

(declare-fun Prims.pure_assert_wp0 (Term) Term)
;;;;;;;;;;;;;;;;p: Type -> Prims.pure_wp Prims.unit
(declare-fun Tm_arrow_14bcf5e7fc38d91827ecd9d25d3b3a67 () Term)
(declare-fun Prims.pure_assert_wp0@tok () Term)

; </end encoding Prims.pure_assert_wp0>


; <Start encoding Prims.pure_assume_wp0>

(declare-fun Prims.pure_assume_wp0 (Term) Term)

(declare-fun Prims.pure_assume_wp0@tok () Term)

; </end encoding Prims.pure_assume_wp0>


; <Skipped Prims.GHOST/>


; <Skipped Prims.purewp_id/>


; <Skipped />


; <Skipped Prims.Ghost/>


; <Skipped Prims.GTot/>


; <Start encoding Prims.op_Equals_Equals_Equals>

(declare-fun Prims.op_Equals_Equals_Equals (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> y: b -> Prims.logical
(declare-fun Tm_arrow_7fcb145b23c2ac843afd9b126c4f71a9 () Term)
(declare-fun Prims.op_Equals_Equals_Equals@tok () Term)

; </end encoding Prims.op_Equals_Equals_Equals>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.dtuple2 (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.dtuple2@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.dtuple2@x1 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun Prims.dtuple2@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Mkdtuple2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Mkdtuple2_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Mkdtuple2_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Mkdtuple2__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Mkdtuple2__2 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mkdtuple2
(declare-fun Prims.Mkdtuple2@tok () Term)



;;;;;;;;;;;;;;;;_1: a -> _2: b _1 -> Prims.dtuple2 a b
(declare-fun Tm_arrow_22a50f5c5c9bb74bac4384fb8999be8b () Term)

; <Start encoding Prims.dtuple2>


; <start constructor Prims.dtuple2>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.dtuple2 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
266)
(exists ((@x0 Term) (@x1 Term))
 (! (= __@x0
(Prims.dtuple2 @x0
@x1))
 
;;no pats
:qid is-Prims.dtuple2))))

; </end constructor Prims.dtuple2>


; </end encoding Prims.dtuple2>


; <Start encoding Prims.Mkdtuple2>


; <start constructor Prims.Mkdtuple2>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Mkdtuple2 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
275)
(= __@x0
(Prims.Mkdtuple2 (Prims.Mkdtuple2_a __@x0)
(Prims.Mkdtuple2_b __@x0)
(Prims.Mkdtuple2__1 __@x0)
(Prims.Mkdtuple2__2 __@x0)))))

; </end constructor Prims.Mkdtuple2>


; </end encoding Prims.Mkdtuple2>


; </end encoding >


; <Start encoding Prims.dtuple2__uu___haseq>



; </end encoding Prims.dtuple2__uu___haseq>


; <Start encoding Prims.uu___is_Mkdtuple2>


(declare-fun Prims.uu___is_Mkdtuple2 (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: Prims.dtuple2 a b -> Prims.bool
(declare-fun Tm_arrow_e6f9f7cb1936ec43b52469e706dcadcc () Term)
(declare-fun Prims.uu___is_Mkdtuple2@tok () Term)

; </end encoding Prims.uu___is_Mkdtuple2>


; <Skipped Prims.uu___is_Mkdtuple2/>


; <Start encoding Prims.__proj__Mkdtuple2__item___1>


(declare-fun Prims.__proj__Mkdtuple2__item___1 (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: Prims.dtuple2 a b -> a
(declare-fun Tm_arrow_26c013ffba39d4f7eeb4bcc80d2d4e22 () Term)
(declare-fun Prims.__proj__Mkdtuple2__item___1@tok () Term)

; </end encoding Prims.__proj__Mkdtuple2__item___1>


; <Skipped Prims.__proj__Mkdtuple2__item___1/>


; <Start encoding Prims.__proj__Mkdtuple2__item___2>


(declare-fun Prims.__proj__Mkdtuple2__item___2 (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: Prims.dtuple2 a b -> b (Mkdtuple2?._1 projectee)
(declare-fun Tm_arrow_870cc7701a0d9a8a2d6fb92427a97d66 () Term)
(declare-fun Prims.__proj__Mkdtuple2__item___2@tok () Term)

; </end encoding Prims.__proj__Mkdtuple2__item___2>


; <Skipped Prims.__proj__Mkdtuple2__item___2/>


; <Start encoding Prims.l_Exists>


(declare-fun Prims.l_Exists (Term Term) Term)


(declare-fun Prims.l_Exists@tok () Term)


(declare-fun Tm_abs_6ba36691ee58dee85cd144324b083848 (Term Term) Term)

; </end encoding Prims.l_Exists>


; <Start encoding Prims.int>

(declare-fun Prims.int () Term)
;;;;;;;;;;;;;;;;int typing
;;; Fact-ids: Name Prims.int; Namespace Prims
(assert (! (forall ((@u0 Int))
 (! (HasType (BoxInt @u0)
Prims.int)
 

:pattern ((BoxInt @u0))
:qid int_typing))
:named int_typing))
;;;;;;;;;;;;;;;;int inversion
;;; Fact-ids: Name Prims.int; Namespace Prims
(assert (! (forall ((@u0 Fuel) (@x1 Term))
 (! (implies (HasTypeFuel @u0
@x1
Prims.int)
(is-BoxInt @x1))
 

:pattern ((HasTypeFuel @u0
@x1
Prims.int))
:qid int_inversion))
:named int_inversion))

; </end encoding Prims.int>


; <Start encoding Prims.op_AmpAmp>

(declare-fun Prims.op_AmpAmp (Term Term) Term)
(declare-fun Prims.op_AmpAmp@tok () Term)

; </end encoding Prims.op_AmpAmp>


; <Start encoding Prims.op_BarBar>

(declare-fun Prims.op_BarBar (Term Term) Term)
(declare-fun Prims.op_BarBar@tok () Term)

; </end encoding Prims.op_BarBar>


; <Start encoding Prims.op_Negation>

(declare-fun Prims.op_Negation (Term) Term)
(declare-fun Prims.op_Negation@tok () Term)

; </end encoding Prims.op_Negation>


; <Start encoding Prims.op_Multiply>

(declare-fun Prims.op_Multiply (Term Term) Term)
(declare-fun Prims.op_Multiply@tok () Term)
;;; Fact-ids: Name Prims.op_Multiply; Namespace Prims
(assert (! 
;; def=prims.fst(519,4-519,15); use=prims.fst(519,4-519,15)
(forall ((@x0 Term) (@x1 Term))
 (! (= (Prims.op_Multiply @x0
@x1)
(BoxInt (* (BoxInt_proj_0 @x0)
(BoxInt_proj_0 @x1))))
 

:pattern ((Prims.op_Multiply @x0
@x1))
:qid primitive_Prims.op_Multiply))

:named primitive_Prims.op_Multiply))

; </end encoding Prims.op_Multiply>


; <Start encoding Prims.op_Subtraction>

(declare-fun Prims.op_Subtraction (Term Term) Term)
(declare-fun Prims.op_Subtraction@tok () Term)
;;; Fact-ids: Name Prims.op_Subtraction; Namespace Prims
(assert (! 
;; def=prims.fst(525,4-525,18); use=prims.fst(525,4-525,18)
(forall ((@x0 Term) (@x1 Term))
 (! (= (Prims.op_Subtraction @x0
@x1)
(BoxInt (- (BoxInt_proj_0 @x0)
(BoxInt_proj_0 @x1))))
 

:pattern ((Prims.op_Subtraction @x0
@x1))
:qid primitive_Prims.op_Subtraction))

:named primitive_Prims.op_Subtraction))

; </end encoding Prims.op_Subtraction>


; <Start encoding Prims.op_Addition>

(declare-fun Prims.op_Addition (Term Term) Term)
(declare-fun Prims.op_Addition@tok () Term)
;;; Fact-ids: Name Prims.op_Addition; Namespace Prims
(assert (! 
;; def=prims.fst(531,4-531,15); use=prims.fst(531,4-531,15)
(forall ((@x0 Term) (@x1 Term))
 (! (= (Prims.op_Addition @x0
@x1)
(BoxInt (+ (BoxInt_proj_0 @x0)
(BoxInt_proj_0 @x1))))
 

:pattern ((Prims.op_Addition @x0
@x1))
:qid primitive_Prims.op_Addition))

:named primitive_Prims.op_Addition))

; </end encoding Prims.op_Addition>


; <Start encoding Prims.op_Minus>

(declare-fun Prims.op_Minus (Term) Term)
(declare-fun Prims.op_Minus@tok () Term)

; </end encoding Prims.op_Minus>


; <Start encoding Prims.op_LessThanOrEqual>

(declare-fun Prims.op_LessThanOrEqual (Term Term) Term)
(declare-fun Prims.op_LessThanOrEqual@tok () Term)
;;; Fact-ids: Name Prims.op_LessThanOrEqual; Namespace Prims
(assert (! 
;; def=prims.fst(543,4-543,22); use=prims.fst(543,4-543,22)
(forall ((@x0 Term) (@x1 Term))
 (! (= (Prims.op_LessThanOrEqual @x0
@x1)
(BoxBool (<= (BoxInt_proj_0 @x0)
(BoxInt_proj_0 @x1))))
 

:pattern ((Prims.op_LessThanOrEqual @x0
@x1))
:qid primitive_Prims.op_LessThanOrEqual))

:named primitive_Prims.op_LessThanOrEqual))

; </end encoding Prims.op_LessThanOrEqual>


; <Start encoding Prims.op_GreaterThan>

(declare-fun Prims.op_GreaterThan (Term Term) Term)
(declare-fun Prims.op_GreaterThan@tok () Term)

; </end encoding Prims.op_GreaterThan>


; <Start encoding Prims.op_GreaterThanOrEqual>

(declare-fun Prims.op_GreaterThanOrEqual (Term Term) Term)
(declare-fun Prims.op_GreaterThanOrEqual@tok () Term)

; </end encoding Prims.op_GreaterThanOrEqual>


; <Start encoding Prims.op_LessThan>

(declare-fun Prims.op_LessThan (Term Term) Term)
(declare-fun Prims.op_LessThan@tok () Term)

; </end encoding Prims.op_LessThan>


; <Start encoding Prims.op_Equality>

(declare-fun Prims.op_Equality (Term Term Term) Term)
(declare-fun Prims.op_Equality@tok () Term)

; </end encoding Prims.op_Equality>


; <Start encoding Prims.op_disEquality>

(declare-fun Prims.op_disEquality (Term Term Term) Term)
(declare-fun Prims.op_disEquality@tok () Term)

; </end encoding Prims.op_disEquality>


; <Start encoding Prims.exn>

(declare-fun Prims.exn () Term)

; </end encoding Prims.exn>


; <Start encoding Prims.array>

(declare-fun Prims.array (Term) Term)

(declare-fun Prims.array@tok () Term)

; </end encoding Prims.array>


; <Start encoding Prims.strcat>

(declare-fun Prims.strcat (Term Term) Term)
;;;;;;;;;;;;;;;;_: Prims.string -> _: Prims.string -> Prims.string
(declare-fun Tm_arrow_b66cecec1d56111347abe61e89557dd1 () Term)
(declare-fun Prims.strcat@tok () Term)

; </end encoding Prims.strcat>


; <Skipped Prims.op_Hat/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.list (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.list@x0 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun Prims.list@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Nil (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Nil_a (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Nil
(declare-fun Prims.Nil@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun Prims.Cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Cons_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Cons_hd (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun Prims.Cons_tl (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Cons
(declare-fun Prims.Cons@tok () Term)
;;;;;;;;;;;;;;;;Prims.list a
(declare-fun Tm_arrow_3864bd5fbb999b4fe4487408df9b3401 () Term)
;;;;;;;;;;;;;;;;hd: a -> tl: Prims.list a -> Prims.list a
(declare-fun Tm_arrow_02c072760cbad0f5a4706f6cffab6c94 () Term)

; <Start encoding Prims.list>


; <start constructor Prims.list>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.list ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
326)
(exists ((@x0 Term))
 (! (= __@x0
(Prims.list @x0))
 
;;no pats
:qid is-Prims.list))))

; </end constructor Prims.list>


; </end encoding Prims.list>


; <Start encoding Prims.Nil>


; <start constructor Prims.Nil>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Nil ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
333)
(= __@x0
(Prims.Nil (Prims.Nil_a __@x0)))))

; </end constructor Prims.Nil>


; </end encoding Prims.Nil>


; <Start encoding Prims.Cons>


; <start constructor Prims.Cons>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-Prims.Cons ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
338)
(= __@x0
(Prims.Cons (Prims.Cons_a __@x0)
(Prims.Cons_hd __@x0)
(Prims.Cons_tl __@x0)))))

; </end constructor Prims.Cons>


; </end encoding Prims.Cons>


; </end encoding >


; <Start encoding Prims.list__uu___haseq>


; </end encoding Prims.list__uu___haseq>


; <Start encoding Prims.uu___is_Nil>

(declare-fun Prims.uu___is_Nil (Term Term) Term)
;;;;;;;;;;;;;;;;projectee: Prims.list a -> Prims.bool
(declare-fun Tm_arrow_606904b0fa72729a20285beb231f9f2e () Term)
(declare-fun Prims.uu___is_Nil@tok () Term)

; </end encoding Prims.uu___is_Nil>


; <Skipped Prims.uu___is_Nil/>


; <Start encoding Prims.uu___is_Cons>

(declare-fun Prims.uu___is_Cons (Term Term) Term)

(declare-fun Prims.uu___is_Cons@tok () Term)

; </end encoding Prims.uu___is_Cons>


; <Skipped Prims.uu___is_Cons/>


; <Start encoding Prims.__proj__Cons__item__hd>

(declare-fun Tm_refine_7aac12c24449a22c34d98a0ea8ed4a32 (Term) Term)
(declare-fun Prims.__proj__Cons__item__hd (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: Prims.list a {Cons? _} -> a
(declare-fun Tm_arrow_27c3547831737e5a63950f3d18bf3d22 () Term)
(declare-fun Prims.__proj__Cons__item__hd@tok () Term)

; </end encoding Prims.__proj__Cons__item__hd>


; <Skipped Prims.__proj__Cons__item__hd/>


; <Start encoding Prims.__proj__Cons__item__tl>


(declare-fun Prims.__proj__Cons__item__tl (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: Prims.list a {Cons? _} -> Prims.list a
(declare-fun Tm_arrow_4e740085106d54d8b48ffe3c6c20ef21 () Term)
(declare-fun Prims.__proj__Cons__item__tl@tok () Term)

; </end encoding Prims.__proj__Cons__item__tl>


; <Skipped Prims.__proj__Cons__item__tl/>


; <Skipped Prims.M/>


; <Start encoding Prims.returnM>

(declare-fun Prims.returnM (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> x: a -> Prims.M a
(declare-fun Tm_arrow_99724436653747ac6f5a6a00c64ff8bc () Term)
(declare-fun Prims.returnM@tok () Term)

; </end encoding Prims.returnM>


; <Skipped Prims.as_requires/>


; <Skipped Prims.as_ensures/>


; <Start encoding Prims._assume>

(declare-fun Prims._assume (Term) Term)
(declare-fun Non_total_Tm_arrow_724d0dab46b8b51a1bb19d329f7fc4b2 () Term)
(declare-fun Prims._assume@tok () Term)

; </end encoding Prims._assume>


; <Start encoding Prims.admit>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun Prims.admit (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun Prims.admit@tok () Term)

; </end encoding Prims.admit>


; <Start encoding Prims.magic>

(declare-fun Prims.magic (Term Term) Term)
;;;;;;;;;;;;;;;;_: Prims.unit -> a
(declare-fun Tm_arrow_f5df98ce82fbcebbbdb844c958bee4fb () Term)
(declare-fun Prims.magic@tok () Term)

; </end encoding Prims.magic>


; <Start encoding Prims.unsafe_coerce>

(declare-fun Prims.unsafe_coerce (Term Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> b
(declare-fun Tm_arrow_443ab41008720460b7a09e280558a60f () Term)
(declare-fun Prims.unsafe_coerce@tok () Term)

; </end encoding Prims.unsafe_coerce>


; <Start encoding Prims.admitP>

(declare-fun Prims.admitP (Term) Term)

(declare-fun Prims.admitP@tok () Term)

; </end encoding Prims.admitP>


; <Skipped Prims._assert/>


; <Start encoding Prims._assert>

(declare-fun Prims._assert (Term) Term)
(declare-fun Non_total_Tm_arrow_bb2d1b4bdb07c87bf5990ad3e5fd8642 () Term)
(declare-fun Prims._assert@tok () Term)

; </end encoding Prims._assert>


; <Skipped Prims.cut/>


; <Start encoding Prims.cut>

(declare-fun Prims.cut (Term) Term)

(declare-fun Prims.cut@tok () Term)

; </end encoding Prims.cut>


; <Start encoding Prims.nat>

(declare-fun Prims.nat () Term)
(declare-fun Tm_refine_542f9d4f129664613f2483a6c88bc7c2 () Term)
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name Prims.nat; Namespace Prims
(assert (! 
;; def=prims.fst(659,11-659,25); use=prims.fst(659,11-659,25)
(forall ((@u0 Fuel) (@x1 Term))
 (! (iff (HasTypeFuel @u0
@x1
Tm_refine_542f9d4f129664613f2483a6c88bc7c2)
(and (HasTypeFuel @u0
@x1
Prims.int)

;; def=prims.fst(659,18-659,24); use=prims.fst(659,18-659,24)
(>= (BoxInt_proj_0 @x1)
(BoxInt_proj_0 (BoxInt 0)))
))
 

:pattern ((HasTypeFuel @u0
@x1
Tm_refine_542f9d4f129664613f2483a6c88bc7c2))
:qid refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2))

:named refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2))
;;;;;;;;;;;;;;;;Equation for Prims.nat
;;; Fact-ids: Name Prims.nat; Namespace Prims
(assert (! (= Prims.nat
Tm_refine_542f9d4f129664613f2483a6c88bc7c2)
:named equation_Prims.nat))

; </end encoding Prims.nat>


; <Start encoding Prims.pos>

(declare-fun Prims.pos () Term)
(declare-fun Tm_refine_774ba3f728d91ead8ef40be66c9802e5 () Term)

; </end encoding Prims.pos>


; <Start encoding Prims.nonzero>

(declare-fun Prims.nonzero () Term)
(declare-fun Tm_refine_0766302b68bb44ab7aff8c4d8be0b46f () Term)

; </end encoding Prims.nonzero>


; <Start encoding Prims.op_Modulus>

(declare-fun Prims.op_Modulus (Term Term) Term)
(declare-fun Prims.op_Modulus@tok () Term)

; </end encoding Prims.op_Modulus>


; <Start encoding Prims.op_Division>

(declare-fun Prims.op_Division (Term Term) Term)
(declare-fun Prims.op_Division@tok () Term)

; </end encoding Prims.op_Division>


; <Start encoding Prims.pow2>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun Prims.pow2.fuel_instrumented (Fuel Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun Prims.pow2.fuel_instrumented_token () Term)
(declare-fun Prims.pow2 (Term) Term)
(declare-fun Prims.pow2@tok () Term)
;;;;;;;;;;;;;;;;x: Prims.nat -> Prims.pos
(declare-fun Tm_arrow_c331a0e032e021e1eaa359b3983de4f2 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name Prims.pow2; Namespace Prims
(assert (! 
;; def=prims.fst(687,8-687,12); use=prims.fst(687,8-687,12)
(forall ((@u0 Fuel) (@x1 Term))
 (! (= (Prims.pow2.fuel_instrumented (SFuel @u0)
@x1)
(Prims.pow2.fuel_instrumented ZFuel
@x1))
 

:pattern ((Prims.pow2.fuel_instrumented (SFuel @u0)
@x1))
:qid @fuel_irrelevance_Prims.pow2.fuel_instrumented))

:named @fuel_irrelevance_Prims.pow2.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name Prims.pow2; Namespace Prims
(assert (! 
;; def=prims.fst(687,8-687,12); use=prims.fst(687,8-687,12)
(forall ((@x0 Term))
 (! (= (Prims.pow2 @x0)
(Prims.pow2.fuel_instrumented MaxFuel
@x0))
 

:pattern ((Prims.pow2 @x0))
:qid @fuel_correspondence_Prims.pow2.fuel_instrumented))

:named @fuel_correspondence_Prims.pow2.fuel_instrumented))

; </end encoding Prims.pow2>


; <Start encoding Prims.min>

(declare-fun Prims.min (Term Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> y: Prims.int -> Prims.int
(declare-fun Tm_arrow_47fc285d7b44e13bcb7e420cbfc55623 () Term)
(declare-fun Prims.min@tok () Term)

; </end encoding Prims.min>


; <Start encoding Prims.abs>

(declare-fun Prims.abs (Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> Prims.int
(declare-fun Tm_arrow_35447810753695c4fe25c93af1251992 () Term)
(declare-fun Prims.abs@tok () Term)

; </end encoding Prims.abs>


; <Start encoding Prims.string_of_bool>

(declare-fun Prims.string_of_bool (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.bool -> Prims.string
(declare-fun Tm_arrow_e86b54405c2a58719f5e8112efd48c09 () Term)
(declare-fun Prims.string_of_bool@tok () Term)

; </end encoding Prims.string_of_bool>


; <Start encoding Prims.string_of_int>

(declare-fun Prims.string_of_int (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.int -> Prims.string
(declare-fun Tm_arrow_2bc066ec63734c94a3c008e1e72cae2b () Term)
(declare-fun Prims.string_of_int@tok () Term)

; </end encoding Prims.string_of_int>


; <Start encoding Prims.__cache_version_number__>

(declare-fun Prims.__cache_version_number__ () Term)

; </end encoding Prims.__cache_version_number__>


; End Externals for module Prims


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module Prims (654 decls; total size 38438)

;;; Start module FStar.Pervasives.Native

; Externals for module FStar.Pervasives.Native


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.option (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.option@x0 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.option@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.None (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.None_a (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: None
(declare-fun FStar.Pervasives.Native.None@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Some (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Some_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Some_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Some
(declare-fun FStar.Pervasives.Native.Some@tok () Term)
;;;;;;;;;;;;;;;;FStar.Pervasives.Native.option a
(declare-fun Tm_arrow_48b914114ec9f2f1caadf0f6848a9741 () Term)
;;;;;;;;;;;;;;;;v: a -> FStar.Pervasives.Native.option a
(declare-fun Tm_arrow_b93a364b5144c2a5f3e9d1ea7b881752 () Term)

; <Start encoding FStar.Pervasives.Native.option>


; <start constructor FStar.Pervasives.Native.option>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.option ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
101)
(exists ((@x0 Term))
 (! (= __@x0
(FStar.Pervasives.Native.option @x0))
 
;;no pats
:qid is-FStar.Pervasives.Native.option))))

; </end constructor FStar.Pervasives.Native.option>


; </end encoding FStar.Pervasives.Native.option>


; <Start encoding FStar.Pervasives.Native.None>


; <start constructor FStar.Pervasives.Native.None>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.None ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
108)
(= __@x0
(FStar.Pervasives.Native.None (FStar.Pervasives.Native.None_a __@x0)))))

; </end constructor FStar.Pervasives.Native.None>


; </end encoding FStar.Pervasives.Native.None>


; <Start encoding FStar.Pervasives.Native.Some>


; <start constructor FStar.Pervasives.Native.Some>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Some ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
113)
(= __@x0
(FStar.Pervasives.Native.Some (FStar.Pervasives.Native.Some_a __@x0)
(FStar.Pervasives.Native.Some_v __@x0)))))

; </end constructor FStar.Pervasives.Native.Some>


; </end encoding FStar.Pervasives.Native.Some>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.option__uu___haseq>


; </end encoding FStar.Pervasives.Native.option__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_None>

(declare-fun FStar.Pervasives.Native.uu___is_None (Term Term) Term)
;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.Native.option a -> Prims.bool
(declare-fun Tm_arrow_f1a97bcd6ba9b40d22609b756f183afa () Term)
(declare-fun FStar.Pervasives.Native.uu___is_None@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_None>


; <Skipped FStar.Pervasives.Native.uu___is_None/>


; <Start encoding FStar.Pervasives.Native.uu___is_Some>

(declare-fun FStar.Pervasives.Native.uu___is_Some (Term Term) Term)

(declare-fun FStar.Pervasives.Native.uu___is_Some@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Some>


; <Skipped FStar.Pervasives.Native.uu___is_Some/>


; <Start encoding FStar.Pervasives.Native.__proj__Some__item__v>

(declare-fun Tm_refine_4d5241eb6fe198666a8101195bbd4a2a (Term) Term)
(declare-fun FStar.Pervasives.Native.__proj__Some__item__v (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.Native.option a {Some? _} -> a
(declare-fun Tm_arrow_1b1398c011ff53e4194fc2ec00c7b411 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Some__item__v@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Some__item__v>


; <Skipped FStar.Pervasives.Native.__proj__Some__item__v/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple2 (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple2@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple2@x1 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple2@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple2__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple2__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple2__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple2__2 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple2
(declare-fun FStar.Pervasives.Native.Mktuple2@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> 'a * 'b
(declare-fun Tm_arrow_4054cc0a51327db54c2ed9ba3376a093 () Term)

; <Start encoding FStar.Pervasives.Native.tuple2>


; <start constructor FStar.Pervasives.Native.tuple2>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple2 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
125)
(exists ((@x0 Term) (@x1 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple2 @x0
@x1))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple2))))

; </end constructor FStar.Pervasives.Native.tuple2>


; </end encoding FStar.Pervasives.Native.tuple2>


; <Start encoding FStar.Pervasives.Native.Mktuple2>


; <start constructor FStar.Pervasives.Native.Mktuple2>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple2 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
132)
(= __@x0
(FStar.Pervasives.Native.Mktuple2 (FStar.Pervasives.Native.Mktuple2__a __@x0)
(FStar.Pervasives.Native.Mktuple2__b __@x0)
(FStar.Pervasives.Native.Mktuple2__1 __@x0)
(FStar.Pervasives.Native.Mktuple2__2 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple2>


; </end encoding FStar.Pervasives.Native.Mktuple2>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple2__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple2__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple2>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ('a * 'b) -> Prims.bool
(declare-fun Tm_arrow_eff71eeee4474e017e02350f86f54756 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple2@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple2>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple2__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple2__item___1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ('a * 'b) -> 'a
(declare-fun Tm_arrow_b8cce376a4a678a51298a0f3945f25ce () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple2__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple2__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple2__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple2__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple2__item___2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ('a * 'b) -> 'b
(declare-fun Tm_arrow_d952d001575ecb20c572af535c88dd2d () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple2__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple2__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple2__item___2/>


; <Start encoding FStar.Pervasives.Native.fst>

(declare-fun FStar.Pervasives.Native.fst (Term Term Term) Term)

(declare-fun FStar.Pervasives.Native.fst@tok () Term)

; </end encoding FStar.Pervasives.Native.fst>


; <Start encoding FStar.Pervasives.Native.snd>

(declare-fun FStar.Pervasives.Native.snd (Term Term Term) Term)

(declare-fun FStar.Pervasives.Native.snd@tok () Term)

; </end encoding FStar.Pervasives.Native.snd>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple3 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple3@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple3@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple3@x2 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple3@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple3 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple3__3 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple3
(declare-fun FStar.Pervasives.Native.Mktuple3@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> ('a * 'b) * 'c
(declare-fun Tm_arrow_1bedda193f13e939931cf5d46ad84216 () Term)

; <Start encoding FStar.Pervasives.Native.tuple3>


; <start constructor FStar.Pervasives.Native.tuple3>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple3 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
146)
(exists ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple3 @x0
@x1
@x2))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple3))))

; </end constructor FStar.Pervasives.Native.tuple3>


; </end encoding FStar.Pervasives.Native.tuple3>


; <Start encoding FStar.Pervasives.Native.Mktuple3>


; <start constructor FStar.Pervasives.Native.Mktuple3>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple3 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
153)
(= __@x0
(FStar.Pervasives.Native.Mktuple3 (FStar.Pervasives.Native.Mktuple3__a __@x0)
(FStar.Pervasives.Native.Mktuple3__b __@x0)
(FStar.Pervasives.Native.Mktuple3__c __@x0)
(FStar.Pervasives.Native.Mktuple3__1 __@x0)
(FStar.Pervasives.Native.Mktuple3__2 __@x0)
(FStar.Pervasives.Native.Mktuple3__3 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple3>


; </end encoding FStar.Pervasives.Native.Mktuple3>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple3__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple3__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple3>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple3 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (('a * 'b) * 'c) -> Prims.bool
(declare-fun Tm_arrow_f03c6dc5b30146aaca49ed4bf6f332a7 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple3@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple3>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple3__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (('a * 'b) * 'c) -> 'a
(declare-fun Tm_arrow_592c45439d32a71e1933eacb9776c9ed () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple3__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple3__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple3__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (('a * 'b) * 'c) -> 'b
(declare-fun Tm_arrow_9c9b0c5ac9b0fbfc367f406af296ecab () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple3__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple3__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple3__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___3 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (('a * 'b) * 'c) -> 'c
(declare-fun Tm_arrow_08246a62c9aeca08c44c602ad80e95a4 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple3__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple3__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple3__item___3/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple4 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple4@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple4@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple4@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple4@x3 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple4@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple4 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple4__4 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple4
(declare-fun FStar.Pervasives.Native.Mktuple4@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> (('a * 'b) * 'c) * 'd
(declare-fun Tm_arrow_cbe72a10167439fe1ecfaf4fec8fd23f () Term)

; <Start encoding FStar.Pervasives.Native.tuple4>


; <start constructor FStar.Pervasives.Native.tuple4>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple4 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
165)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple4 @x0
@x1
@x2
@x3))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple4))))

; </end constructor FStar.Pervasives.Native.tuple4>


; </end encoding FStar.Pervasives.Native.tuple4>


; <Start encoding FStar.Pervasives.Native.Mktuple4>


; <start constructor FStar.Pervasives.Native.Mktuple4>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple4 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
172)
(= __@x0
(FStar.Pervasives.Native.Mktuple4 (FStar.Pervasives.Native.Mktuple4__a __@x0)
(FStar.Pervasives.Native.Mktuple4__b __@x0)
(FStar.Pervasives.Native.Mktuple4__c __@x0)
(FStar.Pervasives.Native.Mktuple4__d __@x0)
(FStar.Pervasives.Native.Mktuple4__1 __@x0)
(FStar.Pervasives.Native.Mktuple4__2 __@x0)
(FStar.Pervasives.Native.Mktuple4__3 __@x0)
(FStar.Pervasives.Native.Mktuple4__4 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple4>


; </end encoding FStar.Pervasives.Native.Mktuple4>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple4__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple4__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple4>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple4 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((('a * 'b) * 'c) * 'd) -> Prims.bool
(declare-fun Tm_arrow_4319694c225efa92ce9fad6e9d81f761 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple4@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple4>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple4__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___1 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((('a * 'b) * 'c) * 'd) -> 'a
(declare-fun Tm_arrow_382d1e9129053162252ec57e86d46f82 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple4__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple4__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple4__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___2 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((('a * 'b) * 'c) * 'd) -> 'b
(declare-fun Tm_arrow_fffd25e5325d259efa0675ef649c6864 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple4__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple4__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple4__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___3 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((('a * 'b) * 'c) * 'd) -> 'c
(declare-fun Tm_arrow_57b4005e0833f7b396e349ed7cdd1bb2 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple4__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple4__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple4__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___4 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((('a * 'b) * 'c) * 'd) -> 'd
(declare-fun Tm_arrow_9e6c1a63d63f8735645b9898955a2dca () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple4__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple4__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple4__item___4/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple5 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple5@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple5@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple5@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple5@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple5@x4 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple5@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple5 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple5__5 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple5
(declare-fun FStar.Pervasives.Native.Mktuple5@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> ((('a * 'b) * 'c) * 'd) * 'e
(declare-fun Tm_arrow_dd8a078a1b97a81b5089dc3637cc2887 () Term)

; <Start encoding FStar.Pervasives.Native.tuple5>


; <start constructor FStar.Pervasives.Native.tuple5>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple5 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
186)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple5 @x0
@x1
@x2
@x3
@x4))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple5))))

; </end constructor FStar.Pervasives.Native.tuple5>


; </end encoding FStar.Pervasives.Native.tuple5>


; <Start encoding FStar.Pervasives.Native.Mktuple5>


; <start constructor FStar.Pervasives.Native.Mktuple5>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple5 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
193)
(= __@x0
(FStar.Pervasives.Native.Mktuple5 (FStar.Pervasives.Native.Mktuple5__a __@x0)
(FStar.Pervasives.Native.Mktuple5__b __@x0)
(FStar.Pervasives.Native.Mktuple5__c __@x0)
(FStar.Pervasives.Native.Mktuple5__d __@x0)
(FStar.Pervasives.Native.Mktuple5__e __@x0)
(FStar.Pervasives.Native.Mktuple5__1 __@x0)
(FStar.Pervasives.Native.Mktuple5__2 __@x0)
(FStar.Pervasives.Native.Mktuple5__3 __@x0)
(FStar.Pervasives.Native.Mktuple5__4 __@x0)
(FStar.Pervasives.Native.Mktuple5__5 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple5>


; </end encoding FStar.Pervasives.Native.Mktuple5>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple5__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple5__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple5>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple5 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> Prims.bool
(declare-fun Tm_arrow_cfa2e2c8b8b41312889ff659c4faa5f9 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple5@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple5>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple5__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___1 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> 'a
(declare-fun Tm_arrow_7519f72fe101267af170e00c6ce694af () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple5__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple5__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple5__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___2 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> 'b
(declare-fun Tm_arrow_3e46329f224aa70981a337f98afbaa87 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple5__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple5__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple5__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___3 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> 'c
(declare-fun Tm_arrow_55e6dc1b736536de45fedf844003f847 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple5__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple5__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple5__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___4 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> 'd
(declare-fun Tm_arrow_3a4e86c6aee1a39b4811bdbc12405398 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple5__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple5__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple5__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___5 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((('a * 'b) * 'c) * 'd) * 'e) -> 'e
(declare-fun Tm_arrow_1a78355922fdaba3f3848932dfc0a089 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple5__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple5__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple5__item___5/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple6 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple6@x5 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple6@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple6 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple6__6 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple6
(declare-fun FStar.Pervasives.Native.Mktuple6@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> _6: 'f -> (((('a * 'b) * 'c) * 'd) * 'e) * 'f
(declare-fun Tm_arrow_f277ffaa7e891207f9c6bff5b88ffd67 () Term)

; <Start encoding FStar.Pervasives.Native.tuple6>


; <start constructor FStar.Pervasives.Native.tuple6>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple6 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
209)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple6 @x0
@x1
@x2
@x3
@x4
@x5))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple6))))

; </end constructor FStar.Pervasives.Native.tuple6>


; </end encoding FStar.Pervasives.Native.tuple6>


; <Start encoding FStar.Pervasives.Native.Mktuple6>


; <start constructor FStar.Pervasives.Native.Mktuple6>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple6 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
216)
(= __@x0
(FStar.Pervasives.Native.Mktuple6 (FStar.Pervasives.Native.Mktuple6__a __@x0)
(FStar.Pervasives.Native.Mktuple6__b __@x0)
(FStar.Pervasives.Native.Mktuple6__c __@x0)
(FStar.Pervasives.Native.Mktuple6__d __@x0)
(FStar.Pervasives.Native.Mktuple6__e __@x0)
(FStar.Pervasives.Native.Mktuple6__f __@x0)
(FStar.Pervasives.Native.Mktuple6__1 __@x0)
(FStar.Pervasives.Native.Mktuple6__2 __@x0)
(FStar.Pervasives.Native.Mktuple6__3 __@x0)
(FStar.Pervasives.Native.Mktuple6__4 __@x0)
(FStar.Pervasives.Native.Mktuple6__5 __@x0)
(FStar.Pervasives.Native.Mktuple6__6 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple6>


; </end encoding FStar.Pervasives.Native.Mktuple6>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple6__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple6__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple6>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple6 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> Prims.bool
(declare-fun Tm_arrow_286587a1b9d299ba75a076f54a6dad5f () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple6@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple6>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___1 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'a
(declare-fun Tm_arrow_5b1e145eeceab869b8e427e6927dbd63 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___2 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'b
(declare-fun Tm_arrow_3207475e225d584881d3e0a297482887 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___3 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'c
(declare-fun Tm_arrow_43e491b3b537a523a4f10de18b1915f5 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___4 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'd
(declare-fun Tm_arrow_f5747d5b721642d7ecb757b043f20880 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___5 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'e
(declare-fun Tm_arrow_d6501381a0206e157ecc43950bb31fea () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple6__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___6 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) -> 'f
(declare-fun Tm_arrow_9c342f41120d0c7aea115b09b58cefb2 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple6__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple6__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple6__item___6/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple7 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple7@x6 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple7@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple7 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple7__7 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple7
(declare-fun FStar.Pervasives.Native.Mktuple7@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> _6: 'f -> _7: 'g   -> ((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g
(declare-fun Tm_arrow_37ee9ec407a0f7bb69bf1b308f74a230 () Term)

; <Start encoding FStar.Pervasives.Native.tuple7>


; <start constructor FStar.Pervasives.Native.tuple7>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple7 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
234)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple7 @x0
@x1
@x2
@x3
@x4
@x5
@x6))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple7))))

; </end constructor FStar.Pervasives.Native.tuple7>


; </end encoding FStar.Pervasives.Native.tuple7>


; <Start encoding FStar.Pervasives.Native.Mktuple7>


; <start constructor FStar.Pervasives.Native.Mktuple7>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple7 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
241)
(= __@x0
(FStar.Pervasives.Native.Mktuple7 (FStar.Pervasives.Native.Mktuple7__a __@x0)
(FStar.Pervasives.Native.Mktuple7__b __@x0)
(FStar.Pervasives.Native.Mktuple7__c __@x0)
(FStar.Pervasives.Native.Mktuple7__d __@x0)
(FStar.Pervasives.Native.Mktuple7__e __@x0)
(FStar.Pervasives.Native.Mktuple7__f __@x0)
(FStar.Pervasives.Native.Mktuple7__g __@x0)
(FStar.Pervasives.Native.Mktuple7__1 __@x0)
(FStar.Pervasives.Native.Mktuple7__2 __@x0)
(FStar.Pervasives.Native.Mktuple7__3 __@x0)
(FStar.Pervasives.Native.Mktuple7__4 __@x0)
(FStar.Pervasives.Native.Mktuple7__5 __@x0)
(FStar.Pervasives.Native.Mktuple7__6 __@x0)
(FStar.Pervasives.Native.Mktuple7__7 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple7>


; </end encoding FStar.Pervasives.Native.Mktuple7>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple7__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple7__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple7>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple7 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> Prims.bool
(declare-fun Tm_arrow_612dde2fedb1440c5d790ba7f5015319 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple7@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple7>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___1 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'a
(declare-fun Tm_arrow_01c4488a68699f466c59799f5c1173ff () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___2 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'b
(declare-fun Tm_arrow_f317591858699585c67fe4ba8664e34c () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___3 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'c
(declare-fun Tm_arrow_44afce9d86f095aacc82b3ea2e0e223c () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___4 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'd
(declare-fun Tm_arrow_e857539d4cc5be0510cbcfb97cb64b35 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___5 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'e
(declare-fun Tm_arrow_a249d3d5ba06026b12d41e289bb88061 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___6 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'f
(declare-fun Tm_arrow_bf614c740d11cac9b5f8eb20b24c7d00 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple7__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___7 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) -> 'g
(declare-fun Tm_arrow_e775fbf03b08091e48143165286522f7 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple7__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple7__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple7__item___7/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple8 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple8@x7 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple8@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple8 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple8__8 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple8
(declare-fun FStar.Pervasives.Native.Mktuple8@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> _6: 'f -> _7: 'g -> _8: 'h   -> (((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h
(declare-fun Tm_arrow_e922a339a0aa0f375ed7113049811583 () Term)

; <Start encoding FStar.Pervasives.Native.tuple8>


; <start constructor FStar.Pervasives.Native.tuple8>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple8 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
261)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple8 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple8))))

; </end constructor FStar.Pervasives.Native.tuple8>


; </end encoding FStar.Pervasives.Native.tuple8>


; <Start encoding FStar.Pervasives.Native.Mktuple8>


; <start constructor FStar.Pervasives.Native.Mktuple8>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple8 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
268)
(= __@x0
(FStar.Pervasives.Native.Mktuple8 (FStar.Pervasives.Native.Mktuple8__a __@x0)
(FStar.Pervasives.Native.Mktuple8__b __@x0)
(FStar.Pervasives.Native.Mktuple8__c __@x0)
(FStar.Pervasives.Native.Mktuple8__d __@x0)
(FStar.Pervasives.Native.Mktuple8__e __@x0)
(FStar.Pervasives.Native.Mktuple8__f __@x0)
(FStar.Pervasives.Native.Mktuple8__g __@x0)
(FStar.Pervasives.Native.Mktuple8__h __@x0)
(FStar.Pervasives.Native.Mktuple8__1 __@x0)
(FStar.Pervasives.Native.Mktuple8__2 __@x0)
(FStar.Pervasives.Native.Mktuple8__3 __@x0)
(FStar.Pervasives.Native.Mktuple8__4 __@x0)
(FStar.Pervasives.Native.Mktuple8__5 __@x0)
(FStar.Pervasives.Native.Mktuple8__6 __@x0)
(FStar.Pervasives.Native.Mktuple8__7 __@x0)
(FStar.Pervasives.Native.Mktuple8__8 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple8>


; </end encoding FStar.Pervasives.Native.Mktuple8>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple8__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple8__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple8>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple8 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> Prims.bool
(declare-fun Tm_arrow_ee31533e24c78558f4566668a6ec027c () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple8@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple8>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___1 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'a
(declare-fun Tm_arrow_c971649e117e4941e7317eff508d5ea7 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___2 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'b
(declare-fun Tm_arrow_97dd51e3888c1c543d8f6c73d1808548 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___3 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'c
(declare-fun Tm_arrow_3931d1873633dc65fed4e022ee3df3ca () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___4 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'd
(declare-fun Tm_arrow_5c791e62f9472e4c351c2befb2b7a3d8 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___5 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'e
(declare-fun Tm_arrow_7ef7cac898ca0ef25893959e91d8c6ce () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___6 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'f
(declare-fun Tm_arrow_b0ae5f58a7fa002e0313b58bf5fc74cb () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___7 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'g
(declare-fun Tm_arrow_7fcd94f7549ca8acfadc26bc5b82f590 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple8__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___8 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) -> 'h
(declare-fun Tm_arrow_feaaf61fa62fef18c5ee7c39e9f86573 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple8__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple8__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple8__item___8/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple9 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple9@x8 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple9@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple9 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple9__9 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple9
(declare-fun FStar.Pervasives.Native.Mktuple9@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> _6: 'f -> _7: 'g -> _8: 'h -> _9: 'i   -> ((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i
(declare-fun Tm_arrow_0c6bc368a301d7de6e1939ebea91ee60 () Term)

; <Start encoding FStar.Pervasives.Native.tuple9>


; <start constructor FStar.Pervasives.Native.tuple9>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple9 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
290)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple9 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple9))))

; </end constructor FStar.Pervasives.Native.tuple9>


; </end encoding FStar.Pervasives.Native.tuple9>


; <Start encoding FStar.Pervasives.Native.Mktuple9>


; <start constructor FStar.Pervasives.Native.Mktuple9>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple9 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
297)
(= __@x0
(FStar.Pervasives.Native.Mktuple9 (FStar.Pervasives.Native.Mktuple9__a __@x0)
(FStar.Pervasives.Native.Mktuple9__b __@x0)
(FStar.Pervasives.Native.Mktuple9__c __@x0)
(FStar.Pervasives.Native.Mktuple9__d __@x0)
(FStar.Pervasives.Native.Mktuple9__e __@x0)
(FStar.Pervasives.Native.Mktuple9__f __@x0)
(FStar.Pervasives.Native.Mktuple9__g __@x0)
(FStar.Pervasives.Native.Mktuple9__h __@x0)
(FStar.Pervasives.Native.Mktuple9__i __@x0)
(FStar.Pervasives.Native.Mktuple9__1 __@x0)
(FStar.Pervasives.Native.Mktuple9__2 __@x0)
(FStar.Pervasives.Native.Mktuple9__3 __@x0)
(FStar.Pervasives.Native.Mktuple9__4 __@x0)
(FStar.Pervasives.Native.Mktuple9__5 __@x0)
(FStar.Pervasives.Native.Mktuple9__6 __@x0)
(FStar.Pervasives.Native.Mktuple9__7 __@x0)
(FStar.Pervasives.Native.Mktuple9__8 __@x0)
(FStar.Pervasives.Native.Mktuple9__9 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple9>


; </end encoding FStar.Pervasives.Native.Mktuple9>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple9__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple9__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple9>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple9 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> Prims.bool
(declare-fun Tm_arrow_9ac8f39c7b1df1e87db7c9bf5bc37a38 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple9@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple9>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___1 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'a
(declare-fun Tm_arrow_270119cc1f13c9afeb25322d78efc328 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___2 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'b
(declare-fun Tm_arrow_3c368dee2c86a1af7bd7ea91baab7613 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___3 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'c
(declare-fun Tm_arrow_e9c745e2da3dec50930b0a7e01a11cc3 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___4 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'd
(declare-fun Tm_arrow_a82ff41c5c66cd37481c83584c94a54d () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___5 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'e
(declare-fun Tm_arrow_1b3b4c5e68fdf7277f64bde93e6534de () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___6 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'f
(declare-fun Tm_arrow_837f1324f6fa51bb8a0e45ee48e4e058 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___7 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'g
(declare-fun Tm_arrow_a7562220963e3431d35de76c3c9c87b9 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___8 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'h
(declare-fun Tm_arrow_861b810bc1c20bbd221cecbce824b695 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple9__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___9 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) -> 'i
(declare-fun Tm_arrow_9a54b18d8e08fdf0e20244b3f960c9dc () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple9__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple9__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple9__item___9/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple10 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple10@x9 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple10@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple10 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__j (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple10__10 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple10
(declare-fun FStar.Pervasives.Native.Mktuple10@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a -> _2: 'b -> _3: 'c -> _4: 'd -> _5: 'e -> _6: 'f -> _7: 'g -> _8: 'h -> _9: 'i -> _10: 'j   -> (((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j
(declare-fun Tm_arrow_61d31241317018093b2245d256adbcb5 () Term)

; <Start encoding FStar.Pervasives.Native.tuple10>


; <start constructor FStar.Pervasives.Native.tuple10>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple10 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
321)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term) (@x9 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple10 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8
@x9))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple10))))

; </end constructor FStar.Pervasives.Native.tuple10>


; </end encoding FStar.Pervasives.Native.tuple10>


; <Start encoding FStar.Pervasives.Native.Mktuple10>


; <start constructor FStar.Pervasives.Native.Mktuple10>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple10 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
328)
(= __@x0
(FStar.Pervasives.Native.Mktuple10 (FStar.Pervasives.Native.Mktuple10__a __@x0)
(FStar.Pervasives.Native.Mktuple10__b __@x0)
(FStar.Pervasives.Native.Mktuple10__c __@x0)
(FStar.Pervasives.Native.Mktuple10__d __@x0)
(FStar.Pervasives.Native.Mktuple10__e __@x0)
(FStar.Pervasives.Native.Mktuple10__f __@x0)
(FStar.Pervasives.Native.Mktuple10__g __@x0)
(FStar.Pervasives.Native.Mktuple10__h __@x0)
(FStar.Pervasives.Native.Mktuple10__i __@x0)
(FStar.Pervasives.Native.Mktuple10__j __@x0)
(FStar.Pervasives.Native.Mktuple10__1 __@x0)
(FStar.Pervasives.Native.Mktuple10__2 __@x0)
(FStar.Pervasives.Native.Mktuple10__3 __@x0)
(FStar.Pervasives.Native.Mktuple10__4 __@x0)
(FStar.Pervasives.Native.Mktuple10__5 __@x0)
(FStar.Pervasives.Native.Mktuple10__6 __@x0)
(FStar.Pervasives.Native.Mktuple10__7 __@x0)
(FStar.Pervasives.Native.Mktuple10__8 __@x0)
(FStar.Pervasives.Native.Mktuple10__9 __@x0)
(FStar.Pervasives.Native.Mktuple10__10 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple10>


; </end encoding FStar.Pervasives.Native.Mktuple10>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple10__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple10__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple10>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple10 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> Prims.bool
(declare-fun Tm_arrow_f27282a056f525d8710bf32204d252ec () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple10@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple10>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple10/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___1 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'a
(declare-fun Tm_arrow_c581e9177cd071a1b6e057fca49ea75b () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___2 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'b
(declare-fun Tm_arrow_ae4b2db87d7c69a8380f4d5ae20f2149 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___3 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'c
(declare-fun Tm_arrow_a21274cb112dc6619b2bde244e6a0f9a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___4 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'd
(declare-fun Tm_arrow_9a051d5cacf4367d170d590ba8bb720d () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___5 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'e
(declare-fun Tm_arrow_bbd73769b626202d4de52d4d60cd3b75 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___6 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'f
(declare-fun Tm_arrow_7ceeded5a3852448c1a5406becbd990e () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___7 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'g
(declare-fun Tm_arrow_c68947c71d484ad43cd50646c4e1daf4 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___8 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'h
(declare-fun Tm_arrow_e7b9ff90289491020fe84c6ab3bc60c6 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___9 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'i
(declare-fun Tm_arrow_6dbb3170f112f78092d1caee0b341678 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple10__item___10>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___10 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) -> 'j
(declare-fun Tm_arrow_45598a99c0a7fcc1bf2258b9ad4256cf () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple10__item___10@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple10__item___10>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple10__item___10/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple11 (Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple11@x10 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple11@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple11 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__j (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__k (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple11__11 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple11
(declare-fun FStar.Pervasives.Native.Mktuple11@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a ->     _2: 'b ->     _3: 'c ->     _4: 'd ->     _5: 'e ->     _6: 'f ->     _7: 'g ->     _8: 'h ->     _9: 'i ->     _10: 'j ->     _11: 'k   -> ((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k
(declare-fun Tm_arrow_bf9783a1a3bf19ab918f42acff1daa32 () Term)

; <Start encoding FStar.Pervasives.Native.tuple11>


; <start constructor FStar.Pervasives.Native.tuple11>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple11 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
354)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term) (@x9 Term) (@x10 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple11 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8
@x9
@x10))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple11))))

; </end constructor FStar.Pervasives.Native.tuple11>


; </end encoding FStar.Pervasives.Native.tuple11>


; <Start encoding FStar.Pervasives.Native.Mktuple11>


; <start constructor FStar.Pervasives.Native.Mktuple11>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple11 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
361)
(= __@x0
(FStar.Pervasives.Native.Mktuple11 (FStar.Pervasives.Native.Mktuple11__a __@x0)
(FStar.Pervasives.Native.Mktuple11__b __@x0)
(FStar.Pervasives.Native.Mktuple11__c __@x0)
(FStar.Pervasives.Native.Mktuple11__d __@x0)
(FStar.Pervasives.Native.Mktuple11__e __@x0)
(FStar.Pervasives.Native.Mktuple11__f __@x0)
(FStar.Pervasives.Native.Mktuple11__g __@x0)
(FStar.Pervasives.Native.Mktuple11__h __@x0)
(FStar.Pervasives.Native.Mktuple11__i __@x0)
(FStar.Pervasives.Native.Mktuple11__j __@x0)
(FStar.Pervasives.Native.Mktuple11__k __@x0)
(FStar.Pervasives.Native.Mktuple11__1 __@x0)
(FStar.Pervasives.Native.Mktuple11__2 __@x0)
(FStar.Pervasives.Native.Mktuple11__3 __@x0)
(FStar.Pervasives.Native.Mktuple11__4 __@x0)
(FStar.Pervasives.Native.Mktuple11__5 __@x0)
(FStar.Pervasives.Native.Mktuple11__6 __@x0)
(FStar.Pervasives.Native.Mktuple11__7 __@x0)
(FStar.Pervasives.Native.Mktuple11__8 __@x0)
(FStar.Pervasives.Native.Mktuple11__9 __@x0)
(FStar.Pervasives.Native.Mktuple11__10 __@x0)
(FStar.Pervasives.Native.Mktuple11__11 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple11>


; </end encoding FStar.Pervasives.Native.Mktuple11>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple11__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple11__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple11>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple11 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> Prims.bool
(declare-fun Tm_arrow_005819ee7a23a5c47189bae72b85d85c () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple11@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple11>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple11/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___1 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'a
(declare-fun Tm_arrow_31968e334e9582d95281307f534992a9 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___2 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'b
(declare-fun Tm_arrow_6252dd9f4473dc54a3482810e8556404 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___3 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'c
(declare-fun Tm_arrow_ec3ce6b7406c091cd7d0961922bb5a02 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___4 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'd
(declare-fun Tm_arrow_be7571e73b0e7fc24d03efe0e003c054 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___5 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'e
(declare-fun Tm_arrow_97ae7d913e508c46c48c3b51553d4459 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___6 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'f
(declare-fun Tm_arrow_1dca311798936510e0ead61e14cf32a6 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___7 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'g
(declare-fun Tm_arrow_eec431ea31093a646681ef2ceb2e2986 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___8 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'h
(declare-fun Tm_arrow_689b2f06e9fd83f7a84ce80a13d338c6 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___9 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'i
(declare-fun Tm_arrow_20210a3d9498f929cb7aa68d9e8b5ebf () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___10>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___10 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'j
(declare-fun Tm_arrow_96812f2124d88760b2002bbe1502c3c9 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___10@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___10>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___10/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple11__item___11>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___11 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) -> 'k
(declare-fun Tm_arrow_abcfa2582f68905d460c5ef4a7642f2d () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple11__item___11@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple11__item___11>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple11__item___11/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple12 (Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple12@x11 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple12@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple12 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__j (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__k (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__l (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__11 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple12__12 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple12
(declare-fun FStar.Pervasives.Native.Mktuple12@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a ->     _2: 'b ->     _3: 'c ->     _4: 'd ->     _5: 'e ->     _6: 'f ->     _7: 'g ->     _8: 'h ->     _9: 'i ->     _10: 'j ->     _11: 'k ->     _12: 'l   -> (((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l
(declare-fun Tm_arrow_4d5cd995d6f44a2ec39d0f193be0be65 () Term)

; <Start encoding FStar.Pervasives.Native.tuple12>


; <start constructor FStar.Pervasives.Native.tuple12>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple12 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
389)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term) (@x9 Term) (@x10 Term) (@x11 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple12 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8
@x9
@x10
@x11))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple12))))

; </end constructor FStar.Pervasives.Native.tuple12>


; </end encoding FStar.Pervasives.Native.tuple12>


; <Start encoding FStar.Pervasives.Native.Mktuple12>


; <start constructor FStar.Pervasives.Native.Mktuple12>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple12 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
396)
(= __@x0
(FStar.Pervasives.Native.Mktuple12 (FStar.Pervasives.Native.Mktuple12__a __@x0)
(FStar.Pervasives.Native.Mktuple12__b __@x0)
(FStar.Pervasives.Native.Mktuple12__c __@x0)
(FStar.Pervasives.Native.Mktuple12__d __@x0)
(FStar.Pervasives.Native.Mktuple12__e __@x0)
(FStar.Pervasives.Native.Mktuple12__f __@x0)
(FStar.Pervasives.Native.Mktuple12__g __@x0)
(FStar.Pervasives.Native.Mktuple12__h __@x0)
(FStar.Pervasives.Native.Mktuple12__i __@x0)
(FStar.Pervasives.Native.Mktuple12__j __@x0)
(FStar.Pervasives.Native.Mktuple12__k __@x0)
(FStar.Pervasives.Native.Mktuple12__l __@x0)
(FStar.Pervasives.Native.Mktuple12__1 __@x0)
(FStar.Pervasives.Native.Mktuple12__2 __@x0)
(FStar.Pervasives.Native.Mktuple12__3 __@x0)
(FStar.Pervasives.Native.Mktuple12__4 __@x0)
(FStar.Pervasives.Native.Mktuple12__5 __@x0)
(FStar.Pervasives.Native.Mktuple12__6 __@x0)
(FStar.Pervasives.Native.Mktuple12__7 __@x0)
(FStar.Pervasives.Native.Mktuple12__8 __@x0)
(FStar.Pervasives.Native.Mktuple12__9 __@x0)
(FStar.Pervasives.Native.Mktuple12__10 __@x0)
(FStar.Pervasives.Native.Mktuple12__11 __@x0)
(FStar.Pervasives.Native.Mktuple12__12 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple12>


; </end encoding FStar.Pervasives.Native.Mktuple12>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple12__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple12__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple12>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple12 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l)   -> Prims.bool
(declare-fun Tm_arrow_5c9f47d9090f554c9826d2f65e388f20 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple12@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple12>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple12/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___1 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'a
(declare-fun Tm_arrow_618941d7cf5ddbaabe15df8579b4a387 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___2 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'b
(declare-fun Tm_arrow_84e9e2280e9bcb3233e4f33f86d66ea6 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___3 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'c
(declare-fun Tm_arrow_1fa79c5abf9f18607bd2e46a1a6967fa () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___4 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'd
(declare-fun Tm_arrow_0f49c582489d782b08195e81221181dc () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___5 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'e
(declare-fun Tm_arrow_29b7181ebb44f9e4a45f95c4f8478c6a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___6 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'f
(declare-fun Tm_arrow_3cc2863a7d7f23e3916fa1e43483cb90 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___7 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'g
(declare-fun Tm_arrow_c7deea49701ab64a73985bf522e46359 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___8 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'h
(declare-fun Tm_arrow_380615e7761919086537a14273a02d22 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___9 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'i
(declare-fun Tm_arrow_245250918a4432b31aea8152d056489a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___10>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___10 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'j
(declare-fun Tm_arrow_2a967c8402c441e6d8a9336a7568e4de () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___10@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___10>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___10/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___11>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___11 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'k
(declare-fun Tm_arrow_543c3feac0cd9e04ecb6cfd74ced8964 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___11@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___11>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___11/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple12__item___12>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___12 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) -> 'l
(declare-fun Tm_arrow_e91029e2320896c60e94f554727a0c41 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple12__item___12@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple12__item___12>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple12__item___12/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple13 (Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x11 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple13@x12 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple13@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple13 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__j (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__k (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__l (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__m (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__11 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__12 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple13__13 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple13
(declare-fun FStar.Pervasives.Native.Mktuple13@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a ->     _2: 'b ->     _3: 'c ->     _4: 'd ->     _5: 'e ->     _6: 'f ->     _7: 'g ->     _8: 'h ->     _9: 'i ->     _10: 'j ->     _11: 'k ->     _12: 'l ->     _13: 'm   -> ((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm
(declare-fun Tm_arrow_6462785e86ca440ee74ed32e1053eae3 () Term)

; <Start encoding FStar.Pervasives.Native.tuple13>


; <start constructor FStar.Pervasives.Native.tuple13>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple13 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
426)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term) (@x9 Term) (@x10 Term) (@x11 Term) (@x12 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple13 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8
@x9
@x10
@x11
@x12))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple13))))

; </end constructor FStar.Pervasives.Native.tuple13>


; </end encoding FStar.Pervasives.Native.tuple13>


; <Start encoding FStar.Pervasives.Native.Mktuple13>


; <start constructor FStar.Pervasives.Native.Mktuple13>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple13 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
433)
(= __@x0
(FStar.Pervasives.Native.Mktuple13 (FStar.Pervasives.Native.Mktuple13__a __@x0)
(FStar.Pervasives.Native.Mktuple13__b __@x0)
(FStar.Pervasives.Native.Mktuple13__c __@x0)
(FStar.Pervasives.Native.Mktuple13__d __@x0)
(FStar.Pervasives.Native.Mktuple13__e __@x0)
(FStar.Pervasives.Native.Mktuple13__f __@x0)
(FStar.Pervasives.Native.Mktuple13__g __@x0)
(FStar.Pervasives.Native.Mktuple13__h __@x0)
(FStar.Pervasives.Native.Mktuple13__i __@x0)
(FStar.Pervasives.Native.Mktuple13__j __@x0)
(FStar.Pervasives.Native.Mktuple13__k __@x0)
(FStar.Pervasives.Native.Mktuple13__l __@x0)
(FStar.Pervasives.Native.Mktuple13__m __@x0)
(FStar.Pervasives.Native.Mktuple13__1 __@x0)
(FStar.Pervasives.Native.Mktuple13__2 __@x0)
(FStar.Pervasives.Native.Mktuple13__3 __@x0)
(FStar.Pervasives.Native.Mktuple13__4 __@x0)
(FStar.Pervasives.Native.Mktuple13__5 __@x0)
(FStar.Pervasives.Native.Mktuple13__6 __@x0)
(FStar.Pervasives.Native.Mktuple13__7 __@x0)
(FStar.Pervasives.Native.Mktuple13__8 __@x0)
(FStar.Pervasives.Native.Mktuple13__9 __@x0)
(FStar.Pervasives.Native.Mktuple13__10 __@x0)
(FStar.Pervasives.Native.Mktuple13__11 __@x0)
(FStar.Pervasives.Native.Mktuple13__12 __@x0)
(FStar.Pervasives.Native.Mktuple13__13 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple13>


; </end encoding FStar.Pervasives.Native.Mktuple13>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple13__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple13__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple13>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple13 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> Prims.bool
(declare-fun Tm_arrow_68c092e8b387730b412c4dcf592b12d3 () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple13@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple13>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple13/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___1 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'a
(declare-fun Tm_arrow_82a3dc3a5dbad615d8d4a31db238e43f () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___2 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'b
(declare-fun Tm_arrow_1da976aaa65f1c6b8b256dfc45c41306 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___3 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'c
(declare-fun Tm_arrow_ca5cf529c415deee29e0a34c0c5d1c9f () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___4 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'd
(declare-fun Tm_arrow_94f6c578541b6cb528ca9e7dd1dacc3b () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___5 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'e
(declare-fun Tm_arrow_211e172b7220adc186d8a02ff17e8780 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___6 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'f
(declare-fun Tm_arrow_9276a4f669d8497205e8d59f12da53ba () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___7 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'g
(declare-fun Tm_arrow_8aa8f381a5ed57cbbae9dcd2405ce80f () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___8 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'h
(declare-fun Tm_arrow_51814106613688cf259d7cdba9c24d93 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___9 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'i
(declare-fun Tm_arrow_05fec25e6f03f974bb2933a910642d7e () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___10>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___10 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'j
(declare-fun Tm_arrow_3280ee04611a7985c9d107bb1a8a330a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___10@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___10>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___10/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___11>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___11 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'k
(declare-fun Tm_arrow_86c868d5d5058e8e5ec1f4d0285c7e90 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___11@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___11>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___11/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___12>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___12 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'l
(declare-fun Tm_arrow_7263c1a3c4475bb4e4b41a1be4bf22da () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___12@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___12>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___12/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple13__item___13>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___13 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm)   -> 'm
(declare-fun Tm_arrow_338c65ae58844787891c6f47cf01c068 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple13__item___13@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple13__item___13>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple13__item___13/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.tuple14 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x11 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x12 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.tuple14@x13 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.Native.tuple14@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Native.Mktuple14 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__f (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__g (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__h (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__i (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__j (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__k (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__l (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__m (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__n (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__5 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__6 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__7 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__8 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__9 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__10 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__11 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__12 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__13 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Native.Mktuple14__14 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mktuple14
(declare-fun FStar.Pervasives.Native.Mktuple14@tok () Term)
;;;;;;;;;;;;;;;;_1: 'a ->     _2: 'b ->     _3: 'c ->     _4: 'd ->     _5: 'e ->     _6: 'f ->     _7: 'g ->     _8: 'h ->     _9: 'i ->     _10: 'j ->     _11: 'k ->     _12: 'l ->     _13: 'm ->     _14: 'n   -> (((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n
(declare-fun Tm_arrow_484e3bf88a886900f7e695d7333615e9 () Term)

; <Start encoding FStar.Pervasives.Native.tuple14>


; <start constructor FStar.Pervasives.Native.tuple14>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.tuple14 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
465)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term) (@x8 Term) (@x9 Term) (@x10 Term) (@x11 Term) (@x12 Term) (@x13 Term))
 (! (= __@x0
(FStar.Pervasives.Native.tuple14 @x0
@x1
@x2
@x3
@x4
@x5
@x6
@x7
@x8
@x9
@x10
@x11
@x12
@x13))
 
;;no pats
:qid is-FStar.Pervasives.Native.tuple14))))

; </end constructor FStar.Pervasives.Native.tuple14>


; </end encoding FStar.Pervasives.Native.tuple14>


; <Start encoding FStar.Pervasives.Native.Mktuple14>


; <start constructor FStar.Pervasives.Native.Mktuple14>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Native.Mktuple14 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
472)
(= __@x0
(FStar.Pervasives.Native.Mktuple14 (FStar.Pervasives.Native.Mktuple14__a __@x0)
(FStar.Pervasives.Native.Mktuple14__b __@x0)
(FStar.Pervasives.Native.Mktuple14__c __@x0)
(FStar.Pervasives.Native.Mktuple14__d __@x0)
(FStar.Pervasives.Native.Mktuple14__e __@x0)
(FStar.Pervasives.Native.Mktuple14__f __@x0)
(FStar.Pervasives.Native.Mktuple14__g __@x0)
(FStar.Pervasives.Native.Mktuple14__h __@x0)
(FStar.Pervasives.Native.Mktuple14__i __@x0)
(FStar.Pervasives.Native.Mktuple14__j __@x0)
(FStar.Pervasives.Native.Mktuple14__k __@x0)
(FStar.Pervasives.Native.Mktuple14__l __@x0)
(FStar.Pervasives.Native.Mktuple14__m __@x0)
(FStar.Pervasives.Native.Mktuple14__n __@x0)
(FStar.Pervasives.Native.Mktuple14__1 __@x0)
(FStar.Pervasives.Native.Mktuple14__2 __@x0)
(FStar.Pervasives.Native.Mktuple14__3 __@x0)
(FStar.Pervasives.Native.Mktuple14__4 __@x0)
(FStar.Pervasives.Native.Mktuple14__5 __@x0)
(FStar.Pervasives.Native.Mktuple14__6 __@x0)
(FStar.Pervasives.Native.Mktuple14__7 __@x0)
(FStar.Pervasives.Native.Mktuple14__8 __@x0)
(FStar.Pervasives.Native.Mktuple14__9 __@x0)
(FStar.Pervasives.Native.Mktuple14__10 __@x0)
(FStar.Pervasives.Native.Mktuple14__11 __@x0)
(FStar.Pervasives.Native.Mktuple14__12 __@x0)
(FStar.Pervasives.Native.Mktuple14__13 __@x0)
(FStar.Pervasives.Native.Mktuple14__14 __@x0)))))

; </end constructor FStar.Pervasives.Native.Mktuple14>


; </end encoding FStar.Pervasives.Native.Mktuple14>


; </end encoding >


; <Start encoding FStar.Pervasives.Native.tuple14__uu___haseq>


; </end encoding FStar.Pervasives.Native.tuple14__uu___haseq>


; <Start encoding FStar.Pervasives.Native.uu___is_Mktuple14>

(declare-fun FStar.Pervasives.Native.uu___is_Mktuple14 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> Prims.bool
(declare-fun Tm_arrow_2de133cfaca100fc23d8bf4b3421db9a () Term)
(declare-fun FStar.Pervasives.Native.uu___is_Mktuple14@tok () Term)

; </end encoding FStar.Pervasives.Native.uu___is_Mktuple14>


; <Skipped FStar.Pervasives.Native.uu___is_Mktuple14/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___1>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___1 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'a
(declare-fun Tm_arrow_2e3216cab266e138debd68d0a503c177 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___1@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___1>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___1/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___2>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___2 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'b
(declare-fun Tm_arrow_958b0270e487d0bf5fe9191b9efaa127 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___2@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___2>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___2/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___3>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___3 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'c
(declare-fun Tm_arrow_08349f596f8c0acf60d1587bebe8c91b () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___3@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___3>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___3/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___4>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___4 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'd
(declare-fun Tm_arrow_2b069168147ba0f67f117ad5b0ac078b () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___4@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___4>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___4/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___5>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___5 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'e
(declare-fun Tm_arrow_1e38bb16245a24a197c44a262fee7bf1 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___5@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___5>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___5/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___6>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___6 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'f
(declare-fun Tm_arrow_7a148953a3884454d8a1dffddce086bb () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___6@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___6>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___6/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___7>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___7 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'g
(declare-fun Tm_arrow_812eeb3fdab56dfea8e419236740acb0 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___7@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___7>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___7/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___8>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___8 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'h
(declare-fun Tm_arrow_9dc932ce7cdfd6fa57f6536787fcb65b () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___8@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___8>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___8/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___9>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___9 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'i
(declare-fun Tm_arrow_2600722933f06bc55e28bb3fc2ce4a6a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___9@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___9>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___9/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___10>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___10 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'j
(declare-fun Tm_arrow_f51203e57fd66f9e9293b8962c57edfe () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___10@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___10>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___10/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___11>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___11 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'k
(declare-fun Tm_arrow_7c34e0c28edc5fc4ad24d0b749c0adb7 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___11@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___11>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___11/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___12>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___12 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'l
(declare-fun Tm_arrow_8772cc50ea320af17b3f2371c273679a () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___12@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___12>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___12/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___13>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___13 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'm
(declare-fun Tm_arrow_039da0b9a8da1a651a1c570e55456614 () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___13@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___13>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___13/>


; <Start encoding FStar.Pervasives.Native.__proj__Mktuple14__item___14>

(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___14 (Term Term Term Term Term Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee:       ((((((((((((('a * 'b) * 'c) * 'd) * 'e) * 'f) * 'g) * 'h) * 'i) * 'j) * 'k) * 'l) * 'm) * 'n)   -> 'n
(declare-fun Tm_arrow_579ada2eb036c15c7306dac5b648153e () Term)
(declare-fun FStar.Pervasives.Native.__proj__Mktuple14__item___14@tok () Term)

; </end encoding FStar.Pervasives.Native.__proj__Mktuple14__item___14>


; <Skipped FStar.Pervasives.Native.__proj__Mktuple14__item___14/>


; End Externals for module FStar.Pervasives.Native


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Pervasives.Native (1325 decls; total size 133367)

;;; Start interface FStar.Pervasives

; Externals for interface FStar.Pervasives


; <Start encoding FStar.Pervasives.remove_unused_type_parameters>

(declare-fun FStar.Pervasives.remove_unused_type_parameters (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.list Prims.int -> Prims.unit
(declare-fun Tm_arrow_555d62757eeaf90340982fcdf25f6704 () Term)
(declare-fun FStar.Pervasives.remove_unused_type_parameters@tok () Term)

; </end encoding FStar.Pervasives.remove_unused_type_parameters>


; <Start encoding FStar.Pervasives.pattern>

(declare-fun FStar.Pervasives.pattern () Term)

; </end encoding FStar.Pervasives.pattern>


; <Start encoding FStar.Pervasives.smt_pat>

(declare-fun FStar.Pervasives.smt_pat (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> FStar.Pervasives.pattern
(declare-fun Tm_arrow_ce7b692455ad1649f97902066cf7c9aa () Term)
(declare-fun FStar.Pervasives.smt_pat@tok () Term)

; </end encoding FStar.Pervasives.smt_pat>


; <Start encoding FStar.Pervasives.smt_pat_or>

(declare-fun FStar.Pervasives.smt_pat_or (Term) Term)
;;;;;;;;;;;;;;;;x: Prims.list (Prims.list FStar.Pervasives.pattern) -> FStar.Pervasives.pattern
(declare-fun Tm_arrow_cbfaca2770c8ef7d6393b664b7ea1a41 () Term)
(declare-fun FStar.Pervasives.smt_pat_or@tok () Term)

; </end encoding FStar.Pervasives.smt_pat_or>


; <Start encoding FStar.Pervasives.eqtype_u>

(declare-fun FStar.Pervasives.eqtype_u () Term)


; </end encoding FStar.Pervasives.eqtype_u>


; <Skipped FStar.Pervasives.Lemma/>


; <Start encoding FStar.Pervasives.spinoff>

(declare-fun FStar.Pervasives.spinoff (Term) Term)

(declare-fun FStar.Pervasives.spinoff@tok () Term)

; </end encoding FStar.Pervasives.spinoff>


; <Start encoding FStar.Pervasives.assert_spinoff>

(declare-fun FStar.Pervasives.assert_spinoff (Term) Term)

;;;;;;;;;;;;;;;;p: Type -> Prims.Pure Prims.unit
(declare-fun Tm_arrow_071538fd1c72fb82c7bb7b280daddf84 () Term)
(declare-fun FStar.Pervasives.assert_spinoff@tok () Term)


; </end encoding FStar.Pervasives.assert_spinoff>


; <Start encoding FStar.Pervasives.id>

(declare-fun FStar.Pervasives.id (Term Term) Term)

(declare-fun FStar.Pervasives.id@tok () Term)

; </end encoding FStar.Pervasives.id>


; <Start encoding FStar.Pervasives.trivial_pure_post>

(declare-fun FStar.Pervasives.trivial_pure_post (Term) Term)
;;;;;;;;;;;;;;;;a: Type -> Prims.pure_post a
(declare-fun Tm_arrow_53823f439377767fbcd3e27ebcdb971b () Term)
(declare-fun FStar.Pervasives.trivial_pure_post@tok () Term)

(declare-fun Tm_abs_5e34897418ce4950a4effcc8c159cf53 (Term) Term)

; </end encoding FStar.Pervasives.trivial_pure_post>


; <Start encoding FStar.Pervasives.ambient>

(declare-fun FStar.Pervasives.ambient (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> Type
(declare-fun Tm_arrow_9e007179360e2932d75ab29019e3d7fa () Term)
(declare-fun FStar.Pervasives.ambient@tok () Term)

; </end encoding FStar.Pervasives.ambient>


; <Start encoding FStar.Pervasives.intro_ambient>

(declare-fun FStar.Pervasives.intro_ambient (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> Prims.squash (FStar.Pervasives.ambient x)
(declare-fun Tm_arrow_6fc6334d56387f3d408122a4bd045e7e () Term)
(declare-fun FStar.Pervasives.intro_ambient@tok () Term)

; </end encoding FStar.Pervasives.intro_ambient>


; <Start encoding FStar.Pervasives.normalize_term>

(declare-fun FStar.Pervasives.normalize_term (Term Term) Term)

(declare-fun FStar.Pervasives.normalize_term@tok () Term)

; </end encoding FStar.Pervasives.normalize_term>


; <Start encoding FStar.Pervasives.normalize>

(declare-fun FStar.Pervasives.normalize (Term) Term)

(declare-fun FStar.Pervasives.normalize@tok () Term)

; </end encoding FStar.Pervasives.normalize>


; <Start encoding FStar.Pervasives.norm_step>

(declare-fun FStar.Pervasives.norm_step () Term)

; </end encoding FStar.Pervasives.norm_step>


; <Start encoding FStar.Pervasives.simplify>

(declare-fun FStar.Pervasives.simplify (Dummy_sort) Term)

; </end encoding FStar.Pervasives.simplify>


; <Start encoding FStar.Pervasives.weak>

(declare-fun FStar.Pervasives.weak (Dummy_sort) Term)

; </end encoding FStar.Pervasives.weak>


; <Start encoding FStar.Pervasives.hnf>

(declare-fun FStar.Pervasives.hnf (Dummy_sort) Term)

; </end encoding FStar.Pervasives.hnf>


; <Start encoding FStar.Pervasives.primops>

(declare-fun FStar.Pervasives.primops (Dummy_sort) Term)

; </end encoding FStar.Pervasives.primops>


; <Start encoding FStar.Pervasives.delta>

(declare-fun FStar.Pervasives.delta (Dummy_sort) Term)

; </end encoding FStar.Pervasives.delta>


; <Start encoding FStar.Pervasives.zeta>

(declare-fun FStar.Pervasives.zeta (Dummy_sort) Term)

; </end encoding FStar.Pervasives.zeta>


; <Start encoding FStar.Pervasives.zeta_full>

(declare-fun FStar.Pervasives.zeta_full (Dummy_sort) Term)

; </end encoding FStar.Pervasives.zeta_full>


; <Start encoding FStar.Pervasives.iota>

(declare-fun FStar.Pervasives.iota (Dummy_sort) Term)

; </end encoding FStar.Pervasives.iota>


; <Start encoding FStar.Pervasives.nbe>

(declare-fun FStar.Pervasives.nbe (Dummy_sort) Term)

; </end encoding FStar.Pervasives.nbe>


; <Start encoding FStar.Pervasives.reify_>

(declare-fun FStar.Pervasives.reify_ (Dummy_sort) Term)

; </end encoding FStar.Pervasives.reify_>


; <Start encoding FStar.Pervasives.delta_only>

(declare-fun FStar.Pervasives.delta_only (Term) Term)
;;;;;;;;;;;;;;;;s: Prims.list Prims.string -> FStar.Pervasives.norm_step
(declare-fun Tm_arrow_f14a20345cd55ddda96b6c4cc49e05f1 () Term)
(declare-fun FStar.Pervasives.delta_only@tok () Term)

; </end encoding FStar.Pervasives.delta_only>


; <Start encoding FStar.Pervasives.delta_fully>

(declare-fun FStar.Pervasives.delta_fully (Term) Term)

(declare-fun FStar.Pervasives.delta_fully@tok () Term)

; </end encoding FStar.Pervasives.delta_fully>


; <Start encoding FStar.Pervasives.delta_attr>

(declare-fun FStar.Pervasives.delta_attr (Term) Term)

(declare-fun FStar.Pervasives.delta_attr@tok () Term)

; </end encoding FStar.Pervasives.delta_attr>


; <Start encoding FStar.Pervasives.delta_qualifier>

(declare-fun FStar.Pervasives.delta_qualifier (Term) Term)

(declare-fun FStar.Pervasives.delta_qualifier@tok () Term)

; </end encoding FStar.Pervasives.delta_qualifier>


; <Start encoding FStar.Pervasives.delta_namespace>

(declare-fun FStar.Pervasives.delta_namespace (Term) Term)

(declare-fun FStar.Pervasives.delta_namespace@tok () Term)

; </end encoding FStar.Pervasives.delta_namespace>


; <Start encoding FStar.Pervasives.unmeta>

(declare-fun FStar.Pervasives.unmeta (Dummy_sort) Term)

; </end encoding FStar.Pervasives.unmeta>


; <Start encoding FStar.Pervasives.unascribe>

(declare-fun FStar.Pervasives.unascribe (Dummy_sort) Term)

; </end encoding FStar.Pervasives.unascribe>


; <Start encoding FStar.Pervasives.norm>

(declare-fun FStar.Pervasives.norm (Term Term Term) Term)
;;;;;;;;;;;;;;;;s: Prims.list FStar.Pervasives.norm_step -> x: a -> a
(declare-fun Tm_arrow_7d92e7a4aa7eee4098b10c5f1b3d77ea () Term)
(declare-fun FStar.Pervasives.norm@tok () Term)

; </end encoding FStar.Pervasives.norm>


; <Start encoding FStar.Pervasives.assert_norm>

(declare-fun FStar.Pervasives.assert_norm (Term) Term)

;;;;;;;;;;;;;;;;p: Type -> Prims.Pure Prims.unit
(declare-fun Tm_arrow_ee24fcf624d074d3c637ee61e4a867fb () Term)
(declare-fun FStar.Pervasives.assert_norm@tok () Term)


; </end encoding FStar.Pervasives.assert_norm>


; <Start encoding FStar.Pervasives.normalize_term_spec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Pervasives.normalize_term_spec (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Pervasives.normalize_term_spec@tok () Term)

; </end encoding FStar.Pervasives.normalize_term_spec>


; <Start encoding FStar.Pervasives.normalize_spec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Pervasives.normalize_spec (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Pervasives.normalize_spec@tok () Term)

; </end encoding FStar.Pervasives.normalize_spec>


; <Start encoding FStar.Pervasives.norm_spec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Pervasives.norm_spec (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Pervasives.norm_spec@tok () Term)

; </end encoding FStar.Pervasives.norm_spec>


; <Start encoding FStar.Pervasives.reveal_opaque>

(declare-fun FStar.Pervasives.reveal_opaque (Term Term) Term)
(declare-fun Tm_refine_9cce35912d99bf51042f02fff62b6cf5 (Term Term Term) Term)
;;;;;;;;;;;;;;;;x: _   -> FStar.Pervasives.Lemma (ensures FStar.Pervasives.norm [FStar.Pervasives.delta_only [s]] x == x)
(declare-fun Tm_arrow_90324bd6d0db52152d012eefdf7852a1 (Term Term) Term)
;;;;;;;;;;;;;;;;s: Prims.string -> x: _   -> FStar.Pervasives.Lemma (ensures FStar.Pervasives.norm [FStar.Pervasives.delta_only [s]] x == x)
(declare-fun Tm_arrow_d3acaf108460ddc930424dea55f7d40f () Term)
(declare-fun FStar.Pervasives.reveal_opaque@tok () Term)


;;;;;;;;;;;;;;;;kick_partial_app
;;; Fact-ids: Name FStar.Pervasives.reveal_opaque; Namespace FStar.Pervasives
(assert (! (Valid (ApplyTT __uu__PartialApp
FStar.Pervasives.norm_spec@tok))
:named @kick_partial_app_e5c933a9bc2cb06571c2abdcc101b877))

; </end encoding FStar.Pervasives.reveal_opaque>


; <Start encoding FStar.Pervasives.pure_return>

(declare-fun FStar.Pervasives.pure_return (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> x: a -> Prims.pure_wp a
(declare-fun Tm_arrow_0dff8d294aeaf0b1d7e9cad664c9a15f () Term)
(declare-fun FStar.Pervasives.pure_return@tok () Term)

(declare-fun Tm_abs_bc5117f6a06a581c69e04141781c86d0 (Term Term Term) Term)
;;;;;;;;;;;;;;;;p: Prims.pure_post a -> Prims.logical
(declare-fun Tm_arrow_c88a29758356586fc450d481d4b685f3 (Term) Term)
(declare-fun Tm_abs_bdac9a3f32789788b83138a3a4262d0d (Term Term) Term)

; </end encoding FStar.Pervasives.pure_return>


; <Start encoding FStar.Pervasives.pure_bind_wp>

;;;;;;;;;;;;;;;;_: a -> Prims.pure_wp b
(declare-fun Tm_arrow_c05bc9331677cc1a187ad7677301a601 (Term Term) Term)
(declare-fun FStar.Pervasives.pure_bind_wp (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;a: Type -> b: Type -> wp1: Prims.pure_wp a -> wp2: (_: a -> Prims.pure_wp b) -> Prims.pure_wp b
(declare-fun Tm_arrow_f8eba41a4bcb9aca0e3c11224f695d1e () Term)
(declare-fun FStar.Pervasives.pure_bind_wp@tok () Term)


(declare-fun Tm_abs_d0f415a5361a9d7988d8e425dc193472 (Term Term Term) Term)

(declare-fun Tm_abs_72b65b71b828688dbb0ba657715a194c (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.pure_bind_wp>


; <Start encoding FStar.Pervasives.pure_if_then_else>

(declare-fun FStar.Pervasives.pure_if_then_else (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> p: Type -> wp_then: Prims.pure_wp a -> wp_else: Prims.pure_wp a -> Prims.pure_wp a
(declare-fun Tm_arrow_5911c11ab85061b4a8acf6a6ff43aaea () Term)
(declare-fun FStar.Pervasives.pure_if_then_else@tok () Term)

(declare-fun Tm_abs_614d7ab3976dfea6b6428085a93bafcc (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.pure_if_then_else>


; <Start encoding FStar.Pervasives.pure_ite_wp>

(declare-fun FStar.Pervasives.pure_ite_wp (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp: Prims.pure_wp a -> Prims.pure_wp a
(declare-fun Tm_arrow_983cc9c1e881fffd3b37c61b327d54c8 () Term)
(declare-fun FStar.Pervasives.pure_ite_wp@tok () Term)




(declare-fun Tm_abs_f9993abe3e10fc5902262cf22e5f5e1c (Term Term Term) Term)

(declare-fun Tm_abs_a4d8a67e708eb0f8e41d6eefd90d8b3d (Term Term) Term)

; </end encoding FStar.Pervasives.pure_ite_wp>


; <Start encoding FStar.Pervasives.pure_close_wp>


(declare-fun FStar.Pervasives.pure_close_wp (Term Term Term) Term)

;;;;;;;;;;;;;;;;a: Type -> b: Type -> wp: (_: b -> Prims.pure_wp a) -> Prims.pure_wp a
(declare-fun Tm_arrow_6424f17922e38fc8eb3b7ad8d9107d78 () Term)
(declare-fun FStar.Pervasives.pure_close_wp@tok () Term)




(declare-fun Tm_abs_bef01cdc202d648a37f7725a1e8579fa (Term Term Term) Term)

; </end encoding FStar.Pervasives.pure_close_wp>


; <Start encoding FStar.Pervasives.pure_null_wp>

(declare-fun FStar.Pervasives.pure_null_wp (Term) Term)
;;;;;;;;;;;;;;;;a: Type -> Prims.pure_wp a
(declare-fun Tm_arrow_e02f472dad10492b4fdaf21971ae643f () Term)
(declare-fun FStar.Pervasives.pure_null_wp@tok () Term)



(declare-fun Tm_abs_c7a599bd05f6d553477b7b3a5a51d357 (Term) Term)

; </end encoding FStar.Pervasives.pure_null_wp>


; <Start encoding FStar.Pervasives.pure_assert_wp>

(declare-fun FStar.Pervasives.pure_assert_wp (Term) Term)

(declare-fun FStar.Pervasives.pure_assert_wp@tok () Term)

; </end encoding FStar.Pervasives.pure_assert_wp>


; <Start encoding FStar.Pervasives.pure_assume_wp>

(declare-fun FStar.Pervasives.pure_assume_wp (Term) Term)

(declare-fun FStar.Pervasives.pure_assume_wp@tok () Term)

; </end encoding FStar.Pervasives.pure_assume_wp>


; <Skipped FStar.Pervasives.DIV/>


; <Skipped />


; <Start encoding FStar.Pervasives.div_hoare_to_wp>

(declare-fun FStar.Pervasives.div_hoare_to_wp (Term Term Term) Term)
;;;;;;;;;;;;;;;;post: Prims.pure_post' a pre -> Prims.pure_wp a
(declare-fun Tm_arrow_e81e37f60b892c60a4b806bfecd6c240 () Term)
(declare-fun FStar.Pervasives.div_hoare_to_wp@tok () Term)




(declare-fun Tm_abs_69982e78bbdc9cbdfcc98c8c3ec276f1 (Term Term Term Term) Term)


(declare-fun Tm_abs_95829b03554cf2093d2bc29e28500b94 (Term Term Term) Term)

; </end encoding FStar.Pervasives.div_hoare_to_wp>


; <Skipped FStar.Pervasives.Div/>


; <Skipped FStar.Pervasives.Dv/>


; <Skipped FStar.Pervasives.EXT/>


; <Start encoding FStar.Pervasives.st_pre_h>

(declare-fun FStar.Pervasives.st_pre_h (Term) Term)

(declare-fun FStar.Pervasives.st_pre_h@tok () Term)


; </end encoding FStar.Pervasives.st_pre_h>


; <Start encoding FStar.Pervasives.st_post_h'>

(declare-fun FStar.Pervasives.st_post_h_ (Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type -> a: Type -> pre: Type -> Type
(declare-fun Tm_arrow_659175ed40df3b798f91ffaee9e689bd () Term)
(declare-fun FStar.Pervasives.st_post_h_@tok () Term)

;;;;;;;;;;;;;;;;_: a -> _: heap{pre} -> Prims.GTot Type
(declare-fun Tm_arrow_14435f7112db17792f8cd33f8f7ea859 (Term Term Term) Term)

; </end encoding FStar.Pervasives.st_post_h'>


; <Start encoding FStar.Pervasives.st_post_h>

(declare-fun FStar.Pervasives.st_post_h (Term Term) Term)

(declare-fun FStar.Pervasives.st_post_h@tok () Term)

; </end encoding FStar.Pervasives.st_post_h>


; <Start encoding FStar.Pervasives.st_wp_h>

(declare-fun FStar.Pervasives.st_wp_h (Term Term) Term)

(declare-fun FStar.Pervasives.st_wp_h@tok () Term)
;;;;;;;;;;;;;;;;_: FStar.Pervasives.st_post_h heap a -> FStar.Pervasives.st_pre_h heap
(declare-fun Tm_arrow_c80b139653078194d2de90941effdc68 (Term Term) Term)

; </end encoding FStar.Pervasives.st_wp_h>


; <Start encoding FStar.Pervasives.st_return>

(declare-fun FStar.Pervasives.st_return (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type -> a: Type -> x: a -> p: FStar.Pervasives.st_post_h heap a -> _: heap -> Prims.GTot Type
(declare-fun Tm_arrow_6bfe4bf6faf1fb53a521d575cefc35ef () Term)
(declare-fun FStar.Pervasives.st_return@tok () Term)


; </end encoding FStar.Pervasives.st_return>


; <Start encoding FStar.Pervasives.st_bind_wp>

;;;;;;;;;;;;;;;;_: a -> Prims.GTot (FStar.Pervasives.st_wp_h heap b)
(declare-fun Tm_arrow_c6e0af8c2ccbdda79db5c09d07e87e35 (Term Term Term) Term)
(declare-fun FStar.Pervasives.st_bind_wp (Term Term Term Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     b: Type ->     wp1: FStar.Pervasives.st_wp_h heap a ->     wp2: (_: a -> Prims.GTot (FStar.Pervasives.st_wp_h heap b)) ->     p: FStar.Pervasives.st_post_h heap b ->     h0: heap   -> Type
(declare-fun Tm_arrow_0eca50df2f29485bdbf578799f16b4a6 () Term)
(declare-fun FStar.Pervasives.st_bind_wp@tok () Term)

;;;;;;;;;;;;;;;;a: a -> h1: heap -> Prims.GTot Type
(declare-fun Tm_arrow_eb9b1a038524b37579c152a3f169145e (Term Term) Term)
(declare-fun Tm_abs_0f3b5ee9eaa8de8cacad7d3dcacb4558 (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.st_bind_wp>


; <Start encoding FStar.Pervasives.st_if_then_else>

(declare-fun FStar.Pervasives.st_if_then_else (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     p: Type ->     wp_then: FStar.Pervasives.st_wp_h heap a ->     wp_else: FStar.Pervasives.st_wp_h heap a ->     post: FStar.Pervasives.st_post_h heap a ->     h0: heap   -> Prims.logical
(declare-fun Tm_arrow_6e48361e1a1c92df6ec1ff87e622ddad () Term)
(declare-fun FStar.Pervasives.st_if_then_else@tok () Term)

; </end encoding FStar.Pervasives.st_if_then_else>


; <Start encoding FStar.Pervasives.st_ite_wp>

(declare-fun FStar.Pervasives.st_ite_wp (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     wp: FStar.Pervasives.st_wp_h heap a ->     post: FStar.Pervasives.st_post_h heap a ->     h0: heap   -> Prims.logical
(declare-fun Tm_arrow_eaad896c6afdcb7ade2e80b5a6a930af () Term)
(declare-fun FStar.Pervasives.st_ite_wp@tok () Term)

(declare-fun Tm_abs_13c1fbac4d566537e0d0aac54993e867 (Term Term Term Term) Term)

(declare-fun Tm_abs_8fca386860b07bf7135ad6a5ed4b8699 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;k: FStar.Pervasives.st_post_h heap a -> Prims.GTot Type
(declare-fun Tm_arrow_8be317cd0e95382d209163cbcd734f9d (Term Term) Term)
(declare-fun Tm_abs_72e7071059c05ac5aef1b1a6a8b5e8f4 (Term Term Term Term Term) Term)

; </end encoding FStar.Pervasives.st_ite_wp>


; <Start encoding FStar.Pervasives.st_stronger>

(declare-fun FStar.Pervasives.st_stronger (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     wp1: FStar.Pervasives.st_wp_h heap a ->     wp2: FStar.Pervasives.st_wp_h heap a   -> Prims.logical
(declare-fun Tm_arrow_ae4d7f489de84317e0022bf89d45dd95 () Term)
(declare-fun FStar.Pervasives.st_stronger@tok () Term)



(declare-fun Tm_abs_01fd99084e2163543709c497ff1c256b (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.st_stronger>


; <Start encoding FStar.Pervasives.st_close_wp>


(declare-fun FStar.Pervasives.st_close_wp (Term Term Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     b: Type ->     wp: (_: b -> Prims.GTot (FStar.Pervasives.st_wp_h heap a)) ->     p: FStar.Pervasives.st_post_h heap a ->     h: heap   -> Prims.logical
(declare-fun Tm_arrow_de6d3045642382698e9e38d41acfd7cc () Term)
(declare-fun FStar.Pervasives.st_close_wp@tok () Term)


(declare-fun Tm_abs_bd152fc5c12cc6aa125ccf543aa53813 (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.st_close_wp>


; <Start encoding FStar.Pervasives.st_trivial>

(declare-fun FStar.Pervasives.st_trivial (Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type -> a: Type -> wp: FStar.Pervasives.st_wp_h heap a -> Prims.logical
(declare-fun Tm_arrow_f145e04ff3c7033bdfc718f7f5bb1df0 () Term)
(declare-fun FStar.Pervasives.st_trivial@tok () Term)

(declare-fun Tm_abs_89b21c42be5bc00d63e29f63ae20d4e2 (Term Term) Term)

(declare-fun Tm_abs_59c4fa07f408d911e59025309cce9942 (Term Term Term) Term)





; </end encoding FStar.Pervasives.st_trivial>


; <Skipped FStar.Pervasives.STATE_h/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.result (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.result@x0 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.result@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.V (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.V_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.V_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: V
(declare-fun FStar.Pervasives.V@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.E (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.E_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.E_e (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: E
(declare-fun FStar.Pervasives.E@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Err (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Err_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Err_msg (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Err
(declare-fun FStar.Pervasives.Err@tok () Term)
;;;;;;;;;;;;;;;;v: a -> FStar.Pervasives.result a
(declare-fun Tm_arrow_30908143640041985b9200e2fb38a259 () Term)
;;;;;;;;;;;;;;;;e: Prims.exn -> FStar.Pervasives.result a
(declare-fun Tm_arrow_f8bb10130fea772e0f786d78a188c381 () Term)
;;;;;;;;;;;;;;;;msg: Prims.string -> FStar.Pervasives.result a
(declare-fun Tm_arrow_93661c87034b0b64c4714dafbe2b02e6 () Term)

; <Start encoding FStar.Pervasives.result>


; <start constructor FStar.Pervasives.result>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.result ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
281)
(exists ((@x0 Term))
 (! (= __@x0
(FStar.Pervasives.result @x0))
 
;;no pats
:qid is-FStar.Pervasives.result))))

; </end constructor FStar.Pervasives.result>


; </end encoding FStar.Pervasives.result>


; <Start encoding FStar.Pervasives.V>


; <start constructor FStar.Pervasives.V>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.V ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
288)
(= __@x0
(FStar.Pervasives.V (FStar.Pervasives.V_a __@x0)
(FStar.Pervasives.V_v __@x0)))))

; </end constructor FStar.Pervasives.V>


; </end encoding FStar.Pervasives.V>


; <Start encoding FStar.Pervasives.E>


; <start constructor FStar.Pervasives.E>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.E ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
293)
(= __@x0
(FStar.Pervasives.E (FStar.Pervasives.E_a __@x0)
(FStar.Pervasives.E_e __@x0)))))

; </end constructor FStar.Pervasives.E>


; </end encoding FStar.Pervasives.E>


; <Start encoding FStar.Pervasives.Err>


; <start constructor FStar.Pervasives.Err>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Err ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
298)
(= __@x0
(FStar.Pervasives.Err (FStar.Pervasives.Err_a __@x0)
(FStar.Pervasives.Err_msg __@x0)))))

; </end constructor FStar.Pervasives.Err>


; </end encoding FStar.Pervasives.Err>


; </end encoding >


; <Start encoding FStar.Pervasives.uu___is_V>

(declare-fun FStar.Pervasives.uu___is_V (Term Term) Term)
;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.result a -> Prims.bool
(declare-fun Tm_arrow_5cd1d0722a6a986faf6f8e557186fe24 () Term)
(declare-fun FStar.Pervasives.uu___is_V@tok () Term)

; </end encoding FStar.Pervasives.uu___is_V>


; <Skipped FStar.Pervasives.uu___is_V/>


; <Start encoding FStar.Pervasives.__proj__V__item__v>

(declare-fun Tm_refine_9db520b26a7f39c5a01493a3f375290d (Term) Term)
(declare-fun FStar.Pervasives.__proj__V__item__v (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.result a {V? _} -> a
(declare-fun Tm_arrow_1ea119bf213c016916a7095486e28467 () Term)
(declare-fun FStar.Pervasives.__proj__V__item__v@tok () Term)

; </end encoding FStar.Pervasives.__proj__V__item__v>


; <Skipped FStar.Pervasives.__proj__V__item__v/>


; <Start encoding FStar.Pervasives.uu___is_E>

(declare-fun FStar.Pervasives.uu___is_E (Term Term) Term)

(declare-fun FStar.Pervasives.uu___is_E@tok () Term)

; </end encoding FStar.Pervasives.uu___is_E>


; <Skipped FStar.Pervasives.uu___is_E/>


; <Start encoding FStar.Pervasives.__proj__E__item__e>

(declare-fun Tm_refine_95e1e2ee29104754cc3740f5575fc6e5 (Term) Term)
(declare-fun FStar.Pervasives.__proj__E__item__e (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.result a {E? _} -> Prims.exn
(declare-fun Tm_arrow_19e73c373dbf3f9945c6fcfce8a07661 () Term)
(declare-fun FStar.Pervasives.__proj__E__item__e@tok () Term)

; </end encoding FStar.Pervasives.__proj__E__item__e>


; <Skipped FStar.Pervasives.__proj__E__item__e/>


; <Start encoding FStar.Pervasives.uu___is_Err>

(declare-fun FStar.Pervasives.uu___is_Err (Term Term) Term)

(declare-fun FStar.Pervasives.uu___is_Err@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Err>


; <Skipped FStar.Pervasives.uu___is_Err/>


; <Start encoding FStar.Pervasives.__proj__Err__item__msg>

(declare-fun Tm_refine_22fb403854eba07427f92e79848f9d9f (Term) Term)
(declare-fun FStar.Pervasives.__proj__Err__item__msg (Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.result a {Err? _} -> Prims.string
(declare-fun Tm_arrow_f7e3debb858e412c9497460c5187d5cd () Term)
(declare-fun FStar.Pervasives.__proj__Err__item__msg@tok () Term)

; </end encoding FStar.Pervasives.__proj__Err__item__msg>


; <Skipped FStar.Pervasives.__proj__Err__item__msg/>


; <Start encoding FStar.Pervasives.ex_pre>

(declare-fun FStar.Pervasives.ex_pre () Term)

; </end encoding FStar.Pervasives.ex_pre>


; <Start encoding FStar.Pervasives.ex_post'>

(declare-fun FStar.Pervasives.ex_post_ (Term Term) Term)

(declare-fun FStar.Pervasives.ex_post_@tok () Term)
(declare-fun Tm_refine_a4dcdeeacbcb04d05a6720f786918fd6 (Term Term) Term)
;;;;;;;;;;;;;;;;_: FStar.Pervasives.result a {pre} -> Prims.GTot Type
(declare-fun Tm_arrow_68b66d987e8a7bdf825af8b370553e65 (Term Term) Term)

; </end encoding FStar.Pervasives.ex_post'>


; <Start encoding FStar.Pervasives.ex_post>

(declare-fun FStar.Pervasives.ex_post (Term) Term)

(declare-fun FStar.Pervasives.ex_post@tok () Term)

; </end encoding FStar.Pervasives.ex_post>


; <Start encoding FStar.Pervasives.ex_wp>

(declare-fun FStar.Pervasives.ex_wp (Term) Term)

(declare-fun FStar.Pervasives.ex_wp@tok () Term)
;;;;;;;;;;;;;;;;_: FStar.Pervasives.ex_post a -> Prims.GTot FStar.Pervasives.ex_pre
(declare-fun Tm_arrow_58168e52ae0908fefec42cac825ecc69 (Term) Term)

; </end encoding FStar.Pervasives.ex_wp>


; <Start encoding FStar.Pervasives.ex_return>

(declare-fun FStar.Pervasives.ex_return (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> x: a -> p: FStar.Pervasives.ex_post a -> Prims.GTot Type
(declare-fun Tm_arrow_375264f6f19b4e37d33ffba9f6b1c7d2 () Term)
(declare-fun FStar.Pervasives.ex_return@tok () Term)

; </end encoding FStar.Pervasives.ex_return>


; <Start encoding FStar.Pervasives.ex_bind_wp>

;;;;;;;;;;;;;;;;_: a -> Prims.GTot (FStar.Pervasives.ex_wp b)
(declare-fun Tm_arrow_3eb2992a529511f5b0ff2fef4e4594ad (Term Term) Term)
(declare-fun FStar.Pervasives.ex_bind_wp (Term Term Term Term Term) Term)

;;;;;;;;;;;;;;;;a: Type ->     b: Type ->     wp1: FStar.Pervasives.ex_wp a ->     wp2: (_: a -> Prims.GTot (FStar.Pervasives.ex_wp b)) ->     p: FStar.Pervasives.ex_post b   -> Prims.GTot Type
(declare-fun Tm_arrow_1da2056f1a2fe3dc8db7decf5cbd5885 () Term)
(declare-fun FStar.Pervasives.ex_bind_wp@tok () Term)

;;;;;;;;;;;;;;;;rb: FStar.Pervasives.result b -> Prims.GTot Type
(declare-fun Tm_arrow_ca5db633696caf7e0cd44c11654eed8b (Term) Term)
(declare-fun Tm_abs_8074df73d1580fbbdc3470addabaf08b (Term Term Term) Term)

(declare-fun Tm_abs_c1d9037a5cc10cc07ba9b6a7a58728db (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;k: FStar.Pervasives.ex_post b -> Prims.GTot Type
(declare-fun Tm_arrow_9ea71f7f2fb13f04805b20355b90ad43 (Term) Term)
(declare-fun Tm_abs_f22410a64e648d873bed4822ca022bb4 (Term Term Term Term Term) Term)





; </end encoding FStar.Pervasives.ex_bind_wp>


; <Start encoding FStar.Pervasives.ex_if_then_else>

(declare-fun FStar.Pervasives.ex_if_then_else (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type ->     p: Type ->     wp_then: FStar.Pervasives.ex_wp a ->     wp_else: FStar.Pervasives.ex_wp a ->     post: FStar.Pervasives.ex_post a   -> Prims.logical
(declare-fun Tm_arrow_08bd7ce530cc6e8b4a3f8dadbd0806b0 () Term)
(declare-fun FStar.Pervasives.ex_if_then_else@tok () Term)

; </end encoding FStar.Pervasives.ex_if_then_else>


; <Start encoding FStar.Pervasives.ex_ite_wp>

(declare-fun FStar.Pervasives.ex_ite_wp (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp: FStar.Pervasives.ex_wp a -> post: FStar.Pervasives.ex_post a -> Prims.logical
(declare-fun Tm_arrow_c2a8c761b16a75376b24262cd8c50369 () Term)
(declare-fun FStar.Pervasives.ex_ite_wp@tok () Term)




(declare-fun Tm_abs_a7cc1da09b297d88bb79b1fa0b467b00 (Term Term Term) Term)

; </end encoding FStar.Pervasives.ex_ite_wp>


; <Start encoding FStar.Pervasives.ex_stronger>

(declare-fun FStar.Pervasives.ex_stronger (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp1: FStar.Pervasives.ex_wp a -> wp2: FStar.Pervasives.ex_wp a -> Prims.logical
(declare-fun Tm_arrow_1376d97b5d43e7d77d56729e2a3e04af () Term)
(declare-fun FStar.Pervasives.ex_stronger@tok () Term)

(declare-fun Tm_abs_1518a3cc57d7490d55df77581752827a (Term Term Term) Term)

; </end encoding FStar.Pervasives.ex_stronger>


; <Start encoding FStar.Pervasives.ex_close_wp>


(declare-fun FStar.Pervasives.ex_close_wp (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;a: Type ->     b: Type ->     wp: (_: b -> Prims.GTot (FStar.Pervasives.ex_wp a)) ->     p: FStar.Pervasives.ex_post a   -> Prims.logical
(declare-fun Tm_arrow_814af0adff92aa08c5b8b0951bcb1959 () Term)
(declare-fun FStar.Pervasives.ex_close_wp@tok () Term)





; </end encoding FStar.Pervasives.ex_close_wp>


; <Start encoding FStar.Pervasives.ex_trivial>

(declare-fun FStar.Pervasives.ex_trivial (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp: FStar.Pervasives.ex_wp a -> FStar.Pervasives.ex_pre
(declare-fun Tm_arrow_ee4a787765920b0cb4357a47a0d3ac5c () Term)
(declare-fun FStar.Pervasives.ex_trivial@tok () Term)

(declare-fun Tm_abs_5cc223716d095f4545f0dcc745acad5d (Term) Term)

; </end encoding FStar.Pervasives.ex_trivial>


; <Skipped FStar.Pervasives.EXN/>


; <Skipped FStar.Pervasives.Exn/>


; <Start encoding FStar.Pervasives.lift_div_exn>

(declare-fun FStar.Pervasives.lift_div_exn (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> wp: Prims.pure_wp a -> p: FStar.Pervasives.ex_post a -> Prims.pure_pre
(declare-fun Tm_arrow_8196682216f286f6fe3a7dffb3de7d02 () Term)
(declare-fun FStar.Pervasives.lift_div_exn@tok () Term)

(declare-fun Tm_abs_c2b605ddd5d1991642baf5762d2b1dc5 (Term Term) Term)

; </end encoding FStar.Pervasives.lift_div_exn>


; <Skipped />


; <Skipped FStar.Pervasives.Ex/>


; <Start encoding FStar.Pervasives.all_pre_h>

(declare-fun FStar.Pervasives.all_pre_h (Term) Term)

(declare-fun FStar.Pervasives.all_pre_h@tok () Term)


; </end encoding FStar.Pervasives.all_pre_h>


; <Start encoding FStar.Pervasives.all_post_h'>

(declare-fun FStar.Pervasives.all_post_h_ (Term Term Term) Term)

(declare-fun FStar.Pervasives.all_post_h_@tok () Term)

;;;;;;;;;;;;;;;;_: FStar.Pervasives.result a -> _: h{pre} -> Prims.GTot Type
(declare-fun Tm_arrow_fc269489cb2e24a10c7710a1f7f9d269 (Term Term Term) Term)

; </end encoding FStar.Pervasives.all_post_h'>


; <Start encoding FStar.Pervasives.all_post_h>

(declare-fun FStar.Pervasives.all_post_h (Term Term) Term)

(declare-fun FStar.Pervasives.all_post_h@tok () Term)

; </end encoding FStar.Pervasives.all_post_h>


; <Start encoding FStar.Pervasives.all_wp_h>

(declare-fun FStar.Pervasives.all_wp_h (Term Term) Term)

(declare-fun FStar.Pervasives.all_wp_h@tok () Term)
;;;;;;;;;;;;;;;;_: FStar.Pervasives.all_post_h h a -> FStar.Pervasives.all_pre_h h
(declare-fun Tm_arrow_1cd90c71d90a216d9fb0ba0321a1d3b5 (Term Term) Term)

; </end encoding FStar.Pervasives.all_wp_h>


; <Start encoding FStar.Pervasives.all_return>

(declare-fun FStar.Pervasives.all_return (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type -> a: Type -> x: a -> p: FStar.Pervasives.all_post_h heap a -> _: heap -> Prims.GTot Type
(declare-fun Tm_arrow_3f61557667800fb54cc62e48a5201f9d () Term)
(declare-fun FStar.Pervasives.all_return@tok () Term)


; </end encoding FStar.Pervasives.all_return>


; <Start encoding FStar.Pervasives.all_bind_wp>

;;;;;;;;;;;;;;;;_: a -> Prims.GTot (FStar.Pervasives.all_wp_h heap b)
(declare-fun Tm_arrow_b567b509414635f00096b9b1c3e30b57 (Term Term Term) Term)
(declare-fun FStar.Pervasives.all_bind_wp (Term Term Term Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     b: Type ->     wp1: FStar.Pervasives.all_wp_h heap a ->     wp2: (_: a -> Prims.GTot (FStar.Pervasives.all_wp_h heap b)) ->     p: FStar.Pervasives.all_post_h heap b ->     h0: heap   -> Prims.GTot Type
(declare-fun Tm_arrow_6ac18e25eb49f55ae0ce9c14679ecc22 () Term)
(declare-fun FStar.Pervasives.all_bind_wp@tok () Term)

;;;;;;;;;;;;;;;;ra: FStar.Pervasives.result a -> h1: heap -> Prims.GTot Type
(declare-fun Tm_arrow_59cac8a9b1ae3aa9511b8a867f8e934e (Term Term) Term)
(declare-fun Tm_abs_35ddc99cefc0079215f6f6ab3c58856d (Term Term Term Term Term) Term)

; </end encoding FStar.Pervasives.all_bind_wp>


; <Start encoding FStar.Pervasives.all_if_then_else>

(declare-fun FStar.Pervasives.all_if_then_else (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     p: Type ->     wp_then: FStar.Pervasives.all_wp_h heap a ->     wp_else: FStar.Pervasives.all_wp_h heap a ->     post: FStar.Pervasives.all_post_h heap a ->     h0: heap   -> Prims.logical
(declare-fun Tm_arrow_491eee2c8dc4eab4d420326a8285d2c4 () Term)
(declare-fun FStar.Pervasives.all_if_then_else@tok () Term)

; </end encoding FStar.Pervasives.all_if_then_else>


; <Start encoding FStar.Pervasives.all_ite_wp>

(declare-fun FStar.Pervasives.all_ite_wp (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     wp: FStar.Pervasives.all_wp_h heap a ->     post: FStar.Pervasives.all_post_h heap a ->     h0: heap   -> Prims.logical
(declare-fun Tm_arrow_20fdb4e6d0c32f949f55e39a059913a7 () Term)
(declare-fun FStar.Pervasives.all_ite_wp@tok () Term)



(declare-fun Tm_abs_b4b0cf2e42901b42be235caa84bfc7f4 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;k: FStar.Pervasives.all_post_h heap a -> Prims.GTot Type
(declare-fun Tm_arrow_f7466eb55fd31463cdfb5b4251fe1912 (Term Term) Term)
(declare-fun Tm_abs_aa90876ffd60a4411fdca7eeeb8f6130 (Term Term Term Term Term) Term)

; </end encoding FStar.Pervasives.all_ite_wp>


; <Start encoding FStar.Pervasives.all_stronger>

(declare-fun FStar.Pervasives.all_stronger (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     wp1: FStar.Pervasives.all_wp_h heap a ->     wp2: FStar.Pervasives.all_wp_h heap a   -> Prims.logical
(declare-fun Tm_arrow_073b21d0ec8edf2dda32907b45ec5f68 () Term)
(declare-fun FStar.Pervasives.all_stronger@tok () Term)



(declare-fun Tm_abs_61b202c6a5250875acb8de09e14bb21a (Term Term Term Term) Term)

; </end encoding FStar.Pervasives.all_stronger>


; <Start encoding FStar.Pervasives.all_close_wp>


(declare-fun FStar.Pervasives.all_close_wp (Term Term Term Term Term Term) Term)

;;;;;;;;;;;;;;;;heap: Type ->     a: Type ->     b: Type ->     wp: (_: b -> Prims.GTot (FStar.Pervasives.all_wp_h heap a)) ->     p: FStar.Pervasives.all_post_h heap a ->     h: heap   -> Prims.logical
(declare-fun Tm_arrow_803d195802308e8beadf04438d3a6508 () Term)
(declare-fun FStar.Pervasives.all_close_wp@tok () Term)





; </end encoding FStar.Pervasives.all_close_wp>


; <Start encoding FStar.Pervasives.all_trivial>

(declare-fun FStar.Pervasives.all_trivial (Term Term Term) Term)
;;;;;;;;;;;;;;;;heap: Type -> a: Type -> wp: FStar.Pervasives.all_wp_h heap a -> Prims.logical
(declare-fun Tm_arrow_957927b0d25001784693eee8b2182308 () Term)
(declare-fun FStar.Pervasives.all_trivial@tok () Term)

(declare-fun Tm_abs_22e463dbd987016e31d6bc67025a7cd9 (Term Term) Term)

(declare-fun Tm_abs_c7ae205604ad646e5515eb4bea93d3f2 (Term Term Term) Term)





; </end encoding FStar.Pervasives.all_trivial>


; <Skipped FStar.Pervasives.ALL_h/>


; <Start encoding FStar.Pervasives.inversion>

(declare-fun FStar.Pervasives.inversion (Term) Term)

(declare-fun FStar.Pervasives.inversion@tok () Term)

; </end encoding FStar.Pervasives.inversion>


; <Start encoding FStar.Pervasives.allow_inversion>

(declare-fun FStar.Pervasives.allow_inversion (Term) Term)
(declare-fun Tm_refine_363615bee79fae5066b7c8bd06c286d0 (Term) Term)
;;;;;;;;;;;;;;;;a: Type -> Prims.Pure Prims.unit
(declare-fun Tm_arrow_bcab9cce464ec0f76562bc48c17ba410 () Term)
(declare-fun FStar.Pervasives.allow_inversion@tok () Term)


; </end encoding FStar.Pervasives.allow_inversion>


; <Start encoding FStar.Pervasives.invertOption>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Pervasives.invertOption (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Pervasives.invertOption@tok () Term)

; </end encoding FStar.Pervasives.invertOption>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.either (Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.either@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.either@x1 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.either@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Inl (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inl_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inl_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inl_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Inl
(declare-fun FStar.Pervasives.Inl@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Inr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inr_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inr_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Inr_v (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Inr
(declare-fun FStar.Pervasives.Inr@tok () Term)
;;;;;;;;;;;;;;;;v: a -> FStar.Pervasives.either a b
(declare-fun Tm_arrow_065da0adeba0c4ae0da1476ececee84c () Term)
;;;;;;;;;;;;;;;;v: b -> FStar.Pervasives.either a b
(declare-fun Tm_arrow_c883938642e6d97d79c975d8d94b4aac () Term)

; <Start encoding FStar.Pervasives.either>


; <start constructor FStar.Pervasives.either>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.either ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
454)
(exists ((@x0 Term) (@x1 Term))
 (! (= __@x0
(FStar.Pervasives.either @x0
@x1))
 
;;no pats
:qid is-FStar.Pervasives.either))))

; </end constructor FStar.Pervasives.either>


; </end encoding FStar.Pervasives.either>


; <Start encoding FStar.Pervasives.Inl>


; <start constructor FStar.Pervasives.Inl>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Inl ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
461)
(= __@x0
(FStar.Pervasives.Inl (FStar.Pervasives.Inl_a __@x0)
(FStar.Pervasives.Inl_b __@x0)
(FStar.Pervasives.Inl_v __@x0)))))

; </end constructor FStar.Pervasives.Inl>


; </end encoding FStar.Pervasives.Inl>


; <Start encoding FStar.Pervasives.Inr>


; <start constructor FStar.Pervasives.Inr>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Inr ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
466)
(= __@x0
(FStar.Pervasives.Inr (FStar.Pervasives.Inr_a __@x0)
(FStar.Pervasives.Inr_b __@x0)
(FStar.Pervasives.Inr_v __@x0)))))

; </end constructor FStar.Pervasives.Inr>


; </end encoding FStar.Pervasives.Inr>


; </end encoding >


; <Start encoding FStar.Pervasives.either__uu___haseq>


; </end encoding FStar.Pervasives.either__uu___haseq>


; <Start encoding FStar.Pervasives.uu___is_Inl>

(declare-fun FStar.Pervasives.uu___is_Inl (Term Term Term) Term)
;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.either a b -> Prims.bool
(declare-fun Tm_arrow_af0c68f1e39d4d6020c0873b16730c7d () Term)
(declare-fun FStar.Pervasives.uu___is_Inl@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Inl>


; <Skipped FStar.Pervasives.uu___is_Inl/>


; <Start encoding FStar.Pervasives.__proj__Inl__item__v>

(declare-fun Tm_refine_85e0cc884f8457202f90cd77f23733ba (Term Term) Term)
(declare-fun FStar.Pervasives.__proj__Inl__item__v (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.either a b {Inl? _} -> a
(declare-fun Tm_arrow_a80e0750277867ba1a434ad3bba8702d () Term)
(declare-fun FStar.Pervasives.__proj__Inl__item__v@tok () Term)

; </end encoding FStar.Pervasives.__proj__Inl__item__v>


; <Skipped FStar.Pervasives.__proj__Inl__item__v/>


; <Start encoding FStar.Pervasives.uu___is_Inr>

(declare-fun FStar.Pervasives.uu___is_Inr (Term Term Term) Term)

(declare-fun FStar.Pervasives.uu___is_Inr@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Inr>


; <Skipped FStar.Pervasives.uu___is_Inr/>


; <Start encoding FStar.Pervasives.__proj__Inr__item__v>

(declare-fun Tm_refine_8f1f5f564dae90240db429de2eb41517 (Term Term) Term)
(declare-fun FStar.Pervasives.__proj__Inr__item__v (Term Term Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.either a b {Inr? _} -> b
(declare-fun Tm_arrow_df618db6b42762940f198036c8a56200 () Term)
(declare-fun FStar.Pervasives.__proj__Inr__item__v@tok () Term)

; </end encoding FStar.Pervasives.__proj__Inr__item__v>


; <Skipped FStar.Pervasives.__proj__Inr__item__v/>


; <Start encoding FStar.Pervasives.dfst>


(declare-fun FStar.Pervasives.dfst (Term Term Term) Term)


(declare-fun FStar.Pervasives.dfst@tok () Term)


; </end encoding FStar.Pervasives.dfst>


; <Start encoding FStar.Pervasives.dsnd>


(declare-fun FStar.Pervasives.dsnd (Term Term Term) Term)


(declare-fun FStar.Pervasives.dsnd@tok () Term)


; </end encoding FStar.Pervasives.dsnd>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.dtuple3 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple3@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple3@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple3@x2 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.dtuple3@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Mkdtuple3 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3_c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple3__3 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mkdtuple3
(declare-fun FStar.Pervasives.Mkdtuple3@tok () Term)

;;;;;;;;;;;;;;;;x: a -> _: b x -> Prims.GTot Type
(declare-fun Tm_arrow_0b6559e6ff3addf84b0c2880affbb335 (Term Term) Term)




;;;;;;;;;;;;;;;;_1: a -> _2: b _1 -> _3: c _1 _2 -> FStar.Pervasives.dtuple3 a b c
(declare-fun Tm_arrow_8423f67df62f9e824c55756f9e26058d () Term)

; <Start encoding FStar.Pervasives.dtuple3>


; <start constructor FStar.Pervasives.dtuple3>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.dtuple3 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
502)
(exists ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= __@x0
(FStar.Pervasives.dtuple3 @x0
@x1
@x2))
 
;;no pats
:qid is-FStar.Pervasives.dtuple3))))

; </end constructor FStar.Pervasives.dtuple3>


; </end encoding FStar.Pervasives.dtuple3>


; <Start encoding FStar.Pervasives.Mkdtuple3>


; <start constructor FStar.Pervasives.Mkdtuple3>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Mkdtuple3 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
513)
(= __@x0
(FStar.Pervasives.Mkdtuple3 (FStar.Pervasives.Mkdtuple3_a __@x0)
(FStar.Pervasives.Mkdtuple3_b __@x0)
(FStar.Pervasives.Mkdtuple3_c __@x0)
(FStar.Pervasives.Mkdtuple3__1 __@x0)
(FStar.Pervasives.Mkdtuple3__2 __@x0)
(FStar.Pervasives.Mkdtuple3__3 __@x0)))))

; </end constructor FStar.Pervasives.Mkdtuple3>


; </end encoding FStar.Pervasives.Mkdtuple3>


; </end encoding >


; <Start encoding FStar.Pervasives.dtuple3__uu___haseq>




; </end encoding FStar.Pervasives.dtuple3__uu___haseq>


; <Start encoding FStar.Pervasives.uu___is_Mkdtuple3>



(declare-fun FStar.Pervasives.uu___is_Mkdtuple3 (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple3 a b c -> Prims.bool
(declare-fun Tm_arrow_70452cb82cd0a282ca9a2dbeb54c1b04 () Term)
(declare-fun FStar.Pervasives.uu___is_Mkdtuple3@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Mkdtuple3>


; <Skipped FStar.Pervasives.uu___is_Mkdtuple3/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple3__item___1>



(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___1 (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple3 a b c -> a
(declare-fun Tm_arrow_255f0cfe499b1d2e9836e157bce1dba3 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___1@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple3__item___1>


; <Skipped FStar.Pervasives.__proj__Mkdtuple3__item___1/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple3__item___2>



(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___2 (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple3 a b c -> b (Mkdtuple3?._1 projectee)
(declare-fun Tm_arrow_ea1ded11f7d194a26e812f407333a011 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___2@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple3__item___2>


; <Skipped FStar.Pervasives.__proj__Mkdtuple3__item___2/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple3__item___3>



(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___3 (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple3 a b c -> c (Mkdtuple3?._1 projectee) (Mkdtuple3?._2 projectee)
(declare-fun Tm_arrow_1d7ad5cfa0fff643640e3f74466d283e () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple3__item___3@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple3__item___3>


; <Skipped FStar.Pervasives.__proj__Mkdtuple3__item___3/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.dtuple4 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple4@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple4@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple4@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple4@x3 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.dtuple4@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Mkdtuple4 (Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4_c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4_d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple4__4 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mkdtuple4
(declare-fun FStar.Pervasives.Mkdtuple4@tok () Term)


;;;;;;;;;;;;;;;;x: a -> y: b x -> z: c x y -> Prims.GTot Type
(declare-fun Tm_arrow_af8eda99ba3685403be22a88669dcb35 (Term Term Term) Term)






;;;;;;;;;;;;;;;;_1: a -> _2: b _1 -> _3: c _1 _2 -> _4: d _1 _2 _3 -> FStar.Pervasives.dtuple4 a b c d
(declare-fun Tm_arrow_cef44a6056754f192c2446237c4c1408 () Term)

; <Start encoding FStar.Pervasives.dtuple4>


; <start constructor FStar.Pervasives.dtuple4>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.dtuple4 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
571)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= __@x0
(FStar.Pervasives.dtuple4 @x0
@x1
@x2
@x3))
 
;;no pats
:qid is-FStar.Pervasives.dtuple4))))

; </end constructor FStar.Pervasives.dtuple4>


; </end encoding FStar.Pervasives.dtuple4>


; <Start encoding FStar.Pervasives.Mkdtuple4>


; <start constructor FStar.Pervasives.Mkdtuple4>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Mkdtuple4 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
584)
(= __@x0
(FStar.Pervasives.Mkdtuple4 (FStar.Pervasives.Mkdtuple4_a __@x0)
(FStar.Pervasives.Mkdtuple4_b __@x0)
(FStar.Pervasives.Mkdtuple4_c __@x0)
(FStar.Pervasives.Mkdtuple4_d __@x0)
(FStar.Pervasives.Mkdtuple4__1 __@x0)
(FStar.Pervasives.Mkdtuple4__2 __@x0)
(FStar.Pervasives.Mkdtuple4__3 __@x0)
(FStar.Pervasives.Mkdtuple4__4 __@x0)))))

; </end constructor FStar.Pervasives.Mkdtuple4>


; </end encoding FStar.Pervasives.Mkdtuple4>


; </end encoding >


; <Start encoding FStar.Pervasives.dtuple4__uu___haseq>





; </end encoding FStar.Pervasives.dtuple4__uu___haseq>


; <Start encoding FStar.Pervasives.uu___is_Mkdtuple4>




(declare-fun FStar.Pervasives.uu___is_Mkdtuple4 (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple4 a b c d -> Prims.bool
(declare-fun Tm_arrow_76a226dc2cea2ddd4e4258637fc95e5b () Term)
(declare-fun FStar.Pervasives.uu___is_Mkdtuple4@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Mkdtuple4>


; <Skipped FStar.Pervasives.uu___is_Mkdtuple4/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple4__item___1>




(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___1 (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple4 a b c d -> a
(declare-fun Tm_arrow_1da4d60ab69f411b912e76cc25e77965 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___1@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple4__item___1>


; <Skipped FStar.Pervasives.__proj__Mkdtuple4__item___1/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple4__item___2>




(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___2 (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple4 a b c d -> b (Mkdtuple4?._1 projectee)
(declare-fun Tm_arrow_a86867091548f3d7d3ca1cb8b0458b9f () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___2@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple4__item___2>


; <Skipped FStar.Pervasives.__proj__Mkdtuple4__item___2/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple4__item___3>




(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___3 (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple4 a b c d -> c (Mkdtuple4?._1 projectee) (Mkdtuple4?._2 projectee)
(declare-fun Tm_arrow_ee72552fcc293405aa0e854ba26f27ac () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___3@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple4__item___3>


; <Skipped FStar.Pervasives.__proj__Mkdtuple4__item___3/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple4__item___4>




(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___4 (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple4 a b c d   -> d (Mkdtuple4?._1 projectee) (Mkdtuple4?._2 projectee) (Mkdtuple4?._3 projectee)
(declare-fun Tm_arrow_6c79def96aa5d5d9eb9555c48dd9ebb6 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple4__item___4@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple4__item___4>


; <Skipped FStar.Pervasives.__proj__Mkdtuple4__item___4/>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.dtuple5 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple5@x0 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple5@x1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple5@x2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple5@x3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.dtuple5@x4 (Term) Term)
;;;;;;;;;;;;;;;;token
(declare-fun FStar.Pervasives.dtuple5@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Mkdtuple5 (Term Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5_a (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5_b (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5_c (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5_d (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5_e (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5__1 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5__2 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5__3 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5__4 (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Mkdtuple5__5 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Mkdtuple5
(declare-fun FStar.Pervasives.Mkdtuple5@tok () Term)



;;;;;;;;;;;;;;;;x: a -> y: b x -> z: c x y -> w: d x y z -> Prims.GTot Type
(declare-fun Tm_arrow_e2051b23ee191036cd2c8f08b57577cc (Term Term Term Term) Term)








;;;;;;;;;;;;;;;;_1: a -> _2: b _1 -> _3: c _1 _2 -> _4: d _1 _2 _3 -> _5: e _1 _2 _3 _4   -> FStar.Pervasives.dtuple5 a b c d e
(declare-fun Tm_arrow_7c47a0b67fa3d6e69e51a1ade2982e74 () Term)

; <Start encoding FStar.Pervasives.dtuple5>


; <start constructor FStar.Pervasives.dtuple5>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.dtuple5 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
678)
(exists ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= __@x0
(FStar.Pervasives.dtuple5 @x0
@x1
@x2
@x3
@x4))
 
;;no pats
:qid is-FStar.Pervasives.dtuple5))))

; </end constructor FStar.Pervasives.dtuple5>


; </end encoding FStar.Pervasives.dtuple5>


; <Start encoding FStar.Pervasives.Mkdtuple5>


; <start constructor FStar.Pervasives.Mkdtuple5>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Mkdtuple5 ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
693)
(= __@x0
(FStar.Pervasives.Mkdtuple5 (FStar.Pervasives.Mkdtuple5_a __@x0)
(FStar.Pervasives.Mkdtuple5_b __@x0)
(FStar.Pervasives.Mkdtuple5_c __@x0)
(FStar.Pervasives.Mkdtuple5_d __@x0)
(FStar.Pervasives.Mkdtuple5_e __@x0)
(FStar.Pervasives.Mkdtuple5__1 __@x0)
(FStar.Pervasives.Mkdtuple5__2 __@x0)
(FStar.Pervasives.Mkdtuple5__3 __@x0)
(FStar.Pervasives.Mkdtuple5__4 __@x0)
(FStar.Pervasives.Mkdtuple5__5 __@x0)))))

; </end constructor FStar.Pervasives.Mkdtuple5>


; </end encoding FStar.Pervasives.Mkdtuple5>


; </end encoding >


; <Start encoding FStar.Pervasives.dtuple5__uu___haseq>






; </end encoding FStar.Pervasives.dtuple5__uu___haseq>


; <Start encoding FStar.Pervasives.uu___is_Mkdtuple5>





(declare-fun FStar.Pervasives.uu___is_Mkdtuple5 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e -> Prims.bool
(declare-fun Tm_arrow_790317d9d2afaf2417875fd8f65cee9f () Term)
(declare-fun FStar.Pervasives.uu___is_Mkdtuple5@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Mkdtuple5>


; <Skipped FStar.Pervasives.uu___is_Mkdtuple5/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple5__item___1>





(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___1 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e -> a
(declare-fun Tm_arrow_855676d3a54fc2cdf0dfa3ac2f15fdad () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___1@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple5__item___1>


; <Skipped FStar.Pervasives.__proj__Mkdtuple5__item___1/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple5__item___2>





(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___2 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e -> b (Mkdtuple5?._1 projectee)
(declare-fun Tm_arrow_3c181aa1af161d84af20412908ff5981 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___2@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple5__item___2>


; <Skipped FStar.Pervasives.__proj__Mkdtuple5__item___2/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple5__item___3>





(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___3 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e   -> c (Mkdtuple5?._1 projectee) (Mkdtuple5?._2 projectee)
(declare-fun Tm_arrow_cc152ea4c314cfd6854de94e70041031 () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___3@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple5__item___3>


; <Skipped FStar.Pervasives.__proj__Mkdtuple5__item___3/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple5__item___4>





(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___4 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e   -> d (Mkdtuple5?._1 projectee) (Mkdtuple5?._2 projectee) (Mkdtuple5?._3 projectee)
(declare-fun Tm_arrow_6a05465acf4f8d1b8f43fd30077a772a () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___4@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple5__item___4>


; <Skipped FStar.Pervasives.__proj__Mkdtuple5__item___4/>


; <Start encoding FStar.Pervasives.__proj__Mkdtuple5__item___5>





(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___5 (Term Term Term Term Term Term) Term)




;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.dtuple5 a b c d e   -> e (Mkdtuple5?._1 projectee)       (Mkdtuple5?._2 projectee)       (Mkdtuple5?._3 projectee)       (Mkdtuple5?._4 projectee)
(declare-fun Tm_arrow_d5d61b48d54646c5c411627b3a20c98f () Term)
(declare-fun FStar.Pervasives.__proj__Mkdtuple5__item___5@tok () Term)

; </end encoding FStar.Pervasives.__proj__Mkdtuple5__item___5>


; <Skipped FStar.Pervasives.__proj__Mkdtuple5__item___5/>


; <Start encoding FStar.Pervasives.ignore>

(declare-fun FStar.Pervasives.ignore (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> Prims.unit
(declare-fun Tm_arrow_962476a7eea46a6ffc9b658c6d8fbc71 () Term)
(declare-fun FStar.Pervasives.ignore@tok () Term)

; </end encoding FStar.Pervasives.ignore>


; <Start encoding FStar.Pervasives.false_elim>

(declare-fun Tm_refine_f1ecc6ab6882a651504f328937700647 () Term)
(declare-fun FStar.Pervasives.false_elim (Term Term) Term)

;;;;;;;;;;;;;;;;u55780: u55781: Prims.unit{Prims.l_False} -> a
(declare-fun Tm_arrow_7636fbfab5cd88ba06f60c10ea8caef2 () Term)
(declare-fun FStar.Pervasives.false_elim@tok () Term)

; </end encoding FStar.Pervasives.false_elim>


; <Start encoding >

;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.__internal_ocaml_attributes () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.PpxDerivingShow () Term)
;;;;;;;;;;;;;;;;data constructor proxy: PpxDerivingShow
(declare-fun FStar.Pervasives.PpxDerivingShow@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.PpxDerivingShowConstant (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.PpxDerivingShowConstant__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: PpxDerivingShowConstant
(declare-fun FStar.Pervasives.PpxDerivingShowConstant@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.PpxDerivingYoJson () Term)
;;;;;;;;;;;;;;;;data constructor proxy: PpxDerivingYoJson
(declare-fun FStar.Pervasives.PpxDerivingYoJson@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CInline () Term)
;;;;;;;;;;;;;;;;data constructor proxy: CInline
(declare-fun FStar.Pervasives.CInline@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Substitute () Term)
;;;;;;;;;;;;;;;;data constructor proxy: Substitute
(declare-fun FStar.Pervasives.Substitute@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Gc () Term)
;;;;;;;;;;;;;;;;data constructor proxy: Gc
(declare-fun FStar.Pervasives.Gc@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.Comment (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.Comment__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: Comment
(declare-fun FStar.Pervasives.Comment@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CPrologue (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.CPrologue__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: CPrologue
(declare-fun FStar.Pervasives.CPrologue@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CEpilogue (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.CEpilogue__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: CEpilogue
(declare-fun FStar.Pervasives.CEpilogue@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CConst (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.CConst__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: CConst
(declare-fun FStar.Pervasives.CConst@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CCConv (Term) Term)
;;;;;;;;;;;;;;;;Projector
(declare-fun FStar.Pervasives.CCConv__0 (Term) Term)
;;;;;;;;;;;;;;;;data constructor proxy: CCConv
(declare-fun FStar.Pervasives.CCConv@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CAbstractStruct () Term)
;;;;;;;;;;;;;;;;data constructor proxy: CAbstractStruct
(declare-fun FStar.Pervasives.CAbstractStruct@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CIfDef () Term)
;;;;;;;;;;;;;;;;data constructor proxy: CIfDef
(declare-fun FStar.Pervasives.CIfDef@tok () Term)
;;;;;;;;;;;;;;;;Constructor
(declare-fun FStar.Pervasives.CMacro () Term)
;;;;;;;;;;;;;;;;data constructor proxy: CMacro
(declare-fun FStar.Pervasives.CMacro@tok () Term)
;;;;;;;;;;;;;;;;_0: Prims.string -> FStar.Pervasives.__internal_ocaml_attributes
(declare-fun Tm_arrow_a25c6dbdd7c43412e925069991c0ef48 () Term)






; <Start encoding FStar.Pervasives.__internal_ocaml_attributes>


; <start constructor FStar.Pervasives.__internal_ocaml_attributes>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.__internal_ocaml_attributes ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
827)
(= __@x0
FStar.Pervasives.__internal_ocaml_attributes)))

; </end constructor FStar.Pervasives.__internal_ocaml_attributes>


; </end encoding FStar.Pervasives.__internal_ocaml_attributes>


; <Start encoding FStar.Pervasives.PpxDerivingShow>


; <start constructor FStar.Pervasives.PpxDerivingShow>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.PpxDerivingShow ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
833)
(= __@x0
FStar.Pervasives.PpxDerivingShow)))

; </end constructor FStar.Pervasives.PpxDerivingShow>


; </end encoding FStar.Pervasives.PpxDerivingShow>


; <Start encoding FStar.Pervasives.PpxDerivingShowConstant>


; <start constructor FStar.Pervasives.PpxDerivingShowConstant>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.PpxDerivingShowConstant ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
835)
(= __@x0
(FStar.Pervasives.PpxDerivingShowConstant (FStar.Pervasives.PpxDerivingShowConstant__0 __@x0)))))

; </end constructor FStar.Pervasives.PpxDerivingShowConstant>


; </end encoding FStar.Pervasives.PpxDerivingShowConstant>


; <Start encoding FStar.Pervasives.PpxDerivingYoJson>


; <start constructor FStar.Pervasives.PpxDerivingYoJson>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.PpxDerivingYoJson ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
840)
(= __@x0
FStar.Pervasives.PpxDerivingYoJson)))

; </end constructor FStar.Pervasives.PpxDerivingYoJson>


; </end encoding FStar.Pervasives.PpxDerivingYoJson>


; <Start encoding FStar.Pervasives.CInline>


; <start constructor FStar.Pervasives.CInline>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CInline ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
842)
(= __@x0
FStar.Pervasives.CInline)))

; </end constructor FStar.Pervasives.CInline>


; </end encoding FStar.Pervasives.CInline>


; <Start encoding FStar.Pervasives.Substitute>


; <start constructor FStar.Pervasives.Substitute>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Substitute ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
844)
(= __@x0
FStar.Pervasives.Substitute)))

; </end constructor FStar.Pervasives.Substitute>


; </end encoding FStar.Pervasives.Substitute>


; <Start encoding FStar.Pervasives.Gc>


; <start constructor FStar.Pervasives.Gc>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Gc ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
846)
(= __@x0
FStar.Pervasives.Gc)))

; </end constructor FStar.Pervasives.Gc>


; </end encoding FStar.Pervasives.Gc>


; <Start encoding FStar.Pervasives.Comment>


; <start constructor FStar.Pervasives.Comment>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.Comment ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
848)
(= __@x0
(FStar.Pervasives.Comment (FStar.Pervasives.Comment__0 __@x0)))))

; </end constructor FStar.Pervasives.Comment>


; </end encoding FStar.Pervasives.Comment>


; <Start encoding FStar.Pervasives.CPrologue>


; <start constructor FStar.Pervasives.CPrologue>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CPrologue ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
853)
(= __@x0
(FStar.Pervasives.CPrologue (FStar.Pervasives.CPrologue__0 __@x0)))))

; </end constructor FStar.Pervasives.CPrologue>


; </end encoding FStar.Pervasives.CPrologue>


; <Start encoding FStar.Pervasives.CEpilogue>


; <start constructor FStar.Pervasives.CEpilogue>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CEpilogue ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
858)
(= __@x0
(FStar.Pervasives.CEpilogue (FStar.Pervasives.CEpilogue__0 __@x0)))))

; </end constructor FStar.Pervasives.CEpilogue>


; </end encoding FStar.Pervasives.CEpilogue>


; <Start encoding FStar.Pervasives.CConst>


; <start constructor FStar.Pervasives.CConst>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CConst ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
863)
(= __@x0
(FStar.Pervasives.CConst (FStar.Pervasives.CConst__0 __@x0)))))

; </end constructor FStar.Pervasives.CConst>


; </end encoding FStar.Pervasives.CConst>


; <Start encoding FStar.Pervasives.CCConv>


; <start constructor FStar.Pervasives.CCConv>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CCConv ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
868)
(= __@x0
(FStar.Pervasives.CCConv (FStar.Pervasives.CCConv__0 __@x0)))))

; </end constructor FStar.Pervasives.CCConv>


; </end encoding FStar.Pervasives.CCConv>


; <Start encoding FStar.Pervasives.CAbstractStruct>


; <start constructor FStar.Pervasives.CAbstractStruct>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CAbstractStruct ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
873)
(= __@x0
FStar.Pervasives.CAbstractStruct)))

; </end constructor FStar.Pervasives.CAbstractStruct>


; </end encoding FStar.Pervasives.CAbstractStruct>


; <Start encoding FStar.Pervasives.CIfDef>


; <start constructor FStar.Pervasives.CIfDef>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CIfDef ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
875)
(= __@x0
FStar.Pervasives.CIfDef)))

; </end constructor FStar.Pervasives.CIfDef>


; </end encoding FStar.Pervasives.CIfDef>


; <Start encoding FStar.Pervasives.CMacro>


; <start constructor FStar.Pervasives.CMacro>

;;;;;;;;;;;;;;;;Discriminator definition
(define-fun is-FStar.Pervasives.CMacro ((__@x0 Term)) Bool
 (and (= (Term_constr_id __@x0)
877)
(= __@x0
FStar.Pervasives.CMacro)))

; </end constructor FStar.Pervasives.CMacro>


; </end encoding FStar.Pervasives.CMacro>


; </end encoding >


; <Start encoding FStar.Pervasives.__internal_ocaml_attributes__uu___haseq>


; </end encoding FStar.Pervasives.__internal_ocaml_attributes__uu___haseq>


; <Start encoding FStar.Pervasives.uu___is_PpxDerivingShow>

(declare-fun FStar.Pervasives.uu___is_PpxDerivingShow (Term) Term)
;;;;;;;;;;;;;;;;projectee: FStar.Pervasives.__internal_ocaml_attributes -> Prims.bool
(declare-fun Tm_arrow_89dc0c243f5e74d4fefc48cfe123db41 () Term)
(declare-fun FStar.Pervasives.uu___is_PpxDerivingShow@tok () Term)

; </end encoding FStar.Pervasives.uu___is_PpxDerivingShow>


; <Skipped FStar.Pervasives.uu___is_PpxDerivingShow/>


; <Start encoding FStar.Pervasives.uu___is_PpxDerivingShowConstant>

(declare-fun FStar.Pervasives.uu___is_PpxDerivingShowConstant (Term) Term)

(declare-fun FStar.Pervasives.uu___is_PpxDerivingShowConstant@tok () Term)

; </end encoding FStar.Pervasives.uu___is_PpxDerivingShowConstant>


; <Skipped FStar.Pervasives.uu___is_PpxDerivingShowConstant/>


; <Start encoding FStar.Pervasives.__proj__PpxDerivingShowConstant__item___0>

(declare-fun Tm_refine_564db2f0aa0878b4d96c60508be3dd36 () Term)
(declare-fun FStar.Pervasives.__proj__PpxDerivingShowConstant__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {PpxDerivingShowConstant? _}   -> Prims.string
(declare-fun Tm_arrow_dbb84ef8131159481071b6d6a41b7f31 () Term)
(declare-fun FStar.Pervasives.__proj__PpxDerivingShowConstant__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__PpxDerivingShowConstant__item___0>


; <Skipped FStar.Pervasives.__proj__PpxDerivingShowConstant__item___0/>


; <Start encoding FStar.Pervasives.uu___is_PpxDerivingYoJson>

(declare-fun FStar.Pervasives.uu___is_PpxDerivingYoJson (Term) Term)

(declare-fun FStar.Pervasives.uu___is_PpxDerivingYoJson@tok () Term)

; </end encoding FStar.Pervasives.uu___is_PpxDerivingYoJson>


; <Skipped FStar.Pervasives.uu___is_PpxDerivingYoJson/>


; <Start encoding FStar.Pervasives.uu___is_CInline>

(declare-fun FStar.Pervasives.uu___is_CInline (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CInline@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CInline>


; <Skipped FStar.Pervasives.uu___is_CInline/>


; <Start encoding FStar.Pervasives.uu___is_Substitute>

(declare-fun FStar.Pervasives.uu___is_Substitute (Term) Term)

(declare-fun FStar.Pervasives.uu___is_Substitute@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Substitute>


; <Skipped FStar.Pervasives.uu___is_Substitute/>


; <Start encoding FStar.Pervasives.uu___is_Gc>

(declare-fun FStar.Pervasives.uu___is_Gc (Term) Term)

(declare-fun FStar.Pervasives.uu___is_Gc@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Gc>


; <Skipped FStar.Pervasives.uu___is_Gc/>


; <Start encoding FStar.Pervasives.uu___is_Comment>

(declare-fun FStar.Pervasives.uu___is_Comment (Term) Term)

(declare-fun FStar.Pervasives.uu___is_Comment@tok () Term)

; </end encoding FStar.Pervasives.uu___is_Comment>


; <Skipped FStar.Pervasives.uu___is_Comment/>


; <Start encoding FStar.Pervasives.__proj__Comment__item___0>

(declare-fun Tm_refine_c53089e2d20d1b0f5a267296ac8e45f0 () Term)
(declare-fun FStar.Pervasives.__proj__Comment__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {Comment? _} -> Prims.string
(declare-fun Tm_arrow_d4c2bbf4fb852b3f4b9961c7cbc2f3a2 () Term)
(declare-fun FStar.Pervasives.__proj__Comment__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__Comment__item___0>


; <Skipped FStar.Pervasives.__proj__Comment__item___0/>


; <Start encoding FStar.Pervasives.uu___is_CPrologue>

(declare-fun FStar.Pervasives.uu___is_CPrologue (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CPrologue@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CPrologue>


; <Skipped FStar.Pervasives.uu___is_CPrologue/>


; <Start encoding FStar.Pervasives.__proj__CPrologue__item___0>

(declare-fun Tm_refine_ac46c1a2a06ce46a180e0eda48004c47 () Term)
(declare-fun FStar.Pervasives.__proj__CPrologue__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {CPrologue? _} -> Prims.string
(declare-fun Tm_arrow_929b9daa0a2a2e99e3571b146c52feaf () Term)
(declare-fun FStar.Pervasives.__proj__CPrologue__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__CPrologue__item___0>


; <Skipped FStar.Pervasives.__proj__CPrologue__item___0/>


; <Start encoding FStar.Pervasives.uu___is_CEpilogue>

(declare-fun FStar.Pervasives.uu___is_CEpilogue (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CEpilogue@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CEpilogue>


; <Skipped FStar.Pervasives.uu___is_CEpilogue/>


; <Start encoding FStar.Pervasives.__proj__CEpilogue__item___0>

(declare-fun Tm_refine_47384bef739d1f0729fd782d351dc9a5 () Term)
(declare-fun FStar.Pervasives.__proj__CEpilogue__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {CEpilogue? _} -> Prims.string
(declare-fun Tm_arrow_e37361b66babb46a30183ad1ff072689 () Term)
(declare-fun FStar.Pervasives.__proj__CEpilogue__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__CEpilogue__item___0>


; <Skipped FStar.Pervasives.__proj__CEpilogue__item___0/>


; <Start encoding FStar.Pervasives.uu___is_CConst>

(declare-fun FStar.Pervasives.uu___is_CConst (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CConst@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CConst>


; <Skipped FStar.Pervasives.uu___is_CConst/>


; <Start encoding FStar.Pervasives.__proj__CConst__item___0>

(declare-fun Tm_refine_5036c6b2983454bc3afeffcba3f00f50 () Term)
(declare-fun FStar.Pervasives.__proj__CConst__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {CConst? _} -> Prims.string
(declare-fun Tm_arrow_2d0b7639551b88b0df758d7b36c8f77a () Term)
(declare-fun FStar.Pervasives.__proj__CConst__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__CConst__item___0>


; <Skipped FStar.Pervasives.__proj__CConst__item___0/>


; <Start encoding FStar.Pervasives.uu___is_CCConv>

(declare-fun FStar.Pervasives.uu___is_CCConv (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CCConv@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CCConv>


; <Skipped FStar.Pervasives.uu___is_CCConv/>


; <Start encoding FStar.Pervasives.__proj__CCConv__item___0>

(declare-fun Tm_refine_2c4510f48649a66c3dca1fc9e3a2d320 () Term)
(declare-fun FStar.Pervasives.__proj__CCConv__item___0 (Term) Term)

;;;;;;;;;;;;;;;;projectee: _: FStar.Pervasives.__internal_ocaml_attributes {CCConv? _} -> Prims.string
(declare-fun Tm_arrow_b7e884ec94708f2b05c42d4d8834eac6 () Term)
(declare-fun FStar.Pervasives.__proj__CCConv__item___0@tok () Term)

; </end encoding FStar.Pervasives.__proj__CCConv__item___0>


; <Skipped FStar.Pervasives.__proj__CCConv__item___0/>


; <Start encoding FStar.Pervasives.uu___is_CAbstractStruct>

(declare-fun FStar.Pervasives.uu___is_CAbstractStruct (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CAbstractStruct@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CAbstractStruct>


; <Skipped FStar.Pervasives.uu___is_CAbstractStruct/>


; <Start encoding FStar.Pervasives.uu___is_CIfDef>

(declare-fun FStar.Pervasives.uu___is_CIfDef (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CIfDef@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CIfDef>


; <Skipped FStar.Pervasives.uu___is_CIfDef/>


; <Start encoding FStar.Pervasives.uu___is_CMacro>

(declare-fun FStar.Pervasives.uu___is_CMacro (Term) Term)

(declare-fun FStar.Pervasives.uu___is_CMacro@tok () Term)

; </end encoding FStar.Pervasives.uu___is_CMacro>


; <Skipped FStar.Pervasives.uu___is_CMacro/>


; <Start encoding FStar.Pervasives.inline_let>

(declare-fun FStar.Pervasives.inline_let (Dummy_sort) Term)

; </end encoding FStar.Pervasives.inline_let>


; <Start encoding FStar.Pervasives.rename_let>

(declare-fun FStar.Pervasives.rename_let (Term) Term)

(declare-fun FStar.Pervasives.rename_let@tok () Term)

; </end encoding FStar.Pervasives.rename_let>


; <Start encoding FStar.Pervasives.plugin>

(declare-fun FStar.Pervasives.plugin (Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> Prims.unit
(declare-fun Tm_arrow_f12575a0ee171a8be16a63e3359708f8 () Term)
(declare-fun FStar.Pervasives.plugin@tok () Term)

; </end encoding FStar.Pervasives.plugin>


; <Start encoding FStar.Pervasives.tcnorm>

(declare-fun FStar.Pervasives.tcnorm (Dummy_sort) Term)

; </end encoding FStar.Pervasives.tcnorm>


; <Start encoding FStar.Pervasives.must_erase_for_extraction>

(declare-fun FStar.Pervasives.must_erase_for_extraction (Dummy_sort) Term)

; </end encoding FStar.Pervasives.must_erase_for_extraction>


; <Start encoding FStar.Pervasives.dm4f_bind_range>

(declare-fun FStar.Pervasives.dm4f_bind_range (Dummy_sort) Term)

; </end encoding FStar.Pervasives.dm4f_bind_range>


; <Start encoding FStar.Pervasives.expect_failure>

(declare-fun FStar.Pervasives.expect_failure (Term) Term)

(declare-fun FStar.Pervasives.expect_failure@tok () Term)

; </end encoding FStar.Pervasives.expect_failure>


; <Start encoding FStar.Pervasives.expect_lax_failure>

(declare-fun FStar.Pervasives.expect_lax_failure (Term) Term)

(declare-fun FStar.Pervasives.expect_lax_failure@tok () Term)

; </end encoding FStar.Pervasives.expect_lax_failure>


; <Start encoding FStar.Pervasives.tcdecltime>

(declare-fun FStar.Pervasives.tcdecltime (Dummy_sort) Term)

; </end encoding FStar.Pervasives.tcdecltime>


; <Start encoding FStar.Pervasives.unifier_hint_injective>

(declare-fun FStar.Pervasives.unifier_hint_injective (Dummy_sort) Term)

; </end encoding FStar.Pervasives.unifier_hint_injective>


; <Start encoding FStar.Pervasives.strict_on_arguments>

(declare-fun FStar.Pervasives.strict_on_arguments (Term) Term)

(declare-fun FStar.Pervasives.strict_on_arguments@tok () Term)

; </end encoding FStar.Pervasives.strict_on_arguments>


; <Start encoding FStar.Pervasives.resolve_implicits>

(declare-fun FStar.Pervasives.resolve_implicits (Dummy_sort) Term)

; </end encoding FStar.Pervasives.resolve_implicits>


; <Start encoding FStar.Pervasives.override_resolve_implicits_handler>

(declare-fun FStar.Pervasives.override_resolve_implicits_handler (Term Term Term) Term)
;;;;;;;;;;;;;;;;_: a -> _: Prims.list Prims.string -> Prims.unit
(declare-fun Tm_arrow_93e6548cfc250f7cc25301579d62a018 () Term)
(declare-fun FStar.Pervasives.override_resolve_implicits_handler@tok () Term)

; </end encoding FStar.Pervasives.override_resolve_implicits_handler>


; <Start encoding FStar.Pervasives.handle_smt_goals>

(declare-fun FStar.Pervasives.handle_smt_goals (Dummy_sort) Term)

; </end encoding FStar.Pervasives.handle_smt_goals>


; <Start encoding FStar.Pervasives.erasable>

(declare-fun FStar.Pervasives.erasable (Dummy_sort) Term)

; </end encoding FStar.Pervasives.erasable>


; <Start encoding FStar.Pervasives.commute_nested_matches>

(declare-fun FStar.Pervasives.commute_nested_matches (Dummy_sort) Term)

; </end encoding FStar.Pervasives.commute_nested_matches>


; <Start encoding FStar.Pervasives.noextract_to>

(declare-fun FStar.Pervasives.noextract_to (Term) Term)

(declare-fun FStar.Pervasives.noextract_to@tok () Term)

; </end encoding FStar.Pervasives.noextract_to>


; <Start encoding FStar.Pervasives.normalize_for_extraction>

(declare-fun FStar.Pervasives.normalize_for_extraction (Term) Term)
;;;;;;;;;;;;;;;;steps: Prims.list FStar.Pervasives.norm_step -> Prims.unit
(declare-fun Tm_arrow_5a371649389ebd695db0478470787cef () Term)
(declare-fun FStar.Pervasives.normalize_for_extraction@tok () Term)

; </end encoding FStar.Pervasives.normalize_for_extraction>


; <Start encoding FStar.Pervasives.ite_soundness_by>

(declare-fun FStar.Pervasives.ite_soundness_by (Term) Term)
;;;;;;;;;;;;;;;;attribute: Prims.unit -> Prims.unit
(declare-fun Tm_arrow_0c55530a575bf8e94add46ffb548393c () Term)
(declare-fun FStar.Pervasives.ite_soundness_by@tok () Term)

; </end encoding FStar.Pervasives.ite_soundness_by>


; <Start encoding FStar.Pervasives.default_effect>

(declare-fun FStar.Pervasives.default_effect (Term) Term)

(declare-fun FStar.Pervasives.default_effect@tok () Term)

; </end encoding FStar.Pervasives.default_effect>


; <Start encoding FStar.Pervasives.top_level_effect>

(declare-fun FStar.Pervasives.top_level_effect (Term) Term)

(declare-fun FStar.Pervasives.top_level_effect@tok () Term)

; </end encoding FStar.Pervasives.top_level_effect>


; <Start encoding FStar.Pervasives.effect_param>

(declare-fun FStar.Pervasives.effect_param (Dummy_sort) Term)

; </end encoding FStar.Pervasives.effect_param>


; <Start encoding FStar.Pervasives.bind_has_range_args>

(declare-fun FStar.Pervasives.bind_has_range_args (Dummy_sort) Term)

; </end encoding FStar.Pervasives.bind_has_range_args>


; <Start encoding FStar.Pervasives.primitive_extraction>

(declare-fun FStar.Pervasives.primitive_extraction (Dummy_sort) Term)

; </end encoding FStar.Pervasives.primitive_extraction>


; <Start encoding FStar.Pervasives.strictly_positive>

(declare-fun FStar.Pervasives.strictly_positive (Dummy_sort) Term)

; </end encoding FStar.Pervasives.strictly_positive>


; <Start encoding FStar.Pervasives.unused>

(declare-fun FStar.Pervasives.unused (Dummy_sort) Term)

; </end encoding FStar.Pervasives.unused>


; <Start encoding FStar.Pervasives.no_auto_projectors>

(declare-fun FStar.Pervasives.no_auto_projectors (Dummy_sort) Term)

; </end encoding FStar.Pervasives.no_auto_projectors>


; <Start encoding FStar.Pervasives.no_subtyping>

(declare-fun FStar.Pervasives.no_subtyping (Dummy_sort) Term)

; </end encoding FStar.Pervasives.no_subtyping>


; <Start encoding FStar.Pervasives.admit_termination>

(declare-fun FStar.Pervasives.admit_termination (Dummy_sort) Term)

; </end encoding FStar.Pervasives.admit_termination>


; <Start encoding FStar.Pervasives.singleton>

(declare-fun FStar.Pervasives.singleton (Term Term) Term)
(declare-fun Tm_refine_2fbd657fe85bcb2423f9c7e5f9b3bcb5 (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> y: a{y == x}
(declare-fun Tm_arrow_9cdb4ebd85da757e86217b6fb07ef9fc () Term)
(declare-fun FStar.Pervasives.singleton@tok () Term)


; </end encoding FStar.Pervasives.singleton>


; <Start encoding FStar.Pervasives.eqtype_as_type>

(declare-fun FStar.Pervasives.eqtype_as_type (Term) Term)
;;;;;;;;;;;;;;;;a: Prims.eqtype -> Type
(declare-fun Tm_arrow_7e9afc6da5407011473323ad80ff51bf () Term)
(declare-fun FStar.Pervasives.eqtype_as_type@tok () Term)

; </end encoding FStar.Pervasives.eqtype_as_type>


; <Start encoding FStar.Pervasives.coerce_eq>

(declare-fun Tm_refine_0dee8cb03258a67c2f7ec66427696212 (Term Term) Term)
(declare-fun FStar.Pervasives.coerce_eq (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;_: Prims.squash (a == b) -> x: a -> b
(declare-fun Tm_arrow_f44173fb14fa6046eedce5f32cb8aae8 () Term)
(declare-fun FStar.Pervasives.coerce_eq@tok () Term)


; </end encoding FStar.Pervasives.coerce_eq>


; <Start encoding FStar.Pervasives.coercion>

(declare-fun FStar.Pervasives.coercion (Dummy_sort) Term)

; </end encoding FStar.Pervasives.coercion>


; End Externals for interface FStar.Pervasives


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Pervasives (1419 decls; total size 84657)

;;; Start interface FStar.Sealed

; Externals for interface FStar.Sealed


; <Start encoding FStar.Sealed.sealed>

(declare-fun FStar.Sealed.sealed (Term) Term)

(declare-fun FStar.Sealed.sealed@tok () Term)

; </end encoding FStar.Sealed.sealed>


; <Start encoding FStar.Sealed.sealed_singl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Sealed.sealed_singl (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Sealed.sealed_singl@tok () Term)

; </end encoding FStar.Sealed.sealed_singl>


; <Start encoding FStar.Sealed.seal>

(declare-fun FStar.Sealed.seal (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> FStar.Sealed.sealed a
(declare-fun Tm_arrow_202c0385508095b22536f7b176783182 () Term)
(declare-fun FStar.Sealed.seal@tok () Term)

; </end encoding FStar.Sealed.seal>


; End Externals for interface FStar.Sealed


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Sealed (17 decls; total size 1862)

;;; Start interface FStar.Range

; Externals for interface FStar.Range


; <Start encoding FStar.Range.__range>

(declare-fun FStar.Range.__range () Term)

; </end encoding FStar.Range.__range>


; <Start encoding FStar.Range.range>

(declare-fun FStar.Range.range () Term)

; </end encoding FStar.Range.range>


; <Start encoding FStar.Range.range_0>

(declare-fun FStar.Range.range_0 (Dummy_sort) Term)

; </end encoding FStar.Range.range_0>


; <Start encoding FStar.Range.mk_range>

(declare-fun FStar.Range.mk_range (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;file: Prims.string ->     from_line: Prims.int ->     from_col: Prims.int ->     to_line: Prims.int ->     to_col: Prims.int   -> FStar.Range.range
(declare-fun Tm_arrow_2d3d853349c0a35c0e0c8d13086384ea () Term)
(declare-fun FStar.Range.mk_range@tok () Term)

; </end encoding FStar.Range.mk_range>


; <Start encoding FStar.Range.labeled>

(declare-fun FStar.Range.labeled (Term Term Term) Term)
;;;;;;;;;;;;;;;;r: FStar.Range.range -> msg: Prims.string -> b: Type -> Type
(declare-fun Tm_arrow_ab5899212fc4c148181589be7fd78af1 () Term)
(declare-fun FStar.Range.labeled@tok () Term)

; </end encoding FStar.Range.labeled>


; End Externals for interface FStar.Range


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Range (22 decls; total size 2200)

;;; Start module FStar.Preorder

; Externals for module FStar.Preorder


; <Start encoding FStar.Preorder.relation>

(declare-fun FStar.Preorder.relation (Term) Term)

(declare-fun FStar.Preorder.relation@tok () Term)
;;;;;;;;;;;;;;;;_: a -> _: a -> Type
(declare-fun Tm_arrow_a19f9d49348d4e0038f0ded87d87802f (Term) Term)

; </end encoding FStar.Preorder.relation>


; <Start encoding FStar.Preorder.predicate>

(declare-fun FStar.Preorder.predicate (Term) Term)

(declare-fun FStar.Preorder.predicate@tok () Term)


; </end encoding FStar.Preorder.predicate>


; <Start encoding FStar.Preorder.reflexive>

(declare-fun FStar.Preorder.reflexive (Term Term) Term)
;;;;;;;;;;;;;;;;rel: FStar.Preorder.relation a -> Prims.logical
(declare-fun Tm_arrow_8e677a33afbeb812aa3779b7bdd0131c () Term)
(declare-fun FStar.Preorder.reflexive@tok () Term)

(declare-fun Tm_abs_041ca30bb30651295f49214aabfc20e1 (Term Term) Term)

; </end encoding FStar.Preorder.reflexive>


; <Start encoding FStar.Preorder.transitive>

(declare-fun FStar.Preorder.transitive (Term Term) Term)

(declare-fun FStar.Preorder.transitive@tok () Term)

(declare-fun Tm_abs_525ac99f317c80c2a25889a544a8753a (Term Term Term Term) Term)

(declare-fun Tm_abs_20d4094a6595eea2d6f8dba526584b13 (Term Term Term) Term)

(declare-fun Tm_abs_fe51396746e5c4e834cef57991c4de56 (Term Term) Term)

; </end encoding FStar.Preorder.transitive>


; <Start encoding FStar.Preorder.preorder_rel>

(declare-fun FStar.Preorder.preorder_rel (Term Term) Term)

(declare-fun FStar.Preorder.preorder_rel@tok () Term)

; </end encoding FStar.Preorder.preorder_rel>


; <Start encoding FStar.Preorder.preorder>

(declare-fun FStar.Preorder.preorder (Term) Term)

(declare-fun FStar.Preorder.preorder@tok () Term)
(declare-fun Tm_refine_bd10f09297e0e7dc08314f7d9211801c (Term) Term)

; </end encoding FStar.Preorder.preorder>


; <Start encoding FStar.Preorder.stable>


(declare-fun FStar.Preorder.stable (Term Term Term) Term)

;;;;;;;;;;;;;;;;p: FStar.Preorder.predicate a -> rel: FStar.Preorder.relation a {FStar.Preorder.preorder_rel rel}   -> Prims.logical
(declare-fun Tm_arrow_88036d0811eee3361efd6229bae2556d () Term)
(declare-fun FStar.Preorder.stable@tok () Term)


(declare-fun Tm_abs_cd75e79d2823f61d656a9d90e99d6551 (Term Term Term Term) Term)

(declare-fun Tm_abs_c6795a80dc788b30d6c7864438942482 (Term Term Term) Term)

; </end encoding FStar.Preorder.stable>


; End Externals for module FStar.Preorder


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Preorder (56 decls; total size 3420)

;;; Start interface FStar.Calc

; Externals for interface FStar.Calc


; <Start encoding FStar.Calc.calc_chain>

(declare-fun FStar.Calc.calc_chain (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;rs: Prims.list (FStar.Preorder.relation a) -> x: a -> y: a -> Type
(declare-fun Tm_arrow_10ae2c328e1918eb5ddde4274ac6d32a () Term)
(declare-fun FStar.Calc.calc_chain@tok () Term)

; </end encoding FStar.Calc.calc_chain>


; <Start encoding FStar.Calc.calc_chain_related>

(declare-fun FStar.Calc.calc_chain_related (Term Term Term Term) Term)

(declare-fun FStar.Calc.calc_chain_related@tok () Term)

; </end encoding FStar.Calc.calc_chain_related>


; <Start encoding FStar.Calc.calc_chain_compatible>

(declare-fun FStar.Calc.calc_chain_compatible (Term Term Term) Term)
;;;;;;;;;;;;;;;;rs: Prims.list (FStar.Preorder.relation t) -> p: FStar.Preorder.relation t -> Type
(declare-fun Tm_arrow_5d25af94b872513cc464e94bbc6a8348 () Term)
(declare-fun FStar.Calc.calc_chain_compatible@tok () Term)

; </end encoding FStar.Calc.calc_chain_compatible>


; <Start encoding FStar.Calc.calc_pack>

(declare-fun FStar.Calc.calc_pack (Term Term Term Term) Term)

(declare-fun FStar.Calc.calc_pack@tok () Term)

; </end encoding FStar.Calc.calc_pack>


; <Start encoding FStar.Calc.calc_init>

(declare-fun FStar.Calc.calc_init (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> FStar.Calc.calc_pack [] x x
(declare-fun Tm_arrow_f5787f683bb2167ab433229a103fa97e () Term)
(declare-fun FStar.Calc.calc_init@tok () Term)

; </end encoding FStar.Calc.calc_init>


; <Start encoding FStar.Calc.calc_step>

;;;;;;;;;;;;;;;;_: Prims.unit -> FStar.Calc.calc_pack rs x y
(declare-fun Tm_arrow_cfbfcb3d713af6cfef9741902fbd7eae (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;_: Prims.unit -> Prims.squash (p y z)
(declare-fun Tm_arrow_924fe8a596e8d1052263cdb50ea0b3f9 (Term Term Term) Term)
(declare-fun FStar.Calc.calc_step (Term Term Term Term Term Term Term Term) Term)


;;;;;;;;;;;;;;;;p: FStar.Preorder.relation a ->     z: a ->     pf: (_: Prims.unit -> FStar.Calc.calc_pack rs x y) ->     j: (_: Prims.unit -> Prims.squash (p y z))   -> FStar.Calc.calc_pack (p :: rs) x z
(declare-fun Tm_arrow_c19042f2eb54c518b0ae4e8fbbb1b795 () Term)
(declare-fun FStar.Calc.calc_step@tok () Term)

; </end encoding FStar.Calc.calc_step>


; <Start encoding FStar.Calc.calc_finish>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Calc.calc_finish (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Calc.calc_finish@tok () Term)

; </end encoding FStar.Calc.calc_finish>


; <Start encoding FStar.Calc.calc_push_impl>

;;;;;;;;;;;;;;;;_: Prims.squash p -> Prims.GTot (Prims.squash q)
(declare-fun Tm_arrow_9d84457d1c8d2a3cb1cecf47a390b833 (Term Term) Term)
(declare-fun FStar.Calc.calc_push_impl (Term Term Term) Term)

(declare-fun Tm_refine_913239c2cf9dc8a14e1f047e0206138d (Term Term) Term)
;;;;;;;;;;;;;;;;f: (_: Prims.squash p -> Prims.GTot (Prims.squash q)) -> Prims.squash (p ==> q)
(declare-fun Tm_arrow_8e288ee6e01162bc0e93f91b1d4ccf81 () Term)
(declare-fun FStar.Calc.calc_push_impl@tok () Term)


; </end encoding FStar.Calc.calc_push_impl>


; End Externals for interface FStar.Calc


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Calc (50 decls; total size 4208)

;;; Start interface FStar.Classical

; Externals for interface FStar.Classical


; <Start encoding FStar.Classical.give_witness>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.give_witness (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.give_witness@tok () Term)

; </end encoding FStar.Classical.give_witness>


; <Start encoding FStar.Classical.give_witness_from_squash>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.give_witness_from_squash (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.give_witness_from_squash@tok () Term)

; </end encoding FStar.Classical.give_witness_from_squash>


; <Start encoding FStar.Classical.lemma_to_squash_gtot>


(declare-fun Tm_refine_839524df17f415c122f40f00685d3fe6 (Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> FStar.Pervasives.Lemma (ensures p x)
(declare-fun Tm_arrow_9a028cfcf6111a85dd3c28d61b4efdfd (Term Term) Term)
(declare-fun FStar.Classical.lemma_to_squash_gtot (Term Term Term Term) Term)



;;;;;;;;;;;;;;;;$_: (x: a -> FStar.Pervasives.Lemma (ensures p x)) -> x: a -> Prims.GTot (Prims.squash (p x))
(declare-fun Tm_arrow_6f9100982820dfbce0fb9c6dae0cee11 () Term)
(declare-fun FStar.Classical.lemma_to_squash_gtot@tok () Term)

; </end encoding FStar.Classical.lemma_to_squash_gtot>


; <Start encoding FStar.Classical.get_equality>

(declare-fun FStar.Classical.get_equality (Term Term Term) Term)
(declare-fun Tm_refine_7c805cbd5439f1b21f6463c70e57d0f1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: t -> b: t -> Prims.Pure (a == b)
(declare-fun Tm_arrow_158af926c0cd4bc1ff513e80f99f4b49 () Term)
(declare-fun FStar.Classical.get_equality@tok () Term)


; </end encoding FStar.Classical.get_equality>


; <Start encoding FStar.Classical.impl_to_arrow>

(declare-fun FStar.Classical.impl_to_arrow (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;_: (a ==> b) -> _: Prims.squash a -> Prims.squash b
(declare-fun Tm_arrow_156c500bdf0e99cc45ffd26a33a603a8 () Term)
(declare-fun FStar.Classical.impl_to_arrow@tok () Term)

; </end encoding FStar.Classical.impl_to_arrow>


; <Start encoding FStar.Classical.arrow_to_impl>


(declare-fun FStar.Classical.arrow_to_impl (Term Term Term) Term)

;;;;;;;;;;;;;;;;_: (_: Prims.squash a -> Prims.GTot (Prims.squash b)) -> Prims.GTot (a ==> b)
(declare-fun Tm_arrow_78d787b8a2633e2185ded4267a81cc32 () Term)
(declare-fun FStar.Classical.arrow_to_impl@tok () Term)

; </end encoding FStar.Classical.arrow_to_impl>


; <Start encoding FStar.Classical.impl_intro_gtot>


(declare-fun FStar.Classical.impl_intro_gtot (Term Term Term) Term)

;;;;;;;;;;;;;;;;$_: (_: p -> Prims.GTot q) -> Prims.GTot (p ==> q)
(declare-fun Tm_arrow_d2cdd2f18b92810e3048c35d07f1c9ea () Term)
(declare-fun FStar.Classical.impl_intro_gtot@tok () Term)

; </end encoding FStar.Classical.impl_intro_gtot>


; <Start encoding FStar.Classical.impl_intro_tot>

;;;;;;;;;;;;;;;;_: p -> q
(declare-fun Tm_arrow_6980332764c4493a7b0df5c02f7aefbe (Term Term) Term)
(declare-fun FStar.Classical.impl_intro_tot (Term Term Term) Term)

;;;;;;;;;;;;;;;;$_: (_: p -> q) -> (p ==> q)
(declare-fun Tm_arrow_91aadb43edc4c2c0c091af8dbe745057 () Term)
(declare-fun FStar.Classical.impl_intro_tot@tok () Term)

; </end encoding FStar.Classical.impl_intro_tot>


; <Start encoding FStar.Classical.impl_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.impl_intro (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.impl_intro@tok () Term)

; </end encoding FStar.Classical.impl_intro>


; <Start encoding FStar.Classical.move_requires>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.move_requires (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.move_requires@tok () Term)

; </end encoding FStar.Classical.move_requires>


; <Start encoding FStar.Classical.move_requires_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.move_requires_2 (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.move_requires_2@tok () Term)

; </end encoding FStar.Classical.move_requires_2>


; <Start encoding FStar.Classical.move_requires_3>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.move_requires_3 (Term Term Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.move_requires_3@tok () Term)

; </end encoding FStar.Classical.move_requires_3>


; <Start encoding FStar.Classical.impl_intro_gen>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.impl_intro_gen (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.impl_intro_gen@tok () Term)

; </end encoding FStar.Classical.impl_intro_gen>


; <Start encoding FStar.Classical.get_forall>


(declare-fun FStar.Classical.get_forall (Term Term) Term)




(declare-fun Tm_refine_7a68552470cd0b3e3b5038c890f62a82 (Term Term) Term)
;;;;;;;;;;;;;;;;p: (_: a -> Prims.GTot Type) -> Prims.Pure (forall (x: a). p x)
(declare-fun Tm_arrow_44be7520ed79d38d96b60844cf5c3637 () Term)
(declare-fun FStar.Classical.get_forall@tok () Term)





; </end encoding FStar.Classical.get_forall>


; <Start encoding FStar.Classical.forall_intro_gtot>



(declare-fun FStar.Classical.forall_intro_gtot (Term Term Term) Term)


(declare-fun Tm_refine_eee646ed2c0261b2e37307734e5990b8 (Term Term) Term)
;;;;;;;;;;;;;;;;$_: (x: a -> Prims.GTot (p x)) -> Prims.squash (forall (x: a). p x)
(declare-fun Tm_arrow_0d81b083331cc2ca8277c22c2c6ed09c () Term)
(declare-fun FStar.Classical.forall_intro_gtot@tok () Term)


; </end encoding FStar.Classical.forall_intro_gtot>


; <Start encoding FStar.Classical.lemma_forall_intro_gtot>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.lemma_forall_intro_gtot (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.lemma_forall_intro_gtot@tok () Term)

; </end encoding FStar.Classical.lemma_forall_intro_gtot>


; <Start encoding FStar.Classical.gtot_to_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.gtot_to_lemma (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.gtot_to_lemma@tok () Term)

; </end encoding FStar.Classical.gtot_to_lemma>


; <Start encoding FStar.Classical.forall_intro_squash_gtot>


;;;;;;;;;;;;;;;;x: a -> Prims.GTot (Prims.squash (p x))
(declare-fun Tm_arrow_e44b1a1960e76c65248b9976ee453bf1 (Term Term) Term)
(declare-fun FStar.Classical.forall_intro_squash_gtot (Term Term Term) Term)



;;;;;;;;;;;;;;;;$_: (x: a -> Prims.GTot (Prims.squash (p x))) -> Prims.squash (forall (x: a). p x)
(declare-fun Tm_arrow_810fc5a930eab84e2e1c9bc10e65f526 () Term)
(declare-fun FStar.Classical.forall_intro_squash_gtot@tok () Term)


; </end encoding FStar.Classical.forall_intro_squash_gtot>


; <Start encoding FStar.Classical.forall_intro_squash_gtot_join>



(declare-fun FStar.Classical.forall_intro_squash_gtot_join (Term Term Term) Term)





;;;;;;;;;;;;;;;;$_: (x: a -> Prims.GTot (Prims.squash (p x))) -> (forall (x: a). p x)
(declare-fun Tm_arrow_cd9381a0f782f3a2a2b6ba363e2e0508 () Term)
(declare-fun FStar.Classical.forall_intro_squash_gtot_join@tok () Term)




; </end encoding FStar.Classical.forall_intro_squash_gtot_join>


; <Start encoding FStar.Classical.forall_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro@tok () Term)

; </end encoding FStar.Classical.forall_intro>


; <Start encoding FStar.Classical.forall_intro_with_pat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_with_pat (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_with_pat@tok () Term)

; </end encoding FStar.Classical.forall_intro_with_pat>


; <Start encoding FStar.Classical.forall_intro_sub>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_sub (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_sub@tok () Term)

; </end encoding FStar.Classical.forall_intro_sub>


; <Start encoding FStar.Classical.forall_intro_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_2@tok () Term)

; </end encoding FStar.Classical.forall_intro_2>


; <Start encoding FStar.Classical.forall_intro_2_with_pat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_2_with_pat (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_2_with_pat@tok () Term)

; </end encoding FStar.Classical.forall_intro_2_with_pat>


; <Start encoding FStar.Classical.forall_intro_3>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_3 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_3@tok () Term)

; </end encoding FStar.Classical.forall_intro_3>


; <Start encoding FStar.Classical.forall_intro_3_with_pat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_3_with_pat (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_3_with_pat@tok () Term)

; </end encoding FStar.Classical.forall_intro_3_with_pat>


; <Start encoding FStar.Classical.forall_intro_4>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_intro_4 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_intro_4@tok () Term)

; </end encoding FStar.Classical.forall_intro_4>


; <Start encoding FStar.Classical.forall_impl_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_impl_intro (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_impl_intro@tok () Term)

; </end encoding FStar.Classical.forall_impl_intro>


; <Start encoding FStar.Classical.ghost_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.ghost_lemma (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.ghost_lemma@tok () Term)

; </end encoding FStar.Classical.ghost_lemma>


; <Start encoding FStar.Classical.exists_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.exists_intro (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.exists_intro@tok () Term)

; </end encoding FStar.Classical.exists_intro>


; <Start encoding FStar.Classical.exists_intro_not_all_not>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.exists_intro_not_all_not (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.exists_intro_not_all_not@tok () Term)

; </end encoding FStar.Classical.exists_intro_not_all_not>


; <Start encoding FStar.Classical.forall_to_exists>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_to_exists (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_to_exists@tok () Term)

; </end encoding FStar.Classical.forall_to_exists>


; <Start encoding FStar.Classical.forall_to_exists_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.forall_to_exists_2 (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.forall_to_exists_2@tok () Term)

; </end encoding FStar.Classical.forall_to_exists_2>


; <Start encoding FStar.Classical.exists_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.exists_elim (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.exists_elim@tok () Term)

; </end encoding FStar.Classical.exists_elim>


; <Start encoding FStar.Classical.or_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.or_elim (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.or_elim@tok () Term)

; </end encoding FStar.Classical.or_elim>


; <Start encoding FStar.Classical.excluded_middle>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Classical.excluded_middle (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Classical.excluded_middle@tok () Term)

; </end encoding FStar.Classical.excluded_middle>


; End Externals for interface FStar.Classical


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Classical (203 decls; total size 14986)

;;; Start module FStar.StrongExcludedMiddle

; Externals for module FStar.StrongExcludedMiddle


; <Start encoding FStar.StrongExcludedMiddle.strong_excluded_middle>

(declare-fun FStar.StrongExcludedMiddle.strong_excluded_middle (Term) Term)
(declare-fun Tm_refine_2c7ecebd8a41d0890aab4251b61d6458 (Term) Term)
;;;;;;;;;;;;;;;;p: Type -> Prims.GTot (b: Prims.bool{b = true <==> p})
(declare-fun Tm_ghost_arrow_13b822d9f45311e725609e40f68f39a1 () Term)
(declare-fun FStar.StrongExcludedMiddle.strong_excluded_middle@tok () Term)


; </end encoding FStar.StrongExcludedMiddle.strong_excluded_middle>


; End Externals for module FStar.StrongExcludedMiddle


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.StrongExcludedMiddle (10 decls; total size 1606)

;;; Start interface FStar.Classical.Sugar

; Externals for interface FStar.Classical.Sugar


; <Start encoding FStar.Classical.Sugar.forall_elim>



(declare-fun FStar.Classical.Sugar.forall_elim (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;v: a -> f: Prims.squash (forall (x: a). p x) -> Prims.squash (p v)
(declare-fun Tm_arrow_6fad81cdc8376c5921e448133045a0b5 () Term)
(declare-fun FStar.Classical.Sugar.forall_elim@tok () Term)

; </end encoding FStar.Classical.Sugar.forall_elim>


; <Start encoding FStar.Classical.Sugar.exists_elim>


(declare-fun Tm_refine_df2d65c00128265e81a98d1694fa32db (Term Term) Term)
;;;;;;;;;;;;;;;;x: t -> _: Prims.squash (p x) -> Prims.squash q
(declare-fun Tm_arrow_757abf2ff34845ceba7272f2bf4c779d (Term Term Term) Term)
(declare-fun FStar.Classical.Sugar.exists_elim (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;$s_ex_p: Prims.squash (exists (x: t). p x) -> f: (x: t -> _: Prims.squash (p x) -> Prims.squash q)   -> Prims.squash q
(declare-fun Tm_arrow_8a831fa0f832c0a8df6d5207189d1bd6 () Term)
(declare-fun FStar.Classical.Sugar.exists_elim@tok () Term)

; </end encoding FStar.Classical.Sugar.exists_elim>


; <Start encoding FStar.Classical.Sugar.implies_elim>


;;;;;;;;;;;;;;;;_: Prims.unit -> Prims.squash p
(declare-fun Tm_arrow_e5bf96e6b202d44baf035cb07df2da84 (Term) Term)
(declare-fun FStar.Classical.Sugar.implies_elim (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;p: Type -> q: Type -> _: Prims.squash (p ==> q) -> f: (_: Prims.unit -> Prims.squash p)   -> Prims.squash q
(declare-fun Tm_arrow_08b9f1333754078ddc08d25d2c8efab0 () Term)
(declare-fun FStar.Classical.Sugar.implies_elim@tok () Term)



; </end encoding FStar.Classical.Sugar.implies_elim>


; <Start encoding FStar.Classical.Sugar.or_elim>

(declare-fun Tm_refine_953a6e4e702e848a7213bd619baaa22d (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.squash (~p) -> Type
(declare-fun Tm_arrow_0e4b9f46020ce2f098f1971a515a22d9 (Term) Term)
(declare-fun Tm_refine_044452ce08fa2077b813992baef379a7 (Term Term) Term)


;;;;;;;;;;;;;;;;_: Prims.squash (~p) -> _: Prims.squash (q ()) -> Prims.squash r
(declare-fun Tm_arrow_3cd50b91a1a8ff7ae2a33f725c49ca25 (Term Term Term) Term)
(declare-fun FStar.Classical.Sugar.or_elim (Term Term Term Term Term Term) Term)






;;;;;;;;;;;;;;;;p: Type ->     q: (_: Prims.squash (~p) -> Type) ->     r: Type ->     p_or: Prims.squash (p \/ q ()) ->     left: (_: Prims.squash p -> Prims.squash r) ->     right: (_: Prims.squash (~p) -> _: Prims.squash (q ()) -> Prims.squash r)   -> Prims.squash r
(declare-fun Tm_arrow_cd3704184434b709df08ad54743b9ddc () Term)
(declare-fun FStar.Classical.Sugar.or_elim@tok () Term)

; </end encoding FStar.Classical.Sugar.or_elim>


; <Start encoding FStar.Classical.Sugar.and_elim>

;;;;;;;;;;;;;;;;_: Prims.squash p -> Type
(declare-fun Tm_arrow_e617226e5c485cfca90836a8d37dc422 (Term) Term)
(declare-fun Tm_refine_0e6764ea7cf9747bf338120e46774802 (Term Term) Term)
;;;;;;;;;;;;;;;;_: Prims.squash p -> _: Prims.squash (q ()) -> Prims.squash r
(declare-fun Tm_arrow_9cf3fa74bead5ce84576f320b610d107 (Term Term Term) Term)
(declare-fun FStar.Classical.Sugar.and_elim (Term Term Term Term Term) Term)



;;;;;;;;;;;;;;;;p: Type ->     q: (_: Prims.squash p -> Type) ->     r: Type ->     _: Prims.squash (p /\ q ()) ->     f: (_: Prims.squash p -> _: Prims.squash (q ()) -> Prims.squash r)   -> Prims.squash r
(declare-fun Tm_arrow_46e93521f3e2b84feb3b4c45d4b82427 () Term)
(declare-fun FStar.Classical.Sugar.and_elim@tok () Term)

; </end encoding FStar.Classical.Sugar.and_elim>


; <Start encoding FStar.Classical.Sugar.forall_intro>



(declare-fun FStar.Classical.Sugar.forall_intro (Term Term Term) Term)




(declare-fun FStar.Classical.Sugar.forall_intro@tok () Term)


; </end encoding FStar.Classical.Sugar.forall_intro>


; <Start encoding FStar.Classical.Sugar.exists_intro>


;;;;;;;;;;;;;;;;_: Prims.unit -> Prims.squash (p v)
(declare-fun Tm_arrow_1f559753133c819dcbab21eb87f04504 (Term Term) Term)
(declare-fun FStar.Classical.Sugar.exists_intro (Term Term Term Term) Term)



;;;;;;;;;;;;;;;;a: Type -> p: (_: a -> Type) -> v: a -> x: (_: Prims.unit -> Prims.squash (p v))   -> Prims.squash (exists (x: a). p x)
(declare-fun Tm_arrow_98a6ce57193d862f12aa770d807de2d6 () Term)
(declare-fun FStar.Classical.Sugar.exists_intro@tok () Term)


; </end encoding FStar.Classical.Sugar.exists_intro>


; <Start encoding FStar.Classical.Sugar.implies_intro>


;;;;;;;;;;;;;;;;_: Prims.squash p -> Prims.squash (q ())
(declare-fun Tm_arrow_8cc28639983f141a2c2b513a3b9f2226 (Term Term) Term)
(declare-fun FStar.Classical.Sugar.implies_intro (Term Term Term) Term)


(declare-fun Tm_refine_210741fcd012f6981d2132fb47059ae4 (Term Term) Term)
;;;;;;;;;;;;;;;;p: Type -> q: (_: Prims.squash p -> Type) -> f: (_: Prims.squash p -> Prims.squash (q ()))   -> Prims.squash (p ==> q ())
(declare-fun Tm_arrow_16ed35ee40b45e5834d942fa129fc7a5 () Term)
(declare-fun FStar.Classical.Sugar.implies_intro@tok () Term)


; </end encoding FStar.Classical.Sugar.implies_intro>


; <Start encoding FStar.Classical.Sugar.or_intro_left>




(declare-fun FStar.Classical.Sugar.or_intro_left (Term Term Term) Term)




;;;;;;;;;;;;;;;;p: Type -> q: (_: Prims.squash (~p) -> Type) -> f: (_: Prims.unit -> Prims.squash p)   -> Prims.squash (p \/ q ())
(declare-fun Tm_arrow_ba201b0aaffa8c92349bb033309546c6 () Term)
(declare-fun FStar.Classical.Sugar.or_intro_left@tok () Term)


; </end encoding FStar.Classical.Sugar.or_intro_left>


; <Start encoding FStar.Classical.Sugar.or_intro_right>




;;;;;;;;;;;;;;;;_: Prims.squash (~p) -> Prims.squash (q ())
(declare-fun Tm_arrow_c1f46761cba190a64ceda9e0b423d73a (Term Term) Term)
(declare-fun FStar.Classical.Sugar.or_intro_right (Term Term Term) Term)





;;;;;;;;;;;;;;;;p: Type -> q: (_: Prims.squash (~p) -> Type) -> f: (_: Prims.squash (~p) -> Prims.squash (q ()))   -> Prims.squash (p \/ q ())
(declare-fun Tm_arrow_820ce98d2bc904183444162411ef4873 () Term)
(declare-fun FStar.Classical.Sugar.or_intro_right@tok () Term)


; </end encoding FStar.Classical.Sugar.or_intro_right>


; <Start encoding FStar.Classical.Sugar.and_intro>




(declare-fun FStar.Classical.Sugar.and_intro (Term Term Term Term) Term)




;;;;;;;;;;;;;;;;p: Type ->     q: (_: Prims.squash p -> Type) ->     left: (_: Prims.unit -> Prims.squash p) ->     right: (_: Prims.squash p -> Prims.squash (q ()))   -> Prims.squash (p /\ q ())
(declare-fun Tm_arrow_74dd2a70fe5b8eefc42b899233f1e113 () Term)
(declare-fun FStar.Classical.Sugar.and_intro@tok () Term)


; </end encoding FStar.Classical.Sugar.and_intro>


; End Externals for interface FStar.Classical.Sugar


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Classical.Sugar (136 decls; total size 7602)

;;; Start module FStar.List.Tot.Base

; Externals for module FStar.List.Tot.Base


; <Skipped FStar.List.Tot.Base.isEmpty/>


; <Start encoding FStar.List.Tot.Base.isEmpty>

(declare-fun FStar.List.Tot.Base.isEmpty (Term Term) Term)

(declare-fun FStar.List.Tot.Base.isEmpty@tok () Term)

; </end encoding FStar.List.Tot.Base.isEmpty>


; <Skipped FStar.List.Tot.Base.hd/>


; <Start encoding FStar.List.Tot.Base.hd>


(declare-fun FStar.List.Tot.Base.hd (Term Term) Term)


(declare-fun FStar.List.Tot.Base.hd@tok () Term)


; </end encoding FStar.List.Tot.Base.hd>


; <Skipped FStar.List.Tot.Base.tail/>


; <Start encoding FStar.List.Tot.Base.tail>


(declare-fun FStar.List.Tot.Base.tail (Term Term) Term)


(declare-fun FStar.List.Tot.Base.tail@tok () Term)


; </end encoding FStar.List.Tot.Base.tail>


; <Skipped FStar.List.Tot.Base.tl/>


; <Start encoding FStar.List.Tot.Base.tl>


(declare-fun FStar.List.Tot.Base.tl (Term Term) Term)


(declare-fun FStar.List.Tot.Base.tl@tok () Term)


; </end encoding FStar.List.Tot.Base.tl>


; <Skipped FStar.List.Tot.Base.last/>


; <Start encoding FStar.List.Tot.Base.last>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.last.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.last.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.last (Term Term) Term)
(declare-fun FStar.List.Tot.Base.last@tok () Term)




;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.last; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(59,8-59,12); use=FStar.List.Tot.Base.fst(59,8-59,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.last.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.last.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.last.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.last.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.last.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.last; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(59,8-59,12); use=FStar.List.Tot.Base.fst(59,8-59,12)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.last @x0
@x1)
(FStar.List.Tot.Base.last.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.last @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.last.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.last.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.last>


; <Skipped FStar.List.Tot.Base.init/>


; <Start encoding FStar.List.Tot.Base.init>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.init.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.init.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.init (Term Term) Term)
(declare-fun FStar.List.Tot.Base.init@tok () Term)




;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.init; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(67,8-67,12); use=FStar.List.Tot.Base.fst(67,8-67,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.init.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.init.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.init.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.init.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.init.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.init; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(67,8-67,12); use=FStar.List.Tot.Base.fst(67,8-67,12)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.init @x0
@x1)
(FStar.List.Tot.Base.init.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.init @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.init.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.init.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.init>


; <Skipped FStar.List.Tot.Base.length/>


; <Start encoding FStar.List.Tot.Base.length>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.length.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.length.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.length (Term Term) Term)
(declare-fun FStar.List.Tot.Base.length@tok () Term)
;;;;;;;;;;;;;;;;_: Prims.list 'a -> Prims.nat
(declare-fun Tm_arrow_5adbd6bc13eabd8f92e79f380e1498f0 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.length; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(74,8-74,14); use=FStar.List.Tot.Base.fst(74,8-74,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.length.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.length.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.length.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.length.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.length.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.length; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(74,8-74,14); use=FStar.List.Tot.Base.fst(74,8-74,14)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.length @x0
@x1)
(FStar.List.Tot.Base.length.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.length @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.length.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.length.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.length>


; <Skipped FStar.List.Tot.Base.nth/>


; <Start encoding FStar.List.Tot.Base.nth>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.nth.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.nth.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.nth (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.nth@tok () Term)
;;;;;;;;;;;;;;;;l: Prims.list 'a -> n: Prims.nat -> FStar.Pervasives.Native.option 'a
(declare-fun Tm_arrow_c96efec76dd44fb4c1c29ca8a004927d () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.nth; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(82,8-82,11); use=FStar.List.Tot.Base.fst(82,8-82,11)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.nth.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.nth.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.nth.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.nth.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.nth.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.nth; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(82,8-82,11); use=FStar.List.Tot.Base.fst(82,8-82,11)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.nth @x0
@x1
@x2)
(FStar.List.Tot.Base.nth.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.nth @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.nth.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.nth.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.nth>


; <Skipped FStar.List.Tot.Base.index/>


; <Start encoding FStar.List.Tot.Base.index>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.index.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.index.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.index (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.index@tok () Term)
(declare-fun Tm_refine_c86aba5c6243e6b7f9a4b0ad41b4e9a0 (Term Term) Term)


;;;;;;;;;;;;;;;;l: Prims.list a -> i: Prims.nat{i < FStar.List.Tot.Base.length l} -> a
(declare-fun Tm_arrow_87330224a075c52374b0ca2b4b909772 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.index; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(90,8-90,13); use=FStar.List.Tot.Base.fst(90,8-90,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.index.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.index.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.index.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.index.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.index.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.index; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(90,8-90,13); use=FStar.List.Tot.Base.fst(90,8-90,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.index @x0
@x1
@x2)
(FStar.List.Tot.Base.index.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.index @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.index.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.index.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.index>


; <Skipped FStar.List.Tot.Base.count/>


; <Start encoding FStar.List.Tot.Base.count>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.count.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.count.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.count (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.count@tok () Term)
;;;;;;;;;;;;;;;;x: a -> _: Prims.list a -> Prims.nat
(declare-fun Tm_arrow_d7494a533e0c3edea69ad484d93aa0e5 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.count; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(100,8-100,13); use=FStar.List.Tot.Base.fst(100,8-100,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.count.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.count.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.count.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.count.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.count.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.count; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(100,8-100,13); use=FStar.List.Tot.Base.fst(100,8-100,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.count @x0
@x1
@x2)
(FStar.List.Tot.Base.count.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.count @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.count.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.count.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.count>


; <Skipped FStar.List.Tot.Base.rev_acc/>


; <Start encoding FStar.List.Tot.Base.rev_acc>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.rev_acc.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.rev_acc.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.rev_acc (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.rev_acc@tok () Term)
;;;;;;;;;;;;;;;;l: Prims.list 'a -> acc: Prims.list 'a -> Prims.list 'a
(declare-fun Tm_arrow_54e38bdd456bab4cdb32b5d540c2274c () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.rev_acc; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(108,8-108,15); use=FStar.List.Tot.Base.fst(108,8-108,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.rev_acc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.rev_acc.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.rev_acc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.rev_acc.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.rev_acc.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.rev_acc; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(108,8-108,15); use=FStar.List.Tot.Base.fst(108,8-108,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.rev_acc @x0
@x1
@x2)
(FStar.List.Tot.Base.rev_acc.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.rev_acc @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.rev_acc.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.rev_acc.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.rev_acc>


; <Skipped FStar.List.Tot.Base.rev/>


; <Start encoding FStar.List.Tot.Base.rev>

(declare-fun FStar.List.Tot.Base.rev (Term Term) Term)
;;;;;;;;;;;;;;;;l: Prims.list 'a -> Prims.list 'a
(declare-fun Tm_arrow_f9ba16c6212a483d195bbb8ceec3eef1 () Term)
(declare-fun FStar.List.Tot.Base.rev@tok () Term)

; </end encoding FStar.List.Tot.Base.rev>


; <Skipped FStar.List.Tot.Base.append/>


; <Start encoding FStar.List.Tot.Base.append>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.append.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.append.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.append (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.append@tok () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.append; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(119,8-119,14); use=FStar.List.Tot.Base.fst(119,8-119,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.append.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.append.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.append.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.append.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.append.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.append; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(119,8-119,14); use=FStar.List.Tot.Base.fst(119,8-119,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.append @x0
@x1
@x2)
(FStar.List.Tot.Base.append.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.append @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.append.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.append.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.append>


; <Start encoding FStar.List.Tot.Base.op_At>

(declare-fun FStar.List.Tot.Base.op_At (Term Term Term) Term)

(declare-fun FStar.List.Tot.Base.op_At@tok () Term)

; </end encoding FStar.List.Tot.Base.op_At>


; <Skipped FStar.List.Tot.Base.snoc/>


; <Start encoding FStar.List.Tot.Base.snoc>

(declare-fun FStar.List.Tot.Base.snoc (Term Term) Term)
;;;;;;;;;;;;;;;;_: (Prims.list 'a * 'a) -> Prims.list 'a
(declare-fun Tm_arrow_07ff48a1c7b541b0963ce508064e29fb () Term)
(declare-fun FStar.List.Tot.Base.snoc@tok () Term)

; </end encoding FStar.List.Tot.Base.snoc>


; <Skipped FStar.List.Tot.Base.flatten/>


; <Start encoding FStar.List.Tot.Base.flatten>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.flatten.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.flatten.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.flatten (Term Term) Term)
(declare-fun FStar.List.Tot.Base.flatten@tok () Term)
;;;;;;;;;;;;;;;;l: Prims.list (Prims.list 'a) -> Prims.list 'a
(declare-fun Tm_arrow_7e18fd6b36805c1f1c9a77e024fdec2e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.flatten; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(143,8-143,15); use=FStar.List.Tot.Base.fst(143,8-143,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.flatten.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.flatten.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.flatten.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.flatten.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.flatten.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.flatten; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(143,8-143,15); use=FStar.List.Tot.Base.fst(143,8-143,15)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.flatten @x0
@x1)
(FStar.List.Tot.Base.flatten.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.flatten @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.flatten.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.flatten.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.flatten>


; <Skipped FStar.List.Tot.Base.map/>


; <Start encoding FStar.List.Tot.Base.map>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.map.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.map.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.map (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.map@tok () Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> 'b) -> x: Prims.list 'a -> Prims.list 'b
(declare-fun Tm_arrow_28431dcf5044bcdd56dbe625f9e3df4e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.map; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(151,8-151,11); use=FStar.List.Tot.Base.fst(151,8-151,11)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.map.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.map.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.map.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.map.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.map.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.map; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(151,8-151,11); use=FStar.List.Tot.Base.fst(151,8-151,11)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.map @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.map.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.map @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.map.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.map.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.map>


; <Skipped FStar.List.Tot.Base.mapi_init/>


; <Start encoding FStar.List.Tot.Base.mapi_init>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.mapi_init.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.mapi_init.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.mapi_init (Term Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.mapi_init@tok () Term)
;;;;;;;;;;;;;;;;_: Prims.int -> _: 'a -> 'b
(declare-fun Tm_arrow_010f318679809a99aeced42f5ba95505 (Term Term) Term)


;;;;;;;;;;;;;;;;f: (_: Prims.int -> _: 'a -> 'b) -> l: Prims.list 'a -> i: Prims.int -> Prims.list 'b
(declare-fun Tm_arrow_9a89e146e4bb6b361bc4526b891ed1f1 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.mapi_init; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(160,8-160,17); use=FStar.List.Tot.Base.fst(160,8-160,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.List.Tot.Base.mapi_init.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.List.Tot.Base.mapi_init.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.List.Tot.Base.mapi_init.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.List.Tot.Base.mapi_init.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.mapi_init.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.mapi_init; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(160,8-160,17); use=FStar.List.Tot.Base.fst(160,8-160,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.mapi_init @x0
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.mapi_init.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.mapi_init @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.List.Tot.Base.mapi_init.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.mapi_init.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.mapi_init>


; <Skipped FStar.List.Tot.Base.mapi/>


; <Start encoding FStar.List.Tot.Base.mapi>


(declare-fun FStar.List.Tot.Base.mapi (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: Prims.int -> _: 'a -> 'b) -> l: Prims.list 'a -> Prims.list 'b
(declare-fun Tm_arrow_b2a07f422fceebd0f3ee3abd5e4aeed2 () Term)
(declare-fun FStar.List.Tot.Base.mapi@tok () Term)


; </end encoding FStar.List.Tot.Base.mapi>


; <Skipped FStar.List.Tot.Base.concatMap/>


; <Start encoding FStar.List.Tot.Base.concatMap>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.concatMap.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.concatMap.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.concatMap (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.concatMap@tok () Term)
;;;;;;;;;;;;;;;;_: 'a -> Prims.list 'b
(declare-fun Tm_arrow_121fa5bc200f7b3946a5e35040f266b9 (Term Term) Term)


;;;;;;;;;;;;;;;;f: (_: 'a -> Prims.list 'b) -> _: Prims.list 'a -> Prims.list 'b
(declare-fun Tm_arrow_c35dd4e5f8c08f94183bf93963fac92f () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.concatMap; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(176,8-176,17); use=FStar.List.Tot.Base.fst(176,8-176,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.concatMap.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.concatMap.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.concatMap.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.concatMap.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.concatMap.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.concatMap; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(176,8-176,17); use=FStar.List.Tot.Base.fst(176,8-176,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.concatMap @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.concatMap.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.concatMap @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.concatMap.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.concatMap.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.concatMap>


; <Skipped FStar.List.Tot.Base.fold_left/>


; <Start encoding FStar.List.Tot.Base.fold_left>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.fold_left.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.fold_left.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.fold_left (Term Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.fold_left@tok () Term)
;;;;;;;;;;;;;;;;_: 'a -> _: 'b -> 'a
(declare-fun Tm_arrow_f0225aaf6b987d44876e7f498390aa39 (Term Term) Term)


;;;;;;;;;;;;;;;;f: (_: 'a -> _: 'b -> 'a) -> x: 'a -> l: Prims.list 'b -> Prims.Tot 'a
(declare-fun Tm_arrow_230697841c1116c0d5f3958097856e6e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.fold_left; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(187,8-187,17); use=FStar.List.Tot.Base.fst(187,8-187,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.List.Tot.Base.fold_left.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.List.Tot.Base.fold_left.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.List.Tot.Base.fold_left.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.List.Tot.Base.fold_left.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.fold_left.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.fold_left; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(187,8-187,17); use=FStar.List.Tot.Base.fst(187,8-187,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.fold_left @x0
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.fold_left.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.fold_left @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.List.Tot.Base.fold_left.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.fold_left.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.fold_left>


; <Skipped FStar.List.Tot.Base.fold_right/>


; <Start encoding FStar.List.Tot.Base.fold_right>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.fold_right.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.fold_right.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.fold_right (Term Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.fold_right@tok () Term)
;;;;;;;;;;;;;;;;_: 'a -> _: 'b -> 'b
(declare-fun Tm_arrow_3c1d21b8f6dcc5e202b4ff1cafbaba81 (Term Term) Term)


;;;;;;;;;;;;;;;;f: (_: 'a -> _: 'b -> 'b) -> l: Prims.list 'a -> x: 'b -> 'b
(declare-fun Tm_arrow_105b39eeae3a464c82e64975ac399cdb () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.fold_right; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(195,8-195,18); use=FStar.List.Tot.Base.fst(195,8-195,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.List.Tot.Base.fold_right.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.List.Tot.Base.fold_right.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.List.Tot.Base.fold_right.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.List.Tot.Base.fold_right.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.fold_right.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.fold_right; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(195,8-195,18); use=FStar.List.Tot.Base.fst(195,8-195,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.fold_right @x0
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.fold_right.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.fold_right @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.List.Tot.Base.fold_right.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.fold_right.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.fold_right>


; <Start encoding FStar.List.Tot.Base.fold_right_gtot>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.fold_right_gtot (Term Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.fold_right_gtot@tok () Term)
;;;;;;;;;;;;;;;;_: a -> _: b -> Prims.GTot b
(declare-fun Tm_ghost_arrow_d7e9834b8fd0407a723f5f3f4b012fdd (Term Term) Term)


;;;;;;;;;;;;;;;;l: Prims.list a -> f: (_: a -> _: b -> Prims.GTot b) -> x: b -> Prims.GTot b
(declare-fun Tm_ghost_arrow_fab043b8cdd2296e8d98a06066e4b2d2 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.fold_right_gtot; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(201,8-201,23); use=FStar.List.Tot.Base.fst(201,8-201,23)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.fold_right_gtot; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(201,8-201,23); use=FStar.List.Tot.Base.fst(201,8-201,23)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.fold_right_gtot @x0
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.fold_right_gtot @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.fold_right_gtot.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.fold_right_gtot>


; <Start encoding FStar.List.Tot.Base.map_gtot>


(declare-fun FStar.List.Tot.Base.map_gtot (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: a -> Prims.GTot b) -> x: Prims.list a -> Prims.GTot (Prims.list b)
(declare-fun Tm_ghost_arrow_d0c7be07105bf8d5ad60b7f603c725f3 () Term)
(declare-fun FStar.List.Tot.Base.map_gtot@tok () Term)

;;;;;;;;;;;;;;;;x: a -> tl: Prims.list b -> Prims.GTot (Prims.list b)
(declare-fun Tm_ghost_arrow_21583233c98863da294c5e5d657cf78a (Term Term) Term)
(declare-fun Tm_abs_469cd3853c3ff3e8cd408b5521fdbd9d (Term Term Term) Term)

; </end encoding FStar.List.Tot.Base.map_gtot>


; <Skipped FStar.List.Tot.Base.fold_left2/>


; <Start encoding FStar.List.Tot.Base.fold_left2>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.fold_left2.fuel_instrumented (Fuel Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.fold_left2.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.fold_left2 (Term Term Term Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.fold_left2@tok () Term)
;;;;;;;;;;;;;;;;_: 'a -> _: 'b -> _: 'c -> 'a
(declare-fun Tm_arrow_40dd30796dd695d143ec6ed01d322177 (Term Term Term) Term)
(declare-fun Tm_refine_c16bc1b61f58b349bf6fc1c94dcaf83b (Term) Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> _: 'b -> _: 'c -> 'a) -> accu: 'a -> l1: Prims.list 'b -> l2: Prims.list 'c   -> Prims.Pure 'a
(declare-fun Tm_arrow_3f28d1abbd43ddded682cbec516ea7bb () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.fold_left2; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(218,8-218,18); use=FStar.List.Tot.Base.fst(218,8-218,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term) (@x7 Term))
 (! (= (FStar.List.Tot.Base.fold_left2.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5
@x6
@x7)
(FStar.List.Tot.Base.fold_left2.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5
@x6
@x7))
 

:pattern ((FStar.List.Tot.Base.fold_left2.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5
@x6
@x7))
:qid @fuel_irrelevance_FStar.List.Tot.Base.fold_left2.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.fold_left2.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.fold_left2; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(218,8-218,18); use=FStar.List.Tot.Base.fst(218,8-218,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term) (@x6 Term))
 (! (= (FStar.List.Tot.Base.fold_left2 @x0
@x1
@x2
@x3
@x4
@x5
@x6)
(FStar.List.Tot.Base.fold_left2.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4
@x5
@x6))
 

:pattern ((FStar.List.Tot.Base.fold_left2 @x0
@x1
@x2
@x3
@x4
@x5
@x6))
:qid @fuel_correspondence_FStar.List.Tot.Base.fold_left2.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.fold_left2.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.fold_left2>


; <Start encoding FStar.List.Tot.Base.memP>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.memP.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.memP.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.memP (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.memP@tok () Term)
;;;;;;;;;;;;;;;;x: a -> l: Prims.list a -> Type
(declare-fun Tm_arrow_9a5de17321abf8ec257671c9a474c08a () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.memP; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(228,8-228,12); use=FStar.List.Tot.Base.fst(228,8-228,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.memP.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.memP.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.memP.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.memP.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.memP.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.memP; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(228,8-228,12); use=FStar.List.Tot.Base.fst(228,8-228,12)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.memP @x0
@x1
@x2)
(FStar.List.Tot.Base.memP.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.memP @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.memP.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.memP.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.memP>


; <Skipped FStar.List.Tot.Base.mem/>


; <Start encoding FStar.List.Tot.Base.mem>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.mem.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.mem.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.mem (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.mem@tok () Term)
;;;;;;;;;;;;;;;;x: a -> _: Prims.list a -> Prims.bool
(declare-fun Tm_arrow_8b16b79a9f8fab7cb6911016a8022992 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.mem; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(240,8-240,11); use=FStar.List.Tot.Base.fst(240,8-240,11)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.mem.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.mem.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.mem.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.mem.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.mem.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.mem; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(240,8-240,11); use=FStar.List.Tot.Base.fst(240,8-240,11)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.mem @x0
@x1
@x2)
(FStar.List.Tot.Base.mem.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.mem @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.mem.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.mem.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.mem>


; <Start encoding FStar.List.Tot.Base.contains>

(declare-fun FStar.List.Tot.Base.contains (Term Term Term) Term)

(declare-fun FStar.List.Tot.Base.contains@tok () Term)

; </end encoding FStar.List.Tot.Base.contains>


; <Skipped FStar.List.Tot.Base.existsb/>


; <Start encoding FStar.List.Tot.Base.existsb>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.existsb.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.existsb.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.existsb (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.existsb@tok () Term)
;;;;;;;;;;;;;;;;_: a -> Prims.bool
(declare-fun Tm_arrow_84543425b818e2d10a976186b8e8c250 (Term) Term)


;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: Prims.list a -> Prims.bool
(declare-fun Tm_arrow_98dbecc64760e6a41f037a6881cd5df8 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.existsb; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(256,8-256,15); use=FStar.List.Tot.Base.fst(256,8-256,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.existsb.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.existsb.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.existsb.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.existsb.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.existsb.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.existsb; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(256,8-256,15); use=FStar.List.Tot.Base.fst(256,8-256,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.existsb @x0
@x1
@x2)
(FStar.List.Tot.Base.existsb.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.existsb @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.existsb.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.existsb.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.existsb>


; <Skipped FStar.List.Tot.Base.find/>


; <Start encoding FStar.List.Tot.Base.find>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.find.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.find.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.find (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.find@tok () Term)

(declare-fun Tm_refine_3b1cb9ec3355fed185c658f53954b3fa (Term Term) Term)





;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: Prims.list a -> FStar.Pervasives.Native.option (x: a{f x})
(declare-fun Tm_arrow_286c509b12b9a2bb9bf1025c6fd97451 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.find; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(266,8-266,12); use=FStar.List.Tot.Base.fst(266,8-266,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.find.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.find.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.find.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.find.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.find.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.find; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(266,8-266,12); use=FStar.List.Tot.Base.fst(266,8-266,12)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.find @x0
@x1
@x2)
(FStar.List.Tot.Base.find.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.find @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.find.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.find.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.find>


; <Skipped FStar.List.Tot.Base.filter/>


; <Start encoding FStar.List.Tot.Base.filter>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.filter.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.filter.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.filter (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.filter@tok () Term)

(declare-fun Tm_refine_5c77e54d118aa26696ff018c647e0d2c (Term Term) Term)



;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: Prims.list a   -> m: Prims.list a {forall (x: a). FStar.List.Tot.Base.memP x m ==> f x}
(declare-fun Tm_arrow_26bc30ca5e980f52cef77fa4ff2b8923 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.filter; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(277,8-277,14); use=FStar.List.Tot.Base.fst(277,8-277,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.filter.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.filter.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.filter.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.filter.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.filter.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.filter; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(277,8-277,14); use=FStar.List.Tot.Base.fst(277,8-277,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.filter @x0
@x1
@x2)
(FStar.List.Tot.Base.filter.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.filter @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.filter.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.filter.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.filter>


; <Skipped FStar.List.Tot.Base.mem_filter/>


; <Start encoding FStar.List.Tot.Base.mem_filter>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.mem_filter (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.mem_filter@tok () Term)

; </end encoding FStar.List.Tot.Base.mem_filter>


; <Skipped FStar.List.Tot.Base.mem_filter_forall/>


; <Start encoding FStar.List.Tot.Base.mem_filter_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.mem_filter_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.mem_filter_forall@tok () Term)


; </end encoding FStar.List.Tot.Base.mem_filter_forall>


; <Skipped FStar.List.Tot.Base.for_all/>


; <Start encoding FStar.List.Tot.Base.for_all>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.for_all.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.for_all.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.for_all (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.for_all@tok () Term)




;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.for_all; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(303,8-303,15); use=FStar.List.Tot.Base.fst(303,8-303,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.for_all.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.for_all.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.for_all.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.for_all.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.for_all.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.for_all; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(303,8-303,15); use=FStar.List.Tot.Base.fst(303,8-303,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.for_all @x0
@x1
@x2)
(FStar.List.Tot.Base.for_all.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.for_all @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.for_all.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.for_all.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.for_all>


; <Start encoding FStar.List.Tot.Base.for_all_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.for_all_mem (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.for_all_mem@tok () Term)

; </end encoding FStar.List.Tot.Base.for_all_mem>


; <Skipped FStar.List.Tot.Base.collect/>


; <Start encoding FStar.List.Tot.Base.collect>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.collect.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.collect.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.collect (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.collect@tok () Term)




;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.collect; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(324,8-324,15); use=FStar.List.Tot.Base.fst(324,8-324,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.collect.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.collect.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.collect.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.collect.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.collect.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.collect; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(324,8-324,15); use=FStar.List.Tot.Base.fst(324,8-324,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.collect @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.collect.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.collect @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.collect.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.collect.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.collect>


; <Skipped FStar.List.Tot.Base.tryFind/>


; <Start encoding FStar.List.Tot.Base.tryFind>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.tryFind.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.tryFind.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.tryFind (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.tryFind@tok () Term)



;;;;;;;;;;;;;;;;p: (_: 'a -> Prims.bool) -> l: Prims.list 'a -> FStar.Pervasives.Native.option 'a
(declare-fun Tm_arrow_4ae6bca87a611585312b8b0d0d66fefe () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.tryFind; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(334,8-334,15); use=FStar.List.Tot.Base.fst(334,8-334,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.tryFind.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.tryFind.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.tryFind.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.tryFind.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.tryFind.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.tryFind; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(334,8-334,15); use=FStar.List.Tot.Base.fst(334,8-334,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.tryFind @x0
@x1
@x2)
(FStar.List.Tot.Base.tryFind.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.tryFind @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.tryFind.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.tryFind.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.tryFind>


; <Skipped FStar.List.Tot.Base.tryPick/>


; <Start encoding FStar.List.Tot.Base.tryPick>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.tryPick.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.tryPick.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.tryPick (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.tryPick@tok () Term)
;;;;;;;;;;;;;;;;_: 'a -> FStar.Pervasives.Native.option 'b
(declare-fun Tm_arrow_4b0c7cc34485afa5854ebe5c95023d4c (Term Term) Term)


;;;;;;;;;;;;;;;;f: (_: 'a -> FStar.Pervasives.Native.option 'b) -> l: Prims.list 'a   -> FStar.Pervasives.Native.option 'b
(declare-fun Tm_arrow_7fbbe8a710b97b9ed9c0d2dfb00b1641 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.tryPick; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(343,8-343,15); use=FStar.List.Tot.Base.fst(343,8-343,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.tryPick.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.tryPick.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.tryPick.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.tryPick.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.tryPick.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.tryPick; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(343,8-343,15); use=FStar.List.Tot.Base.fst(343,8-343,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.tryPick @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.tryPick.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.tryPick @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.tryPick.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.tryPick.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.tryPick>


; <Skipped FStar.List.Tot.Base.choose/>


; <Start encoding FStar.List.Tot.Base.choose>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.choose.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.choose.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.choose (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.choose@tok () Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> FStar.Pervasives.Native.option 'b) -> l: Prims.list 'a -> Prims.list 'b
(declare-fun Tm_arrow_ee03a7411b6d8975b285ea6c772c4d89 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.choose; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(354,8-354,14); use=FStar.List.Tot.Base.fst(354,8-354,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.choose.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.choose.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.choose.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.choose.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.choose.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.choose; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(354,8-354,14); use=FStar.List.Tot.Base.fst(354,8-354,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.choose @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.choose.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.choose @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.choose.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.choose.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.choose>


; <Skipped FStar.List.Tot.Base.partition/>


; <Start encoding FStar.List.Tot.Base.partition>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.partition.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.partition.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.partition (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.partition@tok () Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> Prims.bool) -> _: Prims.list 'a -> Prims.list 'a * Prims.list 'a
(declare-fun Tm_arrow_706f575815ce8a3bbd962da035d8aa14 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.partition; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(367,8-367,17); use=FStar.List.Tot.Base.fst(367,8-367,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.partition.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.partition.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.partition.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.partition.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.partition.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.partition; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(367,8-367,17); use=FStar.List.Tot.Base.fst(367,8-367,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.partition @x0
@x1
@x2)
(FStar.List.Tot.Base.partition.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.partition @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.partition.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.partition.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.partition>


; <Skipped FStar.List.Tot.Base.subset/>


; <Start encoding FStar.List.Tot.Base.subset>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.subset.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.subset.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.subset (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.subset@tok () Term)
;;;;;;;;;;;;;;;;la: Prims.list a -> lb: Prims.list a -> Prims.bool
(declare-fun Tm_arrow_8d819a995fc33b4cb6aa699af88e8d32 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.subset; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(379,8-379,14); use=FStar.List.Tot.Base.fst(379,8-379,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.subset.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.subset.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.subset.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.subset.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.subset.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.subset; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(379,8-379,14); use=FStar.List.Tot.Base.fst(379,8-379,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.subset @x0
@x1
@x2)
(FStar.List.Tot.Base.subset.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.subset @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.subset.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.subset.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.subset>


; <Skipped FStar.List.Tot.Base.noRepeats/>


; <Start encoding FStar.List.Tot.Base.noRepeats>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.noRepeats.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.noRepeats.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.noRepeats (Term Term) Term)
(declare-fun FStar.List.Tot.Base.noRepeats@tok () Term)
;;;;;;;;;;;;;;;;la: Prims.list a -> Prims.bool
(declare-fun Tm_arrow_0dd285b24907a2f8b15dedffef61afa6 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.noRepeats; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(388,8-388,17); use=FStar.List.Tot.Base.fst(388,8-388,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.noRepeats.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.noRepeats.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.noRepeats.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.noRepeats.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.noRepeats.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.noRepeats; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(388,8-388,17); use=FStar.List.Tot.Base.fst(388,8-388,17)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.noRepeats @x0
@x1)
(FStar.List.Tot.Base.noRepeats.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.noRepeats @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.noRepeats.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.noRepeats.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.noRepeats>


; <Skipped FStar.List.Tot.Base.no_repeats_p/>


; <Start encoding FStar.List.Tot.Base.no_repeats_p>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.no_repeats_p.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.no_repeats_p.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.no_repeats_p (Term Term) Term)
(declare-fun FStar.List.Tot.Base.no_repeats_p@tok () Term)
;;;;;;;;;;;;;;;;la: Prims.list a -> Prims.prop
(declare-fun Tm_arrow_79c2442eab9e49d1108d2b7a240dc76e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.no_repeats_p; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(397,8-397,20); use=FStar.List.Tot.Base.fst(397,8-397,20)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.no_repeats_p.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Base.no_repeats_p.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.no_repeats_p.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Base.no_repeats_p.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.no_repeats_p.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.no_repeats_p; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(397,8-397,20); use=FStar.List.Tot.Base.fst(397,8-397,20)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Base.no_repeats_p @x0
@x1)
(FStar.List.Tot.Base.no_repeats_p.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Base.no_repeats_p @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Base.no_repeats_p.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.no_repeats_p.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.no_repeats_p>


; <Skipped FStar.List.Tot.Base.assoc/>


; <Start encoding FStar.List.Tot.Base.assoc>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.assoc.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.assoc.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.assoc (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.assoc@tok () Term)
;;;;;;;;;;;;;;;;x: a -> _: Prims.list (a * b) -> FStar.Pervasives.Native.option b
(declare-fun Tm_arrow_d77cf796c5b72d2c2316c0fcdad1dd79 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.assoc; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(409,8-409,13); use=FStar.List.Tot.Base.fst(409,8-409,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.assoc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.assoc.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.assoc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.assoc.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.assoc.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.assoc; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(409,8-409,13); use=FStar.List.Tot.Base.fst(409,8-409,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.assoc @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.assoc.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.assoc @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.assoc.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.assoc.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.assoc>


; <Skipped FStar.List.Tot.Base.split/>


; <Start encoding FStar.List.Tot.Base.split>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.split.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.split.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.split (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.split@tok () Term)
;;;;;;;;;;;;;;;;l: Prims.list ('a * 'b) -> Prims.list 'a * Prims.list 'b
(declare-fun Tm_arrow_1c3cb31b4ffa47bc6454f5b8a25e2407 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.split; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(417,8-417,13); use=FStar.List.Tot.Base.fst(417,8-417,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.split.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.split.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.split.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.split.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.split.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.split; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(417,8-417,13); use=FStar.List.Tot.Base.fst(417,8-417,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.split @x0
@x1
@x2)
(FStar.List.Tot.Base.split.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.split @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.split.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.split.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.split>


; <Start encoding FStar.List.Tot.Base.unzip>

(declare-fun FStar.List.Tot.Base.unzip (Term Term Term) Term)

(declare-fun FStar.List.Tot.Base.unzip@tok () Term)

; </end encoding FStar.List.Tot.Base.unzip>


; <Skipped FStar.List.Tot.Base.unzip3/>


; <Start encoding FStar.List.Tot.Base.unzip3>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.unzip3.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.unzip3.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.unzip3 (Term Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.unzip3@tok () Term)
;;;;;;;;;;;;;;;;l: Prims.list (('a * 'b) * 'c) -> (Prims.list 'a * Prims.list 'b) * Prims.list 'c
(declare-fun Tm_arrow_d40be6b496fedb6f7a46205c5824b732 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.unzip3; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(432,8-432,14); use=FStar.List.Tot.Base.fst(432,8-432,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.List.Tot.Base.unzip3.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.List.Tot.Base.unzip3.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.List.Tot.Base.unzip3.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.List.Tot.Base.unzip3.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.unzip3.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.unzip3; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(432,8-432,14); use=FStar.List.Tot.Base.fst(432,8-432,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.unzip3 @x0
@x1
@x2
@x3)
(FStar.List.Tot.Base.unzip3.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.unzip3 @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.List.Tot.Base.unzip3.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.unzip3.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.unzip3>


; <Start encoding FStar.List.Tot.Base.splitAt>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.splitAt.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.splitAt.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.splitAt (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.splitAt@tok () Term)
;;;;;;;;;;;;;;;;n: Prims.nat -> l: Prims.list a -> Prims.list a * Prims.list a
(declare-fun Tm_arrow_e36bd078e08c2ac2f1324fef6e0a4a22 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.splitAt; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(443,8-443,15); use=FStar.List.Tot.Base.fst(443,8-443,15)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.splitAt.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.splitAt.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.splitAt.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.splitAt.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.splitAt.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.splitAt; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(443,8-443,15); use=FStar.List.Tot.Base.fst(443,8-443,15)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.splitAt @x0
@x1
@x2)
(FStar.List.Tot.Base.splitAt.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.splitAt @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.splitAt.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.splitAt.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.splitAt>


; <Start encoding FStar.List.Tot.Base.lemma_splitAt_snd_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.lemma_splitAt_snd_length (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.lemma_splitAt_snd_length@tok () Term)

; </end encoding FStar.List.Tot.Base.lemma_splitAt_snd_length>


; <Skipped FStar.List.Tot.Base.unsnoc/>


; <Start encoding FStar.List.Tot.Base.unsnoc>

(declare-fun Tm_refine_3f6b38b2852708f36615f9b4db0f9ff1 (Term) Term)
(declare-fun FStar.List.Tot.Base.unsnoc (Term Term) Term)

;;;;;;;;;;;;;;;;l: Prims.list a {FStar.List.Tot.Base.length l > 0} -> Prims.list a * a
(declare-fun Tm_arrow_f4bc61622db0c39a751170734a140783 () Term)
(declare-fun FStar.List.Tot.Base.unsnoc@tok () Term)


; </end encoding FStar.List.Tot.Base.unsnoc>


; <Skipped FStar.List.Tot.Base.split3/>


; <Start encoding FStar.List.Tot.Base.split3>


(declare-fun FStar.List.Tot.Base.split3 (Term Term Term) Term)

;;;;;;;;;;;;;;;;l: Prims.list a -> i: Prims.nat{i < FStar.List.Tot.Base.length l}   -> (Prims.list a * a) * Prims.list a
(declare-fun Tm_arrow_07dcb44faa0fb6172673970868e7ecff () Term)
(declare-fun FStar.List.Tot.Base.split3@tok () Term)


; </end encoding FStar.List.Tot.Base.split3>


; <Skipped FStar.List.Tot.Base.partition_length/>


; <Start encoding FStar.List.Tot.Base.partition_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.partition_length (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.partition_length@tok () Term)

; </end encoding FStar.List.Tot.Base.partition_length>


; <Skipped FStar.List.Tot.Base.bool_of_compare/>


; <Start encoding FStar.List.Tot.Base.bool_of_compare>

;;;;;;;;;;;;;;;;_: a -> _: a -> Prims.int
(declare-fun Tm_arrow_9877f854fbaabbcfda94f6c19b32ae3f (Term) Term)
(declare-fun FStar.List.Tot.Base.bool_of_compare (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: a -> _: a -> Prims.int) -> x: a -> y: a -> Prims.bool
(declare-fun Tm_arrow_a2f219461d35e20b7bc771538ca96429 () Term)
(declare-fun FStar.List.Tot.Base.bool_of_compare@tok () Term)


; </end encoding FStar.List.Tot.Base.bool_of_compare>


; <Skipped FStar.List.Tot.Base.compare_of_bool/>


; <Start encoding FStar.List.Tot.Base.compare_of_bool>

;;;;;;;;;;;;;;;;_: a -> _: a -> Prims.bool
(declare-fun Tm_arrow_c8126b87a2c25bb477df4a7a6b0eea9e (Term) Term)
(declare-fun FStar.List.Tot.Base.compare_of_bool (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;rel: (_: a -> _: a -> Prims.bool) -> x: a -> y: a -> Prims.int
(declare-fun Tm_arrow_8b54d4820d055c327440d0d4811d3a33 () Term)
(declare-fun FStar.List.Tot.Base.compare_of_bool@tok () Term)


; </end encoding FStar.List.Tot.Base.compare_of_bool>


; <Start encoding FStar.List.Tot.Base.compare_of_bool_of_compare>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Base.compare_of_bool_of_compare (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Base.compare_of_bool_of_compare@tok () Term)

; </end encoding FStar.List.Tot.Base.compare_of_bool_of_compare>


; <Skipped FStar.List.Tot.Base.sortWith/>


; <Start encoding FStar.List.Tot.Base.sortWith>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.sortWith.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.sortWith.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.sortWith (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.sortWith@tok () Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> _: 'a -> Prims.int) -> l: Prims.list 'a -> Prims.Tot (Prims.list 'a)
(declare-fun Tm_arrow_d29fb5884447b657cb725f9be68c5ba6 () Term)
;;;;;;;;;;;;;;;;kick_partial_app
;;; Fact-ids: Name FStar.List.Tot.Base.sortWith; Namespace FStar.List.Tot.Base
(assert (! (Valid (ApplyTT __uu__PartialApp
FStar.List.Tot.Base.bool_of_compare@tok))
:named @kick_partial_app_6123e8040f356c82d11b245dda0e1ccc))
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.sortWith; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(523,8-523,16); use=FStar.List.Tot.Base.fst(523,8-523,16)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.sortWith.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.sortWith.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.sortWith.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.sortWith.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.sortWith.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.sortWith; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(523,8-523,16); use=FStar.List.Tot.Base.fst(523,8-523,16)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.sortWith @x0
@x1
@x2)
(FStar.List.Tot.Base.sortWith.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.sortWith @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.sortWith.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.sortWith.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.sortWith>


; <Start encoding FStar.List.Tot.Base.strict_suffix_of>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.strict_suffix_of (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.strict_suffix_of@tok () Term)
(declare-fun Tm_refine_da3062322c9bea8d5b2058386775b91a () Term)

;;;;;;;;;;;;;;;;l1: Prims.list a -> l2: Prims.list a -> Prims.Pure Type
(declare-fun Tm_arrow_1d91178a138c1826d6a199b1613394f1 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.strict_suffix_of; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(531,8-531,24); use=FStar.List.Tot.Base.fst(531,8-531,24)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.strict_suffix_of; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(531,8-531,24); use=FStar.List.Tot.Base.fst(531,8-531,24)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.strict_suffix_of @x0
@x1
@x2)
(FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.strict_suffix_of @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.strict_suffix_of.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.strict_suffix_of>


; <Start encoding FStar.List.Tot.Base.strict_prefix_of>

(declare-fun FStar.List.Tot.Base.strict_prefix_of (Term) Term)

;;;;;;;;;;;;;;;;l1: Prims.list _ -> l2: Prims.list _ -> Prims.Pure Type
(declare-fun Tm_arrow_0dd65914dd84642f7849df5d67086aa0 (Term) Term)
;;;;;;;;;;;;;;;;l1: Prims.list _ -> l2: Prims.list _ -> Prims.Pure Type
(declare-fun Tm_arrow_25d975ae357f14f725a8d52a81f8be72 () Term)
(declare-fun FStar.List.Tot.Base.strict_prefix_of@tok () Term)


;;;;;;;;;;;;;;;;kick_partial_app
;;; Fact-ids: Name FStar.List.Tot.Base.strict_prefix_of; Namespace FStar.List.Tot.Base
(assert (! (Valid (ApplyTT __uu__PartialApp
FStar.List.Tot.Base.strict_suffix_of@tok))
:named @kick_partial_app_a83d9836dd5fde61849337e3aeefb108))

; </end encoding FStar.List.Tot.Base.strict_prefix_of>


; <Skipped FStar.List.Tot.Base.list_unref/>


; <Start encoding FStar.List.Tot.Base.list_unref>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.list_unref.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.list_unref.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.list_unref (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.list_unref@tok () Term)

(declare-fun Tm_refine_9f8cb5a84b67f50c9d5f87a914037545 (Term Term) Term)




;;;;;;;;;;;;;;;;l: Prims.list (x: a{p x}) -> Prims.list a
(declare-fun Tm_arrow_6b3a7706fc085133138f00ee506ef176 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.list_unref; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(544,8-544,18); use=FStar.List.Tot.Base.fst(544,8-544,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.list_unref.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.list_unref.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.list_unref.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.list_unref.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.list_unref.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.list_unref; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(544,8-544,18); use=FStar.List.Tot.Base.fst(544,8-544,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.list_unref @x0
@x1
@x2)
(FStar.List.Tot.Base.list_unref.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.list_unref @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.list_unref.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.list_unref.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.list_unref>


; <Skipped FStar.List.Tot.Base.list_refb/>


; <Start encoding FStar.List.Tot.Base.list_refb>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.list_refb.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.list_refb.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.list_refb (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.list_refb@tok () Term)

(declare-fun Tm_refine_3dfaece5a1f8e27ecb1367ff50145048 (Term Term) Term)





(declare-fun Tm_refine_b3daba88e15ae8a9be9dd341522270b2 (Term Term Term Term) Term)

(declare-fun Tm_refine_1d1ddbacd892e41ad4ba585e87296d2e (Term Term Term) Term)










;;;;;;;;;;;;;;;;l: Prims.list a {FStar.List.Tot.Base.for_all p l}   -> l':     Prims.list (x: a{p x})       { FStar.List.Tot.Base.length l = FStar.List.Tot.Base.length l' /\         (forall (i:             Prims.nat{i < FStar.List.Tot.Base.length l /\ i < FStar.List.Tot.Base.length l'}).             {:pattern FStar.List.Tot.Base.index l i}             FStar.List.Tot.Base.index l i = FStar.List.Tot.Base.index l' i) }
(declare-fun Tm_arrow_73c684a5823f2875fcceead4ce671ea8 () Term)






;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.list_refb; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(554,8-554,17); use=FStar.List.Tot.Base.fst(554,8-554,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.list_refb.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.list_refb.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.list_refb.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.list_refb.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.list_refb.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.list_refb; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(554,8-554,17); use=FStar.List.Tot.Base.fst(554,8-554,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.list_refb @x0
@x1
@x2)
(FStar.List.Tot.Base.list_refb.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.list_refb @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.list_refb.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.list_refb.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.list_refb>


; <Skipped FStar.List.Tot.Base.list_ref/>


; <Start encoding FStar.List.Tot.Base.list_ref>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Base.list_ref.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Base.list_ref.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Base.list_ref (Term Term Term) Term)
(declare-fun FStar.List.Tot.Base.list_ref@tok () Term)
;;;;;;;;;;;;;;;;_: a -> Prims.prop
(declare-fun Tm_arrow_81e65de2755319ee661cc1adc7d951e3 (Term) Term)
(declare-fun Tm_refine_751cc4d3e845537c495f9d7e1deb8aa9 (Term Term) Term)





(declare-fun Tm_refine_f61b92c00df29b87346e52dcf7670926 (Term Term Term Term) Term)

(declare-fun Tm_refine_16f0c42812e28aba7e30bc8c275306fb (Term Term Term) Term)










;;;;;;;;;;;;;;;;l:       Prims.list a         {forall (x: a). {:pattern FStar.List.Tot.Base.mem x l} FStar.List.Tot.Base.mem x l ==> p x}   -> l':     Prims.list (x: a{p x})       { FStar.List.Tot.Base.length l = FStar.List.Tot.Base.length l' /\         (forall (i:             Prims.nat{i < FStar.List.Tot.Base.length l /\ i < FStar.List.Tot.Base.length l'}).             {:pattern FStar.List.Tot.Base.index l i}             FStar.List.Tot.Base.index l i = FStar.List.Tot.Base.index l' i) }
(declare-fun Tm_arrow_73f29356f974e35d230fb85375ad3965 () Term)






;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Base.list_ref; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(564,8-564,16); use=FStar.List.Tot.Base.fst(564,8-564,16)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Base.list_ref.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Base.list_ref.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Base.list_ref.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Base.list_ref.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Base.list_ref.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Base.list_ref; Namespace FStar.List.Tot.Base
(assert (! 
;; def=FStar.List.Tot.Base.fst(564,8-564,16); use=FStar.List.Tot.Base.fst(564,8-564,16)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Base.list_ref @x0
@x1
@x2)
(FStar.List.Tot.Base.list_ref.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Base.list_ref @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Base.list_ref.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Base.list_ref.fuel_instrumented))

; </end encoding FStar.List.Tot.Base.list_ref>


; End Externals for module FStar.List.Tot.Base


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.List.Tot.Base (675 decls; total size 88349)

;;; Start module FStar.List.Tot.Properties

; Externals for module FStar.List.Tot.Properties


; <Start encoding FStar.List.Tot.Properties.llist>

(declare-fun FStar.List.Tot.Properties.llist (Term Term) Term)
;;;;;;;;;;;;;;;;a: Type -> n: Prims.nat -> Type
(declare-fun Tm_arrow_67c7b2626869cb316f118144000415b9 () Term)
(declare-fun FStar.List.Tot.Properties.llist@tok () Term)
(declare-fun Tm_refine_fbb3412f12fd58a91571022d7c9fa36d (Term Term) Term)

; </end encoding FStar.List.Tot.Properties.llist>


; <Start encoding FStar.List.Tot.Properties.mem_memP>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.mem_memP (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.mem_memP@tok () Term)

; </end encoding FStar.List.Tot.Properties.mem_memP>


; <Start encoding FStar.List.Tot.Properties.lemma_index_memP>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_index_memP (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_index_memP@tok () Term)
(declare-fun Tm_refine_bf2fa1226f2c9a0f6671df3e80ddcb8e (Term Term) Term)

; </end encoding FStar.List.Tot.Properties.lemma_index_memP>


; <Skipped FStar.List.Tot.Properties.memP_empty/>


; <Start encoding FStar.List.Tot.Properties.memP_empty>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.memP_empty (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.memP_empty@tok () Term)

; </end encoding FStar.List.Tot.Properties.memP_empty>


; <Skipped FStar.List.Tot.Properties.memP_existsb/>


; <Start encoding FStar.List.Tot.Properties.memP_existsb>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.memP_existsb (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.memP_existsb@tok () Term)

; </end encoding FStar.List.Tot.Properties.memP_existsb>


; <Start encoding FStar.List.Tot.Properties.memP_map_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.memP_map_intro (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.memP_map_intro@tok () Term)

; </end encoding FStar.List.Tot.Properties.memP_map_intro>


; <Start encoding FStar.List.Tot.Properties.memP_map_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.memP_map_elim (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.memP_map_elim@tok () Term)

; </end encoding FStar.List.Tot.Properties.memP_map_elim>


; <Skipped FStar.List.Tot.Properties.mem_empty/>


; <Start encoding FStar.List.Tot.Properties.mem_empty>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.mem_empty (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.mem_empty@tok () Term)

; </end encoding FStar.List.Tot.Properties.mem_empty>


; <Skipped FStar.List.Tot.Properties.mem_existsb/>


; <Start encoding FStar.List.Tot.Properties.mem_existsb>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.mem_existsb (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.mem_existsb@tok () Term)

; </end encoding FStar.List.Tot.Properties.mem_existsb>


; <Start encoding FStar.List.Tot.Properties.mem_count>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.mem_count (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.mem_count@tok () Term)

; </end encoding FStar.List.Tot.Properties.mem_count>


; <Skipped FStar.List.Tot.Properties.rev_acc_length/>


; <Start encoding FStar.List.Tot.Properties.rev_acc_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_length (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_length@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_acc_length>


; <Skipped FStar.List.Tot.Properties.rev_length/>


; <Start encoding FStar.List.Tot.Properties.rev_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_length (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_length@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_length>


; <Skipped FStar.List.Tot.Properties.rev_acc_memP/>


; <Start encoding FStar.List.Tot.Properties.rev_acc_memP>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_memP (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_memP@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_acc_memP>


; <Skipped FStar.List.Tot.Properties.rev_memP/>


; <Start encoding FStar.List.Tot.Properties.rev_memP>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_memP (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_memP@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_memP>


; <Skipped FStar.List.Tot.Properties.rev_mem/>


; <Start encoding FStar.List.Tot.Properties.rev_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_mem (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_mem@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_mem>


; <Skipped FStar.List.Tot.Properties.append_nil_l/>


; <Start encoding FStar.List.Tot.Properties.append_nil_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_nil_l (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_nil_l@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_nil_l>


; <Skipped FStar.List.Tot.Properties.append_l_nil/>


; <Start encoding FStar.List.Tot.Properties.append_l_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_l_nil (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_l_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_l_nil>


; <Skipped FStar.List.Tot.Properties.append_cons_l/>


; <Start encoding FStar.List.Tot.Properties.append_cons_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_cons_l (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_cons_l@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_cons_l>


; <Skipped FStar.List.Tot.Properties.append_l_cons/>


; <Start encoding FStar.List.Tot.Properties.append_l_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_l_cons (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_l_cons@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_l_cons>


; <Skipped FStar.List.Tot.Properties.append_assoc/>


; <Start encoding FStar.List.Tot.Properties.append_assoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_assoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_assoc@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_assoc>


; <Skipped FStar.List.Tot.Properties.append_length/>


; <Start encoding FStar.List.Tot.Properties.append_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_length (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_length@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_length>


; <Skipped FStar.List.Tot.Properties.append_mem/>


; <Start encoding FStar.List.Tot.Properties.append_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_mem (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_mem@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_mem>


; <Skipped FStar.List.Tot.Properties.append_mem_forall/>


; <Start encoding FStar.List.Tot.Properties.append_mem_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_mem_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_mem_forall@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_mem_forall>


; <Skipped FStar.List.Tot.Properties.append_count/>


; <Start encoding FStar.List.Tot.Properties.append_count>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_count (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_count@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_count>


; <Skipped FStar.List.Tot.Properties.append_count_forall/>


; <Start encoding FStar.List.Tot.Properties.append_count_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_count_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_count_forall@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_count_forall>


; <Skipped FStar.List.Tot.Properties.append_eq_nil/>


; <Start encoding FStar.List.Tot.Properties.append_eq_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_eq_nil (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_eq_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_eq_nil>


; <Skipped FStar.List.Tot.Properties.append_eq_singl/>


; <Start encoding FStar.List.Tot.Properties.append_eq_singl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_eq_singl (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_eq_singl@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_eq_singl>


; <Skipped FStar.List.Tot.Properties.append_inv_head/>


; <Start encoding FStar.List.Tot.Properties.append_inv_head>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_inv_head (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_inv_head@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_inv_head>


; <Skipped FStar.List.Tot.Properties.append_inv_tail/>


; <Start encoding FStar.List.Tot.Properties.append_inv_tail>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_inv_tail (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_inv_tail@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_inv_tail>


; <Start encoding FStar.List.Tot.Properties.append_length_inv_head>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_length_inv_head (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_length_inv_head@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_length_inv_head>


; <Start encoding FStar.List.Tot.Properties.append_length_inv_tail>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_length_inv_tail (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_length_inv_tail@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_length_inv_tail>


; <Start encoding FStar.List.Tot.Properties.append_injective>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_injective (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_injective@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_injective>


; <Start encoding FStar.List.Tot.Properties.lemma_append_last>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_append_last (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_append_last@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_append_last>


; <Skipped FStar.List.Tot.Properties.rev'/>


; <Start encoding FStar.List.Tot.Properties.rev'>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Properties.rev_.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Properties.rev_.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Properties.rev_ (Term Term) Term)
(declare-fun FStar.List.Tot.Properties.rev_@tok () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Properties.rev'; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(316,8-316,12); use=FStar.List.Tot.Properties.fst(316,8-316,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Properties.rev_.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.List.Tot.Properties.rev_.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.List.Tot.Properties.rev_.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.List.Tot.Properties.rev_.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Properties.rev_.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Properties.rev'; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(316,8-316,12); use=FStar.List.Tot.Properties.fst(316,8-316,12)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.List.Tot.Properties.rev_ @x0
@x1)
(FStar.List.Tot.Properties.rev_.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.List.Tot.Properties.rev_ @x0
@x1))
:qid @fuel_correspondence_FStar.List.Tot.Properties.rev_.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Properties.rev_.fuel_instrumented))

; </end encoding FStar.List.Tot.Properties.rev'>


; <Start encoding FStar.List.Tot.Properties.rev'T>

(declare-fun FStar.List.Tot.Properties.rev_T (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.list _ -> Prims.list _
(declare-fun Tm_arrow_f34ce2ad5441b4bd300fa100b397737d (Term) Term)
;;;;;;;;;;;;;;;;_: Prims.list _ -> Prims.list _
(declare-fun Tm_arrow_42c6b27a859866d5307ff94c9f459cb1 () Term)
(declare-fun FStar.List.Tot.Properties.rev_T@tok () Term)

;;;;;;;;;;;;;;;;kick_partial_app
;;; Fact-ids: Name FStar.List.Tot.Properties.rev'T; Namespace FStar.List.Tot.Properties
(assert (! (Valid (ApplyTT __uu__PartialApp
FStar.List.Tot.Properties.rev_@tok))
:named @kick_partial_app_6780e2e9ce16d5330b5fda76b7bde9c5))

; </end encoding FStar.List.Tot.Properties.rev'T>


; <Skipped FStar.List.Tot.Properties.rev_acc_rev'/>


; <Start encoding FStar.List.Tot.Properties.rev_acc_rev'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_rev_ (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_acc_rev_@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_acc_rev'>


; <Skipped FStar.List.Tot.Properties.rev_rev'/>


; <Start encoding FStar.List.Tot.Properties.rev_rev'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_rev_ (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_rev_@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_rev'>


; <Skipped FStar.List.Tot.Properties.rev'_append/>


; <Start encoding FStar.List.Tot.Properties.rev'_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev__append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev__append@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev'_append>


; <Skipped FStar.List.Tot.Properties.rev_append/>


; <Start encoding FStar.List.Tot.Properties.rev_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_append>


; <Skipped FStar.List.Tot.Properties.rev'_involutive/>


; <Start encoding FStar.List.Tot.Properties.rev'_involutive>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev__involutive (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev__involutive@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev'_involutive>


; <Skipped FStar.List.Tot.Properties.rev_involutive/>


; <Start encoding FStar.List.Tot.Properties.rev_involutive>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_involutive (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_involutive@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_involutive>


; <Skipped FStar.List.Tot.Properties.lemma_snoc_length/>


; <Start encoding FStar.List.Tot.Properties.lemma_snoc_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_snoc_length (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_snoc_length@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_snoc_length>


; <Skipped FStar.List.Tot.Properties.rev'_list_ind/>


; <Start encoding FStar.List.Tot.Properties.rev'_list_ind>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev__list_ind (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev__list_ind@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev'_list_ind>


; <Skipped FStar.List.Tot.Properties.rev_ind/>


; <Start encoding FStar.List.Tot.Properties.rev_ind>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.rev_ind (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.rev_ind@tok () Term)

; </end encoding FStar.List.Tot.Properties.rev_ind>


; <Skipped FStar.List.Tot.Properties.map_lemma/>


; <Start encoding FStar.List.Tot.Properties.map_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.map_lemma (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.map_lemma@tok () Term)


; </end encoding FStar.List.Tot.Properties.map_lemma>


; <Skipped FStar.List.Tot.Properties.lemma_unsnoc_snoc/>


; <Start encoding FStar.List.Tot.Properties.lemma_unsnoc_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_snoc (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_snoc@tok () Term)
(declare-fun Tm_refine_e88aba6d4c79a5625ab4330932edf7ed (Term) Term)

; </end encoding FStar.List.Tot.Properties.lemma_unsnoc_snoc>


; <Skipped FStar.List.Tot.Properties.lemma_snoc_unsnoc/>


; <Start encoding FStar.List.Tot.Properties.lemma_snoc_unsnoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_snoc_unsnoc (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_snoc_unsnoc@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_snoc_unsnoc>


; <Skipped FStar.List.Tot.Properties.lemma_unsnoc_length/>


; <Start encoding FStar.List.Tot.Properties.lemma_unsnoc_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_length (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_length@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_unsnoc_length>


; <Start encoding FStar.List.Tot.Properties.lemma_unsnoc_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_unsnoc_append>


; <Start encoding FStar.List.Tot.Properties.lemma_unsnoc_is_last>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_is_last (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_is_last@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_unsnoc_is_last>


; <Start encoding FStar.List.Tot.Properties.lemma_unsnoc_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_unsnoc_index@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_unsnoc_index>


; <Start encoding FStar.List.Tot.Properties.split_using>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Properties.split_using.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Properties.split_using.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Properties.split_using (Term Term Term) Term)
(declare-fun FStar.List.Tot.Properties.split_using@tok () Term)
(declare-fun Tm_refine_ca5b6dc4e0a851997703798a1ffc5f70 (Term Term) Term)


;;;;;;;;;;;;;;;;l: Prims.list t -> x: t{FStar.List.Tot.Base.memP x l} -> Prims.GTot (Prims.list t * Prims.list t)
(declare-fun Tm_ghost_arrow_583c096a402961cd40d8b718fb07bacc () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Properties.split_using; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(466,8-466,19); use=FStar.List.Tot.Properties.fst(466,8-466,19)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Properties.split_using.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Properties.split_using.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Properties.split_using.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Properties.split_using.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Properties.split_using.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Properties.split_using; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(466,8-466,19); use=FStar.List.Tot.Properties.fst(466,8-466,19)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Properties.split_using @x0
@x1
@x2)
(FStar.List.Tot.Properties.split_using.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Properties.split_using @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Properties.split_using.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Properties.split_using.fuel_instrumented))

; </end encoding FStar.List.Tot.Properties.split_using>


; <Start encoding FStar.List.Tot.Properties.lemma_split_using>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.lemma_split_using (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.lemma_split_using@tok () Term)

; </end encoding FStar.List.Tot.Properties.lemma_split_using>


; <Start encoding FStar.List.Tot.Properties.index_of>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Properties.index_of.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Properties.index_of.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Properties.index_of (Term Term Term) Term)
(declare-fun FStar.List.Tot.Properties.index_of@tok () Term)

(declare-fun Tm_refine_cd45ecc9daf74409c394004efbaa3338 (Term Term Term) Term)



;;;;;;;;;;;;;;;;l: Prims.list t -> x: t{FStar.List.Tot.Base.memP x l}   -> Prims.GTot     (i: Prims.nat{i < FStar.List.Tot.Base.length l /\ FStar.List.Tot.Base.index l i == x})
(declare-fun Tm_ghost_arrow_d9cd5e48f458f8c211c59f9048af3929 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Properties.index_of; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(507,8-507,16); use=FStar.List.Tot.Properties.fst(507,8-507,16)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Properties.index_of.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Properties.index_of.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Properties.index_of.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Properties.index_of.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Properties.index_of.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Properties.index_of; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(507,8-507,16); use=FStar.List.Tot.Properties.fst(507,8-507,16)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Properties.index_of @x0
@x1
@x2)
(FStar.List.Tot.Properties.index_of.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Properties.index_of @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Properties.index_of.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Properties.index_of.fuel_instrumented))

; </end encoding FStar.List.Tot.Properties.index_of>


; <Skipped FStar.List.Tot.Properties.partition_mem/>


; <Start encoding FStar.List.Tot.Properties.partition_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem@tok () Term)

; </end encoding FStar.List.Tot.Properties.partition_mem>


; <Skipped FStar.List.Tot.Properties.partition_mem_forall/>


; <Start encoding FStar.List.Tot.Properties.partition_mem_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem_forall@tok () Term)

; </end encoding FStar.List.Tot.Properties.partition_mem_forall>


; <Skipped FStar.List.Tot.Properties.partition_mem_p_forall/>


; <Start encoding FStar.List.Tot.Properties.partition_mem_p_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem_p_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.partition_mem_p_forall@tok () Term)

; </end encoding FStar.List.Tot.Properties.partition_mem_p_forall>


; <Skipped FStar.List.Tot.Properties.partition_count/>


; <Start encoding FStar.List.Tot.Properties.partition_count>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.partition_count (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.partition_count@tok () Term)

; </end encoding FStar.List.Tot.Properties.partition_count>


; <Skipped FStar.List.Tot.Properties.partition_count_forall/>


; <Start encoding FStar.List.Tot.Properties.partition_count_forall>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.partition_count_forall (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.partition_count_forall@tok () Term)

; </end encoding FStar.List.Tot.Properties.partition_count_forall>


; <Skipped FStar.List.Tot.Properties.sortWith_permutation/>


; <Start encoding FStar.List.Tot.Properties.sortWith_permutation>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.sortWith_permutation (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.sortWith_permutation@tok () Term)

; </end encoding FStar.List.Tot.Properties.sortWith_permutation>


; <Skipped FStar.List.Tot.Properties.sorted/>


; <Start encoding FStar.List.Tot.Properties.sorted>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.List.Tot.Properties.sorted.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.List.Tot.Properties.sorted.fuel_instrumented_token () Term)
(declare-fun FStar.List.Tot.Properties.sorted (Term Term Term) Term)
(declare-fun FStar.List.Tot.Properties.sorted@tok () Term)



;;;;;;;;;;;;;;;;f: (_: 'a -> _: 'a -> Prims.bool) -> _: Prims.list 'a -> Prims.bool
(declare-fun Tm_arrow_3ceaaa0abe084cc4615eb380e8d5e0cc () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.List.Tot.Properties.sorted; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(599,8-599,14); use=FStar.List.Tot.Properties.fst(599,8-599,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.List.Tot.Properties.sorted.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.List.Tot.Properties.sorted.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.List.Tot.Properties.sorted.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.List.Tot.Properties.sorted.fuel_instrumented))

:named @fuel_irrelevance_FStar.List.Tot.Properties.sorted.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.List.Tot.Properties.sorted; Namespace FStar.List.Tot.Properties
(assert (! 
;; def=FStar.List.Tot.Properties.fst(599,8-599,14); use=FStar.List.Tot.Properties.fst(599,8-599,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.List.Tot.Properties.sorted @x0
@x1
@x2)
(FStar.List.Tot.Properties.sorted.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.List.Tot.Properties.sorted @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.List.Tot.Properties.sorted.fuel_instrumented))

:named @fuel_correspondence_FStar.List.Tot.Properties.sorted.fuel_instrumented))

; </end encoding FStar.List.Tot.Properties.sorted>


; <Start encoding FStar.List.Tot.Properties.total_order>


(declare-fun FStar.List.Tot.Properties.total_order (Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: a -> _: a -> Prims.bool) -> Prims.logical
(declare-fun Tm_arrow_92649d42e4d7df07b51f92b06355903e () Term)
(declare-fun FStar.List.Tot.Properties.total_order@tok () Term)


(declare-fun Tm_abs_155da43c8f196a519f1d932a1ab4c672 (Term Term) Term)

(declare-fun Tm_abs_04fa90dc867a5ed8e0397827a7571ed5 (Term Term Term) Term)

(declare-fun Tm_abs_fe2be3b4afbe66f9150ee17cf4940baf (Term Term) Term)

(declare-fun Tm_abs_2c9f14a824739c9038583213daad5a5e (Term Term Term Term) Term)

(declare-fun Tm_abs_2d2122604cffa731ca36b55576591613 (Term Term Term) Term)

(declare-fun Tm_abs_b229429e9a9f69e9e2fad0e7209d52f8 (Term Term) Term)

(declare-fun Tm_abs_8c0710121fff62e4b75dbdb8d8081c34 (Term Term Term) Term)

(declare-fun Tm_abs_cc30b17d49d0d8b0d5dad0fc1fc8c31f (Term Term) Term)

; </end encoding FStar.List.Tot.Properties.total_order>


; <Skipped FStar.List.Tot.Properties.append_sorted/>


; <Start encoding FStar.List.Tot.Properties.append_sorted>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_sorted (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_sorted@tok () Term)

(declare-fun Tm_refine_828abd88abe59cf052738363f3952d7b (Term Term) Term)


; </end encoding FStar.List.Tot.Properties.append_sorted>


; <Skipped FStar.List.Tot.Properties.sortWith_sorted/>


; <Start encoding FStar.List.Tot.Properties.sortWith_sorted>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.sortWith_sorted (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.sortWith_sorted@tok () Term)

; </end encoding FStar.List.Tot.Properties.sortWith_sorted>


; <Start encoding FStar.List.Tot.Properties.noRepeats_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_nil (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.noRepeats_nil>


; <Start encoding FStar.List.Tot.Properties.noRepeats_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_cons@tok () Term)

; </end encoding FStar.List.Tot.Properties.noRepeats_cons>


; <Start encoding FStar.List.Tot.Properties.noRepeats_append_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_append_elim (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_append_elim@tok () Term)

; </end encoding FStar.List.Tot.Properties.noRepeats_append_elim>


; <Start encoding FStar.List.Tot.Properties.noRepeats_append_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_append_intro (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.noRepeats_append_intro@tok () Term)

; </end encoding FStar.List.Tot.Properties.noRepeats_append_intro>


; <Start encoding FStar.List.Tot.Properties.assoc_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_nil (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_nil>


; <Start encoding FStar.List.Tot.Properties.assoc_cons_eq>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_cons_eq (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_cons_eq@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_cons_eq>


; <Start encoding FStar.List.Tot.Properties.assoc_cons_not_eq>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_cons_not_eq (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_cons_not_eq@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_cons_not_eq>


; <Start encoding FStar.List.Tot.Properties.assoc_append_elim_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_append_elim_r (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_append_elim_r@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_append_elim_r>


; <Start encoding FStar.List.Tot.Properties.assoc_append_elim_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_append_elim_l (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_append_elim_l@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_append_elim_l>


; <Start encoding FStar.List.Tot.Properties.assoc_memP_some>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_memP_some (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_memP_some@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_memP_some>


; <Start encoding FStar.List.Tot.Properties.assoc_memP_none>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_memP_none (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_memP_none@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_memP_none>


; <Start encoding FStar.List.Tot.Properties.assoc_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_mem (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_mem@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_mem>


; <Start encoding FStar.List.Tot.Properties.fold_left_invar>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_invar (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_invar@tok () Term)

; </end encoding FStar.List.Tot.Properties.fold_left_invar>


; <Start encoding FStar.List.Tot.Properties.fold_left_map>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_map (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_map@tok () Term)

; </end encoding FStar.List.Tot.Properties.fold_left_map>


; <Start encoding FStar.List.Tot.Properties.map_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.map_append (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.map_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.map_append>


; <Start encoding FStar.List.Tot.Properties.fold_left_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_append (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.fold_left_append>


; <Start encoding FStar.List.Tot.Properties.fold_left_monoid>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_monoid (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_monoid@tok () Term)

; </end encoding FStar.List.Tot.Properties.fold_left_monoid>


; <Start encoding FStar.List.Tot.Properties.fold_left_append_monoid>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_append_monoid (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.fold_left_append_monoid@tok () Term)

; </end encoding FStar.List.Tot.Properties.fold_left_append_monoid>


; <Start encoding FStar.List.Tot.Properties.index_extensionality_aux>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.index_extensionality_aux (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.index_extensionality_aux@tok () Term)

; </end encoding FStar.List.Tot.Properties.index_extensionality_aux>


; <Start encoding FStar.List.Tot.Properties.index_extensionality>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.index_extensionality (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.index_extensionality@tok () Term)

; </end encoding FStar.List.Tot.Properties.index_extensionality>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_nil (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_nil>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_or_eq_nil>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_or_eq_nil (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_or_eq_nil@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_or_eq_nil>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_cons@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_cons>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_trans>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_trans (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_trans@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_trans>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_correct>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_correct (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_correct@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_correct>


; <Start encoding FStar.List.Tot.Properties.map_strict_suffix_of>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.map_strict_suffix_of (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.map_strict_suffix_of@tok () Term)

; </end encoding FStar.List.Tot.Properties.map_strict_suffix_of>


; <Start encoding FStar.List.Tot.Properties.mem_strict_suffix_of>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.mem_strict_suffix_of (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.mem_strict_suffix_of@tok () Term)

; </end encoding FStar.List.Tot.Properties.mem_strict_suffix_of>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_exists_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_exists_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_exists_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_exists_append>


; <Start encoding FStar.List.Tot.Properties.strict_suffix_of_or_eq_exists_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_or_eq_exists_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.strict_suffix_of_or_eq_exists_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.strict_suffix_of_or_eq_exists_append>


; <Start encoding FStar.List.Tot.Properties.precedes_tl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.precedes_tl (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.precedes_tl@tok () Term)

; </end encoding FStar.List.Tot.Properties.precedes_tl>


; <Start encoding FStar.List.Tot.Properties.precedes_append_cons_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.precedes_append_cons_r (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.precedes_append_cons_r@tok () Term)

; </end encoding FStar.List.Tot.Properties.precedes_append_cons_r>


; <Start encoding FStar.List.Tot.Properties.precedes_append_cons_prod_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.precedes_append_cons_prod_r (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.precedes_append_cons_prod_r@tok () Term)

; </end encoding FStar.List.Tot.Properties.precedes_append_cons_prod_r>


; <Start encoding FStar.List.Tot.Properties.memP_precedes>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.memP_precedes (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.memP_precedes@tok () Term)

; </end encoding FStar.List.Tot.Properties.memP_precedes>


; <Start encoding FStar.List.Tot.Properties.assoc_precedes>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.assoc_precedes (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.assoc_precedes@tok () Term)

; </end encoding FStar.List.Tot.Properties.assoc_precedes>


; <Start encoding FStar.List.Tot.Properties.find_none>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.find_none (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.find_none@tok () Term)

; </end encoding FStar.List.Tot.Properties.find_none>


; <Start encoding FStar.List.Tot.Properties.append_init_last>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.append_init_last (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.append_init_last@tok () Term)

; </end encoding FStar.List.Tot.Properties.append_init_last>


; <Start encoding FStar.List.Tot.Properties.init_last_def>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.init_last_def (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.init_last_def@tok () Term)

; </end encoding FStar.List.Tot.Properties.init_last_def>


; <Start encoding FStar.List.Tot.Properties.init_last_inj>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.init_last_inj (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.init_last_inj@tok () Term)

; </end encoding FStar.List.Tot.Properties.init_last_inj>


; <Skipped />


; <Start encoding FStar.List.Tot.Properties.for_all_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.List.Tot.Properties.for_all_append (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.List.Tot.Properties.for_all_append@tok () Term)

; </end encoding FStar.List.Tot.Properties.for_all_append>


; <Skipped />


; End Externals for module FStar.List.Tot.Properties


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.List.Tot.Properties (527 decls; total size 52385)

;;; Start module FStar.List.Tot

; Externals for module FStar.List.Tot


; End Externals for module FStar.List.Tot


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.List.Tot (3 decls; total size 1078)

;;; Start interface FStar.Seq.Base

; Externals for interface FStar.Seq.Base


; <Start encoding FStar.Seq.Base.seq>

(declare-fun FStar.Seq.Base.seq (Term) Term)

(declare-fun FStar.Seq.Base.seq@tok () Term)

; </end encoding FStar.Seq.Base.seq>


; <Start encoding FStar.Seq.Base.length>

(declare-fun FStar.Seq.Base.length (Term Term) Term)
;;;;;;;;;;;;;;;;_: FStar.Seq.Base.seq a -> Prims.nat
(declare-fun Tm_arrow_d2c01593e1ccf972aadc4bced72f8166 () Term)
(declare-fun FStar.Seq.Base.length@tok () Term)

; </end encoding FStar.Seq.Base.length>


; <Start encoding FStar.Seq.Base.index>

(declare-fun Tm_refine_d83f8da8ef6c1cb9f71d1465c1bb1c55 (Term Term) Term)
(declare-fun FStar.Seq.Base.index (Term Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> i: Prims.nat{i < FStar.Seq.Base.length s} -> a
(declare-fun Tm_arrow_1910ef5262f2ee8e712b6609a232b1ea () Term)
(declare-fun FStar.Seq.Base.index@tok () Term)

; </end encoding FStar.Seq.Base.index>


; <Start encoding FStar.Seq.Base.create>

(declare-fun FStar.Seq.Base.create (Term Term Term) Term)
;;;;;;;;;;;;;;;;_: Prims.nat -> _: a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_b5b3d4fcc48eb666a8878550e50df9fb () Term)
(declare-fun FStar.Seq.Base.create@tok () Term)

; </end encoding FStar.Seq.Base.create>


; <Start encoding FStar.Seq.Base.init_aux>

(declare-fun Tm_refine_c1424615841f28cac7fc34e92b7ff33c (Term) Term)
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name FStar.Seq.Base.init_aux; Namespace FStar.Seq.Base
(assert (! 
;; def=FStar.Seq.Base.fsti(32,41-32,57); use=FStar.Seq.Base.fsti(32,41-32,57)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (iff (HasTypeFuel @u0
@x1
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x2))
(and (HasTypeFuel @u0
@x1
Prims.nat)

;; def=FStar.Seq.Base.fsti(32,48-32,55); use=FStar.Seq.Base.fsti(32,48-32,55)
(< (BoxInt_proj_0 @x1)
(BoxInt_proj_0 @x2))
))
 

:pattern ((HasTypeFuel @u0
@x1
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x2)))
:qid refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c))

:named refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c))

;;;;;;;;;;;;;;;;i: Prims.nat{i < len} -> a
(declare-fun Tm_arrow_44bb45ed5c2534b346e0f58ea5033251 (Term Term) Term)
(declare-fun FStar.Seq.Base.init_aux (Term Term Term Term) Term)



;;;;;;;;;;;;;;;;len: Prims.nat -> k: Prims.nat{k < len} -> contents: (i: Prims.nat{i < len} -> a)   -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_da6bbab10714c064205223f9990745bd () Term)
(declare-fun FStar.Seq.Base.init_aux@tok () Term)

; </end encoding FStar.Seq.Base.init_aux>


; <Start encoding FStar.Seq.Base.init>



(declare-fun FStar.Seq.Base.init (Term Term Term) Term)


;;;;;;;;;;;;;;;;len: Prims.nat -> contents: (i: Prims.nat{i < len} -> a) -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_d638d84259a58eff38c91944355ac313 () Term)
(declare-fun FStar.Seq.Base.init@tok () Term)

; </end encoding FStar.Seq.Base.init>


; <Start encoding FStar.Seq.Base.init_aux_ghost>



;;;;;;;;;;;;;;;;i: Prims.nat{i < len} -> Prims.GTot a
(declare-fun Tm_ghost_arrow_b7c239afcc620812134a759b53cafcc7 (Term Term) Term)
(declare-fun FStar.Seq.Base.init_aux_ghost (Term Term Term Term) Term)



;;;;;;;;;;;;;;;;len: Prims.nat -> k: Prims.nat{k < len} -> contents: (i: Prims.nat{i < len} -> Prims.GTot a)   -> Prims.GTot (FStar.Seq.Base.seq a)
(declare-fun Tm_ghost_arrow_90c084cce85ad5fa9b6789a83ba7b9d5 () Term)
(declare-fun FStar.Seq.Base.init_aux_ghost@tok () Term)

; </end encoding FStar.Seq.Base.init_aux_ghost>


; <Start encoding FStar.Seq.Base.init_ghost>



(declare-fun FStar.Seq.Base.init_ghost (Term Term Term) Term)


;;;;;;;;;;;;;;;;len: Prims.nat -> contents: (i: Prims.nat{i < len} -> Prims.GTot a)   -> Prims.GTot (FStar.Seq.Base.seq a)
(declare-fun Tm_ghost_arrow_dc3e2497ae3914facc1bb3cecddbafe4 () Term)
(declare-fun FStar.Seq.Base.init_ghost@tok () Term)

; </end encoding FStar.Seq.Base.init_ghost>


; <Start encoding FStar.Seq.Base.empty>

(declare-fun FStar.Seq.Base.empty (Term) Term)
(declare-fun Tm_refine_b913a3f691ca99086652e0a655e72f17 (Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a {FStar.Seq.Base.length s = 0}
(declare-fun Tm_arrow_c39fb4e3e203a822394c714f70ec2d2c () Term)
(declare-fun FStar.Seq.Base.empty@tok () Term)


; </end encoding FStar.Seq.Base.empty>


; <Start encoding FStar.Seq.Base.createEmpty>

(declare-fun FStar.Seq.Base.createEmpty (Term) Term)


(declare-fun FStar.Seq.Base.createEmpty@tok () Term)


; </end encoding FStar.Seq.Base.createEmpty>


; <Start encoding FStar.Seq.Base.lemma_empty>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_empty (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_empty@tok () Term)

; </end encoding FStar.Seq.Base.lemma_empty>


; <Start encoding FStar.Seq.Base.upd>


(declare-fun FStar.Seq.Base.upd (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> n: Prims.nat{n < FStar.Seq.Base.length s} -> _: a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_12766e98f50c8b91e296bbc369061265 () Term)
(declare-fun FStar.Seq.Base.upd@tok () Term)

; </end encoding FStar.Seq.Base.upd>


; <Start encoding FStar.Seq.Base.append>

(declare-fun FStar.Seq.Base.append (Term Term Term) Term)
;;;;;;;;;;;;;;;;_: FStar.Seq.Base.seq a -> _: FStar.Seq.Base.seq a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_22c1b165cc91e8aafbceb8b36244be8e () Term)
(declare-fun FStar.Seq.Base.append@tok () Term)

; </end encoding FStar.Seq.Base.append>


; <Start encoding FStar.Seq.Base.op_At_Bar>

(declare-fun FStar.Seq.Base.op_At_Bar (Term Term Term) Term)

(declare-fun FStar.Seq.Base.op_At_Bar@tok () Term)

; </end encoding FStar.Seq.Base.op_At_Bar>


; <Start encoding FStar.Seq.Base.slice>

(declare-fun Tm_refine_81407705a0828c2c1b1976675443f647 (Term Term Term) Term)
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name FStar.Seq.Base.slice; Namespace FStar.Seq.Base
(assert (! 
;; def=FStar.Seq.Base.fsti(58,43-58,73); use=FStar.Seq.Base.fsti(58,43-58,73)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (iff (HasTypeFuel @u0
@x1
(Tm_refine_81407705a0828c2c1b1976675443f647 @x2
@x3
@x4))
(and (HasTypeFuel @u0
@x1
Prims.nat)
(BoxBool_proj_0 (Prims.op_LessThanOrEqual @x2
@x1))
(BoxBool_proj_0 (Prims.op_LessThanOrEqual @x1
(FStar.Seq.Base.length @x3
@x4)))))
 

:pattern ((HasTypeFuel @u0
@x1
(Tm_refine_81407705a0828c2c1b1976675443f647 @x2
@x3
@x4)))
:qid refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647))

:named refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647))
(declare-fun FStar.Seq.Base.slice (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> i: Prims.nat -> j: Prims.nat{i <= j && j <= FStar.Seq.Base.length s}   -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_f59809c98fadf275c00ce819f5868628 () Term)
(declare-fun FStar.Seq.Base.slice@tok () Term)

; </end encoding FStar.Seq.Base.slice>


; <Start encoding FStar.Seq.Base.lemma_create_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_create_len (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_create_len@tok () Term)

; </end encoding FStar.Seq.Base.lemma_create_len>


; <Start encoding FStar.Seq.Base.lemma_init_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_init_len (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_init_len@tok () Term)



; </end encoding FStar.Seq.Base.lemma_init_len>


; <Start encoding FStar.Seq.Base.lemma_init_aux_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_init_aux_len (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_init_aux_len@tok () Term)




; </end encoding FStar.Seq.Base.lemma_init_aux_len>


; <Start encoding FStar.Seq.Base.lemma_init_ghost_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_init_ghost_len (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_init_ghost_len@tok () Term)



; </end encoding FStar.Seq.Base.lemma_init_ghost_len>


; <Start encoding FStar.Seq.Base.lemma_init_ghost_aux_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_init_ghost_aux_len (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_init_ghost_aux_len@tok () Term)




; </end encoding FStar.Seq.Base.lemma_init_ghost_aux_len>


; <Start encoding FStar.Seq.Base.lemma_len_upd>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_len_upd (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_len_upd@tok () Term)
(declare-fun Tm_refine_2ca062977a42c36634b89c1c4f193f79 (Term Term) Term)

; </end encoding FStar.Seq.Base.lemma_len_upd>


; <Start encoding FStar.Seq.Base.lemma_len_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_len_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_len_append@tok () Term)

; </end encoding FStar.Seq.Base.lemma_len_append>


; <Start encoding FStar.Seq.Base.lemma_len_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_len_slice (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_len_slice@tok () Term)

;;;;;;;;;;;;;;;;Lemma: FStar.Seq.Base.lemma_len_slice
;;; Fact-ids: Name FStar.Seq.Base.lemma_len_slice; Namespace FStar.Seq.Base
(assert (! (forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (implies (and (HasType @x0
Tm_type)
(HasType @x1
(FStar.Seq.Base.seq @x0))
(HasType @x2
Prims.nat)
(HasType @x3
(Tm_refine_81407705a0828c2c1b1976675443f647 @x2
@x0
@x1)))

;; def=FStar.Seq.Base.fsti(98,11-98,41); use=FStar.Seq.Base.fsti(98,11-98,41)
(= (FStar.Seq.Base.length @x0
(FStar.Seq.Base.slice @x0
@x1
@x2
@x3))
(Prims.op_Subtraction @x3
@x2))
)
 

:pattern ((FStar.Seq.Base.length @x0
(FStar.Seq.Base.slice @x0
@x1
@x2
@x3)))
:qid lemma_FStar.Seq.Base.lemma_len_slice))
:named lemma_FStar.Seq.Base.lemma_len_slice))

; </end encoding FStar.Seq.Base.lemma_len_slice>


; <Start encoding FStar.Seq.Base.lemma_index_create>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_create (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_create@tok () Term)


; </end encoding FStar.Seq.Base.lemma_index_create>


; <Start encoding FStar.Seq.Base.lemma_index_upd1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_upd1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_upd1@tok () Term)


; </end encoding FStar.Seq.Base.lemma_index_upd1>


; <Start encoding FStar.Seq.Base.lemma_index_upd2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_upd2 (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_upd2@tok () Term)

(declare-fun Tm_refine_df81b3f17797c6f405c1dbb191651292 (Term Term Term) Term)

; </end encoding FStar.Seq.Base.lemma_index_upd2>


; <Start encoding FStar.Seq.Base.lemma_index_app1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_app1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_app1@tok () Term)


; </end encoding FStar.Seq.Base.lemma_index_app1>


; <Start encoding FStar.Seq.Base.lemma_index_app2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_app2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_app2@tok () Term)
(declare-fun Tm_refine_ac201cf927190d39c033967b63cb957b (Term Term Term) Term)

; </end encoding FStar.Seq.Base.lemma_index_app2>


; <Start encoding FStar.Seq.Base.lemma_index_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_index_slice (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_index_slice@tok () Term)
(declare-fun Tm_refine_d3d07693cd71377864ef84dc97d10ec1 (Term Term Term) Term)
(declare-fun Tm_refine_35a0739c434508f48d0bb1d5cd5df9e8 (Term Term) Term)

; </end encoding FStar.Seq.Base.lemma_index_slice>


; <Start encoding FStar.Seq.Base.hasEq_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.hasEq_lemma (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.hasEq_lemma@tok () Term)

; </end encoding FStar.Seq.Base.hasEq_lemma>


; <Start encoding FStar.Seq.Base.equal>

(declare-fun FStar.Seq.Base.equal (Term Term Term) Term)
;;;;;;;;;;;;;;;;s1: FStar.Seq.Base.seq a -> s2: FStar.Seq.Base.seq a -> Prims.prop
(declare-fun Tm_arrow_c2c0a5f39eee7a5a92db8bac6fe4fb3b () Term)
(declare-fun FStar.Seq.Base.equal@tok () Term)

; </end encoding FStar.Seq.Base.equal>


; <Start encoding FStar.Seq.Base.eq_i>

(declare-fun Tm_refine_4639d389381bee5cf8cf77b7a6585074 (Term Term) Term)
(declare-fun Tm_refine_b361ba8089a6e963921008d537e799a1 (Term Term) Term)
(declare-fun FStar.Seq.Base.eq_i (Term Term Term Term) Term)


(declare-fun Tm_refine_331c14d442c5ee89a4fce6ea305c920f (Term Term Term) Term)
(declare-fun Tm_refine_51f956555266662f5f0ed4aac81d10bc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;s1: FStar.Seq.Base.seq a ->     s2: FStar.Seq.Base.seq a {FStar.Seq.Base.length s1 = FStar.Seq.Base.length s2} ->     i: Prims.nat{i <= FStar.Seq.Base.length s1}   -> r:     Prims.bool       { r <==>         (forall (j:             i:             Prims.int               {i >= 0 /\ i < FStar.Seq.Base.length s2 /\ (i >= 0) /\ (i < FStar.Seq.Base.length s1)})           .             j >= i /\ j < FStar.Seq.Base.length s1 ==>             FStar.Seq.Base.index s1 j = FStar.Seq.Base.index s2 j) }
(declare-fun Tm_arrow_e5286e13b5c071949ebc5146fbef7d7f () Term)
(declare-fun FStar.Seq.Base.eq_i@tok () Term)



; </end encoding FStar.Seq.Base.eq_i>


; <Start encoding FStar.Seq.Base.eq>

(declare-fun FStar.Seq.Base.eq (Term Term Term) Term)
(declare-fun Tm_refine_1c0effbdef48f9b00a1efb7b571fbb69 (Term Term Term) Term)
;;;;;;;;;;;;;;;;s1: FStar.Seq.Base.seq a -> s2: FStar.Seq.Base.seq a   -> r: Prims.bool{r <==> FStar.Seq.Base.equal s1 s2}
(declare-fun Tm_arrow_70ef1e4b9388d8aa6e0d17c5aeed02a7 () Term)
(declare-fun FStar.Seq.Base.eq@tok () Term)


; </end encoding FStar.Seq.Base.eq>


; <Start encoding FStar.Seq.Base.lemma_eq_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_eq_intro (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_eq_intro@tok () Term)


; </end encoding FStar.Seq.Base.lemma_eq_intro>


; <Start encoding FStar.Seq.Base.lemma_eq_refl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_eq_refl (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_eq_refl@tok () Term)

; </end encoding FStar.Seq.Base.lemma_eq_refl>


; <Start encoding FStar.Seq.Base.lemma_eq_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_eq_elim (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_eq_elim@tok () Term)

; </end encoding FStar.Seq.Base.lemma_eq_elim>


; <Start encoding FStar.Seq.Base.append_assoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.append_assoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.append_assoc@tok () Term)

; </end encoding FStar.Seq.Base.append_assoc>


; <Start encoding FStar.Seq.Base.append_empty_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.append_empty_l (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.append_empty_l@tok () Term)

; </end encoding FStar.Seq.Base.append_empty_l>


; <Start encoding FStar.Seq.Base.append_empty_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.append_empty_r (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.append_empty_r@tok () Term)

; </end encoding FStar.Seq.Base.append_empty_r>


; <Start encoding FStar.Seq.Base.init_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.init_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.init_index@tok () Term)

; </end encoding FStar.Seq.Base.init_index>


; <Start encoding FStar.Seq.Base.init_index_>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.init_index_ (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.init_index_@tok () Term)



; </end encoding FStar.Seq.Base.init_index_>


; <Start encoding FStar.Seq.Base.init_ghost_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.init_ghost_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.init_ghost_index@tok () Term)

; </end encoding FStar.Seq.Base.init_ghost_index>


; <Start encoding FStar.Seq.Base.init_ghost_index_>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.init_ghost_index_ (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.init_ghost_index_@tok () Term)



; </end encoding FStar.Seq.Base.init_ghost_index_>


; <Start encoding FStar.Seq.Base.lemma_equal_instances_implies_equal_types>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Base.lemma_equal_instances_implies_equal_types (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Base.lemma_equal_instances_implies_equal_types@tok () Term)

; </end encoding FStar.Seq.Base.lemma_equal_instances_implies_equal_types>


; End Externals for interface FStar.Seq.Base


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Seq.Base (264 decls; total size 20206)

;;; Start interface FStar.Seq.Properties

; Externals for interface FStar.Seq.Properties


; <Start encoding FStar.Seq.Properties.lseq>

(declare-fun FStar.Seq.Properties.lseq (Term Term) Term)

(declare-fun FStar.Seq.Properties.lseq@tok () Term)
(declare-fun Tm_refine_a0cd7d06c5da6444b6b51b319febde8e (Term Term) Term)

; </end encoding FStar.Seq.Properties.lseq>


; <Start encoding FStar.Seq.Properties.indexable>

(declare-fun FStar.Seq.Properties.indexable (Term Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> j: Prims.int -> Prims.logical
(declare-fun Tm_arrow_2c0367dd991d12c77178c7fe63f076c5 () Term)
(declare-fun FStar.Seq.Properties.indexable@tok () Term)

; </end encoding FStar.Seq.Properties.indexable>


; <Start encoding FStar.Seq.Properties.lemma_append_inj_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj_l (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj_l@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_inj_l>


; <Start encoding FStar.Seq.Properties.lemma_append_inj_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj_r (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj_r@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_inj_r>


; <Start encoding FStar.Seq.Properties.lemma_append_len_disj>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_len_disj (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_len_disj@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_len_disj>


; <Start encoding FStar.Seq.Properties.lemma_append_inj>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_inj@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_inj>


; <Start encoding FStar.Seq.Properties.head>

(declare-fun Tm_refine_167ef714932ec832fb671890fc3eee6c (Term) Term)
(declare-fun FStar.Seq.Properties.head (Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a {FStar.Seq.Base.length s > 0} -> a
(declare-fun Tm_arrow_fde6b9111cb8aaf87a1b6689af62ed69 () Term)
(declare-fun FStar.Seq.Properties.head@tok () Term)


; </end encoding FStar.Seq.Properties.head>


; <Start encoding FStar.Seq.Properties.tail>


(declare-fun FStar.Seq.Properties.tail (Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a {FStar.Seq.Base.length s > 0} -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_3db93b3d63ab329f9ab58ee76fda4c87 () Term)
(declare-fun FStar.Seq.Properties.tail@tok () Term)


; </end encoding FStar.Seq.Properties.tail>


; <Start encoding FStar.Seq.Properties.lemma_head_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_head_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_head_append@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_head_append>


; <Start encoding FStar.Seq.Properties.lemma_tail_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_append@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_tail_append>


; <Start encoding FStar.Seq.Properties.last>


(declare-fun FStar.Seq.Properties.last (Term Term) Term)


(declare-fun FStar.Seq.Properties.last@tok () Term)


; </end encoding FStar.Seq.Properties.last>


; <Start encoding FStar.Seq.Properties.cons>

(declare-fun FStar.Seq.Properties.cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> s: FStar.Seq.Base.seq a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_62ad6018b578ef7ed3c0e74bdebff729 () Term)
(declare-fun FStar.Seq.Properties.cons@tok () Term)

; </end encoding FStar.Seq.Properties.cons>


; <Start encoding FStar.Seq.Properties.lemma_cons_inj>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_cons_inj (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_cons_inj@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_cons_inj>


; <Start encoding FStar.Seq.Properties.split>

(declare-fun Tm_refine_17631fa6304dcc08d028bd475a6dd078 (Term Term) Term)
(declare-fun FStar.Seq.Properties.split (Term Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> i: Prims.nat{0 <= i /\ i <= FStar.Seq.Base.length s}   -> FStar.Seq.Base.seq a * FStar.Seq.Base.seq a
(declare-fun Tm_arrow_e8094a245058e1a3364fcb54e52c4b61 () Term)
(declare-fun FStar.Seq.Properties.split@tok () Term)


; </end encoding FStar.Seq.Properties.split>


; <Start encoding FStar.Seq.Properties.lemma_split>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_split (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_split@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_split>


; <Start encoding FStar.Seq.Properties.split_eq>


(declare-fun FStar.Seq.Properties.split_eq (Term Term Term) Term)

(declare-fun Tm_refine_78d42c5dbba01ee594272daa6bb0579c (Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> i: Prims.nat{0 <= i /\ i <= FStar.Seq.Base.length s}   -> Prims.Pure (FStar.Seq.Base.seq a * FStar.Seq.Base.seq a)
(declare-fun Tm_arrow_b88932abf1506cfe956c7a113bc65f4b () Term)
(declare-fun FStar.Seq.Properties.split_eq@tok () Term)



; </end encoding FStar.Seq.Properties.split_eq>


; <Start encoding FStar.Seq.Properties.count>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.count.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.count.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.count (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.count@tok () Term)
;;;;;;;;;;;;;;;;x: a -> s: FStar.Seq.Base.seq a -> Prims.Tot Prims.nat
(declare-fun Tm_arrow_b68daf91c98458f9ea85290d85674a2e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.count; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(73,8-73,13); use=FStar.Seq.Properties.fsti(73,8-73,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.count.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.count.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.count.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.count.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.count.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.count; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(73,8-73,13); use=FStar.Seq.Properties.fsti(73,8-73,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.count @x0
@x1
@x2)
(FStar.Seq.Properties.count.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.count @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.count.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.count.fuel_instrumented))

; </end encoding FStar.Seq.Properties.count>


; <Start encoding FStar.Seq.Properties.mem>

(declare-fun FStar.Seq.Properties.mem (Term Term Term) Term)
;;;;;;;;;;;;;;;;x: a -> l: FStar.Seq.Base.seq a -> Prims.bool
(declare-fun Tm_arrow_8b9021eb78c56c0f1820182c3a3e44b5 () Term)
(declare-fun FStar.Seq.Properties.mem@tok () Term)

; </end encoding FStar.Seq.Properties.mem>


; <Start encoding FStar.Seq.Properties.mem_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.mem_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.mem_index@tok () Term)

; </end encoding FStar.Seq.Properties.mem_index>


; <Start encoding FStar.Seq.Properties.index_mem>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.index_mem.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.index_mem.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.index_mem (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.index_mem@tok () Term)
(declare-fun Tm_refine_7c92df3cf71635bc41483532e738d828 (Term Term Term) Term)

;;;;;;;;;;;;;;;;x: a -> s: FStar.Seq.Base.seq a -> Prims.Pure Prims.nat
(declare-fun Tm_arrow_12def5646e9a05cc547dd67c2eeaec45 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.index_mem; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(88,8-88,17); use=FStar.Seq.Properties.fsti(88,8-88,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.index_mem.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.index_mem.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.index_mem.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.index_mem.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.index_mem.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.index_mem; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(88,8-88,17); use=FStar.Seq.Properties.fsti(88,8-88,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.index_mem @x0
@x1
@x2)
(FStar.Seq.Properties.index_mem.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.index_mem @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.index_mem.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.index_mem.fuel_instrumented))

; </end encoding FStar.Seq.Properties.index_mem>


; <Start encoding FStar.Seq.Properties.swap>



(declare-fun FStar.Seq.Properties.swap (Term Term Term Term) Term)


;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a ->     i: Prims.nat{i < FStar.Seq.Base.length s} ->     j: Prims.nat{j < FStar.Seq.Base.length s}   -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_ed5530d89236443143d2d084ddc97069 () Term)
(declare-fun FStar.Seq.Properties.swap@tok () Term)



; </end encoding FStar.Seq.Properties.swap>


; <Start encoding FStar.Seq.Properties.lemma_slice_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_append@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_slice_append>


; <Start encoding FStar.Seq.Properties.lemma_slice_first_in_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_first_in_append (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_first_in_append@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_slice_first_in_append>


; <Start encoding FStar.Seq.Properties.slice_upd>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.slice_upd (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.slice_upd@tok () Term)



; </end encoding FStar.Seq.Properties.slice_upd>


; <Start encoding FStar.Seq.Properties.upd_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.upd_slice (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.upd_slice@tok () Term)



; </end encoding FStar.Seq.Properties.upd_slice>


; <Start encoding FStar.Seq.Properties.lemma_append_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_cons@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_cons>


; <Start encoding FStar.Seq.Properties.lemma_tl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_tl (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_tl@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_tl>


; <Start encoding FStar.Seq.Properties.sorted>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.sorted.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.sorted.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.sorted (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.sorted@tok () Term)



;;;;;;;;;;;;;;;;f: (_: a -> _: a -> Prims.bool) -> s: FStar.Seq.Base.seq a -> Prims.Tot Prims.bool
(declare-fun Tm_arrow_28685b742721099a6ab3847e4434a96d () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.sorted; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(125,8-125,14); use=FStar.Seq.Properties.fsti(125,8-125,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.sorted.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.sorted.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.sorted.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.sorted.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.sorted.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.sorted; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(125,8-125,14); use=FStar.Seq.Properties.fsti(125,8-125,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.sorted @x0
@x1
@x2)
(FStar.Seq.Properties.sorted.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.sorted @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.sorted.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.sorted.fuel_instrumented))

; </end encoding FStar.Seq.Properties.sorted>


; <Start encoding FStar.Seq.Properties.sorted_feq>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.sorted_feq (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.sorted_feq@tok () Term)

; </end encoding FStar.Seq.Properties.sorted_feq>


; <Start encoding FStar.Seq.Properties.lemma_append_count>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_count (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_count@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_count>


; <Start encoding FStar.Seq.Properties.lemma_append_count_aux>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_append_count_aux (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_append_count_aux@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_append_count_aux>


; <Start encoding FStar.Seq.Properties.lemma_mem_inversion>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_inversion (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_inversion@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_mem_inversion>


; <Start encoding FStar.Seq.Properties.lemma_mem_count>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_count (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_count@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_mem_count>


; <Start encoding FStar.Seq.Properties.lemma_count_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_count_slice (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_count_slice@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_count_slice>


; <Start encoding FStar.Seq.Properties.total_order>


(declare-fun FStar.Seq.Properties.total_order (Term Term) Term)

;;;;;;;;;;;;;;;;a: Prims.eqtype -> f: (_: a -> _: a -> Prims.bool) -> Prims.logical
(declare-fun Tm_arrow_1118b25cace7451b1e5dfdfe482dbb64 () Term)
(declare-fun FStar.Seq.Properties.total_order@tok () Term)





(declare-fun Tm_abs_01f32e6aaf7ac0d0cc4b683776152cee (Term Term Term) Term)

(declare-fun Tm_abs_cbb67fe00406c5e25c3d7764e9189af1 (Term Term) Term)








; </end encoding FStar.Seq.Properties.total_order>


; <Start encoding FStar.Seq.Properties.tot_ord>

(declare-fun FStar.Seq.Properties.tot_ord (Term) Term)

(declare-fun FStar.Seq.Properties.tot_ord@tok () Term)

(declare-fun Tm_refine_a01e88865b4bbd2f0a4bcb261b6760a8 (Term) Term)

; </end encoding FStar.Seq.Properties.tot_ord>


; <Start encoding FStar.Seq.Properties.sorted_concat_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.sorted_concat_lemma (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.sorted_concat_lemma@tok () Term)

; </end encoding FStar.Seq.Properties.sorted_concat_lemma>


; <Start encoding FStar.Seq.Properties.split_5>

(declare-fun Tm_refine_55108d29d63192475ca95f591039cc18 (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.split_5 (Term Term Term Term) Term)

(declare-fun Tm_refine_03fdfb031367b218884098aa9d386676 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> i: Prims.nat -> j: Prims.nat{i < j && j < FStar.Seq.Base.length s}   -> Prims.Pure (FStar.Seq.Base.seq (FStar.Seq.Base.seq a))
(declare-fun Tm_arrow_1ab34f107de5525c681399e3c671c330 () Term)
(declare-fun FStar.Seq.Properties.split_5@tok () Term)


; </end encoding FStar.Seq.Properties.split_5>


; <Start encoding FStar.Seq.Properties.lemma_swap_permutes_aux_frag_eq>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_aux_frag_eq (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_aux_frag_eq@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_permutes_aux_frag_eq>


; <Start encoding FStar.Seq.Properties.lemma_swap_permutes_aux>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_aux (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_aux@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_permutes_aux>


; <Start encoding FStar.Seq.Properties.permutation>

(declare-fun FStar.Seq.Properties.permutation (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: Prims.eqtype -> s1: FStar.Seq.Base.seq a -> s2: FStar.Seq.Base.seq a -> Prims.logical
(declare-fun Tm_arrow_05517904f5779069bb79d90a352f1386 () Term)
(declare-fun FStar.Seq.Properties.permutation@tok () Term)

(declare-fun Tm_abs_0de6cd599146a4faa203cbf0596fd5e3 (Term Term Term) Term)

; </end encoding FStar.Seq.Properties.permutation>


; <Start encoding FStar.Seq.Properties.lemma_swap_permutes>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_permutes>


; <Start encoding FStar.Seq.Properties.perm_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.perm_len (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.perm_len@tok () Term)

; </end encoding FStar.Seq.Properties.perm_len>


; <Start encoding FStar.Seq.Properties.cons_perm>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.cons_perm (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.cons_perm@tok () Term)

; </end encoding FStar.Seq.Properties.cons_perm>


; <Start encoding FStar.Seq.Properties.lemma_mem_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_append (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_append@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_mem_append>


; <Start encoding FStar.Seq.Properties.lemma_slice_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_cons (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_cons@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_slice_cons>


; <Start encoding FStar.Seq.Properties.lemma_slice_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_snoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_slice_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_slice_snoc>


; <Start encoding FStar.Seq.Properties.lemma_ordering_lo_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_ordering_lo_snoc (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_ordering_lo_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_ordering_lo_snoc>


; <Start encoding FStar.Seq.Properties.lemma_ordering_hi_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_ordering_hi_cons (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_ordering_hi_cons@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_ordering_hi_cons>


; <Start encoding FStar.Seq.Properties.swap_frame_lo>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.swap_frame_lo (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.swap_frame_lo@tok () Term)

; </end encoding FStar.Seq.Properties.swap_frame_lo>


; <Start encoding FStar.Seq.Properties.swap_frame_lo'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.swap_frame_lo_ (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.swap_frame_lo_@tok () Term)

; </end encoding FStar.Seq.Properties.swap_frame_lo'>


; <Start encoding FStar.Seq.Properties.swap_frame_hi>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.swap_frame_hi (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.swap_frame_hi@tok () Term)

; </end encoding FStar.Seq.Properties.swap_frame_hi>


; <Start encoding FStar.Seq.Properties.lemma_swap_slice_commute>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_slice_commute (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_slice_commute@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_slice_commute>


; <Start encoding FStar.Seq.Properties.lemma_swap_permutes_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_slice (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_permutes_slice@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_permutes_slice>


; <Start encoding FStar.Seq.Properties.splice>



(declare-fun FStar.Seq.Properties.splice (Term Term Term Term Term) Term)


;;;;;;;;;;;;;;;;s1: FStar.Seq.Base.seq a ->     i: Prims.nat ->     s2: FStar.Seq.Base.seq a {FStar.Seq.Base.length s1 = FStar.Seq.Base.length s2} ->     j: Prims.nat{i <= j /\ j <= FStar.Seq.Base.length s2}   -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_c43a25ef505b9db21532cdb95f3c9f68 () Term)
(declare-fun FStar.Seq.Properties.splice@tok () Term)



; </end encoding FStar.Seq.Properties.splice>


; <Start encoding FStar.Seq.Properties.replace_subseq>


(declare-fun Tm_refine_5542011d20872a6178aad9a072f1b686 (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.replace_subseq (Term Term Term Term Term) Term)


;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a ->     i: Prims.nat ->     j: Prims.nat{i <= j /\ j <= FStar.Seq.Base.length s} ->     sub: FStar.Seq.Base.seq a {FStar.Seq.Base.length sub == j - i}   -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_9fa775abc8f8f9c4e6df626212cddc6a () Term)
(declare-fun FStar.Seq.Properties.replace_subseq@tok () Term)



; </end encoding FStar.Seq.Properties.replace_subseq>


; <Start encoding FStar.Seq.Properties.splice_refl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.splice_refl (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.splice_refl@tok () Term)

; </end encoding FStar.Seq.Properties.splice_refl>


; <Start encoding FStar.Seq.Properties.lemma_swap_splice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_splice (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_swap_splice@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_swap_splice>


; <Start encoding FStar.Seq.Properties.lemma_seq_frame_hi>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_frame_hi (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_frame_hi@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_frame_hi>


; <Start encoding FStar.Seq.Properties.lemma_seq_frame_lo>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_frame_lo (Term Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_frame_lo@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_frame_lo>


; <Start encoding FStar.Seq.Properties.lemma_tail_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_slice (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_slice@tok () Term)
(declare-fun Tm_refine_b138bd5848d4184f7632587e6e4bcf9f (Term Term Term) Term)

; </end encoding FStar.Seq.Properties.lemma_tail_slice>


; <Start encoding FStar.Seq.Properties.lemma_weaken_frame_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_frame_right (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_frame_right@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_weaken_frame_right>


; <Start encoding FStar.Seq.Properties.lemma_weaken_frame_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_frame_left (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_frame_left@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_weaken_frame_left>


; <Start encoding FStar.Seq.Properties.lemma_trans_frame>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_trans_frame (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_trans_frame@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_trans_frame>


; <Start encoding FStar.Seq.Properties.lemma_weaken_perm_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_perm_left (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_perm_left@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_weaken_perm_left>


; <Start encoding FStar.Seq.Properties.lemma_weaken_perm_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_perm_right (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_weaken_perm_right@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_weaken_perm_right>


; <Start encoding FStar.Seq.Properties.lemma_trans_perm>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_trans_perm (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_trans_perm@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_trans_perm>


; <Start encoding FStar.Seq.Properties.snoc>

(declare-fun FStar.Seq.Properties.snoc (Term Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> x: a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_f9b27de7c4505538c6110afe14403cc8 () Term)
(declare-fun FStar.Seq.Properties.snoc@tok () Term)

; </end encoding FStar.Seq.Properties.snoc>


; <Start encoding FStar.Seq.Properties.lemma_cons_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_cons_snoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_cons_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_cons_snoc>


; <Start encoding FStar.Seq.Properties.lemma_tail_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_snoc (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_tail_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_tail_snoc>


; <Start encoding FStar.Seq.Properties.lemma_snoc_inj>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_snoc_inj (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_snoc_inj@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_snoc_inj>


; <Start encoding FStar.Seq.Properties.lemma_mem_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_snoc (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_mem_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_mem_snoc>


; <Start encoding FStar.Seq.Properties.find_l>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.find_l.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.find_l.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.find_l (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.find_l@tok () Term)

(declare-fun Tm_refine_aba7638072c8f1ba6a01b95ec6f9a485 (Term Term) Term)



;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: FStar.Seq.Base.seq a   -> Prims.Tot (o: FStar.Pervasives.Native.option a {Some? o ==> f (Some?.v o)})
(declare-fun Tm_arrow_fd183dc9552028fd54abfbe4a84f515a () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.find_l; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(330,8-330,14); use=FStar.Seq.Properties.fsti(330,8-330,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.find_l.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.find_l.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.find_l.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.find_l.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.find_l.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.find_l; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(330,8-330,14); use=FStar.Seq.Properties.fsti(330,8-330,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.find_l @x0
@x1
@x2)
(FStar.Seq.Properties.find_l.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.find_l @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.find_l.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.find_l.fuel_instrumented))

; </end encoding FStar.Seq.Properties.find_l>


; <Start encoding FStar.Seq.Properties.ghost_find_l>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.ghost_find_l.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.ghost_find_l.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.ghost_find_l (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.ghost_find_l@tok () Term)
;;;;;;;;;;;;;;;;_: a -> Prims.GTot Prims.bool
(declare-fun Tm_ghost_arrow_9a34a9deaac3ca72ad48c3ec79b6656c (Term) Term)




;;;;;;;;;;;;;;;;f: (_: a -> Prims.GTot Prims.bool) -> l: FStar.Seq.Base.seq a   -> Prims.GTot (o: FStar.Pervasives.Native.option a {Some? o ==> f (Some?.v o)})
(declare-fun Tm_ghost_arrow_3f8a537d0d54200d690f80a370cf9031 () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.ghost_find_l; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(337,8-337,20); use=FStar.Seq.Properties.fsti(337,8-337,20)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.ghost_find_l.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.ghost_find_l.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.ghost_find_l.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.ghost_find_l.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.ghost_find_l.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.ghost_find_l; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(337,8-337,20); use=FStar.Seq.Properties.fsti(337,8-337,20)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.ghost_find_l @x0
@x1
@x2)
(FStar.Seq.Properties.ghost_find_l.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.ghost_find_l @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.ghost_find_l.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.ghost_find_l.fuel_instrumented))

; </end encoding FStar.Seq.Properties.ghost_find_l>


; <Start encoding FStar.Seq.Properties.find_append_some>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_append_some (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_append_some@tok () Term)

; </end encoding FStar.Seq.Properties.find_append_some>


; <Start encoding FStar.Seq.Properties.find_append_none>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_append_none (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_append_none@tok () Term)

; </end encoding FStar.Seq.Properties.find_append_none>


; <Start encoding FStar.Seq.Properties.find_append_none_s2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_append_none_s2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_append_none_s2@tok () Term)

; </end encoding FStar.Seq.Properties.find_append_none_s2>


; <Start encoding FStar.Seq.Properties.find_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_snoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.find_snoc>


; <Start encoding FStar.Seq.Properties.un_snoc>

(declare-fun Tm_refine_5739deb21d8cba89243fec27b35b7ef0 (Term) Term)
(declare-fun FStar.Seq.Properties.un_snoc (Term Term) Term)

(declare-fun Tm_refine_16326afaeb5f4d93ab294cc4a965de3e (Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a {FStar.Seq.Base.length s <> 0}   -> r:     (FStar.Seq.Base.seq a * a)       { s ==         FStar.Seq.Properties.snoc (FStar.Pervasives.Native.fst r) (FStar.Pervasives.Native.snd r) }
(declare-fun Tm_arrow_30c2910b2510bbce2598a79ba00a0209 () Term)
(declare-fun FStar.Seq.Properties.un_snoc@tok () Term)



; </end encoding FStar.Seq.Properties.un_snoc>


; <Start encoding FStar.Seq.Properties.un_snoc_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.un_snoc_snoc (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.un_snoc_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.un_snoc_snoc>


; <Start encoding FStar.Seq.Properties.find_r>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.find_r.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.find_r.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.find_r (Term Term Term) Term)
(declare-fun FStar.Seq.Properties.find_r@tok () Term)







;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.find_r; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(369,8-369,14); use=FStar.Seq.Properties.fsti(369,8-369,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.find_r.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.Seq.Properties.find_r.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.find_r.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.Seq.Properties.find_r.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.find_r.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.find_r; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(369,8-369,14); use=FStar.Seq.Properties.fsti(369,8-369,14)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.find_r @x0
@x1
@x2)
(FStar.Seq.Properties.find_r.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.find_r @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.Seq.Properties.find_r.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.find_r.fuel_instrumented))

; </end encoding FStar.Seq.Properties.find_r>


; <Start encoding FStar.Seq.Properties.found>

(declare-fun FStar.Seq.Properties.found (Term) Term)
;;;;;;;;;;;;;;;;i: Prims.nat -> Prims.logical
(declare-fun Tm_arrow_591bcdc53dc583ecc77b1bc5436f9a59 () Term)
(declare-fun FStar.Seq.Properties.found@tok () Term)

; </end encoding FStar.Seq.Properties.found>


; <Start encoding FStar.Seq.Properties.seq_find_aux>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.seq_find_aux.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.seq_find_aux.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.seq_find_aux (Term Term Term Term) Term)
(declare-fun FStar.Seq.Properties.seq_find_aux@tok () Term)


(declare-fun Tm_refine_564e05c43cb7c1f4e1de1a4fb2fd28c8 (Term Term Term) Term)


(declare-fun Tm_refine_5e8ab89510578a938a38bd5dfb813b93 (Term Term Term) Term)









;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: FStar.Seq.Base.seq a -> ctr: Prims.nat{ctr <= FStar.Seq.Base.length l}   -> Prims.Pure (FStar.Pervasives.Native.option a)
(declare-fun Tm_arrow_c2dc7826f3281f473541797eb2d714b7 () Term)



;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.seq_find_aux; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(379,8-379,20); use=FStar.Seq.Properties.fsti(379,8-379,20)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.Seq.Properties.seq_find_aux.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.Seq.Properties.seq_find_aux.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.Seq.Properties.seq_find_aux.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.Seq.Properties.seq_find_aux.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.seq_find_aux.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.seq_find_aux; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(379,8-379,20); use=FStar.Seq.Properties.fsti(379,8-379,20)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.seq_find_aux @x0
@x1
@x2
@x3)
(FStar.Seq.Properties.seq_find_aux.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.seq_find_aux @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.Seq.Properties.seq_find_aux.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.seq_find_aux.fuel_instrumented))

; </end encoding FStar.Seq.Properties.seq_find_aux>


; <Start encoding FStar.Seq.Properties.seq_find>


(declare-fun FStar.Seq.Properties.seq_find (Term Term Term) Term)




;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: FStar.Seq.Base.seq a -> Prims.Pure (FStar.Pervasives.Native.option a)
(declare-fun Tm_arrow_fa1508f34cdf614c6772d290f5ddf827 () Term)
(declare-fun FStar.Seq.Properties.seq_find@tok () Term)





; </end encoding FStar.Seq.Properties.seq_find>


; <Start encoding FStar.Seq.Properties.find_mem>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_mem (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_mem@tok () Term)

; </end encoding FStar.Seq.Properties.find_mem>


; <Start encoding FStar.Seq.Properties.for_all>


(declare-fun FStar.Seq.Properties.for_all (Term Term Term) Term)


(declare-fun Tm_refine_307fd373d8b3749096cf164b41cf1984 (Term Term Term) Term)
;;;;;;;;;;;;;;;;f: (_: a -> Prims.bool) -> l: FStar.Seq.Base.seq a -> Prims.Pure Prims.bool
(declare-fun Tm_arrow_098d0ddce18f722cb743337c9d7dd0b9 () Term)
(declare-fun FStar.Seq.Properties.for_all@tok () Term)




(declare-fun Tm_abs_e818836335067047224d0c19c4cabb2d (Term Term) Term)

; </end encoding FStar.Seq.Properties.for_all>


; <Start encoding FStar.Seq.Properties.seq_mem_k>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.seq_mem_k (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.seq_mem_k@tok () Term)


; </end encoding FStar.Seq.Properties.seq_mem_k>


; <Start encoding FStar.Seq.Properties.seq_to_list>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.seq_to_list.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.seq_to_list.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.seq_to_list (Term Term) Term)
(declare-fun FStar.Seq.Properties.seq_to_list@tok () Term)
(declare-fun Tm_refine_c4e3a92f9bd1d01a07e4fb66c5de2e7e (Term Term) Term)

;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a   -> Prims.Tot (l: Prims.list a {FStar.List.Tot.Base.length l = FStar.Seq.Base.length s})
(declare-fun Tm_arrow_7d1aeb9cf9244f8c50e0ad901486a03b () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.seq_to_list; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(425,8-425,19); use=FStar.Seq.Properties.fsti(425,8-425,19)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.Seq.Properties.seq_to_list.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.Seq.Properties.seq_to_list.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.Seq.Properties.seq_to_list.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.Seq.Properties.seq_to_list.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.seq_to_list.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.seq_to_list; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(425,8-425,19); use=FStar.Seq.Properties.fsti(425,8-425,19)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.Seq.Properties.seq_to_list @x0
@x1)
(FStar.Seq.Properties.seq_to_list.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.Seq.Properties.seq_to_list @x0
@x1))
:qid @fuel_correspondence_FStar.Seq.Properties.seq_to_list.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.seq_to_list.fuel_instrumented))

; </end encoding FStar.Seq.Properties.seq_to_list>


; <Start encoding FStar.Seq.Properties.seq_of_list>

(declare-fun FStar.Seq.Properties.seq_of_list (Term Term) Term)
(declare-fun Tm_refine_d2d1ea66f2b3a92c2deb42edcbb784ce (Term Term) Term)
;;;;;;;;;;;;;;;;l: Prims.list a -> s: FStar.Seq.Base.seq a {FStar.List.Tot.Base.length l = FStar.Seq.Base.length s}
(declare-fun Tm_arrow_4966fa2986a35d9c0803c863a2768cbd () Term)
(declare-fun FStar.Seq.Properties.seq_of_list@tok () Term)


; </end encoding FStar.Seq.Properties.seq_of_list>


; <Start encoding FStar.Seq.Properties.lemma_seq_of_list_induction>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_induction (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_induction@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_of_list_induction>


; <Start encoding FStar.Seq.Properties.lemma_seq_list_bij>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_list_bij (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_list_bij@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_list_bij>


; <Start encoding FStar.Seq.Properties.lemma_list_seq_bij>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_list_seq_bij (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_list_seq_bij@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_list_seq_bij>


; <Start encoding FStar.Seq.Properties.createL_post>

(declare-fun FStar.Seq.Properties.createL_post (Term Term Term) Term)
;;;;;;;;;;;;;;;;l: Prims.list a -> s: FStar.Seq.Base.seq a -> Prims.GTot Type
(declare-fun Tm_arrow_befeea9093c61a572da65bfe7ce35cff () Term)
(declare-fun FStar.Seq.Properties.createL_post@tok () Term)
(declare-fun Tm_refine_1780a0fddfda88c43d203b562c6d3f5b () Term)
(declare-fun Tm_refine_8c2e524fd0f5ac690c4c816ad7d8a461 (Term Term Term) Term)


; </end encoding FStar.Seq.Properties.createL_post>


; <Start encoding FStar.Seq.Properties.createL>

(declare-fun FStar.Seq.Properties.createL (Term Term) Term)

(declare-fun Tm_refine_29f54a8a92d732b7f4111928d707db68 (Term Term) Term)
;;;;;;;;;;;;;;;;l: Prims.list a -> Prims.Pure (FStar.Seq.Base.seq a)
(declare-fun Tm_arrow_6a7bb2ee242e4d89b8744d9965334de3 () Term)
(declare-fun FStar.Seq.Properties.createL@tok () Term)



; </end encoding FStar.Seq.Properties.createL>


; <Start encoding FStar.Seq.Properties.lemma_index_is_nth>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_index_is_nth (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_index_is_nth@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_index_is_nth>


; <Start encoding FStar.Seq.Properties.contains>

(declare-fun FStar.Seq.Properties.contains (Term Term Term) Term)
;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq a -> x: a -> Type
(declare-fun Tm_arrow_65d0102b1211a5d233193433129106a1 () Term)
(declare-fun FStar.Seq.Properties.contains@tok () Term)

; </end encoding FStar.Seq.Properties.contains>


; <Start encoding FStar.Seq.Properties.contains_intro>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.contains_intro (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.contains_intro@tok () Term)

; </end encoding FStar.Seq.Properties.contains_intro>


; <Start encoding FStar.Seq.Properties.contains_elim>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.contains_elim (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.contains_elim@tok () Term)

; </end encoding FStar.Seq.Properties.contains_elim>


; <Start encoding FStar.Seq.Properties.lemma_contains_empty>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_contains_empty (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_contains_empty@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_contains_empty>


; <Start encoding FStar.Seq.Properties.lemma_contains_singleton>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_contains_singleton (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_contains_singleton@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_contains_singleton>


; <Start encoding FStar.Seq.Properties.append_contains_equiv>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.append_contains_equiv (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.append_contains_equiv@tok () Term)

; </end encoding FStar.Seq.Properties.append_contains_equiv>


; <Start encoding FStar.Seq.Properties.contains_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.contains_snoc (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.contains_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.contains_snoc>


; <Start encoding FStar.Seq.Properties.lemma_find_l_contains>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_find_l_contains (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_find_l_contains@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_find_l_contains>


; <Start encoding FStar.Seq.Properties.contains_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.contains_cons (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.contains_cons@tok () Term)

; </end encoding FStar.Seq.Properties.contains_cons>


; <Start encoding FStar.Seq.Properties.append_cons_snoc>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.append_cons_snoc (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.append_cons_snoc@tok () Term)

; </end encoding FStar.Seq.Properties.append_cons_snoc>


; <Start encoding FStar.Seq.Properties.append_slices>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.append_slices (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.append_slices@tok () Term)

; </end encoding FStar.Seq.Properties.append_slices>


; <Start encoding FStar.Seq.Properties.find_l_none_no_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.find_l_none_no_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.find_l_none_no_index@tok () Term)

; </end encoding FStar.Seq.Properties.find_l_none_no_index>


; <Start encoding FStar.Seq.Properties.suffix_of>

(declare-fun FStar.Seq.Properties.suffix_of (Term Term Term) Term)
;;;;;;;;;;;;;;;;s_suff: FStar.Seq.Base.seq a -> s: FStar.Seq.Base.seq a -> Prims.logical
(declare-fun Tm_arrow_2ed6082b86d605508c94c4b8a46966f5 () Term)
(declare-fun FStar.Seq.Properties.suffix_of@tok () Term)
;;;;;;;;;;;;;;;;s_pref: FStar.Seq.Base.seq a -> Prims.GTot Type
(declare-fun Tm_arrow_1b131a144041a50a604cc69ec18e173a (Term) Term)
(declare-fun Tm_abs_1a72f90f7650698378a85f71c4dab2bc (Term Term Term) Term)

; </end encoding FStar.Seq.Properties.suffix_of>


; <Start encoding FStar.Seq.Properties.cons_head_tail>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.cons_head_tail (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.cons_head_tail@tok () Term)


; </end encoding FStar.Seq.Properties.cons_head_tail>


; <Start encoding FStar.Seq.Properties.head_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.head_cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.head_cons@tok () Term)

; </end encoding FStar.Seq.Properties.head_cons>


; <Start encoding FStar.Seq.Properties.suffix_of_tail>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.suffix_of_tail (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.suffix_of_tail@tok () Term)


; </end encoding FStar.Seq.Properties.suffix_of_tail>


; <Start encoding FStar.Seq.Properties.index_cons_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.index_cons_l (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.index_cons_l@tok () Term)

; </end encoding FStar.Seq.Properties.index_cons_l>


; <Start encoding FStar.Seq.Properties.index_cons_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.index_cons_r (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.index_cons_r@tok () Term)

; </end encoding FStar.Seq.Properties.index_cons_r>


; <Start encoding FStar.Seq.Properties.append_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.append_cons (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.append_cons@tok () Term)

; </end encoding FStar.Seq.Properties.append_cons>


; <Start encoding FStar.Seq.Properties.index_tail>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.index_tail (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.index_tail@tok () Term)

; </end encoding FStar.Seq.Properties.index_tail>


; <Start encoding FStar.Seq.Properties.mem_cons>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.mem_cons (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.mem_cons@tok () Term)

; </end encoding FStar.Seq.Properties.mem_cons>


; <Start encoding FStar.Seq.Properties.snoc_slice_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.snoc_slice_index (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.snoc_slice_index@tok () Term)
(declare-fun Tm_refine_095c5722edf0f79bcd7dce7bd084c7b5 (Term Term Term) Term)

; </end encoding FStar.Seq.Properties.snoc_slice_index>


; <Start encoding FStar.Seq.Properties.cons_index_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.cons_index_slice (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.cons_index_slice@tok () Term)
(declare-fun Tm_refine_09d2e9ab3b9c121b24316d151747e281 (Term Term Term) Term)
(declare-fun Tm_refine_ddd44b85040d1947cca83550b7e21966 (Term) Term)

; </end encoding FStar.Seq.Properties.cons_index_slice>


; <Start encoding FStar.Seq.Properties.slice_is_empty>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.slice_is_empty (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.slice_is_empty@tok () Term)


; </end encoding FStar.Seq.Properties.slice_is_empty>


; <Start encoding FStar.Seq.Properties.slice_length>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.slice_length (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.slice_length@tok () Term)

; </end encoding FStar.Seq.Properties.slice_length>


; <Start encoding FStar.Seq.Properties.slice_slice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.slice_slice (Term Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.slice_slice@tok () Term)

(declare-fun Tm_refine_1ba8fd8bb363097813064c67740b2de5 (Term Term Term) Term)

; </end encoding FStar.Seq.Properties.slice_slice>


; <Start encoding FStar.Seq.Properties.lemma_seq_of_list_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_index (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_index@tok () Term)


; </end encoding FStar.Seq.Properties.lemma_seq_of_list_index>


; <Start encoding FStar.Seq.Properties.of_list>

(declare-fun FStar.Seq.Properties.of_list (Term Term) Term)
;;;;;;;;;;;;;;;;l: Prims.list a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_474463878fff5c7c9c02e4f0b8b84aa8 () Term)
(declare-fun FStar.Seq.Properties.of_list@tok () Term)

; </end encoding FStar.Seq.Properties.of_list>


; <Start encoding FStar.Seq.Properties.seq_of_list_tl>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.seq_of_list_tl (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.seq_of_list_tl@tok () Term)

; </end encoding FStar.Seq.Properties.seq_of_list_tl>


; <Start encoding FStar.Seq.Properties.mem_seq_of_list>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.mem_seq_of_list (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.mem_seq_of_list@tok () Term)

; </end encoding FStar.Seq.Properties.mem_seq_of_list>


; <Start encoding FStar.Seq.Properties.explode_and>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.explode_and.fuel_instrumented (Fuel Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.explode_and.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.explode_and (Term Term Term Term) Term)
(declare-fun FStar.Seq.Properties.explode_and@tok () Term)
(declare-fun Tm_refine_5885c715bf599d471c43c6b7dcb2413b (Term Term) Term)
(declare-fun Tm_refine_c731267dd71b747abfd9fc75f6f2da81 (Term Term Term) Term)




;;;;;;;;;;;;;;;;i: Prims.nat ->     s: FStar.Seq.Base.seq a {i <= FStar.Seq.Base.length s} ->     l: Prims.list a {FStar.List.Tot.Base.length l + i = FStar.Seq.Base.length s}   -> Prims.Tot Type
(declare-fun Tm_arrow_62bce6f622c5bc90fd46048dee6dae55 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.explode_and; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(667,8-667,19); use=FStar.Seq.Properties.fsti(667,8-667,19)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.Seq.Properties.explode_and.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4)
(FStar.Seq.Properties.explode_and.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.Seq.Properties.explode_and.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4))
:qid @fuel_irrelevance_FStar.Seq.Properties.explode_and.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.explode_and.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.explode_and; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(667,8-667,19); use=FStar.Seq.Properties.fsti(667,8-667,19)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.Seq.Properties.explode_and @x0
@x1
@x2
@x3)
(FStar.Seq.Properties.explode_and.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3))
 

:pattern ((FStar.Seq.Properties.explode_and @x0
@x1
@x2
@x3))
:qid @fuel_correspondence_FStar.Seq.Properties.explode_and.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.explode_and.fuel_instrumented))

; </end encoding FStar.Seq.Properties.explode_and>


; <Start encoding FStar.Seq.Properties.pointwise_and>

(declare-fun Tm_refine_9f068c7f6ce275579028a195ac18485b (Term) Term)
(declare-fun Tm_refine_1ad818e6438a897337e89a3053cb2002 (Term Term) Term)
(declare-fun FStar.Seq.Properties.pointwise_and (Term Term Term) Term)


;;;;;;;;;;;;;;;;s: FStar.Seq.Base.seq _ {0 <= FStar.Seq.Base.length s} ->     l: Prims.list _ {FStar.List.Tot.Base.length l + 0 = FStar.Seq.Base.length s}   -> Type
(declare-fun Tm_arrow_1d69c34f503e87805d9fa1b40bc9b696 () Term)
(declare-fun FStar.Seq.Properties.pointwise_and@tok () Term)



; </end encoding FStar.Seq.Properties.pointwise_and>


; <Start encoding FStar.Seq.Properties.intro_of_list'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.intro_of_list_ (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.intro_of_list_@tok () Term)

; </end encoding FStar.Seq.Properties.intro_of_list'>


; <Start encoding FStar.Seq.Properties.intro_of_list>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.intro_of_list (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.intro_of_list@tok () Term)

; </end encoding FStar.Seq.Properties.intro_of_list>


; <Start encoding FStar.Seq.Properties.elim_of_list'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.elim_of_list_ (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.elim_of_list_@tok () Term)

; </end encoding FStar.Seq.Properties.elim_of_list'>


; <Start encoding FStar.Seq.Properties.elim_of_list>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.elim_of_list (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.elim_of_list@tok () Term)

; </end encoding FStar.Seq.Properties.elim_of_list>


; <Start encoding FStar.Seq.Properties.sortWith>


(declare-fun FStar.Seq.Properties.sortWith (Term Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: a -> _: a -> Prims.int) -> s: FStar.Seq.Base.seq a -> FStar.Seq.Base.seq a
(declare-fun Tm_arrow_783d577ed6adadfd234f2ce68178463f () Term)
(declare-fun FStar.Seq.Properties.sortWith@tok () Term)


; </end encoding FStar.Seq.Properties.sortWith>


; <Start encoding FStar.Seq.Properties.lemma_seq_to_list_permutation>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_to_list_permutation (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_to_list_permutation@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_to_list_permutation>


; <Start encoding FStar.Seq.Properties.lemma_seq_of_list_permutation>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_permutation (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_permutation@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_of_list_permutation>


; <Start encoding FStar.Seq.Properties.lemma_seq_of_list_sorted>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_sorted (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_of_list_sorted@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_of_list_sorted>


; <Start encoding FStar.Seq.Properties.lemma_seq_sortwith_correctness>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_sortwith_correctness (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.lemma_seq_sortwith_correctness@tok () Term)

; </end encoding FStar.Seq.Properties.lemma_seq_sortwith_correctness>


; <Start encoding FStar.Seq.Properties.sort_lseq>

(declare-fun FStar.Seq.Properties.sort_lseq (Term Term Term Term) Term)
(declare-fun Tm_refine_896d0573468d5c87de125067e75d7d47 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;f: FStar.Seq.Properties.tot_ord a -> s: FStar.Seq.Properties.lseq a n   -> s':     FStar.Seq.Properties.lseq a n       {FStar.Seq.Properties.sorted f s' /\ FStar.Seq.Properties.permutation a s s'}
(declare-fun Tm_arrow_3fb7de3746e0ee65d4a1a51ab385c639 () Term)
(declare-fun FStar.Seq.Properties.sort_lseq@tok () Term)

;;;;;;;;;;;;;;;;kick_partial_app
;;; Fact-ids: Name FStar.Seq.Properties.sort_lseq; Namespace FStar.Seq.Properties
(assert (! (Valid (ApplyTT __uu__PartialApp
FStar.List.Tot.Base.compare_of_bool@tok))
:named @kick_partial_app_168a5a7933bf2aec40b9569f3322d078))

; </end encoding FStar.Seq.Properties.sort_lseq>


; <Start encoding FStar.Seq.Properties.foldr>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.foldr.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.foldr.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.foldr (Term Term Term Term Term) Term)
(declare-fun FStar.Seq.Properties.foldr@tok () Term)



;;;;;;;;;;;;;;;;f: (_: b -> _: a -> a) -> s: FStar.Seq.Base.seq b -> init: a -> Prims.Tot a
(declare-fun Tm_arrow_d44e7807ff692a4b5f624a4c31f6a34b () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.foldr; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(748,8-748,13); use=FStar.Seq.Properties.fsti(748,8-748,13)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.Seq.Properties.foldr.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.Seq.Properties.foldr.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.Seq.Properties.foldr.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.Seq.Properties.foldr.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.foldr.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.foldr; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(748,8-748,13); use=FStar.Seq.Properties.fsti(748,8-748,13)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.Seq.Properties.foldr @x0
@x1
@x2
@x3
@x4)
(FStar.Seq.Properties.foldr.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.Seq.Properties.foldr @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.Seq.Properties.foldr.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.foldr.fuel_instrumented))

; </end encoding FStar.Seq.Properties.foldr>


; <Start encoding FStar.Seq.Properties.foldr_snoc>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Seq.Properties.foldr_snoc.fuel_instrumented (Fuel Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Seq.Properties.foldr_snoc.fuel_instrumented_token () Term)
(declare-fun FStar.Seq.Properties.foldr_snoc (Term Term Term Term Term) Term)
(declare-fun FStar.Seq.Properties.foldr_snoc@tok () Term)




;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Seq.Properties.foldr_snoc; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(753,8-753,18); use=FStar.Seq.Properties.fsti(753,8-753,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term) (@x5 Term))
 (! (= (FStar.Seq.Properties.foldr_snoc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5)
(FStar.Seq.Properties.foldr_snoc.fuel_instrumented ZFuel
@x1
@x2
@x3
@x4
@x5))
 

:pattern ((FStar.Seq.Properties.foldr_snoc.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3
@x4
@x5))
:qid @fuel_irrelevance_FStar.Seq.Properties.foldr_snoc.fuel_instrumented))

:named @fuel_irrelevance_FStar.Seq.Properties.foldr_snoc.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Seq.Properties.foldr_snoc; Namespace FStar.Seq.Properties
(assert (! 
;; def=FStar.Seq.Properties.fsti(753,8-753,18); use=FStar.Seq.Properties.fsti(753,8-753,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term) (@x4 Term))
 (! (= (FStar.Seq.Properties.foldr_snoc @x0
@x1
@x2
@x3
@x4)
(FStar.Seq.Properties.foldr_snoc.fuel_instrumented MaxFuel
@x0
@x1
@x2
@x3
@x4))
 

:pattern ((FStar.Seq.Properties.foldr_snoc @x0
@x1
@x2
@x3
@x4))
:qid @fuel_correspondence_FStar.Seq.Properties.foldr_snoc.fuel_instrumented))

:named @fuel_correspondence_FStar.Seq.Properties.foldr_snoc.fuel_instrumented))

; </end encoding FStar.Seq.Properties.foldr_snoc>


; <Start encoding FStar.Seq.Properties.map_seq>


(declare-fun FStar.Seq.Properties.map_seq (Term Term Term Term) Term)

;;;;;;;;;;;;;;;;f: (_: a -> b) -> s: FStar.Seq.Base.seq a -> FStar.Seq.Base.seq b
(declare-fun Tm_arrow_5f9288474a8f301ac883b79bab9ed39c () Term)
(declare-fun FStar.Seq.Properties.map_seq@tok () Term)

; </end encoding FStar.Seq.Properties.map_seq>


; <Start encoding FStar.Seq.Properties.map_seq_len>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.map_seq_len (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.map_seq_len@tok () Term)

; </end encoding FStar.Seq.Properties.map_seq_len>


; <Start encoding FStar.Seq.Properties.map_seq_index>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.map_seq_index (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.map_seq_index@tok () Term)

; </end encoding FStar.Seq.Properties.map_seq_index>


; <Start encoding FStar.Seq.Properties.map_seq_append>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Seq.Properties.map_seq_append (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Seq.Properties.map_seq_append@tok () Term)

; </end encoding FStar.Seq.Properties.map_seq_append>


; End Externals for interface FStar.Seq.Properties


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End interface FStar.Seq.Properties (842 decls; total size 76686)

;;; Start module FStar.Seq

; Externals for module FStar.Seq


; End Externals for module FStar.Seq


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Seq (3 decls; total size 1068)

;;; Start module FStar.Mul

; Externals for module FStar.Mul


; <Start encoding FStar.Mul.op_Star>

(declare-fun FStar.Mul.op_Star (Term Term) Term)

(declare-fun FStar.Mul.op_Star@tok () Term)

; </end encoding FStar.Mul.op_Star>


; End Externals for module FStar.Mul


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Mul (8 decls; total size 1239)

;;; Start module FStar.Math.Lib

; Externals for module FStar.Math.Lib


; <Skipped FStar.Math.Lib.lemma_div_def/>


; <Start encoding FStar.Math.Lib.lemma_div_def>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.lemma_div_def (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.lemma_div_def@tok () Term)

; </end encoding FStar.Math.Lib.lemma_div_def>


; <Start encoding FStar.Math.Lib.mul_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.mul_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.mul_lemma@tok () Term)

; </end encoding FStar.Math.Lib.mul_lemma>


; <Start encoding FStar.Math.Lib.mul_lemma'>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.mul_lemma_ (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.mul_lemma_@tok () Term)

; </end encoding FStar.Math.Lib.mul_lemma'>


; <Start encoding FStar.Math.Lib.mul_div_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.mul_div_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.mul_div_lemma@tok () Term)

; </end encoding FStar.Math.Lib.mul_div_lemma>


; <Skipped FStar.Math.Lib.slash_decr_axiom/>


; <Start encoding FStar.Math.Lib.slash_decr_axiom>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.slash_decr_axiom (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.slash_decr_axiom@tok () Term)

; </end encoding FStar.Math.Lib.slash_decr_axiom>


; <Start encoding FStar.Math.Lib.lemma_mul_minus_distr_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.lemma_mul_minus_distr_l (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.lemma_mul_minus_distr_l@tok () Term)

; </end encoding FStar.Math.Lib.lemma_mul_minus_distr_l>


; <Skipped />


; <Skipped FStar.Math.Lib.slash_star_axiom/>


; <Start encoding FStar.Math.Lib.slash_star_axiom>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.slash_star_axiom (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.slash_star_axiom@tok () Term)

; </end encoding FStar.Math.Lib.slash_star_axiom>


; <Skipped />


; <Skipped FStar.Math.Lib.log_2/>


; <Start encoding FStar.Math.Lib.log_2>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Math.Lib.log_2.fuel_instrumented (Fuel Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Math.Lib.log_2.fuel_instrumented_token () Term)
(declare-fun FStar.Math.Lib.log_2 (Term) Term)
(declare-fun FStar.Math.Lib.log_2@tok () Term)
;;;;;;;;;;;;;;;;x: Prims.pos -> Prims.nat
(declare-fun Tm_arrow_195a91d0390990c5da9b9b2c7b2e9a5f () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Math.Lib.log_2; Namespace FStar.Math.Lib
(assert (! 
;; def=FStar.Math.Lib.fst(54,8-54,13); use=FStar.Math.Lib.fst(54,8-54,13)
(forall ((@u0 Fuel) (@x1 Term))
 (! (= (FStar.Math.Lib.log_2.fuel_instrumented (SFuel @u0)
@x1)
(FStar.Math.Lib.log_2.fuel_instrumented ZFuel
@x1))
 

:pattern ((FStar.Math.Lib.log_2.fuel_instrumented (SFuel @u0)
@x1))
:qid @fuel_irrelevance_FStar.Math.Lib.log_2.fuel_instrumented))

:named @fuel_irrelevance_FStar.Math.Lib.log_2.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Math.Lib.log_2; Namespace FStar.Math.Lib
(assert (! 
;; def=FStar.Math.Lib.fst(54,8-54,13); use=FStar.Math.Lib.fst(54,8-54,13)
(forall ((@x0 Term))
 (! (= (FStar.Math.Lib.log_2 @x0)
(FStar.Math.Lib.log_2.fuel_instrumented MaxFuel
@x0))
 

:pattern ((FStar.Math.Lib.log_2 @x0))
:qid @fuel_correspondence_FStar.Math.Lib.log_2.fuel_instrumented))

:named @fuel_correspondence_FStar.Math.Lib.log_2.fuel_instrumented))

; </end encoding FStar.Math.Lib.log_2>


; <Skipped FStar.Math.Lib.powx/>


; <Start encoding FStar.Math.Lib.powx>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.Math.Lib.powx.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.Math.Lib.powx.fuel_instrumented_token () Term)
(declare-fun FStar.Math.Lib.powx (Term Term) Term)
(declare-fun FStar.Math.Lib.powx@tok () Term)
;;;;;;;;;;;;;;;;x: Prims.int -> n: Prims.nat -> Prims.int
(declare-fun Tm_arrow_97e79e8898be25d1baac7492eb8157a8 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.Math.Lib.powx; Namespace FStar.Math.Lib
(assert (! 
;; def=FStar.Math.Lib.fst(59,8-59,12); use=FStar.Math.Lib.fst(59,8-59,12)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.Math.Lib.powx.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.Math.Lib.powx.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.Math.Lib.powx.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.Math.Lib.powx.fuel_instrumented))

:named @fuel_irrelevance_FStar.Math.Lib.powx.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.Math.Lib.powx; Namespace FStar.Math.Lib
(assert (! 
;; def=FStar.Math.Lib.fst(59,8-59,12); use=FStar.Math.Lib.fst(59,8-59,12)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.Math.Lib.powx @x0
@x1)
(FStar.Math.Lib.powx.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.Math.Lib.powx @x0
@x1))
:qid @fuel_correspondence_FStar.Math.Lib.powx.fuel_instrumented))

:named @fuel_correspondence_FStar.Math.Lib.powx.fuel_instrumented))

; </end encoding FStar.Math.Lib.powx>


; <Skipped FStar.Math.Lib.abs/>


; <Start encoding FStar.Math.Lib.abs>

(declare-fun FStar.Math.Lib.abs (Term) Term)
(declare-fun Tm_refine_5b706f1316bc4c0722dc2171363a324f (Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> y: Prims.int{(x >= 0 ==> y = x) /\ (x < 0 ==> y = - x)}
(declare-fun Tm_arrow_485462bf1365ac4f0407149110b772cd () Term)
(declare-fun FStar.Math.Lib.abs@tok () Term)


; </end encoding FStar.Math.Lib.abs>


; <Skipped FStar.Math.Lib.max/>


; <Start encoding FStar.Math.Lib.max>

(declare-fun FStar.Math.Lib.max (Term Term) Term)
(declare-fun Tm_refine_3b1de445e68d5a7cbfc9e637b6d5fe5c (Term Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> y: Prims.int -> z: Prims.int{(x >= y ==> z = x) /\ (x < y ==> z = y)}
(declare-fun Tm_arrow_6cac7a49c19aab6d14a44dce4ddd50d7 () Term)
(declare-fun FStar.Math.Lib.max@tok () Term)


; </end encoding FStar.Math.Lib.max>


; <Skipped FStar.Math.Lib.min/>


; <Start encoding FStar.Math.Lib.min>

(declare-fun FStar.Math.Lib.min (Term Term) Term)
(declare-fun Tm_refine_75a39246caf92bd7ba0c54b533ac97ba (Term Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> y: Prims.int -> z: Prims.int{(x >= y ==> z = y) /\ (x < y ==> z = x)}
(declare-fun Tm_arrow_f1c63d0f3ff3d4c0a4e173563f61a3ec () Term)
(declare-fun FStar.Math.Lib.min@tok () Term)


; </end encoding FStar.Math.Lib.min>


; <Skipped FStar.Math.Lib.div/>


; <Start encoding FStar.Math.Lib.div>

(declare-fun FStar.Math.Lib.div (Term Term) Term)
(declare-fun Tm_refine_2a75ac9e9041407930877285ccf479d9 (Term) Term)
;;;;;;;;;;;;;;;;a: Prims.int -> b: Prims.pos -> c: Prims.int{(a < 0 ==> c < 0) /\ (a >= 0 ==> c >= 0)}
(declare-fun Tm_arrow_bb819be7118d7bfb2cedbf3c6477c362 () Term)
(declare-fun FStar.Math.Lib.div@tok () Term)


; </end encoding FStar.Math.Lib.div>


; <Skipped FStar.Math.Lib.div_non_eucl/>


; <Start encoding FStar.Math.Lib.div_non_eucl>

(declare-fun FStar.Math.Lib.div_non_eucl (Term Term) Term)
(declare-fun Tm_refine_0ffeb4b35eb66c9dc7f43d49d6f24837 (Term Term) Term)
;;;;;;;;;;;;;;;;a: Prims.int -> b: Prims.pos -> q: Prims.int{(a >= 0 ==> q = a / b) /\ (a < 0 ==> q = - (- a) / b)}
(declare-fun Tm_arrow_7c4dc753d10246d9d92341a1295260f4 () Term)
(declare-fun FStar.Math.Lib.div_non_eucl@tok () Term)


; </end encoding FStar.Math.Lib.div_non_eucl>


; <Skipped FStar.Math.Lib.shift_left/>


; <Start encoding FStar.Math.Lib.shift_left>

(declare-fun FStar.Math.Lib.shift_left (Term Term) Term)
(declare-fun Tm_refine_180a7ec928fc00449a9ff97fd83eb9f7 (Term Term) Term)
;;;;;;;;;;;;;;;;v: Prims.int -> i: Prims.nat -> res: Prims.int{res = v * Prims.pow2 i}
(declare-fun Tm_arrow_ebb8ce92eba15a16c00c7e434e88c84b () Term)
(declare-fun FStar.Math.Lib.shift_left@tok () Term)


; </end encoding FStar.Math.Lib.shift_left>


; <Skipped FStar.Math.Lib.arithmetic_shift_right/>


; <Start encoding FStar.Math.Lib.arithmetic_shift_right>

(declare-fun FStar.Math.Lib.arithmetic_shift_right (Term Term) Term)
(declare-fun Tm_refine_1b8188dd620bafffed7e311591823814 (Term Term) Term)
;;;;;;;;;;;;;;;;v: Prims.int -> i: Prims.nat -> res: Prims.int{res = FStar.Math.Lib.div v (Prims.pow2 i)}
(declare-fun Tm_arrow_0d2ab070c39795db6825f9a2ab12fa9a () Term)
(declare-fun FStar.Math.Lib.arithmetic_shift_right@tok () Term)


; </end encoding FStar.Math.Lib.arithmetic_shift_right>


; <Skipped FStar.Math.Lib.signed_modulo/>


; <Start encoding FStar.Math.Lib.signed_modulo>

(declare-fun FStar.Math.Lib.signed_modulo (Term Term) Term)
(declare-fun Tm_refine_7f910f581ef6c422e545ac01d1c8b2f5 (Term Term) Term)
;;;;;;;;;;;;;;;;v: Prims.int -> p: Prims.pos -> res: Prims.int{res = v - FStar.Math.Lib.div_non_eucl v p * p}
(declare-fun Tm_arrow_735d78cef45a99c351b2596c50444f63 () Term)
(declare-fun FStar.Math.Lib.signed_modulo@tok () Term)


; </end encoding FStar.Math.Lib.signed_modulo>


; <Skipped FStar.Math.Lib.op_Plus_Percent/>


; <Start encoding FStar.Math.Lib.op_Plus_Percent>

(declare-fun FStar.Math.Lib.op_Plus_Percent (Term Term) Term)
(declare-fun Tm_refine_d653f98e8ce399d5b7ea191c117fe516 (Term Term) Term)
;;;;;;;;;;;;;;;;a: Prims.int -> p: Prims.pos   -> res: Prims.int{(a >= 0 ==> res = a % p) /\ (a < 0 ==> res = - (- a) % p)}
(declare-fun Tm_arrow_47a9b4ba9fff686aea1b155fa584e4a2 () Term)
(declare-fun FStar.Math.Lib.op_Plus_Percent@tok () Term)


; </end encoding FStar.Math.Lib.op_Plus_Percent>


; <Skipped FStar.Math.Lib.powx_lemma1/>


; <Start encoding FStar.Math.Lib.powx_lemma1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.powx_lemma1 (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.powx_lemma1@tok () Term)

; </end encoding FStar.Math.Lib.powx_lemma1>


; <Skipped FStar.Math.Lib.powx_lemma2/>


; <Start encoding FStar.Math.Lib.powx_lemma2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.powx_lemma2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.powx_lemma2@tok () Term)

; </end encoding FStar.Math.Lib.powx_lemma2>


; <Skipped FStar.Math.Lib.abs_mul_lemma/>


; <Start encoding FStar.Math.Lib.abs_mul_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.abs_mul_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.abs_mul_lemma@tok () Term)

; </end encoding FStar.Math.Lib.abs_mul_lemma>


; <Skipped FStar.Math.Lib.signed_modulo_property/>


; <Start encoding FStar.Math.Lib.signed_modulo_property>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.signed_modulo_property (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.signed_modulo_property@tok () Term)

; </end encoding FStar.Math.Lib.signed_modulo_property>


; <Skipped FStar.Math.Lib.div_non_eucl_decr_lemma/>


; <Start encoding FStar.Math.Lib.div_non_eucl_decr_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.div_non_eucl_decr_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.div_non_eucl_decr_lemma@tok () Term)

; </end encoding FStar.Math.Lib.div_non_eucl_decr_lemma>


; <Skipped FStar.Math.Lib.div_non_eucl_bigger_denom_lemma/>


; <Start encoding FStar.Math.Lib.div_non_eucl_bigger_denom_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lib.div_non_eucl_bigger_denom_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lib.div_non_eucl_bigger_denom_lemma@tok () Term)

; </end encoding FStar.Math.Lib.div_non_eucl_bigger_denom_lemma>


; End Externals for module FStar.Math.Lib


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Math.Lib (158 decls; total size 13712)

;;; Start module FStar.Math.Lemmas

; Externals for module FStar.Math.Lemmas


; <Skipped />


; <Skipped FStar.Math.Lemmas.euclidean_div_axiom/>


; <Start encoding FStar.Math.Lemmas.euclidean_div_axiom>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.euclidean_div_axiom (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.euclidean_div_axiom@tok () Term)

; </end encoding FStar.Math.Lemmas.euclidean_div_axiom>


; <Skipped FStar.Math.Lemmas.lemma_eucl_div_bound/>


; <Start encoding FStar.Math.Lemmas.lemma_eucl_div_bound>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_eucl_div_bound (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_eucl_div_bound@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_eucl_div_bound>


; <Skipped FStar.Math.Lemmas.lemma_mult_le_left/>


; <Start encoding FStar.Math.Lemmas.lemma_mult_le_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_left@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_le_left>


; <Skipped FStar.Math.Lemmas.lemma_mult_le_right/>


; <Start encoding FStar.Math.Lemmas.lemma_mult_le_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_right@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_le_right>


; <Skipped FStar.Math.Lemmas.lemma_mult_lt_left/>


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_left@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_left>


; <Skipped FStar.Math.Lemmas.lemma_mult_lt_right/>


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_right@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_right>


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_sqr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_sqr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_sqr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_sqr>


; <Skipped FStar.Math.Lemmas.swap_mul/>


; <Start encoding FStar.Math.Lemmas.swap_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_mul>


; <Skipped FStar.Math.Lemmas.lemma_cancel_mul/>


; <Start encoding FStar.Math.Lemmas.lemma_cancel_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_cancel_mul (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_cancel_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_cancel_mul>


; <Skipped FStar.Math.Lemmas.distributivity_add_left/>


; <Start encoding FStar.Math.Lemmas.distributivity_add_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_left@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_add_left>


; <Skipped FStar.Math.Lemmas.distributivity_add_right/>


; <Start encoding FStar.Math.Lemmas.distributivity_add_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_right@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_add_right>


; <Skipped FStar.Math.Lemmas.paren_mul_left/>


; <Start encoding FStar.Math.Lemmas.paren_mul_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_left@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_mul_left>


; <Skipped FStar.Math.Lemmas.paren_mul_right/>


; <Start encoding FStar.Math.Lemmas.paren_mul_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_right@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_mul_right>


; <Skipped FStar.Math.Lemmas.paren_add_left/>


; <Start encoding FStar.Math.Lemmas.paren_add_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_add_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_add_left@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_add_left>


; <Skipped FStar.Math.Lemmas.paren_add_right/>


; <Start encoding FStar.Math.Lemmas.paren_add_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_add_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_add_right@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_add_right>


; <Skipped FStar.Math.Lemmas.addition_is_associative/>


; <Start encoding FStar.Math.Lemmas.addition_is_associative>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.addition_is_associative (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.addition_is_associative@tok () Term)

; </end encoding FStar.Math.Lemmas.addition_is_associative>


; <Skipped FStar.Math.Lemmas.subtraction_is_distributive/>


; <Start encoding FStar.Math.Lemmas.subtraction_is_distributive>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.subtraction_is_distributive (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.subtraction_is_distributive@tok () Term)

; </end encoding FStar.Math.Lemmas.subtraction_is_distributive>


; <Skipped FStar.Math.Lemmas.swap_add_plus_minus/>


; <Start encoding FStar.Math.Lemmas.swap_add_plus_minus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_add_plus_minus (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_add_plus_minus@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_add_plus_minus>


; <Skipped FStar.Math.Lemmas.neg_mul_left/>


; <Start encoding FStar.Math.Lemmas.neg_mul_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_left (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_left@tok () Term)

; </end encoding FStar.Math.Lemmas.neg_mul_left>


; <Skipped FStar.Math.Lemmas.neg_mul_right/>


; <Start encoding FStar.Math.Lemmas.neg_mul_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_right (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_right@tok () Term)

; </end encoding FStar.Math.Lemmas.neg_mul_right>


; <Skipped FStar.Math.Lemmas.swap_neg_mul/>


; <Start encoding FStar.Math.Lemmas.swap_neg_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_neg_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_neg_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_neg_mul>


; <Skipped FStar.Math.Lemmas.distributivity_sub_left/>


; <Start encoding FStar.Math.Lemmas.distributivity_sub_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_left@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_sub_left>


; <Skipped FStar.Math.Lemmas.distributivity_sub_right/>


; <Start encoding FStar.Math.Lemmas.distributivity_sub_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_right@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_sub_right>


; <Skipped FStar.Math.Lemmas.mul_binds_tighter/>


; <Start encoding FStar.Math.Lemmas.mul_binds_tighter>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_binds_tighter (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_binds_tighter@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_binds_tighter>


; <Skipped FStar.Math.Lemmas.lemma_abs_mul/>


; <Start encoding FStar.Math.Lemmas.lemma_abs_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_abs_mul>


; <Skipped FStar.Math.Lemmas.lemma_abs_bound/>


; <Start encoding FStar.Math.Lemmas.lemma_abs_bound>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_bound (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_bound@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_abs_bound>


; <Skipped FStar.Math.Lemmas.mul_ineq1/>


; <Start encoding FStar.Math.Lemmas.mul_ineq1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_ineq1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_ineq1@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_ineq1>


; <Start encoding FStar.Math.Lemmas.add_zero_left_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.add_zero_left_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.add_zero_left_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.add_zero_left_is_same>


; <Start encoding FStar.Math.Lemmas.add_zero_right_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.add_zero_right_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.add_zero_right_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.add_zero_right_is_same>


; <Start encoding FStar.Math.Lemmas.mul_one_left_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_one_left_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_one_left_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_one_left_is_same>


; <Start encoding FStar.Math.Lemmas.mul_one_right_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_one_right_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_one_right_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_one_right_is_same>


; <Start encoding FStar.Math.Lemmas.mul_zero_left_is_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_left_is_zero (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_left_is_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_zero_left_is_zero>


; <Start encoding FStar.Math.Lemmas.mul_zero_right_is_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_right_is_zero (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_right_is_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_zero_right_is_zero>


; <Skipped FStar.Math.Lemmas.nat_times_nat_is_nat/>


; <Start encoding FStar.Math.Lemmas.nat_times_nat_is_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_times_nat_is_nat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_times_nat_is_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_times_nat_is_nat>


; <Skipped FStar.Math.Lemmas.pos_times_pos_is_pos/>


; <Start encoding FStar.Math.Lemmas.pos_times_pos_is_pos>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pos_times_pos_is_pos (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pos_times_pos_is_pos@tok () Term)

; </end encoding FStar.Math.Lemmas.pos_times_pos_is_pos>


; <Skipped FStar.Math.Lemmas.nat_over_pos_is_nat/>


; <Start encoding FStar.Math.Lemmas.nat_over_pos_is_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_over_pos_is_nat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_over_pos_is_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_over_pos_is_nat>


; <Skipped FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma/>


; <Start encoding FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma>


; <Skipped FStar.Math.Lemmas.int_times_int_equal_zero_lemma/>


; <Start encoding FStar.Math.Lemmas.int_times_int_equal_zero_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.int_times_int_equal_zero_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.int_times_int_equal_zero_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.int_times_int_equal_zero_lemma>


; <Skipped />


; <Skipped FStar.Math.Lemmas.pow2_double_sum/>


; <Start encoding FStar.Math.Lemmas.pow2_double_sum>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_sum (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_sum@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_double_sum>


; <Skipped FStar.Math.Lemmas.pow2_double_mult/>


; <Start encoding FStar.Math.Lemmas.pow2_double_mult>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_mult (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_mult@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_double_mult>


; <Skipped FStar.Math.Lemmas.pow2_lt_compat/>


; <Start encoding FStar.Math.Lemmas.pow2_lt_compat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_lt_compat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_lt_compat@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_lt_compat>


; <Skipped />


; <Skipped FStar.Math.Lemmas.pow2_le_compat/>


; <Start encoding FStar.Math.Lemmas.pow2_le_compat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_le_compat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_le_compat@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_le_compat>


; <Skipped />


; <Skipped FStar.Math.Lemmas.pow2_plus/>


; <Start encoding FStar.Math.Lemmas.pow2_plus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_plus (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_plus@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_plus>


; <Skipped />


; <Skipped FStar.Math.Lemmas.pow2_minus/>


; <Start encoding FStar.Math.Lemmas.pow2_minus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_minus (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_minus@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_minus>


; <Skipped FStar.Math.Lemmas.multiply_fractions/>


; <Start encoding FStar.Math.Lemmas.multiply_fractions>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.multiply_fractions (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.multiply_fractions@tok () Term)

; </end encoding FStar.Math.Lemmas.multiply_fractions>


; <Skipped FStar.Math.Lemmas.modulo_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_lemma>


; <Skipped FStar.Math.Lemmas.lemma_div_mod/>


; <Start encoding FStar.Math.Lemmas.lemma_div_mod>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_mod (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_mod@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_mod>


; <Skipped FStar.Math.Lemmas.lemma_mod_lt/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_lt>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_lt (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_lt@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_lt>


; <Skipped FStar.Math.Lemmas.lemma_div_lt_nat/>


; <Start encoding FStar.Math.Lemmas.lemma_div_lt_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt_nat (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_lt_nat>


; <Skipped FStar.Math.Lemmas.lemma_div_lt/>


; <Start encoding FStar.Math.Lemmas.lemma_div_lt>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_lt>


; <Skipped FStar.Math.Lemmas.bounded_multiple_is_zero/>


; <Start encoding FStar.Math.Lemmas.bounded_multiple_is_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.bounded_multiple_is_zero (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.bounded_multiple_is_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.bounded_multiple_is_zero>


; <Skipped FStar.Math.Lemmas.small_div/>


; <Start encoding FStar.Math.Lemmas.small_div>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_div (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_div@tok () Term)

; </end encoding FStar.Math.Lemmas.small_div>


; <Skipped FStar.Math.Lemmas.small_mod/>


; <Start encoding FStar.Math.Lemmas.small_mod>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_mod (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_mod@tok () Term)

; </end encoding FStar.Math.Lemmas.small_mod>


; <Skipped FStar.Math.Lemmas.lt_multiple_is_equal/>


; <Start encoding FStar.Math.Lemmas.lt_multiple_is_equal>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lt_multiple_is_equal (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lt_multiple_is_equal@tok () Term)

; </end encoding FStar.Math.Lemmas.lt_multiple_is_equal>


; <Skipped FStar.Math.Lemmas.lemma_mod_plus/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_plus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_plus>


; <Skipped FStar.Math.Lemmas.lemma_div_plus/>


; <Start encoding FStar.Math.Lemmas.lemma_div_plus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_plus (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_plus@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_plus>


; <Start encoding FStar.Math.Lemmas.lemma_div_mod_plus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_mod_plus (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_mod_plus@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_mod_plus>


; <Skipped FStar.Math.Lemmas.add_div_mod_1/>


; <Start encoding FStar.Math.Lemmas.add_div_mod_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.add_div_mod_1 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.add_div_mod_1@tok () Term)

; </end encoding FStar.Math.Lemmas.add_div_mod_1>


; <Skipped FStar.Math.Lemmas.sub_div_mod_1/>


; <Start encoding FStar.Math.Lemmas.sub_div_mod_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.sub_div_mod_1 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.sub_div_mod_1@tok () Term)

; </end encoding FStar.Math.Lemmas.sub_div_mod_1>


; <Skipped />


; <Skipped FStar.Math.Lemmas.cancel_mul_div/>


; <Start encoding FStar.Math.Lemmas.cancel_mul_div>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.cancel_mul_div (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.cancel_mul_div@tok () Term)

; </end encoding FStar.Math.Lemmas.cancel_mul_div>


; <Skipped />


; <Skipped FStar.Math.Lemmas.cancel_mul_mod/>


; <Start encoding FStar.Math.Lemmas.cancel_mul_mod>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.cancel_mul_mod (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.cancel_mul_mod@tok () Term)

; </end encoding FStar.Math.Lemmas.cancel_mul_mod>


; <Skipped FStar.Math.Lemmas.lemma_mod_add_distr/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_add_distr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_add_distr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_add_distr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_add_distr>


; <Skipped FStar.Math.Lemmas.lemma_mod_sub_distr/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_sub_distr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_distr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_distr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_sub_distr>


; <Skipped FStar.Math.Lemmas.lemma_mod_sub_0/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_sub_0>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_0 (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_0@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_sub_0>


; <Skipped FStar.Math.Lemmas.lemma_mod_sub_1/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_sub_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_1 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub_1@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_sub_1>


; <Skipped FStar.Math.Lemmas.lemma_mod_mul_distr_l/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_mul_distr_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mul_distr_l (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mul_distr_l@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_mul_distr_l>


; <Skipped FStar.Math.Lemmas.lemma_mod_mul_distr_r/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_mul_distr_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mul_distr_r (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mul_distr_r@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_mul_distr_r>


; <Skipped FStar.Math.Lemmas.lemma_mod_injective/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_injective>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_injective (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_injective@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_injective>


; <Skipped FStar.Math.Lemmas.lemma_mul_sub_distr/>


; <Start encoding FStar.Math.Lemmas.lemma_mul_sub_distr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_sub_distr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_sub_distr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mul_sub_distr>


; <Skipped FStar.Math.Lemmas.lemma_div_exact/>


; <Start encoding FStar.Math.Lemmas.lemma_div_exact>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_exact (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_exact@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_exact>


; <Skipped FStar.Math.Lemmas.div_exact_r/>


; <Start encoding FStar.Math.Lemmas.div_exact_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.div_exact_r (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.div_exact_r@tok () Term)

; </end encoding FStar.Math.Lemmas.div_exact_r>


; <Skipped FStar.Math.Lemmas.lemma_mod_spec/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_spec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_spec (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_spec@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_spec>


; <Skipped FStar.Math.Lemmas.lemma_mod_spec2/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_spec2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_spec2 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_spec2@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_spec2>


; <Skipped FStar.Math.Lemmas.lemma_mod_plus_distr_l/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_plus_distr_l>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_distr_l (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_distr_l@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_plus_distr_l>


; <Skipped FStar.Math.Lemmas.lemma_mod_plus_distr_r/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_plus_distr_r>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_distr_r (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_distr_r@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_plus_distr_r>


; <Skipped FStar.Math.Lemmas.lemma_mod_mod/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_mod>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mod (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mod@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_mod>


; <Skipped FStar.Math.Lemmas.euclidean_division_definition/>


; <Start encoding FStar.Math.Lemmas.euclidean_division_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.euclidean_division_definition (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.euclidean_division_definition@tok () Term)

; </end encoding FStar.Math.Lemmas.euclidean_division_definition>


; <Skipped FStar.Math.Lemmas.modulo_range_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_range_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_range_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_range_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_range_lemma>


; <Skipped FStar.Math.Lemmas.small_modulo_lemma_1/>


; <Start encoding FStar.Math.Lemmas.small_modulo_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_modulo_lemma_1 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_modulo_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.small_modulo_lemma_1>


; <Skipped FStar.Math.Lemmas.small_modulo_lemma_2/>


; <Start encoding FStar.Math.Lemmas.small_modulo_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_modulo_lemma_2 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_modulo_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.small_modulo_lemma_2>


; <Skipped FStar.Math.Lemmas.small_division_lemma_1/>


; <Start encoding FStar.Math.Lemmas.small_division_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_division_lemma_1 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_division_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.small_division_lemma_1>


; <Skipped FStar.Math.Lemmas.small_division_lemma_2/>


; <Start encoding FStar.Math.Lemmas.small_division_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.small_division_lemma_2 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.small_division_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.small_division_lemma_2>


; <Skipped FStar.Math.Lemmas.multiplication_order_lemma/>


; <Start encoding FStar.Math.Lemmas.multiplication_order_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.multiplication_order_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.multiplication_order_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.multiplication_order_lemma>


; <Skipped FStar.Math.Lemmas.division_propriety/>


; <Start encoding FStar.Math.Lemmas.division_propriety>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_propriety (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_propriety@tok () Term)

; </end encoding FStar.Math.Lemmas.division_propriety>


; <Skipped FStar.Math.Lemmas.division_definition_lemma_1/>


; <Start encoding FStar.Math.Lemmas.division_definition_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_definition_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_definition_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.division_definition_lemma_1>


; <Skipped FStar.Math.Lemmas.division_definition_lemma_2/>


; <Start encoding FStar.Math.Lemmas.division_definition_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_definition_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_definition_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.division_definition_lemma_2>


; <Skipped FStar.Math.Lemmas.division_definition/>


; <Start encoding FStar.Math.Lemmas.division_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_definition (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_definition@tok () Term)

; </end encoding FStar.Math.Lemmas.division_definition>


; <Skipped FStar.Math.Lemmas.multiple_division_lemma/>


; <Start encoding FStar.Math.Lemmas.multiple_division_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.multiple_division_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.multiple_division_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.multiple_division_lemma>


; <Skipped FStar.Math.Lemmas.multiple_modulo_lemma/>


; <Start encoding FStar.Math.Lemmas.multiple_modulo_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.multiple_modulo_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.multiple_modulo_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.multiple_modulo_lemma>


; <Skipped FStar.Math.Lemmas.division_addition_lemma/>


; <Start encoding FStar.Math.Lemmas.division_addition_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_addition_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_addition_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.division_addition_lemma>


; <Skipped FStar.Math.Lemmas.modulo_distributivity/>


; <Start encoding FStar.Math.Lemmas.modulo_distributivity>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_distributivity (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_distributivity@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_distributivity>


; <Skipped FStar.Math.Lemmas.lemma_div_le/>


; <Start encoding FStar.Math.Lemmas.lemma_div_le>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_le (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_le@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_le>


; <Skipped FStar.Math.Lemmas.division_sub_lemma/>


; <Start encoding FStar.Math.Lemmas.division_sub_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_sub_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_sub_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.division_sub_lemma>


; <Skipped FStar.Math.Lemmas.lemma_mod_plus_mul_distr/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_plus_mul_distr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_mul_distr (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_mul_distr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_plus_mul_distr>


; <Skipped FStar.Math.Lemmas.modulo_addition_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_addition_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_addition_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_addition_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_addition_lemma>


; <Skipped FStar.Math.Lemmas.lemma_mod_sub/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_sub>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_sub@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_sub>


; <Skipped FStar.Math.Lemmas.mod_mult_exact/>


; <Start encoding FStar.Math.Lemmas.mod_mult_exact>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mod_mult_exact (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mod_mult_exact@tok () Term)

; </end encoding FStar.Math.Lemmas.mod_mult_exact>


; <Skipped FStar.Math.Lemmas.mod_mul_div_exact/>


; <Start encoding FStar.Math.Lemmas.mod_mul_div_exact>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mod_mul_div_exact (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mod_mul_div_exact@tok () Term)

; </end encoding FStar.Math.Lemmas.mod_mul_div_exact>


; <Skipped />


; <Skipped FStar.Math.Lemmas.mod_pow2_div2/>


; <Start encoding FStar.Math.Lemmas.mod_pow2_div2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mod_pow2_div2 (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mod_pow2_div2@tok () Term)

; </end encoding FStar.Math.Lemmas.mod_pow2_div2>


; <Skipped />


; <Skipped FStar.Math.Lemmas.lemma_div_lt_cancel/>


; <Start encoding FStar.Math.Lemmas.lemma_div_lt_cancel>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt_cancel (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_div_lt_cancel@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_div_lt_cancel>


; <Skipped FStar.Math.Lemmas.lemma_mod_mult_zero/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_mult_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mult_zero (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_mult_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_mult_zero>


; <Skipped FStar.Math.Lemmas.division_multiplication_lemma/>


; <Start encoding FStar.Math.Lemmas.division_multiplication_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.division_multiplication_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.division_multiplication_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.division_multiplication_lemma>


; <Skipped FStar.Math.Lemmas.cancel_fraction/>


; <Start encoding FStar.Math.Lemmas.cancel_fraction>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.cancel_fraction (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.cancel_fraction@tok () Term)

; </end encoding FStar.Math.Lemmas.cancel_fraction>


; <Skipped FStar.Math.Lemmas.modulo_scale_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_scale_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_scale_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_scale_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_scale_lemma>


; <Start encoding FStar.Math.Lemmas.lemma_mul_pos_pos_is_pos>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_pos_pos_is_pos (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_pos_pos_is_pos@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mul_pos_pos_is_pos>


; <Start encoding FStar.Math.Lemmas.lemma_mul_nat_pos_is_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_nat_pos_is_nat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mul_nat_pos_is_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mul_nat_pos_is_nat>


; <Start encoding FStar.Math.Lemmas.modulo_division_lemma_0>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_division_lemma_0 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_division_lemma_0@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_division_lemma_0>


; <Skipped FStar.Math.Lemmas.modulo_division_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_division_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_division_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_division_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_division_lemma>


; <Skipped FStar.Math.Lemmas.modulo_modulo_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_modulo_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_modulo_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_modulo_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_modulo_lemma>


; <Skipped FStar.Math.Lemmas.pow2_multiplication_division_lemma_1/>


; <Start encoding FStar.Math.Lemmas.pow2_multiplication_division_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_division_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_division_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_multiplication_division_lemma_1>


; <Skipped FStar.Math.Lemmas.pow2_multiplication_division_lemma_2/>


; <Start encoding FStar.Math.Lemmas.pow2_multiplication_division_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_division_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_division_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_multiplication_division_lemma_2>


; <Skipped FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_1/>


; <Start encoding FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_1>


; <Skipped FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_2/>


; <Start encoding FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_multiplication_modulo_lemma_2>


; <Skipped FStar.Math.Lemmas.pow2_modulo_division_lemma_1/>


; <Start encoding FStar.Math.Lemmas.pow2_modulo_division_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_division_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_division_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_modulo_division_lemma_1>


; <Skipped FStar.Math.Lemmas.pow2_modulo_division_lemma_2/>


; <Start encoding FStar.Math.Lemmas.pow2_modulo_division_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_division_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_division_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_modulo_division_lemma_2>


; <Skipped FStar.Math.Lemmas.pow2_modulo_modulo_lemma_1/>


; <Start encoding FStar.Math.Lemmas.pow2_modulo_modulo_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_modulo_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_modulo_lemma_1@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_modulo_modulo_lemma_1>


; <Skipped FStar.Math.Lemmas.pow2_modulo_modulo_lemma_2/>


; <Start encoding FStar.Math.Lemmas.pow2_modulo_modulo_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_modulo_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_modulo_modulo_lemma_2@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_modulo_modulo_lemma_2>


; <Skipped FStar.Math.Lemmas.modulo_add/>


; <Start encoding FStar.Math.Lemmas.modulo_add>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_add (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_add@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_add>


; <Skipped FStar.Math.Lemmas.lemma_mod_twice/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_twice>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_twice (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_twice@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_twice>


; <Skipped FStar.Math.Lemmas.modulo_sub/>


; <Start encoding FStar.Math.Lemmas.modulo_sub>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_sub (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_sub@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_sub>


; <Skipped FStar.Math.Lemmas.mod_add_both/>


; <Start encoding FStar.Math.Lemmas.mod_add_both>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mod_add_both (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mod_add_both@tok () Term)

; </end encoding FStar.Math.Lemmas.mod_add_both>


; <Skipped FStar.Math.Lemmas.lemma_mod_plus_injective/>


; <Start encoding FStar.Math.Lemmas.lemma_mod_plus_injective>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_injective (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mod_plus_injective@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mod_plus_injective>


; <Skipped FStar.Math.Lemmas.modulo_sub_lemma/>


; <Start encoding FStar.Math.Lemmas.modulo_sub_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.modulo_sub_lemma (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.modulo_sub_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.modulo_sub_lemma>


; End Externals for module FStar.Math.Lemmas


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.Math.Lemmas (616 decls; total size 52963)

;;; Start module FStar.BitVector

; Externals for module FStar.BitVector


; <Start encoding FStar.BitVector.bv_t>

(declare-fun FStar.BitVector.bv_t (Term) Term)
;;;;;;;;;;;;;;;;n: Prims.nat -> Type
(declare-fun Tm_arrow_9974df5c311cfcfa7100bc7bef095e1e () Term)
(declare-fun FStar.BitVector.bv_t@tok () Term)
(declare-fun Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e (Term) Term)
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name FStar.BitVector.bv_t; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(31,21-31,51); use=FStar.BitVector.fst(31,21-31,51)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (iff (HasTypeFuel @u0
@x1
(Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e @x2))
(and (HasTypeFuel @u0
@x1
(FStar.Seq.Base.seq Prims.bool))

;; def=FStar.BitVector.fst(31,36-31,50); use=FStar.BitVector.fst(31,36-31,50)
(= (FStar.Seq.Base.length Prims.bool
@x1)
@x2)
))
 

:pattern ((HasTypeFuel @u0
@x1
(Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e @x2)))
:qid refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e))

:named refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e))
;;;;;;;;;;;;;;;;Equation for FStar.BitVector.bv_t
;;; Fact-ids: Name FStar.BitVector.bv_t; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(31,5-31,9); use=FStar.BitVector.fst(31,5-31,9)
(forall ((@x0 Term))
 (! (= (FStar.BitVector.bv_t @x0)
(Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e @x0))
 

:pattern ((FStar.BitVector.bv_t @x0))
:qid equation_FStar.BitVector.bv_t))

:named equation_FStar.BitVector.bv_t))

; </end encoding FStar.BitVector.bv_t>


; <Start encoding FStar.BitVector.zero_vec>

(declare-fun FStar.BitVector.zero_vec (Term) Term)
;;;;;;;;;;;;;;;;FStar.BitVector.bv_t n
(declare-fun Tm_arrow_b6d52a9c4babaef5c45b062eb8723782 () Term)
(declare-fun FStar.BitVector.zero_vec@tok () Term)

; </end encoding FStar.BitVector.zero_vec>


; <Start encoding FStar.BitVector.elem_vec>


(declare-fun FStar.BitVector.elem_vec (Term Term) Term)

;;;;;;;;;;;;;;;;i: Prims.nat{i < n} -> FStar.BitVector.bv_t n
(declare-fun Tm_arrow_6880b3a4da9e8c38f1dbaa400eb50d7d () Term)
(declare-fun FStar.BitVector.elem_vec@tok () Term)


; </end encoding FStar.BitVector.elem_vec>


; <Start encoding FStar.BitVector.ones_vec>

(declare-fun FStar.BitVector.ones_vec (Term) Term)

(declare-fun FStar.BitVector.ones_vec@tok () Term)

; </end encoding FStar.BitVector.ones_vec>


; <Start encoding FStar.BitVector.logand_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.BitVector.logand_vec.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.BitVector.logand_vec.fuel_instrumented_token () Term)
(declare-fun FStar.BitVector.logand_vec (Term Term Term) Term)
(declare-fun FStar.BitVector.logand_vec@tok () Term)
;;;;;;;;;;;;;;;;a: FStar.BitVector.bv_t n -> b: FStar.BitVector.bv_t n -> FStar.BitVector.bv_t n
(declare-fun Tm_arrow_d5001f682a0789c7aa8e67d06058b034 () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.BitVector.logand_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(45,8-45,18); use=FStar.BitVector.fst(45,8-45,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.BitVector.logand_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.BitVector.logand_vec.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.BitVector.logand_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.BitVector.logand_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.BitVector.logand_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.BitVector.logand_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(45,8-45,18); use=FStar.BitVector.fst(45,8-45,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.BitVector.logand_vec @x0
@x1
@x2)
(FStar.BitVector.logand_vec.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.BitVector.logand_vec @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.BitVector.logand_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.BitVector.logand_vec.fuel_instrumented))

; </end encoding FStar.BitVector.logand_vec>


; <Start encoding FStar.BitVector.logand_vec_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.logand_vec_definition (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.logand_vec_definition@tok () Term)


; </end encoding FStar.BitVector.logand_vec_definition>


; <Start encoding FStar.BitVector.logxor_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.BitVector.logxor_vec.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.BitVector.logxor_vec.fuel_instrumented_token () Term)
(declare-fun FStar.BitVector.logxor_vec (Term Term Term) Term)
(declare-fun FStar.BitVector.logxor_vec@tok () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.BitVector.logxor_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(57,8-57,18); use=FStar.BitVector.fst(57,8-57,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.BitVector.logxor_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.BitVector.logxor_vec.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.BitVector.logxor_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.BitVector.logxor_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.BitVector.logxor_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.BitVector.logxor_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(57,8-57,18); use=FStar.BitVector.fst(57,8-57,18)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.BitVector.logxor_vec @x0
@x1
@x2)
(FStar.BitVector.logxor_vec.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.BitVector.logxor_vec @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.BitVector.logxor_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.BitVector.logxor_vec.fuel_instrumented))

; </end encoding FStar.BitVector.logxor_vec>


; <Start encoding FStar.BitVector.logxor_vec_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.logxor_vec_definition (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.logxor_vec_definition@tok () Term)


; </end encoding FStar.BitVector.logxor_vec_definition>


; <Start encoding FStar.BitVector.logor_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.BitVector.logor_vec.fuel_instrumented (Fuel Term Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.BitVector.logor_vec.fuel_instrumented_token () Term)
(declare-fun FStar.BitVector.logor_vec (Term Term Term) Term)
(declare-fun FStar.BitVector.logor_vec@tok () Term)

;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.BitVector.logor_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(69,8-69,17); use=FStar.BitVector.fst(69,8-69,17)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (= (FStar.BitVector.logor_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3)
(FStar.BitVector.logor_vec.fuel_instrumented ZFuel
@x1
@x2
@x3))
 

:pattern ((FStar.BitVector.logor_vec.fuel_instrumented (SFuel @u0)
@x1
@x2
@x3))
:qid @fuel_irrelevance_FStar.BitVector.logor_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.BitVector.logor_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.BitVector.logor_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(69,8-69,17); use=FStar.BitVector.fst(69,8-69,17)
(forall ((@x0 Term) (@x1 Term) (@x2 Term))
 (! (= (FStar.BitVector.logor_vec @x0
@x1
@x2)
(FStar.BitVector.logor_vec.fuel_instrumented MaxFuel
@x0
@x1
@x2))
 

:pattern ((FStar.BitVector.logor_vec @x0
@x1
@x2))
:qid @fuel_correspondence_FStar.BitVector.logor_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.BitVector.logor_vec.fuel_instrumented))

; </end encoding FStar.BitVector.logor_vec>


; <Start encoding FStar.BitVector.logor_vec_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.logor_vec_definition (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.logor_vec_definition@tok () Term)


; </end encoding FStar.BitVector.logor_vec_definition>


; <Start encoding FStar.BitVector.lognot_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.BitVector.lognot_vec.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.BitVector.lognot_vec.fuel_instrumented_token () Term)
(declare-fun FStar.BitVector.lognot_vec (Term Term) Term)
(declare-fun FStar.BitVector.lognot_vec@tok () Term)
;;;;;;;;;;;;;;;;a: FStar.BitVector.bv_t n -> FStar.BitVector.bv_t n
(declare-fun Tm_arrow_190e27813ba14c0d36577dc3d47778da () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.BitVector.lognot_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(81,8-81,18); use=FStar.BitVector.fst(81,8-81,18)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.BitVector.lognot_vec.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.BitVector.lognot_vec.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.BitVector.lognot_vec.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.BitVector.lognot_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.BitVector.lognot_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.BitVector.lognot_vec; Namespace FStar.BitVector
(assert (! 
;; def=FStar.BitVector.fst(81,8-81,18); use=FStar.BitVector.fst(81,8-81,18)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.BitVector.lognot_vec @x0
@x1)
(FStar.BitVector.lognot_vec.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.BitVector.lognot_vec @x0
@x1))
:qid @fuel_correspondence_FStar.BitVector.lognot_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.BitVector.lognot_vec.fuel_instrumented))

; </end encoding FStar.BitVector.lognot_vec>


; <Start encoding FStar.BitVector.lognot_vec_definition>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.lognot_vec_definition (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.lognot_vec_definition@tok () Term)


; </end encoding FStar.BitVector.lognot_vec_definition>


; <Start encoding FStar.BitVector.lemma_xor_bounded>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.lemma_xor_bounded (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.lemma_xor_bounded@tok () Term)

; </end encoding FStar.BitVector.lemma_xor_bounded>


; <Start encoding FStar.BitVector.is_subset_vec>

(declare-fun FStar.BitVector.is_subset_vec (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: FStar.BitVector.bv_t n -> b: FStar.BitVector.bv_t n -> Prims.logical
(declare-fun Tm_arrow_b51a0c80adeae3f31b1215853bb34fe1 () Term)
(declare-fun FStar.BitVector.is_subset_vec@tok () Term)

(declare-fun Tm_abs_c26b15d92a0324df025664cb1dd69766 (Term Term Term) Term)

; </end encoding FStar.BitVector.is_subset_vec>


; <Start encoding FStar.BitVector.is_superset_vec>

(declare-fun FStar.BitVector.is_superset_vec (Term Term Term) Term)

(declare-fun FStar.BitVector.is_superset_vec@tok () Term)

(declare-fun Tm_abs_8fab78d513bbc3161e95814bf48fbd56 (Term Term Term) Term)

; </end encoding FStar.BitVector.is_superset_vec>


; <Start encoding FStar.BitVector.lemma_slice_subset_vec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.lemma_slice_subset_vec (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.lemma_slice_subset_vec@tok () Term)

; </end encoding FStar.BitVector.lemma_slice_subset_vec>


; <Start encoding FStar.BitVector.lemma_slice_superset_vec>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.lemma_slice_superset_vec (Term Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.lemma_slice_superset_vec@tok () Term)

; </end encoding FStar.BitVector.lemma_slice_superset_vec>


; <Start encoding FStar.BitVector.shift_left_vec>

(declare-fun FStar.BitVector.shift_left_vec (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: FStar.BitVector.bv_t n -> s: Prims.nat -> FStar.BitVector.bv_t n
(declare-fun Tm_arrow_ccbebd343bd3a7caba5f263c2ba5f3be () Term)
(declare-fun FStar.BitVector.shift_left_vec@tok () Term)

; </end encoding FStar.BitVector.shift_left_vec>


; <Start encoding FStar.BitVector.shift_left_vec_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_left_vec_lemma_1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_left_vec_lemma_1@tok () Term)
(declare-fun Tm_refine_6ccf0869e6825997ab860bb25791c11f (Term Term) Term)

; </end encoding FStar.BitVector.shift_left_vec_lemma_1>


; <Start encoding FStar.BitVector.shift_left_vec_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_left_vec_lemma_2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_left_vec_lemma_2@tok () Term)
(declare-fun Tm_refine_e8e1ad4b2203cd724d5b8b2dba0a5826 (Term Term) Term)

; </end encoding FStar.BitVector.shift_left_vec_lemma_2>


; <Start encoding FStar.BitVector.shift_right_vec>

(declare-fun FStar.BitVector.shift_right_vec (Term Term Term) Term)

(declare-fun FStar.BitVector.shift_right_vec@tok () Term)

; </end encoding FStar.BitVector.shift_right_vec>


; <Start encoding FStar.BitVector.shift_right_vec_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_right_vec_lemma_1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_right_vec_lemma_1@tok () Term)
(declare-fun Tm_refine_34425c23b534b8a294f8f063dd9faa4b (Term Term) Term)

; </end encoding FStar.BitVector.shift_right_vec_lemma_1>


; <Start encoding FStar.BitVector.shift_right_vec_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_right_vec_lemma_2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_right_vec_lemma_2@tok () Term)
(declare-fun Tm_refine_c0ec47abc53a2509e744dad22ccf8191 (Term Term) Term)

; </end encoding FStar.BitVector.shift_right_vec_lemma_2>


; <Start encoding FStar.BitVector.shift_arithmetic_right_vec>

(declare-fun FStar.BitVector.shift_arithmetic_right_vec (Term Term Term) Term)

(declare-fun FStar.BitVector.shift_arithmetic_right_vec@tok () Term)

; </end encoding FStar.BitVector.shift_arithmetic_right_vec>


; <Start encoding FStar.BitVector.shift_arithmetic_right_vec_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_arithmetic_right_vec_lemma_1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_arithmetic_right_vec_lemma_1@tok () Term)


; </end encoding FStar.BitVector.shift_arithmetic_right_vec_lemma_1>


; <Start encoding FStar.BitVector.shift_arithmetic_right_vec_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.BitVector.shift_arithmetic_right_vec_lemma_2 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.BitVector.shift_arithmetic_right_vec_lemma_2@tok () Term)


; </end encoding FStar.BitVector.shift_arithmetic_right_vec_lemma_2>


; End Externals for module FStar.BitVector


; UNSAT CORE: @MaxIFuel_assumption, @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented, @fuel_correspondence_Prims.pow2.fuel_instrumented, @query, Prims_pretyping_f537159ed795b314b4e58c260361ae86, bool_typing, equation_FStar.BitVector.bv_t, equation_FStar.UInt.max_int, equation_FStar.UInt.min_int, equation_Prims.eqtype, equation_Prims.nat, function_token_typing_Prims.bool, int_inversion, int_typing, lemma_FStar.Seq.Base.lemma_len_slice, primitive_Prims.op_Addition, primitive_Prims.op_LessThanOrEqual, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxBool_proj_0, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_414d0a9f578ab0048252f8c8f552b99f, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_81407705a0828c2c1b1976675443f647, refinement_interpretation_Tm_refine_c1424615841f28cac7fc34e92b7ff33c, refinement_interpretation_Tm_refine_e2d5d62a90ceed8a6faf9d20615f4e1e

;;; End module FStar.BitVector (156 decls; total size 17485)

; Internals for FStar.UInt

(push)

; encoding sigelt pow2_values


; <Skipped FStar.UInt.pow2_values/>


; encoding sigelt pow2_values


; <Start encoding FStar.UInt.pow2_values>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.pow2_values (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.pow2_values@tok () Term)

; </end encoding FStar.UInt.pow2_values>


; encoding sigelt max_int


; <Start encoding FStar.UInt.max_int>

(declare-fun FStar.UInt.max_int (Term) Term)
;;;;;;;;;;;;;;;;n: Prims.nat -> Prims.int
(declare-fun Tm_arrow_fc34ca66de2f262c06145b17fb7ed6cb () Term)
(declare-fun FStar.UInt.max_int@tok () Term)
;;;;;;;;;;;;;;;;Equation for FStar.UInt.max_int
;;; Fact-ids: Name FStar.UInt.max_int; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(46,4-46,11); use=FStar.UInt.fsti(46,4-46,11)
(forall ((@x0 Term))
 (! (= (FStar.UInt.max_int @x0)
(Prims.op_Subtraction (Prims.pow2 @x0)
(BoxInt 1)))
 

:pattern ((FStar.UInt.max_int @x0))
:qid equation_FStar.UInt.max_int))

:named equation_FStar.UInt.max_int))

; </end encoding FStar.UInt.max_int>


; encoding sigelt min_int


; <Start encoding FStar.UInt.min_int>

(declare-fun FStar.UInt.min_int (Term) Term)

(declare-fun FStar.UInt.min_int@tok () Term)
;;;;;;;;;;;;;;;;Equation for FStar.UInt.min_int
;;; Fact-ids: Name FStar.UInt.min_int; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(47,4-47,11); use=FStar.UInt.fsti(47,4-47,11)
(forall ((@x0 Term))
 (! (= (FStar.UInt.min_int @x0)
(BoxInt 0))
 

:pattern ((FStar.UInt.min_int @x0))
:qid equation_FStar.UInt.min_int))

:named equation_FStar.UInt.min_int))

; </end encoding FStar.UInt.min_int>


; encoding sigelt fits


; <Start encoding FStar.UInt.fits>

(declare-fun FStar.UInt.fits (Term Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> n: Prims.nat -> Prims.bool
(declare-fun Tm_arrow_dea48782e508c14fa98dcf9716548804 () Term)
(declare-fun FStar.UInt.fits@tok () Term)

; </end encoding FStar.UInt.fits>


; encoding sigelt size


; <Start encoding FStar.UInt.size>

(declare-fun FStar.UInt.size (Term Term) Term)
;;;;;;;;;;;;;;;;x: Prims.int -> n: Prims.nat -> Type
(declare-fun Tm_arrow_f4ec8f8bfe492e31741a15356024bbaa () Term)
(declare-fun FStar.UInt.size@tok () Term)

; </end encoding FStar.UInt.size>


; encoding sigelt uint_t


; <Start encoding FStar.UInt.uint_t>

(declare-fun FStar.UInt.uint_t (Term) Term)

(declare-fun FStar.UInt.uint_t@tok () Term)
(declare-fun Tm_refine_f13070840248fced9d9d60d77bdae3ec (Term) Term)

; </end encoding FStar.UInt.uint_t>


; encoding sigelt zero


; <Start encoding FStar.UInt.zero>

(declare-fun FStar.UInt.zero (Term) Term)
;;;;;;;;;;;;;;;;n: Prims.nat -> uint_t n
(declare-fun Tm_arrow_f1dd811328ea3b27fc410fa0f52880f7 () Term)
(declare-fun FStar.UInt.zero@tok () Term)

; </end encoding FStar.UInt.zero>


; encoding sigelt pow2_n


; <Start encoding FStar.UInt.pow2_n>


(declare-fun FStar.UInt.pow2_n (Term Term) Term)

;;;;;;;;;;;;;;;;p: Prims.nat{p < n} -> uint_t n
(declare-fun Tm_arrow_8d41edd1e7b665db26512e6c6d9ece64 () Term)
(declare-fun FStar.UInt.pow2_n@tok () Term)


; </end encoding FStar.UInt.pow2_n>


; encoding sigelt one


; <Start encoding FStar.UInt.one>

(declare-fun FStar.UInt.one (Term) Term)
;;;;;;;;;;;;;;;;n: Prims.pos -> uint_t n
(declare-fun Tm_arrow_89d370fa478cfd1f85a8759662ce0390 () Term)
(declare-fun FStar.UInt.one@tok () Term)

; </end encoding FStar.UInt.one>


; encoding sigelt ones


; <Start encoding FStar.UInt.ones>

(declare-fun FStar.UInt.ones (Term) Term)

(declare-fun FStar.UInt.ones@tok () Term)

; </end encoding FStar.UInt.ones>


; encoding sigelt incr


; <Start encoding FStar.UInt.incr>

(declare-fun FStar.UInt.incr (Term Term) Term)
(declare-fun Tm_refine_22e8629663f0cb1c9de86e57e73778e3 (Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_e8e04e4a1022a7343e76760b76915c9e () Term)
(declare-fun FStar.UInt.incr@tok () Term)


; </end encoding FStar.UInt.incr>


; encoding sigelt decr


; <Start encoding FStar.UInt.decr>

(declare-fun FStar.UInt.decr (Term Term) Term)

;;;;;;;;;;;;;;;;a: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_2a167fb2d2f3f00bff7b73f048db0e83 () Term)
(declare-fun FStar.UInt.decr@tok () Term)


; </end encoding FStar.UInt.decr>


; encoding sigelt incr_underspec


; <Skipped FStar.UInt.incr_underspec/>


; encoding sigelt incr_underspec


; <Start encoding FStar.UInt.incr_underspec>

(declare-fun FStar.UInt.incr_underspec (Term Term) Term)
(declare-fun Tm_refine_6a367e92d5b1ca10009a43bd430dd796 (Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_fb114bd2e9239af1296268eb30490ff7 () Term)
(declare-fun FStar.UInt.incr_underspec@tok () Term)


; </end encoding FStar.UInt.incr_underspec>


; encoding sigelt decr_underspec


; <Skipped FStar.UInt.decr_underspec/>


; encoding sigelt decr_underspec


; <Start encoding FStar.UInt.decr_underspec>

(declare-fun FStar.UInt.decr_underspec (Term Term) Term)
(declare-fun Tm_refine_fa3c796c533e86dc9f3e3ffc647718f6 (Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_f1853f30408c6d0beb7795897a3ab5bc () Term)
(declare-fun FStar.UInt.decr_underspec@tok () Term)


; </end encoding FStar.UInt.decr_underspec>


; encoding sigelt incr_mod


; <Start encoding FStar.UInt.incr_mod>

(declare-fun FStar.UInt.incr_mod (Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> uint_t n
(declare-fun Tm_arrow_a565732dbe0b43ae2274b1f24341f11b () Term)
(declare-fun FStar.UInt.incr_mod@tok () Term)

; </end encoding FStar.UInt.incr_mod>


; encoding sigelt decr_mod


; <Start encoding FStar.UInt.decr_mod>

(declare-fun FStar.UInt.decr_mod (Term Term) Term)

(declare-fun FStar.UInt.decr_mod@tok () Term)

; </end encoding FStar.UInt.decr_mod>


; encoding sigelt add


; <Start encoding FStar.UInt.add>

(declare-fun FStar.UInt.add (Term Term Term) Term)

;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_ea9f73d61c207ec4508af75e87c5ca13 () Term)
(declare-fun FStar.UInt.add@tok () Term)


; </end encoding FStar.UInt.add>


; encoding sigelt add_underspec


; <Skipped FStar.UInt.add_underspec/>


; encoding sigelt add_underspec


; <Start encoding FStar.UInt.add_underspec>

(declare-fun FStar.UInt.add_underspec (Term Term Term) Term)
(declare-fun Tm_refine_c7a9b50c1b5983f8171c03368a208e31 (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_880847ba34dd402fb6567384684864a6 () Term)
(declare-fun FStar.UInt.add_underspec@tok () Term)


; </end encoding FStar.UInt.add_underspec>


; encoding sigelt add_mod


; <Start encoding FStar.UInt.add_mod>

(declare-fun FStar.UInt.add_mod (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> uint_t n
(declare-fun Tm_arrow_2f3c6a962eb1cbbfd959311c0f20b277 () Term)
(declare-fun FStar.UInt.add_mod@tok () Term)

; </end encoding FStar.UInt.add_mod>


; encoding sigelt sub


; <Start encoding FStar.UInt.sub>

(declare-fun FStar.UInt.sub (Term Term Term) Term)

;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_974b47e4388c1a4055fe210bb6a11687 () Term)
(declare-fun FStar.UInt.sub@tok () Term)


; </end encoding FStar.UInt.sub>


; encoding sigelt sub_underspec


; <Skipped FStar.UInt.sub_underspec/>


; encoding sigelt sub_underspec


; <Start encoding FStar.UInt.sub_underspec>

(declare-fun FStar.UInt.sub_underspec (Term Term Term) Term)
(declare-fun Tm_refine_109ae46bb20ad559af297346ec64ae4e (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_1479a03f646b965be1bfedb2ee360f95 () Term)
(declare-fun FStar.UInt.sub_underspec@tok () Term)


; </end encoding FStar.UInt.sub_underspec>


; encoding sigelt sub_mod


; <Start encoding FStar.UInt.sub_mod>

(declare-fun FStar.UInt.sub_mod (Term Term Term) Term)

(declare-fun FStar.UInt.sub_mod@tok () Term)

; </end encoding FStar.UInt.sub_mod>


; encoding sigelt mul


; <Start encoding FStar.UInt.mul>

(declare-fun FStar.UInt.mul (Term Term Term) Term)

;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_45e02637bbbba15e6760300e4a62b58d () Term)
(declare-fun FStar.UInt.mul@tok () Term)


; </end encoding FStar.UInt.mul>


; encoding sigelt mul_underspec


; <Skipped FStar.UInt.mul_underspec/>


; encoding sigelt mul_underspec


; <Start encoding FStar.UInt.mul_underspec>

(declare-fun FStar.UInt.mul_underspec (Term Term Term) Term)
(declare-fun Tm_refine_ea207e5cce50229e615af011837e59a5 (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_1f5fca1fff06689d84a49261819dc580 () Term)
(declare-fun FStar.UInt.mul_underspec@tok () Term)


; </end encoding FStar.UInt.mul_underspec>


; encoding sigelt mul_mod


; <Start encoding FStar.UInt.mul_mod>

(declare-fun FStar.UInt.mul_mod (Term Term Term) Term)

(declare-fun FStar.UInt.mul_mod@tok () Term)

; </end encoding FStar.UInt.mul_mod>


; encoding sigelt 


; <Skipped />


; encoding sigelt lt_square_div_lt


; <Skipped FStar.UInt.lt_square_div_lt/>


; encoding sigelt lt_square_div_lt


; <Start encoding FStar.UInt.lt_square_div_lt>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.lt_square_div_lt (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.lt_square_div_lt@tok () Term)

; </end encoding FStar.UInt.lt_square_div_lt>


; encoding sigelt mul_div


; <Start encoding FStar.UInt.mul_div>

(declare-fun FStar.UInt.mul_div (Term Term Term) Term)

(declare-fun FStar.UInt.mul_div@tok () Term)

; </end encoding FStar.UInt.mul_div>


; encoding sigelt div


; <Start encoding FStar.UInt.div>

(declare-fun Tm_refine_0722e9115d2a1be8d90527397d01011c (Term) Term)
(declare-fun FStar.UInt.div (Term Term Term) Term)

(declare-fun Tm_refine_e49d79feeb1e96b29b0f01b06f8dac23 (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n {b <> 0} -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_6ebc7a9e6ff34015952a4168421980bf () Term)
(declare-fun FStar.UInt.div@tok () Term)



; </end encoding FStar.UInt.div>


; encoding sigelt div_underspec


; <Skipped FStar.UInt.div_underspec/>


; encoding sigelt div_underspec


; <Start encoding FStar.UInt.div_underspec>


(declare-fun FStar.UInt.div_underspec (Term Term Term) Term)

(declare-fun Tm_refine_fafbb762e9b0100ba27aa174122ddaa3 (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n {b <> 0} -> Prims.Pure (uint_t n)
(declare-fun Tm_arrow_ed1485a952a27dc4770fb0182ab26e79 () Term)
(declare-fun FStar.UInt.div_underspec@tok () Term)



; </end encoding FStar.UInt.div_underspec>


; encoding sigelt 


; <Skipped />


; encoding sigelt div_size


; <Skipped FStar.UInt.div_size/>


; encoding sigelt div_size


; <Start encoding FStar.UInt.div_size>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.div_size (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.div_size@tok () Term)

; </end encoding FStar.UInt.div_size>


; encoding sigelt udiv


; <Start encoding FStar.UInt.udiv>


(declare-fun FStar.UInt.udiv (Term Term Term) Term)


;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n {b <> 0} -> c: uint_t n {b <> 0 ==> a / b = c}
(declare-fun Tm_arrow_2b6a409bd2eeb88753b2b6fe89b0d0a9 () Term)
(declare-fun FStar.UInt.udiv@tok () Term)



; </end encoding FStar.UInt.udiv>


; encoding sigelt mod


; <Start encoding FStar.UInt.mod>


(declare-fun FStar.UInt.mod (Term Term Term) Term)

;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n {b <> 0} -> uint_t n
(declare-fun Tm_arrow_6ae50616ce0b08fd950ce0be5e711193 () Term)
(declare-fun FStar.UInt.mod@tok () Term)


; </end encoding FStar.UInt.mod>


; encoding sigelt eq


; <Start encoding FStar.UInt.eq>

(declare-fun FStar.UInt.eq (Term Term Term) Term)
;;;;;;;;;;;;;;;;a: uint_t n -> b: uint_t n -> Prims.bool
(declare-fun Tm_arrow_ed25d9271888f66e143c5c59e11fb3a9 () Term)
(declare-fun FStar.UInt.eq@tok () Term)

; </end encoding FStar.UInt.eq>


; encoding sigelt gt


; <Start encoding FStar.UInt.gt>

(declare-fun FStar.UInt.gt (Term Term Term) Term)

(declare-fun FStar.UInt.gt@tok () Term)

; </end encoding FStar.UInt.gt>


; encoding sigelt gte


; <Start encoding FStar.UInt.gte>

(declare-fun FStar.UInt.gte (Term Term Term) Term)

(declare-fun FStar.UInt.gte@tok () Term)

; </end encoding FStar.UInt.gte>


; encoding sigelt lt


; <Start encoding FStar.UInt.lt>

(declare-fun FStar.UInt.lt (Term Term Term) Term)

(declare-fun FStar.UInt.lt@tok () Term)

; </end encoding FStar.UInt.lt>


; encoding sigelt lte


; <Start encoding FStar.UInt.lte>

(declare-fun FStar.UInt.lte (Term Term Term) Term)

(declare-fun FStar.UInt.lte@tok () Term)

; </end encoding FStar.UInt.lte>


; encoding sigelt to_uint_t


; <Start encoding FStar.UInt.to_uint_t>

(declare-fun FStar.UInt.to_uint_t (Term Term) Term)
;;;;;;;;;;;;;;;;m: Prims.nat -> a: Prims.int -> uint_t m
(declare-fun Tm_arrow_d5257ef463a03617bca88873b50f4e96 () Term)
(declare-fun FStar.UInt.to_uint_t@tok () Term)

; </end encoding FStar.UInt.to_uint_t>


; encoding sigelt to_vec


; <Start encoding FStar.UInt.to_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.UInt.to_vec.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.UInt.to_vec.fuel_instrumented_token () Term)
(declare-fun FStar.UInt.to_vec (Term Term) Term)
(declare-fun FStar.UInt.to_vec@tok () Term)
;;;;;;;;;;;;;;;;num: uint_t n -> FStar.BitVector.bv_t n
(declare-fun Tm_arrow_50c9ac04c4da2f9a3a1512bf3cfd180e () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.UInt.to_vec; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(180,8-180,14); use=FStar.UInt.fsti(180,8-180,14)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.UInt.to_vec.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.UInt.to_vec.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.UInt.to_vec.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.UInt.to_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.UInt.to_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.UInt.to_vec; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(180,8-180,14); use=FStar.UInt.fsti(180,8-180,14)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.UInt.to_vec @x0
@x1)
(FStar.UInt.to_vec.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.UInt.to_vec @x0
@x1))
:qid @fuel_correspondence_FStar.UInt.to_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.UInt.to_vec.fuel_instrumented))

; </end encoding FStar.UInt.to_vec>


; encoding sigelt from_vec


; <Start encoding FStar.UInt.from_vec>

;;;;;;;;;;;;;;;;Fuel-instrumented function name
(declare-fun FStar.UInt.from_vec.fuel_instrumented (Fuel Term Term) Term)
;;;;;;;;;;;;;;;;Token for fuel-instrumented partial applications
(declare-fun FStar.UInt.from_vec.fuel_instrumented_token () Term)
(declare-fun FStar.UInt.from_vec (Term Term) Term)
(declare-fun FStar.UInt.from_vec@tok () Term)
;;;;;;;;;;;;;;;;vec: FStar.BitVector.bv_t n -> uint_t n
(declare-fun Tm_arrow_3a21f80bb386ebae30b30ec5363d47ef () Term)
;;;;;;;;;;;;;;;;Fuel irrelevance
;;; Fact-ids: Name FStar.UInt.from_vec; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(184,8-184,16); use=FStar.UInt.fsti(184,8-184,16)
(forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (= (FStar.UInt.from_vec.fuel_instrumented (SFuel @u0)
@x1
@x2)
(FStar.UInt.from_vec.fuel_instrumented ZFuel
@x1
@x2))
 

:pattern ((FStar.UInt.from_vec.fuel_instrumented (SFuel @u0)
@x1
@x2))
:qid @fuel_irrelevance_FStar.UInt.from_vec.fuel_instrumented))

:named @fuel_irrelevance_FStar.UInt.from_vec.fuel_instrumented))
;;;;;;;;;;;;;;;;Correspondence of recursive function to instrumented version
;;; Fact-ids: Name FStar.UInt.from_vec; Namespace FStar.UInt
(assert (! 
;; def=FStar.UInt.fsti(184,8-184,16); use=FStar.UInt.fsti(184,8-184,16)
(forall ((@x0 Term) (@x1 Term))
 (! (= (FStar.UInt.from_vec @x0
@x1)
(FStar.UInt.from_vec.fuel_instrumented MaxFuel
@x0
@x1))
 

:pattern ((FStar.UInt.from_vec @x0
@x1))
:qid @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented))

:named @fuel_correspondence_FStar.UInt.from_vec.fuel_instrumented))

; </end encoding FStar.UInt.from_vec>


; encoding sigelt to_vec_lemma_1


; <Skipped FStar.UInt.to_vec_lemma_1/>


; encoding sigelt to_vec_lemma_1


; <Start encoding FStar.UInt.to_vec_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.to_vec_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.to_vec_lemma_1@tok () Term)

; </end encoding FStar.UInt.to_vec_lemma_1>


; encoding sigelt to_vec_lemma_2


; <Skipped FStar.UInt.to_vec_lemma_2/>


; encoding sigelt to_vec_lemma_2


; <Start encoding FStar.UInt.to_vec_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.to_vec_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.to_vec_lemma_2@tok () Term)

; </end encoding FStar.UInt.to_vec_lemma_2>


; encoding sigelt inverse_aux


; <Skipped FStar.UInt.inverse_aux/>


; encoding sigelt inverse_aux


; <Start encoding FStar.UInt.inverse_aux>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.inverse_aux (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.inverse_aux@tok () Term)


; </end encoding FStar.UInt.inverse_aux>


; encoding sigelt inverse_vec_lemma


; <Skipped FStar.UInt.inverse_vec_lemma/>


; encoding sigelt inverse_vec_lemma


; <Start encoding FStar.UInt.inverse_vec_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.inverse_vec_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.inverse_vec_lemma@tok () Term)

; </end encoding FStar.UInt.inverse_vec_lemma>


; encoding sigelt inverse_num_lemma


; <Skipped FStar.UInt.inverse_num_lemma/>


; encoding sigelt inverse_num_lemma


; <Start encoding FStar.UInt.inverse_num_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.inverse_num_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.inverse_num_lemma@tok () Term)

; </end encoding FStar.UInt.inverse_num_lemma>


; encoding sigelt from_vec_lemma_1


; <Skipped FStar.UInt.from_vec_lemma_1/>


; encoding sigelt from_vec_lemma_1


; <Start encoding FStar.UInt.from_vec_lemma_1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.from_vec_lemma_1 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.from_vec_lemma_1@tok () Term)

; </end encoding FStar.UInt.from_vec_lemma_1>


; encoding sigelt from_vec_lemma_2


; <Skipped FStar.UInt.from_vec_lemma_2/>


; encoding sigelt from_vec_lemma_2


; <Start encoding FStar.UInt.from_vec_lemma_2>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.UInt.from_vec_lemma_2 (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.UInt.from_vec_lemma_2@tok () Term)

; </end encoding FStar.UInt.from_vec_lemma_2>


; encoding sigelt 


; <Skipped />


; encoding sigelt from_vec_aux


; <Skipped FStar.UInt.from_vec_aux/>


; Starting query at FStar.UInt.fst(92,2-94,30)

(push)
(declare-fun label_14 () Bool)
(declare-fun label_13 () Bool)
(declare-fun label_12 () Bool)
(declare-fun label_11 () Bool)
(declare-fun label_10 () Bool)
(declare-fun label_9 () Bool)
(declare-fun label_8 () Bool)
(declare-fun label_7 () Bool)
(declare-fun label_6 () Bool)
(declare-fun label_5 () Bool)
(declare-fun label_4 () Bool)
(declare-fun label_3 () Bool)
(declare-fun label_2 () Bool)
(declare-fun label_1 () Bool)





; Encoding query formula : forall (n: Prims.nat) (a: FStar.BitVector.bv_t n) (s1: Prims.nat{s1 < n}) (s2: Prims.nat{s2 < s1}).
;   (*could not prove post-condition*)
;   forall (p: Prims.pure_post Prims.unit).
;     (forall (pure_result: Prims.unit).
;         FStar.UInt.from_vec (FStar.Seq.Base.slice a 0 s2) * Prims.pow2 (n - s2) +
;         FStar.UInt.from_vec (FStar.Seq.Base.slice a s2 s1) * Prims.pow2 (n - s1) +
;         FStar.UInt.from_vec (FStar.Seq.Base.slice a s1 n) =
;         (FStar.UInt.from_vec (FStar.Seq.Base.slice a 0 s2) * Prims.pow2 (s1 - s2) +
;           FStar.UInt.from_vec (FStar.Seq.Base.slice a s2 s1)) *
;         Prims.pow2 (n - s1) +
;         FStar.UInt.from_vec (FStar.Seq.Base.slice a s1 n) ==>
;         p pure_result) ==>
;     (s2 < s1 ==> 0 <= s2 && s2 <= FStar.Seq.Base.length a) /\
;     (forall (any_result: s2: Prims.nat{s2 < s1}).
;         s2 == any_result ==>
;         (forall (any_result: FStar.Seq.Base.seq Prims.bool).
;             FStar.Seq.Base.slice a 0 s2 == any_result ==>
;             FStar.Seq.Base.length (FStar.Seq.Base.slice a 0 s2) = s2 /\
;             (forall (return_val: FStar.BitVector.bv_t s2).
;                 return_val == FStar.Seq.Base.slice a 0 s2 ==>
;                 FStar.Seq.Base.slice a 0 s2 == return_val ==>
;                 (forall (any_result: FStar.UInt.uint_t s2).
;                     FStar.UInt.from_vec (FStar.Seq.Base.slice a 0 s2) == any_result ==>
;                     (forall (return_val: FStar.UInt.uint_t s2).
;                         return_val == FStar.UInt.from_vec (FStar.Seq.Base.slice a 0 s2) ==>
;                         FStar.UInt.from_vec (FStar.Seq.Base.slice a 0 s2) == return_val ==>
;                         s1 - s2 >= 0 /\
;                         (forall (return_val: Prims.nat).
;                             return_val == s1 - s2 ==>
;                             (forall (any_result: Prims.pos).
;                                 Prims.pow2 (s1 - s2) == any_result ==>
;                                 (forall (return_val: Prims.pos).
;                                     return_val == Prims.pow2 (s1 - s2) ==>
;                                     Prims.pow2 (s1 - s2) == return_val ==>
;                                     n - s1 >= 0 /\
;                                     (forall (return_val: Prims.nat).
;                                         return_val == n - s1 ==>
;                                         (forall (any_result: Prims.pos).
;                                             Prims.pow2 (n - s1) == any_result ==>
;                                             (forall (return_val: Prims.pos).
;                                                 return_val == Prims.pow2 (n - s1) ==>
;                                                 Prims.pow2 (n - s1) == return_val ==>
;                                                 (forall (pure_result: Prims.unit).
;                                                     (s2 < s1 ==>
;                                                       0 <= s2 && s2 <= FStar.Seq.Base.length a) /\
;                                                     (forall (any_result: s2: Prims.nat{s2 < s1}).
;                                                         s2 == any_result ==>
;                                                         (forall (any_result:
;                                                             FStar.Seq.Base.seq Prims.bool).
;                                                             FStar.Seq.Base.slice a 0 s2 ==
;                                                             any_result ==>
;                                                             FStar.Seq.Base.length (FStar.Seq.Base.slice
;                                                                   a
;                                                                   0
;                                                                   s2) =
;                                                             s2 /\
;                                                             (forall (return_val:
;                                                                 FStar.BitVector.bv_t s2).
;                                                                 return_val ==
;                                                                 FStar.Seq.Base.slice a 0 s2 ==>
;                                                                 FStar.Seq.Base.slice a 0 s2 ==
;                                                                 return_val ==>
;                                                                 (forall (any_result:
;                                                                     FStar.UInt.uint_t s2).
;                                                                     FStar.UInt.from_vec (FStar.Seq.Base.slice
;                                                                           a
;                                                                           0
;                                                                           s2) ==
;                                                                     any_result ==>
;                                                                     (forall (return_val:
;                                                                         FStar.UInt.uint_t s2).
;                                                                         return_val ==
;                                                                         FStar.UInt.from_vec (FStar.Seq.Base.slice
;                                                                               a
;                                                                               0
;                                                                               s2) ==>
;                                                                         FStar.UInt.from_vec (FStar.Seq.Base.slice
;                                                                               a
;                                                                               0
;                                                                               s2) ==
;                                                                         return_val ==>
;                                                                         s1 - s2 >= 0 /\
;                                                                         (forall (return_val:
;                                                                             Prims.nat).
;                                                                             return_val == s1 - s2 ==>
;                                                                             (forall (any_result:
;                                                                                 Prims.pos).
;                                                                                 Prims.pow2 (s1 - s2) ==
;                                                                                 any_result ==>
;                                                                                 (forall (return_val:
;                                                                                     Prims.pos).
;                                                                                     return_val ==
;                                                                                     Prims.pow2 (s1 -
;                                                                                         s2) ==>
;                                                                                     Prims.pow2 (s1 -
;                                                                                         s2) ==
;                                                                                     return_val ==>
;                                                                                     n - s1 >= 0 /\
;                                                                                     (forall (return_val:
;                                                                                         Prims.nat).
;                                                                                         return_val ==
;                                                                                         n - s1 ==>
;                                                                                         (forall (any_result:
;                                                                                             Prims.pos)
;                                                                                           .
;                                                                                             Prims.pow2
;                                                                                               (n -
;                                                                                                 s1) ==
;                                                                                             any_result ==>
;                                                                                             (forall (return_val:
;                                                                                                 Prims.pos)
;                                                                                               .
;                                                                                                 return_val ==
;                                                                                                 Prims.pow2
;                                                                                                   (n -
;                                                                                                     s1
;                                                                                                   ) ==>
;                                                                                                 Prims.pow2
;                                                                                                   (n -
;                                                                                                     s1
;                                                                                                   ) ==
;                                                                                                 return_val ==>
;                                                                                                 (forall
;                                                                                                     (pure_result:
;                                                                                                     Prims.unit)
;                                                                                                   .
;                                                                                                     (FStar.UInt.from_vec
;                                                                                                       (
;                                                                                                         FStar.Seq.Base.slice
;                                                                                                           a
;                                                                                                           0
;                                                                                                           s2
; 
;                                                                                                       )
;                                                                                                      *
;                                                                                                     Prims.pow2
;                                                                                                       (
;                                                                                                         s1 -
;                                                                                                         s2
;                                                                                                       )
;                                                                                                     ) *
;                                                                                                     Prims.pow2
;                                                                                                       (
;                                                                                                         n -
;                                                                                                         s1
;                                                                                                       )
;                                                                                                      =
;                                                                                                     FStar.UInt.from_vec
;                                                                                                       (
;                                                                                                         FStar.Seq.Base.slice
;                                                                                                           a
;                                                                                                           0
;                                                                                                           s2
; 
;                                                                                                       )
;                                                                                                      *
;                                                                                                     (Prims.pow2
;                                                                                                       (
;                                                                                                         s1 -
;                                                                                                         s2
;                                                                                                       )
;                                                                                                      *
;                                                                                                     Prims.pow2
;                                                                                                       (
;                                                                                                         n -
;                                                                                                         s1
;                                                                                                       )
;                                                                                                     ) ==>
;                                                                                                     s1 -
;                                                                                                     s2 >=
;                                                                                                     0 /\
;                                                                                                     (
;                                                                                                       forall
;                                                                                                         (return_val:
;                                                                                                         Prims.nat)
;                                                                                                       .
;                                                                                                         return_val ==
;                                                                                                         s1 -
;                                                                                                         s2 ==>
;                                                                                                         n -
;                                                                                                         s1 >=
;                                                                                                         0 /\
;                                                                                                         (
;                                                                                                           forall
;                                                                                                             (return_val:
;                                                                                                             Prims.nat)
;                                                                                                           .
;                                                                                                             return_val ==
;                                                                                                             n -
;                                                                                                             s1 ==>
;                                                                                                             (
;                                                                                                               forall
;                                                                                                                 (pure_result:
;                                                                                                                 Prims.unit)
;                                                                                                               .
;                                                                                                                 Prims.pow2
;                                                                                                                   (
;                                                                                                                     s1 -
;                                                                                                                     s2
;                                                                                                                   )
;                                                                                                                  *
;                                                                                                                 Prims.pow2
;                                                                                                                   (
;                                                                                                                     n -
;                                                                                                                     s1
;                                                                                                                   )
;                                                                                                                  =
;                                                                                                                 Prims.pow2
;                                                                                                                   (
;                                                                                                                     s1 -
;                                                                                                                     s2 +
;                                                                                                                     (n -
;                                                                                                                     s1)
;                                                                                                                   )
;                                                                                                                  ==>
;                                                                                                                 p
;                                                                                                                   pure_result
; 
;                                                                                                             )
;                                                                                                         )
;                                                                                                     )
;                                                                                                 ))))
;                                                                                 ))))))))))))))))))))

(push)

; <fuel='0' ifuel='0'>

;;; Fact-ids: 
(assert (! (= MaxFuel
ZFuel)
:named @MaxFuel_assumption))
;;; Fact-ids: 
(assert (! (= MaxIFuel
ZFuel)
:named @MaxIFuel_assumption))
;;;;;;;;;;;;;;;;query
;;; Fact-ids: 
(assert (! (not (forall ((@x0 Term) (@x1 Term) (@x2 Term) (@x3 Term))
 (! (implies (and (HasType @x0
Prims.nat)
(HasType @x1
(FStar.BitVector.bv_t @x0))
(HasType @x2
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x0))
(HasType @x3
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x2)))

;; def=prims.fst(385,51-385,91); use=prims.fst(409,19-409,32)
(forall ((@x4 Term))
 (! (implies (and (HasType @x4
(Prims.pure_post Prims.unit))

;; def=prims.fst(420,36-420,97); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x5 Term))
 (! (implies (and (or label_1
(HasType @x5
Prims.unit))

;; def=FStar.UInt.fsti(214,17-214,301); use=FStar.UInt.fst(92,2-94,30)
(or label_2

;; def=FStar.UInt.fsti(214,17-214,301); use=FStar.UInt.fst(92,2-94,30)
(= (Prims.op_Addition (Prims.op_Addition (Prims.op_Multiply (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
(Prims.pow2 (Prims.op_Subtraction @x0
@x3)))
(Prims.op_Multiply (FStar.UInt.from_vec (Prims.op_Subtraction @x2
@x3)
(FStar.Seq.Base.slice Prims.bool
@x1
@x3
@x2))
(Prims.pow2 (Prims.op_Subtraction @x0
@x2))))
(FStar.UInt.from_vec (Prims.op_Subtraction @x0
@x2)
(FStar.Seq.Base.slice Prims.bool
@x1
@x2
@x0)))
(Prims.op_Addition (Prims.op_Multiply (Prims.op_Addition (Prims.op_Multiply (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
(Prims.pow2 (Prims.op_Subtraction @x2
@x3)))
(FStar.UInt.from_vec (Prims.op_Subtraction @x2
@x3)
(FStar.Seq.Base.slice Prims.bool
@x1
@x3
@x2)))
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))
(FStar.UInt.from_vec (Prims.op_Subtraction @x0
@x2)
(FStar.Seq.Base.slice Prims.bool
@x1
@x2
@x0))))
)
)

;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(92,2-94,30)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(92,2-94,30)
(ApplyTT @x4
@x5)
)
)
 

:pattern (
;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(92,2-94,30)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(92,2-94,30)
(ApplyTT @x4
@x5)
)
)
:qid @query.2))
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and (implies 
;; def=FStar.UInt.fsti(212,65-212,72); use=FStar.UInt.fst(92,42-92,44)
(< (BoxInt_proj_0 @x3)
(BoxInt_proj_0 @x2))


;; def=FStar.Seq.Base.fsti(58,49-58,72); use=FStar.UInt.fst(92,42-92,44)
(and 
;; def=FStar.UInt.fst(91,25-91,27); use=FStar.UInt.fst(92,42-92,44)
(or label_3
(BoxBool_proj_0 (Prims.op_LessThanOrEqual (BoxInt 0)
@x3)))


;; def=FStar.UInt.fst(91,25-91,27); use=FStar.UInt.fst(92,42-92,44)
(or label_4
(BoxBool_proj_0 (Prims.op_LessThanOrEqual @x3
(FStar.Seq.Base.length Prims.bool
@x1))))
)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x5 Term))
 (! (implies (and (HasType @x5
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x2))

;; def=FStar.Seq.Base.fsti(58,43-58,44); use=FStar.UInt.fst(92,2-94,30)
(= @x3
@x5)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x6 Term))
 (! (implies (and (HasType @x6
(FStar.Seq.Base.seq Prims.bool))

;; def=FStar.UInt.fsti(184,31-184,37); use=FStar.UInt.fst(92,18-92,45)
(= (FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)
@x6)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=FStar.BitVector.fst(31,36-31,50); use=FStar.UInt.fst(92,31-92,45)
(or label_5

;; def=FStar.BitVector.fst(31,36-31,50); use=FStar.UInt.fst(92,31-92,45)
(= (FStar.Seq.Base.length Prims.bool
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x3)
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x7 Term))
 (! (implies (and (HasType @x7
(FStar.BitVector.bv_t @x3))

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x7
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))


;; def=FStar.UInt.fsti(184,27-184,30); use=FStar.UInt.fst(92,2-94,30)
(= (FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)
@x7)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x8 Term))
 (! (implies (and (HasType @x8
(FStar.UInt.uint_t @x3))

;; def=FStar.UInt.fsti(184,45-184,55); use=FStar.UInt.fst(92,17-92,46)
(= (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x8)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x9 Term))
 (! (implies (and (HasType @x9
(FStar.UInt.uint_t @x3))

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x9
(FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)))


;; def=FStar.Math.Lemmas.fst(93,20-93,21); use=FStar.UInt.fst(92,2-94,30)
(= (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x9)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(92,53-92,62)
(or label_6

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(92,53-92,62)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x2
@x3))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x10 Term))
 (! (implies (and (HasType @x10
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x10
(Prims.op_Subtraction @x2
@x3))
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x11 Term))
 (! (implies (and (HasType @x11
Prims.pos)

;; def=prims.fst(687,28-687,31); use=FStar.UInt.fst(92,47-92,63)
(= (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
@x11)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x12 Term))
 (! (implies (and (HasType @x12
Prims.pos)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x12
(Prims.pow2 (Prims.op_Subtraction @x2
@x3)))


;; def=FStar.Math.Lemmas.fst(93,29-93,30); use=FStar.UInt.fst(92,2-94,30)
(= (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
@x12)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(92,70-92,78)
(or label_7

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(92,70-92,78)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x0
@x2))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x13 Term))
 (! (implies (and (HasType @x13
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x13
(Prims.op_Subtraction @x0
@x2))
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x14 Term))
 (! (implies (and (HasType @x14
Prims.pos)

;; def=prims.fst(687,28-687,31); use=FStar.UInt.fst(92,64-92,79)
(= (Prims.pow2 (Prims.op_Subtraction @x0
@x2))
@x14)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x15 Term))
 (! (implies (and (HasType @x15
Prims.pos)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x15
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))


;; def=FStar.Math.Lemmas.fst(93,38-93,39); use=FStar.UInt.fst(92,2-94,30)
(= (Prims.pow2 (Prims.op_Subtraction @x0
@x2))
@x15)
)

;; def=prims.fst(420,36-420,97); use=FStar.UInt.fst(92,2-92,16)
(forall ((@x16 Term))
 (! (implies (HasType @x16
Prims.unit)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and (implies 
;; def=FStar.UInt.fsti(212,65-212,72); use=FStar.UInt.fst(93,43-93,45)
(< (BoxInt_proj_0 @x3)
(BoxInt_proj_0 @x2))


;; def=FStar.Seq.Base.fsti(58,49-58,72); use=FStar.UInt.fst(93,43-93,45)
(and 
;; def=FStar.UInt.fst(91,25-91,27); use=FStar.UInt.fst(93,43-93,45)
(or label_8
(BoxBool_proj_0 (Prims.op_LessThanOrEqual (BoxInt 0)
@x3)))


;; def=FStar.UInt.fst(91,25-91,27); use=FStar.UInt.fst(93,43-93,45)
(or label_9
(BoxBool_proj_0 (Prims.op_LessThanOrEqual @x3
(FStar.Seq.Base.length Prims.bool
@x1))))
)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x17 Term))
 (! (implies (and (HasType @x17
(Tm_refine_c1424615841f28cac7fc34e92b7ff33c @x2))

;; def=FStar.Seq.Base.fsti(58,43-58,44); use=FStar.UInt.fst(92,2-94,30)
(= @x3
@x17)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x18 Term))
 (! (implies (and (HasType @x18
(FStar.Seq.Base.seq Prims.bool))

;; def=FStar.UInt.fsti(184,31-184,37); use=FStar.UInt.fst(93,19-93,46)
(= (FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)
@x18)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=FStar.BitVector.fst(31,36-31,50); use=FStar.UInt.fst(93,32-93,46)
(or label_10

;; def=FStar.BitVector.fst(31,36-31,50); use=FStar.UInt.fst(93,32-93,46)
(= (FStar.Seq.Base.length Prims.bool
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x3)
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x19 Term))
 (! (implies (and (HasType @x19
(FStar.BitVector.bv_t @x3))

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x19
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))


;; def=FStar.UInt.fsti(184,27-184,30); use=FStar.UInt.fst(92,2-94,30)
(= (FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)
@x19)
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x20 Term))
 (! (implies (and (HasType @x20
(FStar.UInt.uint_t @x3))

;; def=FStar.UInt.fsti(184,45-184,55); use=FStar.UInt.fst(93,18-93,47)
(= (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x20)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x21 Term))
 (! (implies (and (HasType @x21
(FStar.UInt.uint_t @x3))

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x21
(FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3)))


;; def=FStar.Math.Lemmas.fst(98,21-98,22); use=FStar.UInt.fst(92,2-94,30)
(= (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
@x21)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(93,54-93,63)
(or label_11

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(93,54-93,63)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x2
@x3))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x22 Term))
 (! (implies (and (HasType @x22
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x22
(Prims.op_Subtraction @x2
@x3))
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x23 Term))
 (! (implies (and (HasType @x23
Prims.pos)

;; def=prims.fst(687,28-687,31); use=FStar.UInt.fst(93,48-93,64)
(= (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
@x23)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x24 Term))
 (! (implies (and (HasType @x24
Prims.pos)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x24
(Prims.pow2 (Prims.op_Subtraction @x2
@x3)))


;; def=FStar.Math.Lemmas.fst(98,30-98,31); use=FStar.UInt.fst(92,2-94,30)
(= (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
@x24)
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(93,71-93,79)
(or label_12

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(93,71-93,79)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x0
@x2))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x25 Term))
 (! (implies (and (HasType @x25
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x25
(Prims.op_Subtraction @x0
@x2))
)

;; def=prims.fst(430,66-430,102); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x26 Term))
 (! (implies (and (HasType @x26
Prims.pos)

;; def=prims.fst(687,28-687,31); use=FStar.UInt.fst(93,65-93,80)
(= (Prims.pow2 (Prims.op_Subtraction @x0
@x2))
@x26)
)

;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x27 Term))
 (! (implies (and (HasType @x27
Prims.pos)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x27
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))


;; def=FStar.Math.Lemmas.fst(98,39-98,40); use=FStar.UInt.fst(92,2-94,30)
(= (Prims.pow2 (Prims.op_Subtraction @x0
@x2))
@x27)
)

;; def=prims.fst(420,36-420,97); use=FStar.UInt.fst(93,2-93,17)
(forall ((@x28 Term))
 (! (implies (and (HasType @x28
Prims.unit)

;; def=FStar.Math.Lemmas.fst(99,2-99,27); use=FStar.UInt.fst(93,2-93,17)
(= (Prims.op_Multiply (Prims.op_Multiply (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
(Prims.pow2 (Prims.op_Subtraction @x2
@x3)))
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))
(Prims.op_Multiply (FStar.UInt.from_vec @x3
(FStar.Seq.Base.slice Prims.bool
@x1
(BoxInt 0)
@x3))
(Prims.op_Multiply (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))))
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(94,12-94,21)
(or label_13

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(94,12-94,21)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x2
@x3))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x29 Term))
 (! (implies (and (HasType @x29
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x29
(Prims.op_Subtraction @x2
@x3))
)

;; def=prims.fst(438,77-438,89); use=FStar.UInt.fst(92,2-94,30)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(94,22-94,30)
(or label_14

;; def=prims.fst(659,18-659,24); use=FStar.UInt.fst(94,22-94,30)
(>= (BoxInt_proj_0 (Prims.op_Subtraction @x0
@x2))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.UInt.fst(92,2-94,30)
(forall ((@x30 Term))
 (! (implies (and (HasType @x30
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.UInt.fst(92,2-94,30)
(= @x30
(Prims.op_Subtraction @x0
@x2))
)

;; def=prims.fst(420,36-420,97); use=FStar.UInt.fst(94,2-94,11)
(forall ((@x31 Term))
 (! (implies (and (HasType @x31
Prims.unit)

;; def=FStar.Math.Lemmas.fst(249,11-249,43); use=FStar.UInt.fst(94,2-94,11)
(= (Prims.op_Multiply (Prims.pow2 (Prims.op_Subtraction @x2
@x3))
(Prims.pow2 (Prims.op_Subtraction @x0
@x2)))
(Prims.pow2 (Prims.op_Addition (Prims.op_Subtraction @x2
@x3)
(Prims.op_Subtraction @x0
@x2))))
)

;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(94,2-94,11)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.UInt.fst(94,2-94,11)
(ApplyTT @x4
@x31)
)
)
 
;;no pats
:qid @query.29))
)
 
;;no pats
:qid @query.28))
)
)
 
;;no pats
:qid @query.27))
)
)
 
;;no pats
:qid @query.26))
)
 
;;no pats
:qid @query.25))
)
 
;;no pats
:qid @query.24))
)
 
;;no pats
:qid @query.23))
)
)
 
;;no pats
:qid @query.22))
)
 
;;no pats
:qid @query.21))
)
 
;;no pats
:qid @query.20))
)
)
 
;;no pats
:qid @query.19))
)
 
;;no pats
:qid @query.18))
)
 
;;no pats
:qid @query.17))
)
)
 
;;no pats
:qid @query.16))
)
 
;;no pats
:qid @query.15))
)
)
 
;;no pats
:qid @query.14))
)
 
;;no pats
:qid @query.13))
)
 
;;no pats
:qid @query.12))
)
 
;;no pats
:qid @query.11))
)
)
 
;;no pats
:qid @query.10))
)
 
;;no pats
:qid @query.9))
)
 
;;no pats
:qid @query.8))
)
)
 
;;no pats
:qid @query.7))
)
 
;;no pats
:qid @query.6))
)
 
;;no pats
:qid @query.5))
)
)
 
;;no pats
:qid @query.4))
)
 
;;no pats
:qid @query.3))
)
)
 
;;no pats
:qid @query.1))
)
 
;;no pats
:qid @query)))
:named @query))
(set-option :rlimit 2723280)
(check-sat)
