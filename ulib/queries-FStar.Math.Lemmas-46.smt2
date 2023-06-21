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

; </end encoding Prims.eqtype>


; <Start encoding Prims.bool>

(declare-fun Prims.bool () Term)

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
;;;;;;;;;;;;;;;;well-founded ordering on nat (alt)
;;; Fact-ids: Name Prims.int; Namespace Prims
(assert (! (forall ((@u0 Fuel) (@x1 Term) (@x2 Term))
 (! (implies (and (HasTypeFuel @u0
@x1
Prims.int)
(HasTypeFuel @u0
@x2
Prims.int)
(> (BoxInt_proj_0 @x1)
0)
(>= (BoxInt_proj_0 @x2)
0)
(< (BoxInt_proj_0 @x2)
(BoxInt_proj_0 @x1)))
(Valid (Prims.precedes Prims.lex_t
Prims.lex_t
@x2
@x1)))
 

:pattern ((HasTypeFuel @u0
@x1
Prims.int) (HasTypeFuel @u0
@x2
Prims.int) (Valid (Prims.precedes Prims.lex_t
Prims.lex_t
@x2
@x1)))
:qid well-founded-ordering-on-nat))
:named well-founded-ordering-on-nat))

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
;;;;;;;;;;;;;;;;refinement_interpretation
;;; Fact-ids: Name Prims.pos; Namespace Prims
(assert (! 
;; def=prims.fst(662,11-662,24); use=prims.fst(662,11-662,24)
(forall ((@u0 Fuel) (@x1 Term))
 (! (iff (HasTypeFuel @u0
@x1
Tm_refine_774ba3f728d91ead8ef40be66c9802e5)
(and (HasTypeFuel @u0
@x1
Prims.int)

;; def=prims.fst(662,18-662,23); use=prims.fst(662,18-662,23)
(> (BoxInt_proj_0 @x1)
(BoxInt_proj_0 (BoxInt 0)))
))
 

:pattern ((HasTypeFuel @u0
@x1
Tm_refine_774ba3f728d91ead8ef40be66c9802e5))
:qid refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5))

:named refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5))
;;;;;;;;;;;;;;;;Equation for Prims.pos
;;; Fact-ids: Name Prims.pos; Namespace Prims
(assert (! (= Prims.pos
Tm_refine_774ba3f728d91ead8ef40be66c9802e5)
:named equation_Prims.pos))

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
;;;;;;;;;;;;;;;;free var typing
;;; Fact-ids: Name Prims.pow2; Namespace Prims
(assert (! 
;; def=prims.fst(687,8-687,12); use=prims.fst(687,8-687,12)
(forall ((@x0 Term))
 (! (implies (HasType @x0
Prims.nat)
(HasType (Prims.pow2 @x0)
Prims.pos))
 

:pattern ((Prims.pow2 @x0))
:qid typing_Prims.pow2))

:named typing_Prims.pow2))
;;;;;;;;;;;;;;;;Equation for fuel-instrumented recursive function: Prims.pow2
;;; Fact-ids: Name Prims.pow2; Namespace Prims
(assert (! 
;; def=prims.fst(687,8-687,12); use=prims.fst(687,8-687,12)
(forall ((@u0 Fuel) (@x1 Term))
 (! (implies (HasType @x1
Prims.nat)
(= (Prims.pow2.fuel_instrumented (SFuel @u0)
@x1)
(let ((@lb2 @x1))
(ite (= @lb2
(BoxInt 0))
(BoxInt 1)
(Prims.op_Multiply (BoxInt 2)
(Prims.pow2.fuel_instrumented @u0
(Prims.op_Subtraction @x1
(BoxInt 1))))))))
 :weight 0


:pattern ((Prims.pow2.fuel_instrumented (SFuel @u0)
@x1))
:qid equation_with_fuel_Prims.pow2.fuel_instrumented))

:named equation_with_fuel_Prims.pow2.fuel_instrumented))
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
;;;;;;;;;;;;;;;;Typing correspondence of token to term
;;; Fact-ids: Name Prims.pow2; Namespace Prims
(assert (! 
;; def=prims.fst(687,8-687,12); use=prims.fst(687,8-687,12)
(forall ((@u0 Fuel) (@x1 Term))
 (! (implies (HasType @x1
Prims.nat)
(HasType (Prims.pow2.fuel_instrumented @u0
@x1)
Prims.pos))
 

:pattern ((Prims.pow2.fuel_instrumented @u0
@x1))
:qid token_correspondence_Prims.pow2.fuel_instrumented))

:named token_correspondence_Prims.pow2.fuel_instrumented))

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End module Prims (654 decls; total size 39039)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End module FStar.Pervasives.Native (1325 decls; total size 133138)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End interface FStar.Pervasives (1419 decls; total size 84428)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End interface FStar.Sealed (17 decls; total size 1633)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End interface FStar.Range (22 decls; total size 1971)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End module FStar.Preorder (56 decls; total size 3191)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End interface FStar.Calc (50 decls; total size 3979)

;;; Start module FStar.Mul

; Externals for module FStar.Mul


; <Start encoding FStar.Mul.op_Star>

(declare-fun FStar.Mul.op_Star (Term Term) Term)

(declare-fun FStar.Mul.op_Star@tok () Term)

; </end encoding FStar.Mul.op_Star>


; End Externals for module FStar.Mul


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End module FStar.Mul (8 decls; total size 1010)

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


; UNSAT CORE: @MaxFuel_assumption, @MaxIFuel_assumption, @fuel_correspondence_Prims.pow2.fuel_instrumented, @fuel_irrelevance_Prims.pow2.fuel_instrumented, @query, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0, binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1, equality_tok_Prims.LexTop@tok, equation_Prims.nat, equation_Prims.pos, equation_with_fuel_Prims.pow2.fuel_instrumented, int_inversion, int_typing, primitive_Prims.op_Addition, primitive_Prims.op_Multiply, primitive_Prims.op_Subtraction, projection_inverse_BoxInt_proj_0, refinement_interpretation_Tm_refine_542f9d4f129664613f2483a6c88bc7c2, refinement_interpretation_Tm_refine_774ba3f728d91ead8ef40be66c9802e5, token_correspondence_Prims.pow2.fuel_instrumented, typing_Prims.pow2, well-founded-ordering-on-nat

;;; End module FStar.Math.Lib (158 decls; total size 13483)

; Internals for FStar.Math.Lemmas

(push)

; encoding sigelt 


; <Skipped />


; encoding sigelt euclidean_div_axiom


; <Skipped FStar.Math.Lemmas.euclidean_div_axiom/>


; encoding sigelt euclidean_div_axiom


; <Start encoding FStar.Math.Lemmas.euclidean_div_axiom>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.euclidean_div_axiom (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.euclidean_div_axiom@tok () Term)

; </end encoding FStar.Math.Lemmas.euclidean_div_axiom>


; encoding sigelt lemma_eucl_div_bound


; <Skipped FStar.Math.Lemmas.lemma_eucl_div_bound/>


; encoding sigelt lemma_eucl_div_bound


; <Start encoding FStar.Math.Lemmas.lemma_eucl_div_bound>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_eucl_div_bound (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_eucl_div_bound@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_eucl_div_bound>


; encoding sigelt lemma_mult_le_left


; <Skipped FStar.Math.Lemmas.lemma_mult_le_left/>


; encoding sigelt lemma_mult_le_left


; <Start encoding FStar.Math.Lemmas.lemma_mult_le_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_left@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_le_left>


; encoding sigelt lemma_mult_le_right


; <Skipped FStar.Math.Lemmas.lemma_mult_le_right/>


; encoding sigelt lemma_mult_le_right


; <Start encoding FStar.Math.Lemmas.lemma_mult_le_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_le_right@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_le_right>


; encoding sigelt lemma_mult_lt_left


; <Skipped FStar.Math.Lemmas.lemma_mult_lt_left/>


; encoding sigelt lemma_mult_lt_left


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_left@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_left>


; encoding sigelt lemma_mult_lt_right


; <Skipped FStar.Math.Lemmas.lemma_mult_lt_right/>


; encoding sigelt lemma_mult_lt_right


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_right@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_right>


; encoding sigelt lemma_mult_lt_sqr


; <Start encoding FStar.Math.Lemmas.lemma_mult_lt_sqr>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_sqr (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_mult_lt_sqr@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_mult_lt_sqr>


; encoding sigelt swap_mul


; <Skipped FStar.Math.Lemmas.swap_mul/>


; encoding sigelt swap_mul


; <Start encoding FStar.Math.Lemmas.swap_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_mul>


; encoding sigelt lemma_cancel_mul


; <Skipped FStar.Math.Lemmas.lemma_cancel_mul/>


; encoding sigelt lemma_cancel_mul


; <Start encoding FStar.Math.Lemmas.lemma_cancel_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_cancel_mul (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_cancel_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_cancel_mul>


; encoding sigelt distributivity_add_left


; <Skipped FStar.Math.Lemmas.distributivity_add_left/>


; encoding sigelt distributivity_add_left


; <Start encoding FStar.Math.Lemmas.distributivity_add_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_left@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_add_left>


; encoding sigelt distributivity_add_right


; <Skipped FStar.Math.Lemmas.distributivity_add_right/>


; encoding sigelt distributivity_add_right


; <Start encoding FStar.Math.Lemmas.distributivity_add_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_add_right@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_add_right>


; encoding sigelt paren_mul_left


; <Skipped FStar.Math.Lemmas.paren_mul_left/>


; encoding sigelt paren_mul_left


; <Start encoding FStar.Math.Lemmas.paren_mul_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_left@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_mul_left>


; encoding sigelt paren_mul_right


; <Skipped FStar.Math.Lemmas.paren_mul_right/>


; encoding sigelt paren_mul_right


; <Start encoding FStar.Math.Lemmas.paren_mul_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_mul_right@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_mul_right>


; encoding sigelt paren_add_left


; <Skipped FStar.Math.Lemmas.paren_add_left/>


; encoding sigelt paren_add_left


; <Start encoding FStar.Math.Lemmas.paren_add_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_add_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_add_left@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_add_left>


; encoding sigelt paren_add_right


; <Skipped FStar.Math.Lemmas.paren_add_right/>


; encoding sigelt paren_add_right


; <Start encoding FStar.Math.Lemmas.paren_add_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.paren_add_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.paren_add_right@tok () Term)

; </end encoding FStar.Math.Lemmas.paren_add_right>


; encoding sigelt addition_is_associative


; <Skipped FStar.Math.Lemmas.addition_is_associative/>


; encoding sigelt addition_is_associative


; <Start encoding FStar.Math.Lemmas.addition_is_associative>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.addition_is_associative (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.addition_is_associative@tok () Term)

; </end encoding FStar.Math.Lemmas.addition_is_associative>


; encoding sigelt subtraction_is_distributive


; <Skipped FStar.Math.Lemmas.subtraction_is_distributive/>


; encoding sigelt subtraction_is_distributive


; <Start encoding FStar.Math.Lemmas.subtraction_is_distributive>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.subtraction_is_distributive (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.subtraction_is_distributive@tok () Term)

; </end encoding FStar.Math.Lemmas.subtraction_is_distributive>


; encoding sigelt swap_add_plus_minus


; <Skipped FStar.Math.Lemmas.swap_add_plus_minus/>


; encoding sigelt swap_add_plus_minus


; <Start encoding FStar.Math.Lemmas.swap_add_plus_minus>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_add_plus_minus (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_add_plus_minus@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_add_plus_minus>


; encoding sigelt neg_mul_left


; <Skipped FStar.Math.Lemmas.neg_mul_left/>


; encoding sigelt neg_mul_left


; <Start encoding FStar.Math.Lemmas.neg_mul_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_left (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_left@tok () Term)

; </end encoding FStar.Math.Lemmas.neg_mul_left>


; encoding sigelt neg_mul_right


; <Skipped FStar.Math.Lemmas.neg_mul_right/>


; encoding sigelt neg_mul_right


; <Start encoding FStar.Math.Lemmas.neg_mul_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_right (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.neg_mul_right@tok () Term)

; </end encoding FStar.Math.Lemmas.neg_mul_right>


; encoding sigelt swap_neg_mul


; <Skipped FStar.Math.Lemmas.swap_neg_mul/>


; encoding sigelt swap_neg_mul


; <Start encoding FStar.Math.Lemmas.swap_neg_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.swap_neg_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.swap_neg_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.swap_neg_mul>


; encoding sigelt distributivity_sub_left


; <Skipped FStar.Math.Lemmas.distributivity_sub_left/>


; encoding sigelt distributivity_sub_left


; <Start encoding FStar.Math.Lemmas.distributivity_sub_left>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_left (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_left@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_sub_left>


; encoding sigelt distributivity_sub_right


; <Skipped FStar.Math.Lemmas.distributivity_sub_right/>


; encoding sigelt distributivity_sub_right


; <Start encoding FStar.Math.Lemmas.distributivity_sub_right>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_right (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.distributivity_sub_right@tok () Term)

; </end encoding FStar.Math.Lemmas.distributivity_sub_right>


; encoding sigelt mul_binds_tighter


; <Skipped FStar.Math.Lemmas.mul_binds_tighter/>


; encoding sigelt mul_binds_tighter


; <Start encoding FStar.Math.Lemmas.mul_binds_tighter>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_binds_tighter (Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_binds_tighter@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_binds_tighter>


; encoding sigelt lemma_abs_mul


; <Skipped FStar.Math.Lemmas.lemma_abs_mul/>


; encoding sigelt lemma_abs_mul


; <Start encoding FStar.Math.Lemmas.lemma_abs_mul>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_mul (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_mul@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_abs_mul>


; encoding sigelt lemma_abs_bound


; <Skipped FStar.Math.Lemmas.lemma_abs_bound/>


; encoding sigelt lemma_abs_bound


; <Start encoding FStar.Math.Lemmas.lemma_abs_bound>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_bound (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.lemma_abs_bound@tok () Term)

; </end encoding FStar.Math.Lemmas.lemma_abs_bound>


; encoding sigelt mul_ineq1


; <Skipped FStar.Math.Lemmas.mul_ineq1/>


; encoding sigelt mul_ineq1


; <Start encoding FStar.Math.Lemmas.mul_ineq1>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_ineq1 (Term Term Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_ineq1@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_ineq1>


; encoding sigelt add_zero_left_is_same


; <Start encoding FStar.Math.Lemmas.add_zero_left_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.add_zero_left_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.add_zero_left_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.add_zero_left_is_same>


; encoding sigelt add_zero_right_is_same


; <Start encoding FStar.Math.Lemmas.add_zero_right_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.add_zero_right_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.add_zero_right_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.add_zero_right_is_same>


; encoding sigelt mul_one_left_is_same


; <Start encoding FStar.Math.Lemmas.mul_one_left_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_one_left_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_one_left_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_one_left_is_same>


; encoding sigelt mul_one_right_is_same


; <Start encoding FStar.Math.Lemmas.mul_one_right_is_same>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_one_right_is_same (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_one_right_is_same@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_one_right_is_same>


; encoding sigelt mul_zero_left_is_zero


; <Start encoding FStar.Math.Lemmas.mul_zero_left_is_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_left_is_zero (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_left_is_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_zero_left_is_zero>


; encoding sigelt mul_zero_right_is_zero


; <Start encoding FStar.Math.Lemmas.mul_zero_right_is_zero>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_right_is_zero (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.mul_zero_right_is_zero@tok () Term)

; </end encoding FStar.Math.Lemmas.mul_zero_right_is_zero>


; encoding sigelt nat_times_nat_is_nat


; <Skipped FStar.Math.Lemmas.nat_times_nat_is_nat/>


; encoding sigelt nat_times_nat_is_nat


; <Start encoding FStar.Math.Lemmas.nat_times_nat_is_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_times_nat_is_nat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_times_nat_is_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_times_nat_is_nat>


; encoding sigelt pos_times_pos_is_pos


; <Skipped FStar.Math.Lemmas.pos_times_pos_is_pos/>


; encoding sigelt pos_times_pos_is_pos


; <Start encoding FStar.Math.Lemmas.pos_times_pos_is_pos>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pos_times_pos_is_pos (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pos_times_pos_is_pos@tok () Term)

; </end encoding FStar.Math.Lemmas.pos_times_pos_is_pos>


; encoding sigelt nat_over_pos_is_nat


; <Skipped FStar.Math.Lemmas.nat_over_pos_is_nat/>


; encoding sigelt nat_over_pos_is_nat


; <Start encoding FStar.Math.Lemmas.nat_over_pos_is_nat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_over_pos_is_nat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_over_pos_is_nat@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_over_pos_is_nat>


; encoding sigelt nat_plus_nat_equal_zero_lemma


; <Skipped FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma/>


; encoding sigelt nat_plus_nat_equal_zero_lemma


; <Start encoding FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.nat_plus_nat_equal_zero_lemma>


; encoding sigelt int_times_int_equal_zero_lemma


; <Skipped FStar.Math.Lemmas.int_times_int_equal_zero_lemma/>


; encoding sigelt int_times_int_equal_zero_lemma


; <Start encoding FStar.Math.Lemmas.int_times_int_equal_zero_lemma>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.int_times_int_equal_zero_lemma (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.int_times_int_equal_zero_lemma@tok () Term)

; </end encoding FStar.Math.Lemmas.int_times_int_equal_zero_lemma>


; encoding sigelt 


; <Skipped />


; encoding sigelt pow2_double_sum


; <Skipped FStar.Math.Lemmas.pow2_double_sum/>


; encoding sigelt pow2_double_sum


; <Start encoding FStar.Math.Lemmas.pow2_double_sum>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_sum (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_sum@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_double_sum>


; encoding sigelt pow2_double_mult


; <Skipped FStar.Math.Lemmas.pow2_double_mult/>


; encoding sigelt pow2_double_mult


; <Start encoding FStar.Math.Lemmas.pow2_double_mult>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_mult (Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_double_mult@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_double_mult>


; encoding sigelt pow2_lt_compat


; <Skipped FStar.Math.Lemmas.pow2_lt_compat/>


; encoding sigelt pow2_lt_compat


; <Start encoding FStar.Math.Lemmas.pow2_lt_compat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_lt_compat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_lt_compat@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_lt_compat>


; encoding sigelt 


; <Skipped />


; encoding sigelt pow2_le_compat


; <Skipped FStar.Math.Lemmas.pow2_le_compat/>


; encoding sigelt pow2_le_compat


; <Start encoding FStar.Math.Lemmas.pow2_le_compat>

;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_le_compat (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_le_compat@tok () Term)

; </end encoding FStar.Math.Lemmas.pow2_le_compat>


; encoding sigelt 


; <Skipped />


; encoding sigelt pow2_plus


; <Skipped FStar.Math.Lemmas.pow2_plus/>


; Starting query at FStar.Math.Lemmas.fst(252,2-254,28)

(push)
;;;;;;;;;;;;;;;;n#22566 : Prims.nat (Prims.nat)
(declare-fun x_bb4e1c9af0265270f8e7a5f250f730e2_0 () Term)
;;;;;;;;;;;;;;;;binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0
;;; Fact-ids: 
(assert (! (HasType x_bb4e1c9af0265270f8e7a5f250f730e2_0
Prims.nat)
:named binder_x_bb4e1c9af0265270f8e7a5f250f730e2_0))
;;;;;;;;;;;;;;;;m#22567 : Prims.nat (Prims.nat)
(declare-fun x_bb4e1c9af0265270f8e7a5f250f730e2_1 () Term)
;;;;;;;;;;;;;;;;binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1
;;; Fact-ids: 
(assert (! (HasType x_bb4e1c9af0265270f8e7a5f250f730e2_1
Prims.nat)
:named binder_x_bb4e1c9af0265270f8e7a5f250f730e2_1))
;;;;;;;;;;;;;;;;Uninterpreted function symbol for impure function
(declare-fun FStar.Math.Lemmas.pow2_plus (Term Term) Term)
;;;;;;;;;;;;;;;;Uninterpreted name for impure function
(declare-fun FStar.Math.Lemmas.pow2_plus@tok () Term)
(declare-fun label_4 () Bool)
(declare-fun label_3 () Bool)
(declare-fun label_2 () Bool)
(declare-fun label_1 () Bool)

; Encoding query formula : forall (p: Prims.pure_post Prims.unit).
;   (forall (pure_result: Prims.unit).
;       Prims.pow2 n * Prims.pow2 m = Prims.pow2 (n + m) ==> p pure_result) ==>
;   (forall (k: Prims.pure_post Prims.unit).
;       (forall (x: Prims.unit). {:pattern Prims.guard_free (k x)} p x ==> k x) ==>
;       (n == 0 ==> (forall (any_result: Prims.unit). k any_result)) /\
;       (~(n = 0) ==>
;         (forall (b: Prims.int).
;             n == b ==>
;             n - 1 >= 0 /\
;             (forall (return_val: Prims.nat).
;                 return_val == n - 1 ==>
;                 n - 1 << n /\
;                 (forall (any_result: Prims.nat).
;                     m == any_result ==>
;                     (forall (pure_result: Prims.unit).
;                         Prims.pow2 (n - 1) * Prims.pow2 m = Prims.pow2 (n - 1 + m) ==> k pure_result
;                     ))))))

(push)

; <fuel='1' ifuel='1'>

;;; Fact-ids: 
(assert (! (= MaxFuel
(SFuel ZFuel))
:named @MaxFuel_assumption))
;;; Fact-ids: 
(assert (! (= MaxIFuel
(SFuel ZFuel))
:named @MaxIFuel_assumption))
;;;;;;;;;;;;;;;;query
;;; Fact-ids: 
(assert (! (not 
;; def=FStar.Math.Lemmas.fst(252,2-254,28); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x0 Term))
 (! (implies (and (HasType @x0
(Prims.pure_post Prims.unit))

;; def=prims.fst(420,36-420,97); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x1 Term))
 (! (implies (and (or label_1
(HasType @x1
Prims.unit))

;; def=FStar.Math.Lemmas.fst(249,11-249,43); use=FStar.Math.Lemmas.fst(252,2-254,28)
(or label_2

;; def=FStar.Math.Lemmas.fst(249,11-249,43); use=FStar.Math.Lemmas.fst(252,2-254,28)
(= (Prims.op_Multiply (Prims.pow2 x_bb4e1c9af0265270f8e7a5f250f730e2_0)
(Prims.pow2 x_bb4e1c9af0265270f8e7a5f250f730e2_1))
(Prims.pow2 (Prims.op_Addition x_bb4e1c9af0265270f8e7a5f250f730e2_0
x_bb4e1c9af0265270f8e7a5f250f730e2_1)))
)
)

;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(252,2-254,28)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(252,2-254,28)
(ApplyTT @x0
@x1)
)
)
 

:pattern (
;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(252,2-254,28)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(252,2-254,28)
(ApplyTT @x0
@x1)
)
)
:qid @query.1))
)

;; def=prims.fst(381,2-381,97); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x1 Term))
 (! (implies (and (HasType @x1
(Prims.pure_post Prims.unit))

;; def=prims.fst(381,2-381,97); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x2 Term))
 (! (implies 
;; def=prims.fst(381,73-381,79); use=FStar.Math.Lemmas.fst(252,2-254,28)
(Valid 
;; def=prims.fst(381,73-381,79); use=FStar.Math.Lemmas.fst(252,2-254,28)
(ApplyTT @x0
@x2)
)


;; def=prims.fst(381,84-381,87); use=FStar.Math.Lemmas.fst(252,2-254,28)
(Valid 
;; def=prims.fst(381,84-381,87); use=FStar.Math.Lemmas.fst(252,2-254,28)
(ApplyTT @x1
@x2)
)
)
 :weight 0


:pattern ((ApplyTT @x1
@x2))
:qid @query.3))
)

;; def=prims.fst(368,2-368,39); use=FStar.Math.Lemmas.fst(252,2-254,28)
(and (implies 
;; def=FStar.Math.Lemmas.fst(251,18-253,5); use=FStar.Math.Lemmas.fst(252,8-253,5)
(= x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 0))


;; def=prims.fst(430,66-430,102); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x2 Term))
 (! (implies (HasType @x2
Prims.unit)

;; def=prims.fst(430,90-430,102); use=FStar.Math.Lemmas.fst(252,2-254,28)
(Valid 
;; def=prims.fst(430,90-430,102); use=FStar.Math.Lemmas.fst(252,2-254,28)
(ApplyTT @x1
@x2)
)
)
 
;;no pats
:qid @query.4))
)
(implies 
;; def=prims.fst(368,19-368,21); use=FStar.Math.Lemmas.fst(252,2-254,28)
(not 
;; def=FStar.Math.Lemmas.fst(251,18-253,5); use=FStar.Math.Lemmas.fst(252,8-253,5)
(= x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 0))
)


;; def=prims.fst(392,99-392,120); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x2 Term))
 (! (implies (and (HasType @x2
Prims.int)

;; def=FStar.Math.Lemmas.fst(251,18-254,5); use=FStar.Math.Lemmas.fst(252,8-254,5)
(= x_bb4e1c9af0265270f8e7a5f250f730e2_0
@x2)
)

;; def=prims.fst(438,77-438,89); use=FStar.Math.Lemmas.fst(252,2-254,28)
(and 
;; def=prims.fst(659,18-659,24); use=FStar.Math.Lemmas.fst(254,19-254,26)
(or label_3

;; def=prims.fst(659,18-659,24); use=FStar.Math.Lemmas.fst(254,19-254,26)
(>= (BoxInt_proj_0 (Prims.op_Subtraction x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 1)))
(BoxInt_proj_0 (BoxInt 0)))
)


;; def=prims.fst(335,2-335,58); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x3 Term))
 (! (implies (and (HasType @x3
Prims.nat)

;; def=prims.fst(335,26-335,41); use=FStar.Math.Lemmas.fst(252,2-254,28)
(= @x3
(Prims.op_Subtraction x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 1)))
)

;; def=prims.fst(438,77-438,89); use=FStar.Math.Lemmas.fst(252,2-254,28)
(and 
;; def=FStar.Math.Lemmas.fst(252,2-254,28); use=FStar.Math.Lemmas.fst(254,27-254,28)
(or label_4

;; def=FStar.Math.Lemmas.fst(252,2-254,28); use=FStar.Math.Lemmas.fst(254,27-254,28)
(Valid 
;; def=FStar.Math.Lemmas.fst(252,2-254,28); use=FStar.Math.Lemmas.fst(254,27-254,28)
(Prims.precedes Prims.nat
Prims.nat
(Prims.op_Subtraction x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 1))
x_bb4e1c9af0265270f8e7a5f250f730e2_0)
)
)


;; def=prims.fst(430,66-430,102); use=FStar.Math.Lemmas.fst(252,2-254,28)
(forall ((@x4 Term))
 (! (implies (and (HasType @x4
Prims.nat)

;; def=FStar.Math.Lemmas.fst(248,24-251,21); use=FStar.Math.Lemmas.fst(252,2-254,28)
(= x_bb4e1c9af0265270f8e7a5f250f730e2_1
@x4)
)

;; def=prims.fst(420,36-420,97); use=FStar.Math.Lemmas.fst(254,9-254,18)
(forall ((@x5 Term))
 (! (implies (and (HasType @x5
Prims.unit)

;; def=FStar.Math.Lemmas.fst(249,11-249,43); use=FStar.Math.Lemmas.fst(254,9-254,18)
(= (Prims.op_Multiply (Prims.pow2 (Prims.op_Subtraction x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 1)))
(Prims.pow2 x_bb4e1c9af0265270f8e7a5f250f730e2_1))
(Prims.pow2 (Prims.op_Addition (Prims.op_Subtraction x_bb4e1c9af0265270f8e7a5f250f730e2_0
(BoxInt 1))
x_bb4e1c9af0265270f8e7a5f250f730e2_1)))
)

;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(254,9-254,18)
(Valid 
;; def=prims.fst(420,83-420,96); use=FStar.Math.Lemmas.fst(254,9-254,18)
(ApplyTT @x1
@x5)
)
)
 
;;no pats
:qid @query.8))
)
 
;;no pats
:qid @query.7))
)
)
 
;;no pats
:qid @query.6))
)
)
 
;;no pats
:qid @query.5))
))
)
 
;;no pats
:qid @query.2))
)
 
;;no pats
:qid @query))
)
:named @query))
(set-option :rlimit 2723280)
(check-sat)
