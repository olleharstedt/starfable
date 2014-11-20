(*
    Proof that rand returns {z : Z | 0 < z < n2}

    @since 2014-10-23
*)


Require Import ZArith.
Require Import Znumtheory.
Require Import Streams.
Require Import List.
Open Scope Z.

CoFixpoint ones : Stream Z := Cons 1 ones.

Fixpoint approx A (s : Stream A) (n : nat) : list A :=
    match n with
    | O => nil
    | S p => match s with
        | Cons h t => h :: approx A t p
        end
    end.

(*Eval compute in approx Z ones 5.*)

Inductive all_ones : list Z -> Prop :=
| nil_is_ones : all_ones nil
| cons_is_ones : forall l, all_ones l -> all_ones (cons 1%Z l).

(** Proof that approx ones will product lists with only ones. *)
Lemma approx_to_ones : forall n, all_ones (approx _ ones n).
Proof.
    intros.
    induction n.
    constructor.
    constructor.
    apply IHn.
Qed.

(* n1, n2 are primes *)
CoFixpoint rand (seed n1 n2 : Z) : Stream Z :=
    let seed' := Zmod seed n2 in 
    Cons seed' (rand (seed' * n1) n1 n2).

(*
Inductive rand_ge0 : (list Z) (seed n1 n2 : Z) -> Prop :=
| nil_is_limit : rand_limit nil
| cons_is_limit : forall l seed n1 n2, rand_ge0 l seed
*)

(*
Inductive zmod_ge0 : n1 n2 : Z -> Prop :=
|
*)

(** This is not true, since n1 = k * n2 *)
(*Lemma zmod_ge0 : forall (n1 n2 : Z), Zne n1 n2 -> Zgt n1 0 -> Zgt n2 0 -> Zgt (Zmod n1 n2) 0.*)

(* If a and b are primes then a mod b <> 0 *)
(* gcd(a,b) = 1 *)
Lemma zmod_prime : forall (a b : Z), prime a -> prime b -> Zne a b -> Zne (Zmod a b) 0.
(* Lemma zmod_0 : forall (a : Z), Zmod 0 a = 0. *)
(* Lemma zgcd_0 : forall (a : Z), Z.gcd 0 b = 0. *)

(* 1 is coprime with itself? Z.gcd 1 1 = 1, and mod 1 1 = 0. *)
Lemma gcd_prime : forall (a b : Z), a > 1 -> b > 1 -> Z.gcd a b = 1 -> Zmod a b <> 0.
Proof.
  intros a b Ha Hb Hgcd Hmod.
  rewrite (Z_div_mod_eq a b), Hmod, Zplus_0_r in Hgcd; try omega.
  rewrite Z.gcd_comm, Z.gcd_mul_diag_l in Hgcd; omega.
Qed.

(* Lemma mod_0l : forall (a : Z), Zmod 0 a = 0. *) 


