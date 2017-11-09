Require Import List.
Import List.ListNotations.

Fixpoint nth_option {A} (l: list A) (n: nat) : option A :=
  match l with
  | [] => None
  | x :: l =>
    match n with
    | 0 => Some x
    | S n => nth_option l n
    end
  end.

