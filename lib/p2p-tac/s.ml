
(** TAC protocol configuration parameters *)
module type CONFIG = sig

  (** Trust weight in the range [0.0, 2.0].
   * Influences the importance of trust vs similarity in the scoring of nodes:
   * [sim^(e-2) * trust^e], where
   * - [sim] is the similarity metric of the node,
   * - [trust] is the trust value assigned to the node,
   * - [e] is the [trust_weight]  *)
  val trust_weight : float

  (** Minimum trust required for a node to be considered for merging into the view *)
  val min_trust : float

end
