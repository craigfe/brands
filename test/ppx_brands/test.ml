module T : sig
  type t0 [@@deriving branded]
  type _ t1 [@@deriving branded]
  type (_, _) t2 [@@deriving branded]
  type (_, _, _) t3 [@@deriving branded]
end = struct
  type t0 [@@deriving branded]
  type _ t1 [@@deriving branded]
  type (_, _) t2 [@@deriving branded]
  type (_, _, _) t3 [@@deriving branded]
end
