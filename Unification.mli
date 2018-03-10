type typeSchema =
    Alpha of string
  | Bool
  | Int
  | Arrow of typeSchema * typeSchema
  | Cross of typeSchema * typeSchema
val delete : ('a * 'a) list -> ('a * 'a) list
val swap : (typeSchema * typeSchema) list -> (typeSchema * typeSchema) list
