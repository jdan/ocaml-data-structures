open Setoid;;

let crowd = [
  Person.make "Jordan" 25 ;
  Person.make "Steve" 29 ;
  Person.make "Jordan" 25 ;
  Person.make "Tom" 22 ;
  Person.make "Jake" 1 ;
  Person.make "Steve" 29
];;

let unique_members = PersonUniq.f crowd;;

assert (unique_members = [
    ("Jordan", 25) ; ("Tom", 22) ; ("Jake", 1) ; ("Steve", 29)
  ])
