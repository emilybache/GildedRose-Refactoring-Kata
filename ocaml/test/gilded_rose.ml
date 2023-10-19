open Gilded_rose

let test () =
  let items = Items.v ~items:[ Item.v "Foo" 10 20 ] () in
  Alcotest.(check string)
    "Broken test" "Fixme"
    (List.hd items |> fun item -> item.name)

let () =
  let open Alcotest in
  run "Gilded Rose" [ ("Our first test", [ test_case "fixme" `Quick test ]) ]
