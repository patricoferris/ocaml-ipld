let ipld = Alcotest.testable Ipld.pp Stdlib.( = )

let test_find () =
  let data = `Map [ ("hello", `String "world"); ("answer", `Int 42) ] in
  let expect_string = `String "world" in
  let test_string = Ipld.find_exn data "hello" in
  let expect_int = `Int 42 in
  let test_int = Ipld.find_exn data "answer" in
  Alcotest.(check ipld) "same ipld string" expect_string test_string;
  Alcotest.(check ipld) "same ipld string" expect_int test_int

let test_combinators () =
  let expect = `List [ `Int 0; `Int 1; `Int 2 ] in
  let test = Ipld.(list int) [ 0; 1; 2 ] in
  Alcotest.(check ipld) "same ipld list" expect test

let () =
  Alcotest.run "ipld-core"
    [
      ( "simple",
        [
          ("test-find", `Quick, test_find);
          ("test-combinators", `Quick, test_combinators);
        ] );
    ]
