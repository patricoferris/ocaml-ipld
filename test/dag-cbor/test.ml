let ipld = Alcotest.testable Ipld.pp Stdlib.( = )

let cbor =
  Alcotest.testable
    (fun ppf v -> Fmt.pf ppf "%s" (CBOR.Simple.to_diagnostic v))
    Stdlib.( = )

let read_file fpath =
  let ic = open_in fpath in
  let rec read acc =
    try read (input_line ic :: acc)
    with End_of_file -> List.rev acc |> String.concat "\n"
  in
  let s = read [] in
  close_in ic;
  s

let ( / ) = Filename.concat

(* CBOR is having a hard time of integers *)
let disable_list =
  [
    "int-9223372036854775807";
    "int-11959030306112471731";
    "int--9223372036854775808";
    "int-18446744073709551615";
    "int--11959030306112471732";
  ]

let test_fixtures () =
  let fixtures = "../codec-fixtures/fixtures" in
  let files = Sys.readdir fixtures |> Array.to_list in
  let get_data fpath =
    let cid = Filename.chop_extension fpath in
    let data = read_file fpath in
    (cid, data)
  in
  let make_test fpath =
    let dir = fixtures / fpath in
    if (not (Sys.is_directory dir)) || List.mem fpath disable_list then None
    else
      let codec_files = Sys.readdir dir in
      match
        Array.find_opt (fun c -> Filename.extension c = ".dag-cbor") codec_files
      with
      | None -> assert false
      | Some f ->
          let test () =
            let _cid, data = get_data (fixtures / fpath / f) in
            let c = CBOR.Simple.decode data in
            let i = Dag_cbor.encode c in
            let c' = Dag_cbor.decode i in
            let i' = Dag_cbor.encode c' in
            Alcotest.(check cbor) "same cbor" c c';
            Alcotest.(check ipld) "same ipld" i i'
          in
          Some (fpath, `Quick, test)
  in
  List.filter_map (fun f -> make_test f) files

let () = Alcotest.run "ipld-dag-cbor" [ ("fixtures", test_fixtures ()) ]
