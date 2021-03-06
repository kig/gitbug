(*
gitbug.ml: In-repo bug tracker for Git.

Copyright (C) 2008  Ilmari Heikkinen <ilmari.heikkinen@gmail.com>
                    Mauricio Fernandez <mfp@acm.org>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
*)

open Prelude

exception Ambiguous_bug_id of string * string

let editor () = maybeNF "/bin/vi" Sys.getenv "EDITOR"
let editorCmd () = editor () |> strip |> xsplit "\\s+"
let editFile filename = command (editorCmd () @ [filename])

let pager () = maybeNF "/usr/bin/less" Sys.getenv "PAGER"
let pagerCmd () = pager () |> strip |> xsplit "\\s+"
let viewFile filename = command (pagerCmd () @ [filename])

let get_homedir user = (Unix.getpwnam user).Unix.pw_dir
let get_realname user = (Unix.getpwnam user).Unix.pw_gecos |> split "," |> first

let get_user () = (Unix.getpwuid (Unix.getuid ())).Unix.pw_name

let share_dirs () = [
  get_homedir (get_user ()) ^/ ".gitbug";
  "/usr/share/gitbug";
  "/usr/local/share/gitbug";
]

let default_templates = [
  "add", "-- opened by %USER% on %TIMESTAMP%\n\nProblem description:\n\nHow to reproduce:\n\nProposed solution:\n";
  "edit", "\n-- edited by %USER% on %TIMESTAMP%\n\n";
  "close", "\n-- closed by %USER% on %TIMESTAMP%\n\nFIXED\nWONTFIX\nNOTABUG\nDUPLICATE\n\n";
  "reopen", "\n-- reopened by %USER% on %TIMESTAMP%\n\n";
  "merge", "\n-- merged with %NAME% by %USER% on %TIMESTAMP%\n\n";
  "autoclose", "\n-- closed by %USER% on %TIMESTAMP%\n\nFIXED\n\n";
]

let init_templates () =
  maybeNF
    default_templates
    begin fun dirs ->
      let dir = find (fun d -> fileExists (d ^/ "templates")) dirs in
      let dir = dir ^/ "templates" in
      ls dir |> map (fun fn -> fn, readFile (dir ^/ fn))
    end
    (share_dirs ())

let name_and_email () =
  let email = maybe "" (quote " <" ">") (optNF Sys.getenv "EMAIL") in
  let user = get_user () in
  let name = maybeE user get_realname user in
  name ^ email

let printfnl msg = printf (msg ^^ "\n")

let string_of_status = function
    `Open -> "OPEN"
  | `Close -> "CLOSED"

module Ticket =
struct
  module SM = Map.Make(struct type t = string let compare = String.compare end)
  type t = { headers : string SM.t; last_modified : float; body : string list }

  let strip_blank_lines = dropWhile (xmatch "^\\s*$")

  let make name status body =
    let now = timeNow () in
    let hs = foldl (fun m (k, v) -> SM.add k v m) SM.empty
               [
                 "status", string_of_status status;
                 "author", name_and_email ();
                 "date", showTime @@ now;
                 "title", name;
                 "last modified", showTime @@ now;
                 "reported by", "";
                 "assigned to", "";
               ]
    in { headers = hs; last_modified = now; body = strip_blank_lines body; }

  let last_modified t = t.last_modified

  let date_of_string s = Netdate.since_epoch (Netdate.parse s)

  let comp_last_modified fname headers body =
    try
      date_of_string (SM.find "last modified" headers)
    with _ ->
        match concatMap (rexscan_nth (rex "^--.* on (.*)$") 1) body with
            [] -> mtime fname
          | l ->
             let rec loop t = function
                 [] -> (match t with None -> mtime fname | Some t -> t)
               | s :: tl ->
                 let t' = try Some (date_of_string s) with _ -> None in
                   match t, t' with
                       None, None -> loop None tl
                     | Some t, None | None, Some t -> loop (Some t) tl
                     | Some t, Some t' -> loop (Some (max t t')) tl
             in loop None l

  let from_file fname =
    let headers, body = span (xmatch "^[^:]*:") @@ readLines fname in
    let m = foldl (fun m l -> match split ~n:2 ":" l with
                       [k; v] -> SM.add k (strip v) m
                     | _ -> m)
              SM.empty headers
    in { headers = m;
         last_modified = comp_last_modified fname m body;
         body = strip_blank_lines body }

  let set_header k v t = { t with headers = SM.add k (strip v) t.headers }

  let set_last_modified v t =
    { t with last_modified = v;
              headers = SM.add "last modified" (showTime v) t.headers;
    }

  let title t = maybeNF "" (SM.find "title") t.headers

  let to_string t =
    join "\n" @@
    concat [SM.fold (fun k v l -> (k ^ ": " ^ v) :: l) t.headers []; [""]; t.body]

  let string name status body = to_string @@ make name status [body]
end

let make_dir dir = mkdir_p dir; dir
let make_file_dir f = ignore @@ make_dir @@ dirname f; f

let base_bug_dir () =
  let inode d = (Unix.stat d).Unix.st_ino in
  let is_root d = inode d = inode "/" in
  let rec loop base =
    let dir = base ^/ "bugs" in
      if fileExists dir && isDir dir then dir
      else if not (is_root base) then loop (base ^/ "..")
      else failwith "Couldn't find \"bugs\" base directory."
  in loop "."

let git_dir () =
  let inode d = (Unix.stat d).Unix.st_ino in
  let is_root d = inode d = inode "/" in
  let rec loop base =
    let dir = base ^/ ".git" in
      if fileExists dir && isDir dir then dir
      else if not (is_root base) then loop (base ^/ "..")
      else failwith "Couldn't find \".git\" base directory."
  in loop "."

let all_bugs_dir () = make_dir (base_bug_dir () ^/ "all")
let template_dir () =  make_dir (base_bug_dir () ^/ "templates")

let dir_of_status = function
    `Open -> base_bug_dir () ^/ "open"
  | `Close -> base_bug_dir () ^/ "done"

let new_normalized_name = xreplaceMulti ["[^a-zA-Z0-9\\s]", ""; "\\s+", "_"]

let new_id () =
  let t = timeNow () in
  let s = int t in
  let fs = int ((t -. floor t) *. 65536.) in
  sprintf "%08x%04x" s fs |> srev


let readGit cmd args = readCmd ("git"::cmd::args)
let git cmd args = flush stdout; command ("git"::cmd::args)

let git_do f x = git "reset" ["--mixed"]; f x

let git_add fn = git "add" [fn]
let git_commit msg = git "commit" ["-m"; msg]
let git_mv src dst = git "mv" [src; dst]
let git_rm fn = git "rm" [fn]

let git_edit filename =
  editFile filename;
  git_add filename

let add_symlink status file =
  let base = basename file in
  let dst = dir_of_status status ^/ base in
    if fileExists dst then rm dst;
    ln_s (".." ^/ "all" ^/ base) @@ make_file_dir dst;
    git_add dst

let remove_symlink status file =
  let fn = dir_of_status status ^/ basename file in
    if fileExists fn then try git_rm fn with _ -> () (* git rm writes stuff to shell *)

let template tmpl name =
  let template_file = (template_dir () ^/ tmpl) in
  let tdata = if fileExists template_file then readFile template_file else "" in
  tdata |> sreplaceMulti [
    "%USER%", name_and_email ();
    "%TIMESTAMP%", showTime (timeNow());
    "%NAME%", name;
  ]


let file_with_id dir id =
  try ls dir |> find (fun f -> f = id)
  with Sys_error _ -> raise Not_found

let new_bug_file name =
  let id = new_id () in
    (id, all_bugs_dir () ^/ (id ^ "_" ^  new_normalized_name name))

let bug_file ?(dir = all_bugs_dir ()) id =
  let f = file_with_id dir id in
    (dir ^/ f, f)

let bug_name = Ticket.title @. Ticket.from_file @. fst @. bug_file

let writeFile file = writeFile @@ make_file_dir file
let appendFile file = appendFile @@ make_file_dir file

let append_to_file status file text =
  let module T = Ticket in
  writeFile file @@
    (T.to_string @@
     T.set_last_modified (timeNow ()) @@
     T.set_header "status" (string_of_status status) @@ T.from_file file) ^
    text

let open_bug fn =
  add_symlink `Open fn;
  remove_symlink `Close fn

let close_bug fn =
  add_symlink `Close fn;
  remove_symlink `Open fn

let git_bug_add = git_do (fun name ->
  let (id, bug) = new_bug_file name in
  writeFile bug (Ticket.string name `Open @@ template "add" name);
  git_edit bug;
  open_bug bug;
  git_commit (sprintf "BUG added: [%s] %s" id name))

let digest_of_id s =
  let s = first (split "_" s) in
    String.sub s 0 (min 7 (String.length s))

let git_bug_autoclose = git_do (fun bugs ->
  bugs |> iter begin fun id ->
    try
      print_endline id;
      let bug, id = bug_file ~dir:(dir_of_status `Open) id in
      let name = bug_name id in
      let base = basename bug in
      append_to_file `Close bug @@ template "autoclose" name;
      git_add bug;
      close_bug bug;
      git "commit" [
        "--quiet";
        "-m"; (sprintf "BUG closed: [%s] %s" (digest_of_id id) name);
        bug;
        all_bugs_dir () ^/ base;
        dir_of_status `Close ^/ base;
        dir_of_status `Open ^/ base;
      ]
    with _ -> ()
  end)

let git_bug_close = git_do (fun id ->
  let bug, id = bug_file id in
  let name = bug_name id in
  append_to_file `Close bug @@ template "close" name;
  git_edit bug;
  close_bug bug;
  git_commit (sprintf "BUG closed: [%s] %s" (digest_of_id id) name))

let git_bug_reopen = git_do (fun id ->
  let bug, id = bug_file id in
  let name = bug_name id in
  append_to_file `Open bug @@ template "reopen" name;
  git_edit bug;
  open_bug bug;
  git_commit (sprintf "BUG reopened: [%s] %s" (digest_of_id id) name))

let git_bug_edit = git_do (fun id ->
  let bug, id = bug_file id in
  let name = bug_name id in
  appendFile bug (template "edit" name);
  git_edit bug;
  git_commit (sprintf "BUG edited: [%s] %s" (digest_of_id id) name))

let git_bug_merge src dst = git_do (fun () ->
  let sfn, src = bug_file src in
  let dfn, dst = bug_file dst in
  appendFile dfn (template "merge" (bug_name src) ^ readFile sfn);
  remove_symlink `Open sfn;
  git_edit dfn;
  git_commit (sprintf "BUG merged: [%s] -> [%s]" (digest_of_id src) (digest_of_id dst))) ()

let get_bug_list status =
  try
    let dir = dir_of_status status in
    ls dir
      |> filter (fun n -> isFile (dir ^/ n))
      |> sortBy (fun n -> Ticket.last_modified (Ticket.from_file (dir ^/ n)))
  with Sys_error _ -> []

let get_bug_name = function
  | [] -> printf "Enter bug name: %!"; read_line ()
  | args -> join " " args

let find_bug_id s =
  let find_bug re l = match List.filter (fun b -> rexmatch re b) l with
    | [] -> raise Not_found
    | [x] -> x
    | ids -> raise (Ambiguous_bug_id (s, join ", " ids)) in
  let re = rex ("^"^s) in
  try find_bug re (get_bug_list `Open)
  with Not_found -> find_bug re (get_bug_list `Close)

let get_bug_id = function
  | [id] -> find_bug_id id
  | _ -> printf "Enter bug ID: %!"; find_bug_id (read_line ())

let show_bug dir name =
  let file = dir ^/ name in
  let ticket = Ticket.from_file file in
  let time = showTime (Ticket.last_modified ticket) in
  let title = Ticket.title ticket in
    sprintf "%-8s  %-41s  %s" (xfind "^[^_]{0,8}" name)
      (String.sub title 0 (min 41 @@ slen title)) time

let print_bug = puts @.. show_bug

let add args =
  let name = get_bug_name args in
  git_bug_add name;
  printfnl "Added bug: %s" name

let do_with_bug_id f msg args =
  let id = get_bug_id args in
    f id;
    printfnl msg id

let autoclose args =
  let last_commit = readGit "log" ["-1"] in
  let bugs = if smatch "    BUG closed:" last_commit then [] else (* prevent loop *)
    last_commit
    |> scan_nth "\\bFIX[EDS]*:?\\s*\\[([^\\]]+)\\]" 1
    |> concatMap (xsplit "[ ,\n]+") in
  match bugs with
    | [] -> ()
    | bugs ->
      let bugs = map find_bug_id bugs in
      git_bug_autoclose bugs;
      puts (sprintf "Autoclosed bug(s): %s" (bugs |> join ", "))

let close = do_with_bug_id git_bug_close "Closed bug: %s."
let reopen = do_with_bug_id git_bug_reopen "Reopened bug: %s."
let edit = do_with_bug_id git_bug_edit "Edited bug: %s."

let list args =
  let status, dir_name = match args with
    | [] | "open"::_ -> `Open, "Open"
    | "closed"::_ -> `Close, "Closed"
    | x::_ -> invalid_arg (sprintf "Unknown bug category: %S" x) in
  printfnl "-- %s bugs" dir_name;
  try get_bug_list status |> iter (print_bug (dir_of_status status))
  with Sys_error _ -> ()

let merge args =
  let dst, src = match args with
    | s::d::[] -> find_bug_id s, find_bug_id d
    | _ -> invalid_arg "merge src dst: wrong amount of args" in
  git_bug_merge src dst;
  printfnl "Merged bug %s into %s" src dst

let show args =
  let id = get_bug_id args in
  viewFile (fst @@ bug_file id)

let use_autoclose _ =
  let edited pc =
    let lines = readLines pc in
    match lines with
      | "#!/bin/sh"::t -> any (not @. xmatch "^\\s*([:#].*|\\s*)$") t
      | _ -> true in
  let post_commit = git_dir () ^/ "hooks" ^/ "post-commit" in
  if fileExists post_commit && edited post_commit
  then begin
    puts ".git/hooks/post-commit has been edited";
    puts "Please add the autoclose hook manually by calling:";
    puts (sprintf "  %s autoclose;" Sys.argv.(0));
    puts "at the end of your post-commit hook."
  end else begin
    if not (fileExists post_commit) then writeFile post_commit "#!/bin/sh\n";
    appendFile post_commit (sprintf "\n%s autoclose;\n" Sys.argv.(0));
    chmod 0o755 post_commit
  end

let init _ =
  use_autoclose [];
  mkdir "bugs";
  iter mkdir_p [
    dir_of_status `Open;
    dir_of_status `Close;
    all_bugs_dir ();
    template_dir ();
  ];
  iter (fun (fn, s) -> writeFile ("bugs/templates" ^/ fn) s) (init_templates ());
  git_add "bugs";
  git_commit "Initialized bug tracker";
  puts "Initialized the bugs directory."

let handle_cmd cmd =
  let show_help () =

    printfnl "usage: %s CMD [OPTIONS]" (basename Sys.argv.(0));
    printfnl "\nCommands:";
    printfnl "\n  Initialize bug tracker:\n    init";
    printfnl "\n  Manage bugs:\n    add close edit merge reopen show";
    printfnl "\n  List bugs:\n    list all closed open";
    printfnl "\n  Close bugs with \"FIX[bug_number]\" in commit message:\n    autoclose";
    printfnl "\n  Add post-commit hook for autoclose:\n    use_autoclose";
    printfnl "      init does this for you already, you only need this if you";
    printfnl "      haven't done init: i.e. if you're using someone else's repo.\n";
    exit 0 in
  let f = match cmd with
    | "add" -> add
    | "autoclose" -> autoclose
    | "use_autoclose" -> use_autoclose
    | "close" -> close
    | "edit" -> edit
    | "init" -> init
    | "list" -> list
    | "all" -> (fun _ -> list ["open"]; list ["closed"])
    | "open" -> (fun _ -> list ["open"])
    | "closed" -> (fun _ -> list ["closed"])
    | "merge" -> merge
    | "reopen" -> reopen
    | "show" -> show
    | "help" -> show_help ()
    | s -> show_help ()
  in f @@ slice 2 (-1) (Array.to_list Sys.argv)

let () =
  let cmd_s = if alen Sys.argv < 2 then "list" else Sys.argv.(1) in
  handle_cmd cmd_s

