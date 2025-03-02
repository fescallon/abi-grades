type natural_science = Math | Physics 
    | Chemistry | Biology
and language = German | English | Spanish 
and social_study = History | Philosophy | Art
and subject = NaturalScience of natural_science | Language of language 
    | SocialStudy of social_study | PhysicalEducation 
and grade = {
    subject : subject;
    grades  : int option * int option * int option * int option 
}
and profile = {
    name    : string;
    exams   : subject option * subject option * subject option * subject option * subject option;
    grades  : grade list
}

exception Corrupted_file of string
exception End_of_profile
exception Empty_Subject

let subject_of_string s = match s with
    | "art" | "music" | "mus"     -> SocialStudy Art
    | "bio" | "biology"     -> NaturalScience Biology
    | "chm" | "chem" | "chemistry"     -> NaturalScience Chemistry
    | "dam" | "ger" | "german"     -> Language German
    | "eng" | "english"     -> Language English
    | "his" | "history"     -> SocialStudy History
    | "mth" | "math" | "mathematics"     -> NaturalScience Math
    | "phe" | "pe" | "physed"     -> PhysicalEducation
    | "phi" | "philosophy"     -> SocialStudy Philosophy
    | "phy" | "phys" | "physics" -> NaturalScience Physics
    | "spa" | "spanish"     -> Language Spanish
    | "endprof" -> raise End_of_profile
    | "-"       -> raise Empty_Subject
    | _         -> raise @@ Invalid_argument s

let subject_of_string_opt s = 
    try Some (subject_of_string s)
    with _ -> None

let subject_of_string_opt_empty s = 
    try Some (subject_of_string s)
    with Empty_Subject -> None

let string_of_subject s = match s with
    | NaturalScience n -> 
            (match n with
            | Math -> "Math"
            | Physics -> "Physics"
            | Chemistry -> "Chemistry"
            | Biology -> "Biology")
    | Language l -> 
            (match l with
            | German -> "DAM"
            | English -> "English"
            | Spanish -> "Spanish") 
    | SocialStudy s ->
            (match s with
            | Art -> "Art/Music"
            | History -> "History"
            | Philosophy -> "Philosophy")
    | PhysicalEducation -> "Physical Education"

let string_of_subject_table s = match s with
    | NaturalScience n -> 
            (match n with
            | Math -> "\t  Math\t\t|"
            | Physics -> "\t Physics\t|"
            | Chemistry -> "\tChemistry\t|"
            | Biology -> "\t Biology\t|")
    | Language l -> 
            (match l with
            | German -> "\t   DAM\t\t|"
            | English -> "\t English\t|"
            | Spanish -> "\t Spanish\t|") 
    | SocialStudy s ->
            (match s with
            | Art -> "\tArt/Music\t|"
            | History -> "\t History\t|"
            | Philosophy -> "       Philosophy\t|")
    | PhysicalEducation -> "   Physical Education\t|"

let string_of_subject_file s = match s with
    | NaturalScience n -> 
            (match n with
            | Math -> "mth"
            | Physics -> "phy"
            | Chemistry -> "chm"
            | Biology -> "bio")
    | Language l -> 
            (match l with
            | German -> "dam"
            | English -> "eng"
            | Spanish -> "spa") 
    | SocialStudy s ->
            (match s with
            | Art -> "art"
            | History -> "his"
            | Philosophy -> "phi")
    | PhysicalEducation -> "phe"

let is_alpha s = 
    let rec char_list_of_string i acc s = match i with
        | x when x = String.length s -> List.rev acc
        | _ -> let c = s.[i] in char_list_of_string (i + 1) (c::acc) s
    and aux l = match l with
        | [] -> true
        | c::cs -> match c with
            | ' ' | 'A'..'Z' | 'a'..'z' -> aux cs
            | _ -> false
    in aux @@ char_list_of_string 0 [] s


let rec load_config () = 
        if not @@ Sys.file_exists "config.txt" then
            let ret = Sys.command "echo -n \"/proflst\n/end_proflst\nprof:\" > config.txt"
            in if ret <> 0 then failwith "could not create config file"
                else load_config ()
        else let rec config = open_in "config.txt"
        and load_names() = 
            let s = input_line config in if s <> "/proflst" 
                then raise @@ Corrupted_file s
            else let rec aux acc =
            let s = input_line config in
            match s with
            | "/end_proflst" -> List.rev acc
            | _ -> if is_alpha s then aux (s::acc) else raise @@ Corrupted_file s
            in aux []
        and profiles_of_names ns = 
            let rec load_exams_of_name n = 
            let s = input_line config in
            match String.split_on_char ':' @@ s with
            | ["prof"; x] when x = n -> 
                (let s' = input_line config in
                match String.split_on_char ':' @@ s' with
                | ["exams"; es] -> 
                    (let l = List.map subject_of_string_opt_empty @@ String.split_on_char ';' es 
                    in match l with
                    | [a; b; c; d; e] ->
                        (let rec is_complete l = match l with
                        | [] -> true
                        | x::xs -> if x <> None && List.mem x xs then false 
                                    else is_complete xs
                        in if not @@ is_complete l then raise @@ Corrupted_file "Incomplete exams"
                        else match a with
                        | Some Language German -> 
                            if List.mem (Some PhysicalEducation) l 
                            then failwith "Physical Education cannot be an exam subject" 
                            else (match b with
                            | Some NaturalScience Math | Some Language English 
                            | Some Language Spanish | None -> a, b, c, d, e
                            | _ -> raise @@ Corrupted_file 
                                (Printf.sprintf "Error loading \
                        profile '%s': Second exam subject must be an E-Subject" n))
                        | _ -> failwith (Printf.sprintf "Error loading \
                        profile '%s': First exam subject must be German" n))
                    | _ -> raise @@ Corrupted_file "Corrupted exam subjects")
                | _ -> raise @@ Corrupted_file s')
            | _ -> raise @@ Corrupted_file s

            and load_grades () = 
                let st = input_line config in
                match String.split_on_char ':' @@ st with
                | ["grades"] -> 
                        let read_grades () = 
                            let rec aux acc = let s = input_line config in
                            match String.split_on_char ':' s with
                            | ["endprof"] -> 
                                (let is_complete l = 
                                    let rec is_repeated e l = match l with
                                    | [] -> false
                                    | g::gs -> if g.subject = e.subject then true
                                                else is_repeated e gs
                                    in let rec aux l = match l with
                                        | [] -> true
                                        | g::gs -> if is_repeated g gs then false 
                                                    else aux gs
                                    in List.length l = 11 && aux l
                                in match is_complete acc with
                                | true -> List.rev acc
                                | false -> raise @@ Corrupted_file "Incomplete grades")
                            | [sj; g] -> let gs = 
                                let l = String.split_on_char ';' g in
                                (let map s = if s = "-" then None else Some (int_of_string s)
                                in try match List.map map l with
                                | [a; b; c; d] -> a, b, c, d
                                | _ -> raise @@ Corrupted_file "Incomplete grades"
                                with _ -> raise @@ Corrupted_file s)
                            in let grade = {subject = subject_of_string sj; grades = gs}
                            in aux (grade::acc)
                            | _ -> raise @@ Corrupted_file s
                            in aux []
                        in read_grades ()
                | _ -> raise @@ Corrupted_file st
            and aux acc o l = match l with
                | [] -> let s = input_line config in
                        if String.split_on_char ':' @@ s <> ["prof"; ""] 
                        then raise @@ Corrupted_file s; 
                        o, List.rev acc
                | n::ns -> 
                        let es = load_exams_of_name n
                        in let gs = load_grades ()
                        in let p = {name = n; exams = es; grades = gs}
                        in aux (p::acc) o ns
            in try 
                aux [] ns ns; 
            with e -> close_in config; raise e
        in profiles_of_names @@ load_names ()

let create_profile n = 
    let config = open_in "config.txt" in
    let rec check_existing () = 
        match input_line config with
        | l when l = n -> failwith @@ Printf.sprintf "Error creating profile: \
                            a profile named \"%s\" already exists." n
        | "/end_proflst" -> ()
        | _ -> check_existing ()
    in check_existing ();
    close_in config;
    let empty_profile = 
        Printf.sprintf "%s\nexams:dam;-;-;-;-\ngrades\nart:-;-;-;-\nbio:-;-;-;-\
        \nchm:-;-;-;-\ndam:-;-;-;-\neng:-;-;-;-\nhis:-;-;-;-\nmth:-;-;-;-\
        \nphi:-;-;-;-\nphe:-;-;-;-\nphy:-;-;-;-\nspa:-;-;-;-\nendprof\nprof:" n
    in let config = open_in "config.txt" in
    let rec update_file acc = let l = input_line config in
        match l with
        | "/end_proflst" -> update_file (acc ^ n ^ "\n" ^ l ^ "\n")
        | "prof:"        -> acc ^ l ^ empty_profile
        | l when l = n -> failwith @@ Printf.sprintf "Error creating profile: \
                            a profile named \"%s\" already exists." n
        | _              -> update_file (acc ^ l ^ "\n")
    in let s = update_file "" in close_in config; 
    let config = open_out "config.txt" in output_string config s; close_out config

let delete_profile n = 
    let config = open_in "config.txt" in
    let rec profile_list found acc = 
        match input_line config with
        | "/end_proflst" when found = false -> raise Not_found
        | "/end_proflst" as l -> acc ^ l ^ "\n"
        | l when l = n -> profile_list true acc
        | l -> profile_list found (acc ^ l ^ "\n")
    in let rec profiles acc = 
        match input_line config with
        | l when l = Printf.sprintf "prof:%s" n -> 
            let rec skip_profile () =
                match input_line config with
                | "endprof" -> ()
                | _ -> skip_profile ()
            in skip_profile (); profiles acc
        | "prof:" as l -> acc ^ l
        | l -> profiles (acc ^ l ^ "\n")
    in let s = profiles @@ profile_list false ""
    in close_in config;
    let config = open_out "config.txt" in output_string config s; close_out config



