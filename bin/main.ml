open Config

let rec getopt t n = 
    let _ = Sys.command "clear" in
    print_endline t;
    match read_line () with
    | "exit" | "e" | "quit" | "q" -> exit 0
    | "cancel" | "c" -> print_endline "Operation cancelled"; Unix.sleep 1; -1
    | l ->
            match int_of_string_opt l with
            | Some x when x > 0 && x <= n -> x
            | _ -> print_endline "Invalid input."; Unix.sleep 1; getopt t n

let confirm_prompt def =
    match read_line () with
    | "" -> def
    | "Y" | "y" | "yes" | "Yes" -> true
    | _ -> false
    
let open_profile_menu () =
    let names, profiles = load_config () in
    let name_list, cnt =
        let rec aux acc i l =
            match l with
            | [] -> acc ^ string_of_int i ^ ". New Profile", i
            | n :: ns -> aux (acc ^ string_of_int i ^ ". " ^ n ^ "\n") (i + 1) ns in
        aux "" 1 names in
    let msg = "Select profile to open, or enter (c)ancel to return to the menu\n" ^ name_list in 
    match getopt msg cnt with
    | -1 -> None
    | n when n = cnt -> 
            print_string "New profile name: "; 
            let s = read_line () in
            create_profile s; 
            Printf.printf "Profile %s created successfully\n" s;
            Unix.sleep 1;
            let _, new_ps = load_config () in Some (List.nth new_ps (n - 1))
    | n -> Some (List.nth profiles (n - 1))


let delete_profile_menu () =
    let names, profiles = load_config () in
    if names = [] then
        let _ = Sys.command "clear" in
        print_endline "There are no saved profiles. Returning to main menu.";
        Unix.sleep 2;
        None
    else
    let name_list, cnt = 
        let rec aux acc i l = 
            match l with 
            | [] -> acc, i - 1
            | n :: ns -> 
                    let tl = if ns = [] then "" else "\n" in
                    aux (acc ^ string_of_int i ^ ". " ^ n ^ tl) (i + 1) ns in
        aux "" 1 names in
    let msg = "Select profile to delete, or enter (c)ancel to return to the menu.\n" ^ name_list in
    match getopt msg cnt with
    | -1 -> None
    | n ->  
            let p = List.nth names (n - 1) in
            Printf.printf "Delete profile \"%s\"? [y/N] " p;
            if confirm_prompt false then 
                (print_string "Deleting...";
                flush stdout;
                delete_profile p;
                Printf.printf "\rProfile \"%s\" successfully deleted.\n" p;
                Unix.sleep 1; None)
            else 
                (print_endline "Cancelling";
                Unix.sleep 1; None)



let main_menu () =
    let msg = 
    "Abitur Grades Calculator \n\
    Select an option by entering its number, or type (e)xit or (q)uit to exit.\n\
    1. Open profile \n\
    2. Delete profile" in
    match getopt msg 2 with
    | 1 -> open_profile_menu ()
    | _ -> delete_profile_menu ()


let () =
    match main_menu () with
    | None -> print_endline "No profile selected."
    | Some p -> Printf.printf "Profile \"%s\" selected.\n" p.name

    (* let rec loop () = *)
    (* let profile = *)
    (*     let rec menu_loop () = *)
    (*     let _ = Sys.command "clear" in *)
    (*     let names, profiles = load_config () in *)
    (*     let print_names () = *)
    (*         print_endline "Select a profile/an option by entering its number, or \ *)
    (*         type \"exit\" to exit:"; *)
    (*         let rec aux i l = match l with *)
    (*         | [] -> print_endline (string_of_int i ^ ". New Profile"); *) 
    (*                 if List.length profiles > 0 then *)
    (*                 (print_endline (string_of_int (i + 1) ^ ". Delete existing profile"); *)
    (*                 i + 1) *)
    (*                 else i *)
    (*         | n::ns -> *) 
    (*                 print_endline (string_of_int i ^ ". " ^ n); *)
    (*                 aux (i + 1) ns *)
    (*         in aux 1 names *)
    (*     in let count = print_names () in *)
    (*     match read_line () with *)
    (*     | "exit" | "e" | "quit" | "q" -> exit 0 *)
    (*     | l -> *) 
    (*         match int_of_string_opt l with *)
    (*         | None -> print_endline "Invalid input."; Unix.sleep 1; let _ = Sys.command "clear" in menu_loop () *)
    (*         | Some x when x < 1 || x > count -> print_endline "Invalid input."; *) 
    (*             Unix.sleep 1; let _ = Sys.command "clear" in menu_loop () *)
    (*         | Some x -> match x with *)
    (*             | x when x = count && x <> 1 -> *) 
    (*                     let rec delete_loop () = *)
    (*                     print_endline "Select the profile to delete, or type \"c\" to cancel:"; *)
    (*                     let rec print_names i l = match l with *)
    (*                     | [] -> () *)
    (*                     | x::xs -> *) 
    (*                         print_endline (string_of_int i ^ ". " ^ x); *)
    (*                         print_names (i + 1) xs *)
    (*                     in print_names 1 names; *)
    (*                     match read_line () with *)
    (*                     | "c" -> print_endline "Canceling"; *)
    (*                         Unix.sleep 1; let _ = Sys.command "clear" in menu_loop () *)
    (*                     | l -> match int_of_string_opt l with *)
    (*                         | None -> print_endline "Invalid input."; *) 
    (*                             Unix.sleep 1; let _ = Sys.command "clear" in *) 
    (*                             delete_loop () *)
    (*                         | Some x when x < 1 || x > count - 2 -> *) 
    (*                             print_endline "Invalid input."; *) 
    (*                             Unix.sleep 1; let _ = Sys.command "clear" in *) 
    (*                             delete_loop () *)
    (*                         | Some x -> let s = List.nth names (x - 1) in *)
    (*                             let rec confirm () = *)
    (*                             Printf.printf "Are you sure that you want to \ *)
    (*                             delete the profile \"%s\"? This cannot be \ *)
    (*                             undone. [y/N]\n" s; *)
    (*                             match read_line () with *)
    (*                             | "n" | "N" | "" -> *) 
    (*                                 print_endline "Operation canceled. \ *)
    (*                                     Returning to main menu"; *)
    (*                                 Unix.sleep 2; *)
    (*                                 let _ = Sys.command "clear" in menu_loop () *)
    (*                             | "y" | "Y" -> *)
    (*                                 delete_profile s; *)
    (*                                 Printf.printf "Profile \"%s\" deleted.\n\ *)
    (*                                     Returning to main menu.\n" s; *)
    (*                                 Unix.sleep 2; *)
    (*                                 let _ = Sys.command "clear" in menu_loop () *)
    (*                             | _ -> *) 
    (*                                     print_endline "Invalid input."; *)
    (*                                     confirm () *)
    (*                             in confirm () *)
    (*                     in delete_loop () *)
    (*             | x when x = count - 1 || count = 1 && x = count -> *)
    (*                 let _ = Sys.command "clear" in *)
    (*                 print_string "Enter new profile name: "; *)
    (*                 let s = read_line () in create_profile s; *)
    (*                 let _, profiles = load_config () *)
    (*                 in let rec aux l = match l with *)
    (*                 | []    -> failwith "Error creating profile" *)
    (*                 | [p]   -> p *)
    (*                 | _::ps -> aux ps in aux profiles *)
    (*             | _ -> List.nth profiles (x - 1) *)
    (*     in menu_loop () *)
    (* in let rec profile_loop profile = *)
    (*     let _ = Sys.command "clear" in *)
    (*     print_endline @@ Printf.sprintf "Selected profile: %s" profile.name; *)
    (*     print_endline "Select an option, or type \"exit\" to exit:\n\ *)
    (*                     1. Show grades\n\ *)
    (*                     2. Edit grades\n\ *)
    (*                     3. Return to main menu"; *)
    (*     match read_line () with *)
    (*     | "exit" | "e" | "quit" | "q" -> exit 0 *)
    (*     | l -> *)
    (*         match int_of_string_opt l with *) 
    (*         | Some 1 -> *)
    (*             let sep = "==================================================" in *)
    (*             let rec print_grades sum sems l = *) 
    (*                 let avg sum sems = match sems with *)
    (*                         | 0 -> 0. *)
    (*                         | _ -> ((float_of_int sum) /. (float_of_int sems)) *)
    (*                 in match l with *)
    (*                 | [] ->  Printf.printf "Total Average: %.2f\n" (avg sum sems) *) 
    (*                 | g::gs -> *) 
    (*                     let sj = string_of_subject_table g.subject *) 
    (*                     in let string_of_grades (a, b, c, d) = *) 
    (*                             let p g = match g with None -> "-" | Some x -> string_of_int x *)
    (*                             in Printf.sprintf "%s | %s | %s | %s |"  (p a) (p b) (p c) (p d) *)
    (*                     in let g' = string_of_grades g.grades *)
    (*                     in let sum_and_sems (a, b, c, d) = *) 
    (*                         let l = [a; b; c; d] in *)
    (*                         let add acc x = match x with None -> acc | Some y -> acc + y *)
    (*                         in let sem_cnt acc x = match x with None -> acc | _ -> acc + 1 *)
    (*                         in List.fold_left add 0 l, List.fold_left sem_cnt 0 l *)
    (*                     in let sum', sems' = sum_and_sems g.grades *) 
    (*                     in Printf.printf "%s %s  %.2f\n" sj g' *) 
    (*                     (avg sum' sems'); *)
    (*                     print_endline sep; *)
    (*                     print_grades (sum + sum') (sems + sems') gs *)
    (*             in *) 
    (*             Printf.printf "\t\tGrades of %s\n" profile.name; *)
    (*             print_endline sep; *)
    (*             print_endline "\t\t\t|    Semester   |"; *)
    (*             print_endline "         Subject \t| 1 | 2 | 3 | 4 | Average"; *)
    (*             print_endline sep; *)
    (*             print_grades 0 0 profile.grades; *)
    (*             print_endline "Press enter to return to the profile menu"; *)
    (*             let _ = input_char stdin in profile_loop profile *)



    (*         | Some 2 -> let profile = grades_editor profile in profile_loop profile *)
    (*         | Some 3 -> loop () *)
    (*         | _ -> print_endline "\nInvalid input."; profile_loop profile *)
    (*     in profile_loop profile *)
    (* in loop () *)


