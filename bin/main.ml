open Config
let grades_editor profile = 
    print_endline "Grades editor mode. Enter \"help\" or \"h\" for help.";
    let original = profile in
    let rec loop () =
        print_string "> ";
        match read_line () with
        | "help" | "h" -> 
                let help_msg = 
                    "When a command has square brackets around a letter, \
                    the letter in the brackets can be used instead of the \
                    full word. Example: [s]how <subject> means the command can \
                    be used by typing \"show\", or simply \"s\" (\"show chem\" \
                    and \"s chem\" do the same)\n
                    Command list:\n\
                    [s]how <subject>: displays the saved grades from a single \
                    subject. Example: s math\n\
                    [e]dit <subject> <semester number> <grade>: Edits the saved \
                    semester grades in a subject. Example: \"e math 1 10 2 11\" \
                    stores the grade 10 in the 1st semester of math, and an 11 in \
                    the 2nd semester. Up to 4 semester grades can be input.\n\
                    Subject command names:\n\
                    subjects: displays a list of subject names to be used in commands"
                in print_endline help_msg; loop ()
        | "subjects" ->
                let msg = "Subject names\nA slash separates synonyms for the same \
                    subject. A part of a word in square brackets means just that \
                    part can be used instead of the full name. \
                    Example: [bio]logy means both \"biology\" and \"bio\" are \
                    valid options for the subject Biology.\n\
                    Art/Music: art/[mus]ic\nBiology: [bio]logy\nChemistry: [chem]istry\n\
                    English: [eng]lish\nGerman: [ger]man/dam\nHistory: [his]tory\n\
                    Math: [math]ematics\nPhilosophy: [phi]losophy\n\
                    Physical Education: physed/pe\nPhysics: [phy]sics\nSpanish: [spa]nish"
                    in print_endline msg; loop ()
        | l -> if l = "q" then profile else
            match String.split_on_char ' ' l with
            | ["show"; s] | ["s"; s] -> 
                (match subject_of_string_opt s with
                | None -> Printf.printf "Unknown subject: \"%s\"\n" s; loop ()
                | Some sub -> 
                    (let rec grades_of_subject l s = match l with
                        | [] -> raise Not_found
                        | g::gs -> 
                            (match g.subject with
                            | s' when s' = s -> g.grades
                            | _ -> grades_of_subject gs s)
                    in let gs = grades_of_subject profile.grades sub
                    in let string_of_grades(a, b, c, d) =
                        let p g = match g with None -> "-" | Some x -> string_of_int x
                        in Printf.sprintf "1. %s 2. %s 3. %s 4. %s" (p a) (p b) (p c) (p d) 
                    in Printf.printf "Grades in %s: %s - Average: %.2f\n" 
                        (string_of_subject sub) (string_of_grades gs)
                        (let sum_and_sems (a, b, c, d) = 
                            let l = [a; b; c; d] in
                            let add acc x = match x with None -> acc | Some y -> acc + y
                            in let sem_cnt acc x = match x with None -> acc | _ -> acc + 1
                            in List.fold_left add 0 l, List.fold_left sem_cnt 0 l
                        in let sum, sems = sum_and_sems gs 
                        in if sems = 0 then 0. else (float_of_int sum) /. (float_of_int sems)));
                        loop ())
            | _ -> Printf.printf "Unknown command \"%s\"\n" l; loop ()
        in loop ()

                    
open Curses
let () =
    let w = initscr () in
    let _ = noecho () in
    let  _ = keypad w true in
    let rec cursesloop () =
        let y, x = getyx w in
        let _ = match getch() with
        | c when c = Key.up -> move (y-1) x
        |  c when c = Curses.Key.down  -> move (y+1) x
        | c when c = Curses.Key.right -> move y (x+1)
        | c when c = Curses.Key.left -> move y (x-1)
        | 113 -> endwin (); exit 0
        | _ -> true
        in let _ = refresh() in cursesloop ()
    in cursesloop ()

    (*let rec loop () =
    let profile =
        let rec menu_loop () =
        let _ = Sys.command "clear" in
        let names, profiles = load_config () in
        let print_names () =
            print_endline "Select a profile/an option by entering its number, or \
            type \"exit\" to exit:";
            let rec aux i l = match l with
            | [] -> print_endline (string_of_int i ^ ". New Profile"); 
                    if List.length profiles > 0 then
                    (print_endline (string_of_int (i + 1) ^ ". Delete existing profile");
                    i + 1)
                    else i
            | n::ns -> 
                    print_endline (string_of_int i ^ ". " ^ n);
                    aux (i + 1) ns
            in aux 1 names
        in let count = print_names () in
        match read_line () with
        | "exit" | "e" | "quit" | "q" -> exit 0
        | l -> 
            match int_of_string_opt l with
            | None -> print_endline "Invalid input."; Unix.sleep 1; let _ = Sys.command "clear" in menu_loop ()
            | Some x when x < 1 || x > count -> print_endline "Invalid input."; 
                Unix.sleep 1; let _ = Sys.command "clear" in menu_loop ()
            | Some x -> match x with
                | x when x = count && x <> 1 -> 
                        let rec delete_loop () =
                        print_endline "Select the profile to delete, or type \"c\" to cancel:";
                        let rec print_names i l = match l with
                        | [] -> ()
                        | x::xs -> 
                            print_endline (string_of_int i ^ ". " ^ x);
                            print_names (i + 1) xs
                        in print_names 1 names;
                        match read_line () with
                        | "c" -> print_endline "Canceling";
                            Unix.sleep 1; let _ = Sys.command "clear" in menu_loop ()
                        | l -> match int_of_string_opt l with
                            | None -> print_endline "Invalid input."; 
                                Unix.sleep 1; let _ = Sys.command "clear" in 
                                delete_loop ()
                            | Some x when x < 1 || x > count - 2 -> 
                                print_endline "Invalid input."; 
                                Unix.sleep 1; let _ = Sys.command "clear" in 
                                delete_loop ()
                            | Some x -> let s = List.nth names (x - 1) in
                                let rec confirm () =
                                Printf.printf "Are you sure that you want to \
                                delete the profile \"%s\"? This cannot be \
                                undone. [y/N]\n" s;
                                match read_line () with
                                | "n" | "N" | "" -> 
                                    print_endline "Operation canceled. \
                                        Returning to main menu";
                                    Unix.sleep 2;
                                    let _ = Sys.command "clear" in menu_loop ()
                                | "y" | "Y" ->
                                    delete_profile s;
                                    Printf.printf "Profile \"%s\" deleted.\n\
                                        Returning to main menu.\n" s;
                                    Unix.sleep 2;
                                    let _ = Sys.command "clear" in menu_loop ()
                                | _ -> 
                                        print_endline "Invalid input.";
                                        confirm ()
                                in confirm ()
                        in delete_loop ()
                | x when x = count - 1 || count = 1 && x = count ->
                    let _ = Sys.command "clear" in
                    print_string "Enter new profile name: ";
                    let s = read_line () in create_profile s;
                    let _, profiles = load_config ()
                    in let rec aux l = match l with
                    | []    -> failwith "Error creating profile"
                    | [p]   -> p
                    | _::ps -> aux ps in aux profiles
                | _ -> List.nth profiles (x - 1)
        in menu_loop ()
    in let rec profile_loop profile =
        let _ = Sys.command "clear" in
        print_endline @@ Printf.sprintf "Selected profile: %s" profile.name;
        print_endline "Select an option, or type \"exit\" to exit:\n\
                        1. Show grades\n\
                        2. Edit grades\n\
                        3. Return to main menu";
        match read_line () with
        | "exit" | "e" | "quit" | "q" -> exit 0
        | l ->
            match int_of_string_opt l with 
            | Some 1 ->
                let sep = "==================================================" in
                let rec print_grades sum sems l = 
                    let avg sum sems = match sems with
                            | 0 -> 0.
                            | _ -> ((float_of_int sum) /. (float_of_int sems))
                    in match l with
                    | [] ->  Printf.printf "Total Average: %.2f\n" (avg sum sems) 
                    | g::gs -> 
                        let sj = string_of_subject_table g.subject 
                        in let string_of_grades (a, b, c, d) = 
                                let p g = match g with None -> "-" | Some x -> string_of_int x
                                in Printf.sprintf "%s | %s | %s | %s |"  (p a) (p b) (p c) (p d)
                        in let g' = string_of_grades g.grades
                        in let sum_and_sems (a, b, c, d) = 
                            let l = [a; b; c; d] in
                            let add acc x = match x with None -> acc | Some y -> acc + y
                            in let sem_cnt acc x = match x with None -> acc | _ -> acc + 1
                            in List.fold_left add 0 l, List.fold_left sem_cnt 0 l
                        in let sum', sems' = sum_and_sems g.grades 
                        in Printf.printf "%s %s  %.2f\n" sj g' 
                        (avg sum' sems');
                        print_endline sep;
                        print_grades (sum + sum') (sems + sems') gs
                in 
                Printf.printf "\t\tGrades of %s\n" profile.name;
                print_endline sep;
                print_endline "\t\t\t|    Semester   |";
                print_endline "         Subject \t| 1 | 2 | 3 | 4 | Average";
                print_endline sep;
                print_grades 0 0 profile.grades;
                print_endline "Press enter to return to the profile menu";
                let _ = input_char stdin in profile_loop profile



            | Some 2 -> let profile = grades_editor profile in profile_loop profile
            | Some 3 -> loop ()
            | _ -> print_endline "\nInvalid input."; profile_loop profile
        in profile_loop profile
    in loop ()*)


