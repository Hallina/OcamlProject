let rec convertInStringList listInt =
    match listInt with
    | [] -> ["/"]
    | head::body -> List.append [(string_of_int head)] (convertInStringList body);

let separateWithSeparator listString = 
    match listString with
    | [] -> "/"
    | _ -> String.concat "; " listString  

let convertInStringWSeparator listInt =
    separateWithSeparator (convertInStringList listInt)