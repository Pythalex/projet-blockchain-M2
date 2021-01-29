let f str =
  for i = 0 to 1000 do
    Format.printf "%s" str
  done

let () =
  let _ = Thread.create f '*' in
  let _ = Thread.create f '-' in
  ignore (read_line ())
