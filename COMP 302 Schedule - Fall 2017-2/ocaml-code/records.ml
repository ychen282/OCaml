(* Records *)

(* Defining records *)
type host_info =
    { hostname   : string
    ; os_name    : string
    ; cpu_arch   : string
    ; timestamp  : int
    }

(* Creating records *)
let my_host = {hostname = "Emmy" ; os_name = "MacOS" ; cpu_arch = "X86" ; timestamp = 1507848368}

(* Extracting data from records *)
let the_name = my_host.hostname

(* Functional update of a record, this creates a new copy of the record! NOT UPDATE *)
let my_new_host = {my_host with os_name = "Linux"} (* A new copy of the value is created with one value of difference *)

(* Mutable records *)


type theme_park =
  { ride_name : string
  ; mutable num_of_riders : int (* If a field says mutable it contains a reference *)
  }


(* Mutable fields are created just as regular fields *)
let mgr = {ride_name = "Merry go round" ; num_of_riders = 0}

(* References are updated with the <- instead of the := *)
let click ride =
  ride.num_of_riders <- ride.num_of_riders + 1
