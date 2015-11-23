type date = { year : int; month : int; day : int;
              hour : int; minute : int }

let the_origin_of_time = { year = 1; month = 1; day = 1;
                           hour = 0; minute = 0 }

let a_malformed_time = { year = 5; month = 5; day = 4;
                         hour = 1; minute = 2 }

let wellformed date =
  if date.year >= 1 &&
     date.month >= 1 && date.month <= 5 &&
     date.day >= 1 && date.day <= 4 &&
     date.hour >= 0 && date.hour <= 2 &&
     date.minute >= 0 && date.minute <= 1 then
    true
  else
    false;;

let rec normalize_time invalid_date =
  if invalid_date.minute > 1 then
    normalize_time {year = invalid_date.year; month = invalid_date.month; day = invalid_date.day;
     hour = invalid_date.hour + 1; minute = 0}
  else if invalid_date.hour > 2 then
    normalize_time {year = invalid_date.year; month = invalid_date.month; day = invalid_date.day + 1;
     hour = 0; minute = invalid_date.minute}
  else if invalid_date.day > 4 then
    normalize_time {year = invalid_date.year; month = invalid_date.month + 1; day = 1;
     hour = invalid_date.hour; minute = invalid_date.minute}
  else if invalid_date.month > 5 then
    normalize_time {year = invalid_date.year + 1; month = 1; day = invalid_date.day;
     hour = invalid_date.hour; minute = invalid_date.minute}
  else
    {year = invalid_date.year; month = invalid_date.month; day = invalid_date.day;
     hour = invalid_date.hour; minute = invalid_date.minute};;

let next date : date =
  normalize_time {year = date.year; month = date.month; day = date.day;
                  hour = date.hour; minute = date.minute + 1};;

let rec date_constructor date minutes : date =
  if minutes = 0 then
    date
  else
    date_constructor (next date) (minutes - 1);;

let of_int minutes =
  date_constructor the_origin_of_time minutes;;
