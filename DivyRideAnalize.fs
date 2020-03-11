//
// F# program to analyze Divvy daily ride data.
//
// << Ria Gupta >>
// U. of Illinois, Chicago
// CS 341, Fall 2019
// Project #04
//

#light

module project04

//
// ParseLine and ParseInput
//
// Given a sequence of strings representing Divvy data, 
// parses the strings and returns a list of lists.  Each
// sub-list denotes one bike ride.  Example:
//
//   [ [15,22,141,17,5,1124]; ... ]
//
// The values are station id (from), station id (to), bike
// id, starting hour (0..23), starting day of week (0 Sunday-6 Saturday)
// and trip duration (secs), 
//
let ParseLine (line:string) = 
  let tokens = line.Split(',')
  let ints = Array.map System.Int32.Parse tokens
  Array.toList ints

let rec ParseInput lines = 
  let rides = Seq.map ParseLine lines
  Seq.toList rides

//The replacement for .item
//It takes in the first elem of list of list and index 
//then reccursivly goes to the nth element by (x-1)
let rec gotoelement L x =
   match L with
   |[]  -> 0
   |e::_ when x = 0 -> e
   |_::rest -> gotoelement rest (x-1)

//Takes in the list of list and the index number
let rec findelement L y = 
   match L with 
   |[] -> []
   |e:: rest -> gotoelement e y :: findelement rest y

//To count the number of bikes
//simply by adding 1 and rec calling the function
let rec num_bike L  =
   match L with 
   |[] -> 0
   | _::rest   -> 1 + num_bike rest  

//Checks if the given list has the given elem 
//and sends true or false back 
let rec contains x L = 
 match L with
 | []  -> false
 | e::_ when e= x -> true
 | _::rest  -> (contains x rest)

//Takes in the list and then send the helper function 
//the list and an empty list 
//then populate the empty list with the unique elem
//by cheking if the elem is in the list or no
let Remove L =
  let rec remove list1 list2=
      match list1 with 
      | [] -> list2
      | e::rest when (contains e list2) -> remove rest list2 
      | e::rest -> remove rest (e:: list2)
  remove L []

//Takes in the list and the ID and checks in the 
//given list if it occurs if it does then adds to the count
//else checks with the next elem and rec
let rec count_bikeID L x =
  match L with
  | []   -> 0
  | e::rest when e = x -> 1 + count_bikeID rest x  
  | _::rest -> 0 + count_bikeID rest x

//go to the given list them go to the specific index 
//if the elem at index matches the given ID
//then add the index that has the time in the new list 
//and rec
let rec bike_avg_time L x =
  match L with
  | [] -> []
  | e::rest when (gotoelement e 2) = x -> (gotoelement e 5) :: bike_avg_time rest x 
  | _::rest -> bike_avg_time rest x
 
//Count the number of time the given ID is in the given list
//if it is in the list then add in the count
//else rec
let rec count_station_ID L x =
  match L with
  | []   -> 0
  | e::rest when e = x -> 1 + count_station_ID rest x  
  | _::rest -> 0 + count_station_ID rest x

//go to the given list them go to the specific index 
//if the elem at index matches the given ID
//then add the index that has the time in the new list 
//and rec
let rec station_avg_time L x =
  match L with
  | [] -> []
  | e::rest when (gotoelement e 1) = x -> (gotoelement e 5) :: station_avg_time rest x 
  | _::rest -> station_avg_time rest x

//counting the numer of trips using rec 
//for the specific asked ID
let rec count_numOfTrips L x =
  match L with
  | []   -> 0
  | e::rest when e = x -> 1 + count_numOfTrips rest x  
  | _::rest -> count_numOfTrips rest x

//the function to print the stars for the histogram
let rec printstars n =
  match n with
  | 0 -> ()
  | 1 -> printf "*"
  | _ -> printf "*"
         printstars (n-1)

//prints the day number and then the stars
//and then the number of rides
//the starts are printed by calling count and giving the number
//to print star
let printhistogram v L =
    printf "%A: " v
    printstars ((count_numOfTrips L v)/10)
    printfn" %A" (count_numOfTrips L v )

//taking the list of stationIDs and unique stationID
//then counting the number of occurance of the ID taken 
//from the unique ID list and creating a list of list of two elements
//and then reccursing
let rec count_station_rides list1 list2 =
  match list1 with
  |[] -> []
  |e::rest -> [e ; (count_station_ID list2 e)] :: count_station_rides rest list2

//to print number of rides in the formate asked
let rec getTheListOfStaion L =
  match L with
  | [ ] -> [ ]
  | e::rest -> (gotoelement e 1) :: getTheListOfStaion rest
//to print number of rides in the formate asked
let rec getTheListOfOccurance L =
  match L with
  | [ ] -> [ ]
  | e::rest -> (gotoelement e 0) :: getTheListOfOccurance rest

[<EntryPoint>]
let main argv =
  //
  // input file name, then input divvy ride data and build
  // a list of lists:
  //
  printf "filename> "
  let filename = System.Console.ReadLine()
  let contents = System.IO.File.ReadLines(filename)
  let ridedata = ParseInput contents
  let find_bikes = findelement ridedata 2
  let total_bikes = List.length(Remove find_bikes)

  //printfn "%A" ridedata
  let N = List.length ridedata
  printfn ""
  printfn "# of rides: %A" N
  printfn ""
  printfn "# of bikes: %A" total_bikes
  printfn ""
  printf "BikeID> "
  let input = System.Console.ReadLine()
  let bike_ID = System.Int32.Parse(input)
  let count_bike_ID = count_bikeID find_bikes bike_ID
  let avgtime =  bike_avg_time ridedata bike_ID
  let avgTime = (float(List.sum(avgtime)))/(float(List.length(avgtime)))
  let totalmin = (List.sum(avgtime))/60
  let totalsec = (List.sum(avgtime))%60
  printfn ""
  printf "# of rides for BikeID %A" bike_ID
  printfn ": %A"count_bike_ID
  printfn ""
  printf "Total time spent riding BikeID %A" bike_ID
  printf ": %A" totalmin
  printf " minutes %A" totalsec
  printfn " seconds"
  printfn ""
  printf "Average time spent riding BikeID %A" bike_ID
  printf ": %.2f" avgTime
  printfn" seconds"
  printfn ""
  printf "StationID> "
  let input2 = System.Console.ReadLine()
  let station_ID = System.Int32.Parse(input2)
  let find_stationID = findelement ridedata 1
  let count_stationID = count_station_ID find_stationID station_ID
  let station_avgtime =  station_avg_time ridedata station_ID
  let station_avgTime = (float(List.sum(station_avgtime)))/(float(List.length(station_avgtime)))
  printfn ""
  printf "# of rides to StationID %A" station_ID
  printfn ": %A"count_stationID
  printfn ""
  printf "Average time spent on trips leading to StationID %A" station_ID
  printf": %.2f" station_avgTime
  printfn " seconds"
  printfn ""
  let numTrips = findelement ridedata 4
  let filter_stationID = Remove find_stationID
  printfn "Number of Trips on Sunday: %A" (count_numOfTrips numTrips 0)
  printfn "Number of Trips on Monday: %A" (count_numOfTrips numTrips 1)
  printfn "Number of Trips on Tuesday: %A" (count_numOfTrips numTrips 2)
  printfn "Number of Trips on Wednesday: %A" (count_numOfTrips numTrips 3)
  printfn "Number of Trips on Thursday: %A" (count_numOfTrips numTrips 4)
  printfn "Number of Trips on Friday: %A" (count_numOfTrips numTrips 5)
  printfn "Number of Trips on Saturday: %A" (count_numOfTrips numTrips 6)
  printfn ""
  let L = List.map(fun v -> printhistogram v numTrips) [0..6]
  printfn ""

  let all_rides = count_station_rides filter_stationID find_stationID
  let sortx = all_rides |> List.sortBy (fun [x;_y] -> x)
  let sorty = sortx |> List.sortBy (fun [_x;y] -> -y) |> List.take 10
  let first10Station = getTheListOfStaion sorty
  let first10Occu = getTheListOfOccurance sorty
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 0) (gotoelement first10Station 0)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 1) (gotoelement first10Station 1)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 2) (gotoelement first10Station 2)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 3) (gotoelement first10Station 3)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 4) (gotoelement first10Station 4)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 5) (gotoelement first10Station 5)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 6) (gotoelement first10Station 6)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 7) (gotoelement first10Station 7)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 8) (gotoelement first10Station 8)
  printfn"# of rides to station %A: %A" (gotoelement first10Occu 9) (gotoelement first10Station 9)
  printfn ""
  0 
