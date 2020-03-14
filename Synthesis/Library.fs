module Synthesis

open System
open System

let abelar x = (x > 12 && x < 3097 && x%12=0 )
  //  failwith "Not implemented"

let area b h =
   match (b >= 0.0) && (h >= 0.0) with
   | true -> 0.5*b*h
   | false -> failwith("Can't handle negative values")
    
    // failWith("Can't handle negatives")
   
    // failwith "Not implemented"

let zollo a =
   match a > 0 with
   | true -> a*2
   | false -> a*(-1)
    //failwith "Not implemented"

let min a b =
   match a > b with
   |true -> b
   |false -> a
//    failwith "Not implemented"

let max a b =
    match a < b with
    |true -> b
    |false -> a

let ofTime h m s = h*60*60+m*60+s
    // failwith "Not implemented"

let toTime s = 
   match s > 0 with
   | true -> ((s/3600),((s-((s/3600)*3600))/60),(s-(s-(s-(((s-((s/3600)*3600))/60))*60)%60))) 
   | false -> (0,0,0)
   // failwith "Not implemented"

let digits n =
   let rec count n digitnr = 
      match n<>0 with
      | true -> count (n/10) (digitnr+1)
      | false -> max digitnr 1     
   count n 0 
    //failwith "Not implemented"

let minmax (a,b,c,d) = (min (min a b)(min c d) , max(max a b)(max c d))



    //failwith "Not implemented"

let isLeap y =
   match y < 1582 with
   | true -> failwith "Can't be less than year leap was implemented"
   | false -> match ((y % 4 = 0) && ((y % 100 <> 0) || (y % 400 = 0) )) with
              | true -> true
              | false -> false
   
   
  
let month m =
   match m with
   | 1 -> ("January",31)
   | 2 -> ("February",28)
   | 3 -> ("March",31)
   | 4 -> ("April",30)
   | 5 -> ("May",31)
   | 6 -> ("June",30)
   | 7 -> ("July",31)
   | 8 -> ("August",31)
   | 9 -> ("September",30)
   | 10 -> ("October",31)
   | 11 -> ("November",30)
   | 12 -> ("December",31)
   | _ -> failwith "Not a valid int"

let toBinary i =
   match i < 0 with
   |true -> failwith "NOPE"
   |false -> match i=0 with
             | true -> "0"
             | false -> let rec binary i =
                           match (i=0) with
                           | true -> ""
                           | false -> match i%2=0 with
                                      |true -> binary(i/2)+"0"
                                      |false ->binary(i/2)+"1"
                        binary i 


let bizFuzz n =
   let rec fizzing (m,c3s,c5s,both) = 
      match (m <0 || m=1) with
      | true -> (c3s,c5s,both)
      | false -> match m%3=0 with
                 | true -> match m%5=0 with
                           | true -> fizzing (m-1,c3s+1,c5s+1,both+1)
                           | false -> fizzing(m-1,c3s+1,c5s,both)
                 | false -> match m%5=0 with
                            | true -> fizzing(m-1,c3s,c5s+1,both)
                            | false -> fizzing(m-1,c3s,c5s,both)
   
   fizzing (n,0,0,0) 
   // failwith "Not implemented"

//ATTEMPT 3, heavily based off of my second idea had my classmate help me finalise the syntax. His solution is entirely different.
let monthDay day y =
  let leap = [31;29;31;30;31;30;31;31;30;31;30;31]
  let reg = [31;28;31;30;31;30;31;31;30;31;30;31]
  let getMonthName index =
    let (monthName, _) = month index
    monthName

  let rec subtract index days _list =
    match _list with 
    | head::tail -> 
      match days>head with 
      | true -> subtract (index+1) (days-head) tail
      | false -> index
    | _ -> failwith ""

  let isInvalidCase = day<1 || y<1582
  match isInvalidCase, (isLeap y) with 
  | true, _ -> failwith ""
  | _, true -> 
    let monthIndex = subtract 1 day leap
    getMonthName monthIndex
  | _, false -> 
    let monthIndex = subtract 1 day reg
    getMonthName monthIndex

//ATTEMPT 1
//let monthDay d y =
// failwith "Not implemented"
//   match (y < 1582) with
//   |true -> failwith "too soon bro"
//   |false -> match (isLeap y) with
//             | true -> match (d < 1 || d > 366) with
//                       | true -> failwith "No day"
//                       | false -> match d > 31 && d < 61 with
//                                  |true -> "February"
//                                  |false -> 
//                                    let m,_ = month (max (d/30) 1) 
//                                    m
//             | false -> match (d < 1 || d > 365) with
//                        | true -> failwith "No day"
//                        | false -> match d > 31 && d < 60 with
//                                   |true -> "February"
//                                   |false -> 
//                                     let i = fun (m,_ = month (max (d/30) 1) 
//                                     m 
// ATTEMPT 2
//let monthDay day year =
//  match y < 1582 with
//  | true -> "Too Soon"
//  | false -> match (isLeap y) with
//             | true ->  match (day < 1 || day > 366) with
//                        | true -> failwith "Not a day"
//                        | false -> let rec MonthCalc mn dn =
//                                    let leap = [31;28;31;30;31;30;31;31;30;31;30;31]
//                                    match dn > 0 with
//                                    |true  -> MonthCalc (mn+1) dn-leap[mn] 
//                                    |false -> let m,_ = month(mn-1)
//                                              m
//                                   MonthCalc 1 day
//             |false -> match (day < 1 || day > 365) with
//                       | true -> failwith "Not a day"
//                       | false -> let rec MonthCalc mn dn =
//                                    let reg = [31,28,31,30,31,30,31,31,30,31,30,31]
//                                    match dn > 0 with
//                                    |true  -> MonthCalc (mn+1) dn-reg[mn] 
//                                    |false -> let m,_ = month(mn-1)
//                                              m
//                                  MonthCalc 1 day
//[31;28;31;30;31;30;31;31;30;31;30;31]
let sqrt n =
    let rec calculate guess i =
        match i with
        | 10 -> guess
        | _ ->
            let g = (guess + n/guess) / 2.0
            calculate g (i+1)
    match n <= 0.0 with
    | true -> failwith "Impossibru!"
    | _ ->
        calculate (n/2.0) 0


let coord x =
   let distance p2 =
      match x,p2 with |((x1,y1),(x2,y2)) -> sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
   //The code below this comment is not my own.
   let within lc w h =
      match x,lc with | ((ic1,ic2), (lc1,lc2)) -> (ic1 : float) >= (lc1 : float) && (ic1 : float) <= (lc1+w : float) && (ic2 : float) <= (lc2 : float) && (ic2 : float) >= (lc2-h : float)
   (distance, within)
