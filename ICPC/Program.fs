module ICPC
open System

//For COMMASPRINKLER Error Cases 
let getSplitList (input : string) (char : char) = 
    let input = input.Split(char)
    let list = Seq.toList input
    list

let firstWord (input : string) =
    let list = getSplitList input ' '
    match list.Head,list.Head.Length > 1 with
    | " ", false -> None
    | ",", false -> None
    | ".", false -> None
    | "", false -> None
    | a, false -> None
    | a -> Some a

let endPeriod (input : string) =
    let list = getSplitList input ' '
    let lastWord = (list.Tail).ToString()
    match lastWord.EndsWith('.') with
    | false -> None
    | _ ->  match lastWord.Chars(lastWord.Length - 1) with
            | 'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z' -> None
            | _ -> Some lastWord

//For COMMASPRINKLER input cases
//to convert list into string with words separated by space
let foldingString list=List.fold (fun acc x -> match acc="" with 
                                               |true->x
                                               |false->acc+" "+x) "" list

let commaSprinkler (input : string) =
//RUN these for the error cases
        (*let phraseWithoutEndFullstop=input.TrimEnd('.')
        let sentences= Seq.toList (phraseWithoutEndFullstop.Split(". "))
        let separateByComma=Seq.toList (input.Split(','))
        match (firstWord input)=None with
        |true->None
        |false-> match  (endPeriod input)=None with
                 |true->None
                 |false->                 
                        let errorfinder=
                            let rec findFullStops list=
                                match list with
                                |[]-> Some (input)
                                |a::tail-> match a.ToString().EndsWith('.'), a.ToString().StartsWith("  "), a.ToString().StartsWith(" ,"),a.ToString().EndsWith(' ') with
                                           |true,_,_,_->None
                                           |_,true,_,_->None
                                           |_,_,true,_->None
                                           |_,_,_,true->None
                                           |_->findFullStops tail
                            findFullStops sentences
                        let errorfinderusingComma=
                            let rec findFullStops list=
                                match list with
                                |[]-> Some (input)
                                |a::tail-> match a.ToString().StartsWith(' ')<>true with
                                           |false->None
                                           |_->
                                               match a.ToString().EndsWith(',')<>true with
                                               |true->None
                                               |false->findFullStops tail
                            findFullStops separateByComma
                        match input.EndsWith('.')  with
                        |false->None
                        |true->
                            match errorfinder=None with
                            |true->None
                            |false->
                                match errorfinderusingComma=None with
                                |true->None
                                |false-> *)
                           
                                            let phraseWithoutEndFullstop=input.TrimEnd('.')
                                            let sentences= Seq.toList (phraseWithoutEndFullstop.Split(". "))
                                            let separateByComma=Seq.toList (input.Split(','))

                                            let rec CommaAddition phrase=
                                                let originallist= Seq.toList (phrase.ToString().Split(' ')) //split phrase into words list
                                                let Commalist= List.filter (fun x-> x.ToString().EndsWith(',')) originallist //finding word ending with a comma
                                                let removedCommaList= List.map (fun x->x.ToString().TrimEnd(',')) Commalist //words with comma in front    
        
                                                //this is to find all words which are suppose to have commas at the end of it
                                                let checker list= 
                                                    let rec addComma list acc=
                                                            match list with
                                                            |[]-> acc
                                                            |a::tail->
                                                                let acc=List.filter (fun (x:string)-> x=a) originallist                   
                                                                addComma tail acc
                                                    addComma list []       
        
                                                //if could not find any words, then process of putting commas is done.   
                                                match (checker removedCommaList) with
                                                |[]-> phrase|>Some
                                                |_->
                                                    //Getting commas added to words which are suppose to be ending with a comma!!
                                                    let addedCommafrontList list= 
                                                        let rec addComma list acc=
                                                            match list with
                                                            |[]-> foldingString acc
                                                            |a::tail->
                                                                let acc=List.map (fun (x:string)-> 
                                                                            match x=a with
                                                                            |true->x.ToString()+","
                                                                            |false->x) originallist 
                                                                addComma tail acc
                                                        addComma list []          
            
                                                    //generate list to be used to place commas in front of these words
                                                    let frontCommaString=addedCommafrontList removedCommaList
                                                    let sentences=Seq.toList (frontCommaString.Split(". "))//sentences in the phrase
                                                    let commaseparated= Seq.toList (frontCommaString.Split(", "))//to make list with words after comma as heads of each string
                                                    let h::t=commaseparated 
                                                    let toiter=t // start checking the heads of each string from the second element of the list
                                                    let commabacklist=
                                                        let rec findWords list acc=
                                                            match list with
                                                            |[]->acc
                                                            |a::tail->
                                                                let word=(List.head (Seq.toList (a.ToString().Split(' ')))).ToString().TrimEnd('.')
                                                                findWords tail (word::acc)
                                                        findWords toiter []
            
                                                    //generates a string with all words in the commaback list preceeded by a comma
                                                    let finallist=
                                                        let rec addCommas a list accO=
                                                            match list with
                                                            |[]->
                                                                let accO=(accO|>List.rev) 
                                                                let answer=foldingString accO
                                                                //Cleaning off the output by getting rid of duplicates of commas and spaces
                                                                let newstring1=answer.Replace(" ,",", ")
                                                                let newstring2=newstring1.Replace(",,",", ")
                                                                let newstring3=newstring2.Replace(" . ",". ")
                                                                let newstring4=newstring3.Replace(". .",".")
                                                                let finalanswer= newstring4.Replace("  "," ");
                                                                finalanswer
                                                            |a::tail->
                                                                // start checking the heads of each string from the second element of the list
                                                                let wordsOfSentence= Seq.toList (a.ToString().Split(' '))
                                                                let h::t=wordsOfSentence
                                                                let toiter=t
                                                                //to check if the words in commaback list are not preceeded by commas. If not then place comma.
                                                                let rec loopThroughCommaList a list acc=
                                                                    let wordsOfSentence= Seq.toList (a.ToString().Split(' '))
                                                                    let h::t=wordsOfSentence
                                                                    let toiter=t
                                                                    let defaultSentenceStart="."
                                                                    match list with
                                                                    |[]-> 
                                                                        let cha=(defaultSentenceStart::(acc|>List.rev))
                                                                        let currentlist =(h::(cha|>List.rev))//changes made to list of words in the sentence being iterated
                                                                        let newlist= //this is to contain all the modified sentence's words
                                                                                let rec addToAcc list acc=
                                                                                    match list with
                                                                                    |[]->acc
                                                                                    |a::tail-> addToAcc tail (a::acc)
                                                                                addToAcc currentlist accO
                                                                        addCommas a tail newlist
                                                                    |g::leftover->
                                                                            let acc=List.map (fun (x:string)-> 
                                                                                        match x.TrimEnd('.')=g with
                                                                                        |true->","+x.ToString()
                                                                                        |false->x) toiter
                                                                            let cha=((acc|>List.rev))
                                                                            let currentlist =(h::(cha|>List.rev))
                                                                            let a=foldingString currentlist                       
                                                                            loopThroughCommaList a leftover acc
                                                                loopThroughCommaList a commabacklist []
                                                        addCommas "" sentences []
                                                    CommaAddition finallist
                                            CommaAddition input


//For RIVER Error Cases
let convertStringListToString (xs:string list) =
        let sb = System.Text.StringBuilder(xs.Length)
        xs |> List.iter (sb.Append >> ignore)
        sb.ToString()

let convertIntoListOfChars (x:string)= //Convert our string to a char list
            let listOfChar=  Seq.toList(x)
            listOfChar

let stringToListToGetTheWordsLength (s:string)= //convert our string to string list
            let listOfStrings= s.Split(' ')
            Seq.toList listOfStrings

        //now I need to call this method in checkIfTextIsValid on the a,b branch
let checkIfTextIsValid (s:string)= //this will turn 1 character to be a string
            match (stringToListToGetTheWordsLength s).Length,(stringToListToGetTheWordsLength s),(Seq.head (stringToListToGetTheWordsLength s)).Length>80 with
            |1,_,_->None
            |_,_,true-> None
            //|_,h::t,false-> Some s
            |_,head::tail,_-> 
                        match head,tail with
                        |"",_-> None//check this case with F# interative,what will be the result of ""? 
                        |" ",_-> None
                        |a,_->None
  //let goThroughTheList,recusive call for thr strings in a list,everything is cool
            |_-> Some s
                        

let longestWord input =
    let list = getSplitList input ' '
    let rec find (word : string) (index : int) (length : int) =
        match length < list.Length with
        | true ->  match word.Length = length, word.Length < length, word.Length > length, list.Item(index) = list.Item(list.Length-1) with
                   | true,false,false,false -> find (list.Item(index)) (index + 1) (list.Item(index).Length)
                   | false,true,false,false -> find (list.Item(index)) (index + 1) (word.Length)
                   | false, false,true,false -> find (list.Item(index)) (index + 1) (list.Item(index).Length)
                   |  _ -> word.Length
        | _ -> word.Length               
    find list.Head 0 list.Head.Length

let makeLine input lineWidth =
    let list = getSplitList input ' '
       in let rec build (string : string) index count acc =
            match count < input.Length with
            | true ->  match string.Length < lineWidth with
                       | true -> build (input.Substring(0, count)) (index) (count+1) (acc+input.Substring(index, count)) 
                       | false -> acc + "5"
            | _ -> acc
          let acc = build list.Head 0 0 ""
    getSplitList acc '5'

let rivers input =
    checkIfTextIsValid input //To check the errors

[<EntryPoint>]
let main argv =
    //let word = longestWord "I am Thulani yeah right"
    let sentence = makeLine "I am Thulani yeah right" 7
    printfn "Longest word is: %A" sentence
    0 // return an integer exit code
