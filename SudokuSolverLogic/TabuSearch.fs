module TabuSearch

let rec doSearch tabuList maxTabuSize sBest sCandidate stoppingCond neighborGenerator fitnessEvaluator =
    if stoppingCond sBest then sBest
    else
        let sNeighborhood = neighborGenerator sCandidate
        let evaluateCandidate best candidate =
            if ((not (List.contains candidate tabuList)) && ((fitnessEvaluator candidate) > (fitnessEvaluator best))) then candidate
            else best
        let rec checkNeighbors bestCanditate neighborhood =
            match neighborhood with
                | [] -> bestCanditate
                | h :: tail -> checkNeighbors (evaluateCandidate bestCanditate h) tail
        let bestCanditate = checkNeighbors (List.head sNeighborhood) (List.tail sNeighborhood)
        let newBest =
            if (fitnessEvaluator bestCanditate) > (fitnessEvaluator sBest) then bestCanditate
            else sBest
        let tabus =
            if((List.length tabuList) = maxTabuSize) then List.take maxTabuSize (bestCanditate :: tabuList)
            else bestCanditate :: tabuList
        in
        printfn "Best Score: %i, TabuSize: %i" (fitnessEvaluator newBest) (List.length tabuList);
        doSearch tabus maxTabuSize newBest bestCanditate stoppingCond neighborGenerator fitnessEvaluator