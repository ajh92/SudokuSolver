module TabuSearch

let neighborhoodSize = 30

let rec doSearch tabuList maxTabuSize sBest sCandidate stoppingCond neighborGenerator sNeighborhood fitnessEvaluator =
    if stoppingCond sBest then sBest
    else
        let rec buildNeighborhood item acc =
            if (List.length acc) >= neighborhoodSize then acc else buildNeighborhood ((neighborGenerator sCandidate) :: acc)
        let neighborhood = buildNeighborhood sCandidate []
        let evaluateCandidate best candidate =
            if ((not (List.contains candidate tabuList)) && ((fitnessEvaluator candidate) > (fitnessEvaluator best))) then candidate
            else best
        let rec checkNeighbors bestEncountered neighborhood =
            match neighborhood with
                | [] -> bestEncountered
                | h :: tail -> checkNeighbors (evaluateCandidate bestEncountered h) tail
        let bestCandidate = 
            checkNeighbors (List.head neighborhood) (List.tail neighborhood)
        let newBest =
            if (fitnessEvaluator bestCandidate) > (fitnessEvaluator sBest) then bestCandidate else sBest
        let tabus =
            if((List.length tabuList) = maxTabuSize) then List.take maxTabuSize (bestCandidate :: tabuList)
            else bestCandidate :: tabuList
        printfn "Best Score: %i, TabuSize: %i" (fitnessEvaluator newBest) (List.length tabuList);
        doSearch tabus maxTabuSize newBest bestCandidate stoppingCond neighborGenerator neighborhood fitnessEvaluator
