module WeeklyAssignmentThree where
    -- use map, fold, and filter on lists and other data structures 

    --import Data.List

    data Pattern = WildcardPat | --matches anything and produces an empty list of bindings.
                    VariablePat (String) | --matches any value v and produces the single element list holding (s, v).
                    UnitPat | --matches only the Unit value and produces the empty list of bindings.
                    ConstantPat (Int) | --matches only against Constant 17 and produces the empty list of bindings (and similarly for other integers)
                    ConstructorPat (String, Pattern) | --matches Constructor(s2, v) if s1 and s2 are the same string and p matches v. The list of bindings produced is the list of bindings produced by the nested pattern match of p with v. We call the strings s1 and s2 the constructor names. 
                    TuplePat ([Pattern]) --matches a value of the form Tuple vs if ps and vs have the same length and for all i, the ith element of ps matches the ith element of vs. The list of bindings produced is all the lists from the nested pattern matches appended together. 
                        --Nothing - In all other cases return Nothing.
                    deriving (Eq, Show)

    data Value = Constant (Int) | 
                    Unit | 
                    Constructor (String, Value) | 
                    Tuple [Value] 
                    deriving (Eq, Show)
    
    --firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b. 
    --  The first argument should be applied to elements of the second argument (a list) in order until the first time it returns Just v for some v and then Just v is the result of the call to firstAnswer. 
    --  If the first argument returns Nothing for all the elements in the list, then firstAnswer should return Nothing. 
    --  The function that is passed can be thought of as a predicate (see its signature?). 
    --  It returns Nothing or Just v for particular types of a if it determines those are invalid or valid answers. 
    --  Note: The function should produce Nothing if there are not any matches. 
    firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
    firstAnswer f [] = Nothing
    firstAnswer f (x:xs) = 
            case f x of
                    Nothing -> firstAnswer f xs
                    Just v -> Just v

    --allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]. 
    --  The first argument should be applied to elements of the second argument. 
    --  If it returns Nothing for any element, then the result for allAnswers is Nothing. 
    --  Otherwise, calls to the first argument will have produced Just lst1, Just lst2, ..., Just lstn and the result of allAnswers is Just lst, where lst is [lst1, lst2, ..., lstn].
    allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
    allAnswers f [] = Just []
    allAnswers f (x:xs) = case f x of
                            Nothing -> Nothing
                            Just v -> fmap (++) (Just v) <*> allAnswers f xs

    --checkPat 
    --  consumes a Pattern
    --  returns true if and only if all the variables appearing in the pattern are distinct from each other (i.e., they are different strings). 
    --  The constructor names are not relevant. 
    --  Hint: My solution uses two helper functions. 
    --  The first takes a pattern and returns a list of all the strings it uses for variables. 
    --  Using foldl with a function that uses ++ is useful in one case. 
    --  The second takes a list of strings and decides if it has repeats. 
    --  elem might be useful in this case. 
    --  These helper functions are not required but they may simplify the solution.
    checkPat :: Pattern -> Bool
    checkPat p = 
        let 
            helper x = 
                case x of
                    VariablePat s -> [s]
                    ConstructorPat (a,b) -> a: helper b
                    TuplePat ps -> concatMap helper ps

            do_same_exists x = foldr (\y b -> y == x || b) False
            
            check_uniqueness lst =
                case lst of
                    [] -> True
                    (x:xs) -> if (do_same_exists x xs)
                                then False
                                else check_uniqueness xs
        in
            check_uniqueness ( helper p)

    --match 
    --  consumes a (Value, Pattern)
    --  returns a Maybe [(String, Value)] 
    --  (note the list is inside the Maybe type, it is not a list of Maybe types). 
    --  It should produce Nothing if the Pattern does not match and Just lst where lst is the list of bindings if it does. 
    --  Note that if the Value matches but the Pattern has no Patterns of the form VariablePat s, then the result is Just []. 
    --  Hints: The solution has one case expression with 7 branches (that's a pretty big hint...). 
    --  The branch in particular for tuples uses allAnswers and Data.List.zip. 
    --  Sample solution is around 13 lines of code. 
    --  Remember to look above for the rules for what patterns match what values, and what bindings they produce. 
    --  These are hints: we don't require allAnswers and Data.List.zip, but they make it easier.
    match :: (Value, Pattern) -> Maybe [(String, Value)]
    match (_,WildcardPat) =  Just []
    match (Constant v1, ConstantPat p1) = if v1==p1
            then Just []
        else Nothing
    match (Unit,UnitPat) = Just []
    match (Constructor (s ,v1),ConstructorPat (s1, p1) ) = if s == s1 
                                                                then match(v1,p1) 
                                                            else Nothing
    match (v, VariablePat s ) = Just [(s,v)]
    match (Tuple vs,TuplePat ps) = if length vs == length ps 
                                    then case allAnswers match (zip(vs)(ps))  of
                                            Just v2 ->Just v2
                                            _ -> Nothing
                                    else Nothing
    match (_,_) = Nothing

    --firstMatch
    --  consumes a Value and a list of Patterns
    --  returns a Maybe [(String, Value)], in particular, Nothing if no pattern in the list matches, or Just lst, where lst is the list of bindings for the first pattern in the list that matches. 
    --  Use firstAnswer.
    firstMatch :: Value -> [Pattern] -> Maybe [(String, Value)]
    firstMatch v [] = Nothing
    firstMatch v p = firstAnswer (\x -> match(v,x)) p
