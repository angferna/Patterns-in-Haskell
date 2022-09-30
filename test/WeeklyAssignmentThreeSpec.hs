module WeeklyAssignmentThreeSpec where

    import Test.Hspec
    import WeeklyAssignmentThree

    main :: IO ()
    main = hspec $ do

        describe "firstAnswer" $ do
            it "produce firstAnswer of firstAnswer (\\x->if x == 5 then Just 5 else Nothing)  [1,2,3,4,5]" $
            (firstAnswer (\x->if x == 5 then Just 5 else Nothing)  [1,2,3,4,5]) `shouldBe` Just 5

            it "produce firstAnswer of firstAnswer (\\x->if x == 5 then Just 5 else Nothing)  []" $
            (firstAnswer (\x->if x == 5 then Just 5 else Nothing)  []) `shouldBe` Just 5

            it "produce firstAnswer of firstAnswer (\\x->if x == 6 then Just 6 else Nothing)  [1,2,3,4,5]" $
            (firstAnswer (\x->if x == 6 then Just 6 else Nothing)  [1,2,3,4,5]) `shouldBe` Nothing 

        describe "allAnswers" $ do
            it "produce allAnswers of allAnswers (\\x->if x == 5 then Just [5] else Nothing)" $
            (allAnswers (\x->if x == 5 then Just [5] else Nothing)  []) `shouldBe` Just []

            it "produce allAnswers of allAnswers (\\x->if x == 5 then Just [5] else Nothing)  [1,2,3,4,5]" $
            (allAnswers (\x->if x == 5 then Just [5] else Nothing)  [1,2,3,4,5]) `shouldBe` Nothing

            it "produce allAnswers of allAnswers (\\x->if x == 5 then Just 5 else Nothing)  [5]" $
            (allAnswers (\x->if x == 5 then Just [5] else Nothing)  [5]) `shouldBe` Just [5]

            it "produce allAnswers of allAnswers (\\x->if x == 5 then Just [5] else Nothing)  [5,5]" $
            (allAnswers (\x->if x == 5 then Just [5] else Nothing)  [5,5]) `shouldBe` Just [5,5] 

        describe "checkPat" $ do
            it "produce checkPat of checkPat((TuplePat ([VariablePat("")])))" $
            (checkPat((TuplePat ([VariablePat("")]))) `shouldBe` True

            it "produce checkPat of checkPat((TuplePat ([VariablePat("hi")]))))" $
            (checkPat((TuplePat ([VariablePat("hi")])))) `shouldBe` True

            it "produce checkPat of checkPat ((TuplePat ([VariablePat("Hello"),VariablePat("Hello"),VariablePat("H3llo")])))" $
            (checkPat ((TuplePat ([VariablePat("Hello"),VariablePat("Hello"),VariablePat("H3llo")])))) `shouldBe` False

            it "produce checkPat of checkPat ((TuplePat ([VariablePat("Hello1"),VariablePat("Hello2"),VariablePat("Hello3")])))" $
            (checkPat ((TuplePat ([VariablePat("Hello1"),VariablePat("Hello2"),VariablePat("Hello3")])))) `shouldBe` True

        describe "match" $ do
            it "produce match of match (Constant 1,TuplePat ([VariablePat("")]))" $
            (match (Constant 1,TuplePat ([VariablePat("")]))) `shouldBe` Nothing

            it "produce match of match ( Constant 1,ConstantPat 1))" $
            (match ( Constant 1,ConstantPat 1)) `shouldBe` Just []
            
            it "produce match of match ( Tuple [ (Constant 1), Unit,Constructor("asd", Constant 1), Constructor("dsa",Constant 2)], TuplePat ([ConstantPat 1,UnitPat,ConstructorPat ("asd", ConstantPat 1),ConstructorPat("dsa",ConstantPat 2) ]))" $
            (match ( Tuple [ (Constant 1), Unit,Constructor("asd", Constant 1), Constructor("dsa",Constant 2)], TuplePat ([ConstantPat 1,UnitPat,ConstructorPat ("asd", ConstantPat 1),ConstructorPat("dsa",ConstantPat 2) ]))) `shouldBe` Just []
        
            it "produce match of match( Tuple[ (Constant 1), Unit,Constructor("asd", Constant 1), Constructor("dsa",Constant 2)], TuplePat ([VariablePat("dsa1"), UnitPat , VariablePat("dsa2") ,VariablePat("dsa3")]))" $
            (match( Tuple[ (Constant 1), Unit,Constructor("asd", Constant 1), Constructor("dsa",Constant 2)], TuplePat ([VariablePat("dsa1"), UnitPat , VariablePat("dsa2") ,VariablePat("dsa3")]))) `shouldBe` Just [("dsa1",Constant 1),("dsa2",Constructor ("asd",Constant 1)),("dsa3",Constructor ("dsa",Constant 2))]

            it "produce match of match (Constant 1,VariablePat("dsa1"))" $
            (match(Constant 1,VariablePat("dsa1"))) `shouldBe` Just [("dsa1",Constant 1)]
            
            it "produce match of match(Tuple ([Constant 1,Constant 2]),VariablePat("dsa1"))" $
            (match(Tuple ([Constant 1,Constant 2]),VariablePat("dsa1"))) `shouldBe` Just [("dsa1",Tuple [Constant 1,Constant 2])]
        
            it "produce match of match(Constant 1,UnitPat)" $
            (match(Constant 1,UnitPat)) `shouldBe` Nothing

        describe "firstMatch" $ do
            it "produce firstMatch (Constructor("dsa",Constant 1)) ([UnitPat,ConstantPat 1]" $
            (firstMatch (Constructor("dsa",Constant 1)) ([UnitPat,ConstantPat 1])) `shouldBe` Nothing

            it "produce firstMatch of firstMatch (Constant 1) ([UnitPat,VariablePat("a")])" $
            (firstMatch (Constant 1) ([UnitPat,VariablePat("a")])) `shouldBe` Just [("a",Constant 1)]

            it "produce firstMatch of firstMatch (Constant 1) ([VariablePat("a"),VariablePat("a1")])" $
            (firstMatch (Constant 1) ([VariablePat("a"),VariablePat("a1")])) `shouldBe` Just [("a",Constant 1)]