{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec        (Spec, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import School (add, empty, grade, sorted, Grade(..))

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = do

          let fromList = foldr (uncurry add) empty
          let fromGrade g = fromList . zip (repeat g)

          it "add student" $
            sorted (add (Grade 2) "Aimee" empty) `shouldBe` [(Grade 2, ["Aimee"])]

          it "add more students in same class" $
            sorted (fromGrade (Grade 2) ["James", "Blair", "Paul"])
            `shouldBe` [(Grade 2, ["Blair", "James", "Paul"])]

          it "add students to different grades" $
            sorted (fromList [(Grade 3, "Chelsea"), (Grade 7, "Logan")])
            `shouldBe` [(Grade 3, ["Chelsea"]), (Grade 7, ["Logan"])]

          it "empty list if no students" $
            sorted empty `shouldBe` []

          it "get students in a grade" $
            grade (Grade 5) (fromList [(Grade 5, "Franklin"), (Grade 5, "Bradley"), (Grade 1, "Jeff")])
            `shouldBe` ["Bradley", "Franklin"]

          it "get students in a non-existent grade" $
            grade (Grade 1) empty `shouldBe` []

          it "sorted school" $
            sorted (fromList [ (Grade 4, "Jennifer"   )
                             , (Grade 6, "Kareem"     )
                             , (Grade 4, "Christopher")
                             , (Grade 3, "Kyle"       ) ] )
            `shouldBe` [ (Grade 3, ["Kyle"                   ] )
                       , (Grade 4, ["Christopher", "Jennifer"] )
                       , (Grade 6, ["Kareem"                 ] ) ]
