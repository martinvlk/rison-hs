{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson as AE ( Value(..) )
import qualified Data.Attoparsec.ByteString as A
import qualified Data.HashMap.Strict as H
import           Data.Rison
import qualified Data.Vector as V
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Rison Parser" $ do

    context "rison tokens" $ do
      it "null" $ do
        (decode "!n") `shouldBe` (Right Null)
      it "true" $ do
        (decode "!t") `shouldBe` (Right (Bool True))
      it "false" $ do
        (decode "!f") `shouldBe` (Right (Bool False))

    context "strings" $ do
      it "empty string single quotes" $ do
        (decode "''") `shouldBe` (Right $ String "")
      it "simple string single quoted" $ do
        (decode "'ahoj'") `shouldBe` (Right $ String "ahoj")
      it "simple string unquoted" $ do
        (decode "ahoj") `shouldBe` (Right $ String "ahoj")
      it "escapes" $ do
        (decode "'\\\\!\\!!\\!'") `shouldBe` (Right $ String "\\\\!!")
      it "unquoted with special chars" $ do
        (decode "_ah-oj/y") `shouldBe` (Right $ String "_ah-oj/y")

    context "object" $ do
      it "empty" $ do
        (decode "()") `shouldBe` (Right $ Object H.empty)
      it "simple" $ do
        (decode "(property:!n)") `shouldBe`
          (Right $ Object $ H.fromList [("property", Null)])
      it "simple with unquoted string" $ do
        (decode "(property:Off)") `shouldBe`
          (Right $ Object $ H.fromList [("property", String "Off")])
      it "simple with dollar in property name" $ do
        (decode "('$property':Off)") `shouldBe`
          (Right $ Object $ H.fromList [("$property", String "Off")])

    context "array" $ do
      it "empty" $ do
        (decode "!()") `shouldBe` (Right $ Array V.empty)
      it "booleans" $ do
        (decode "!(!t,!t,!f,!t,!f)") `shouldBe`
          (Right $ Array $ V.fromList [ Bool True
                                      , Bool True
                                      , Bool False
                                      , Bool True
                                      , Bool False])
    context "numbers" $ do
      it "zero" $ do
        (decode "0") `shouldBe` (Right $ Number 0)
      it "simple integer" $ do
        (decode "36589") `shouldBe` (Right $ Number 36589)
      it "simple decimal" $ do
        (decode "36589.65200004") `shouldBe` (Right $ Number 36589.65200004)
      it "scienticfic 1" $ do
        (decode "3.6589e3") `shouldBe` (Right $ Number 3658.9)
      it "scienticfic 2" $ do
        (decode "3.6589e-3") `shouldBe` (Right $ Number 0.0036589)

  describe "Rison Writer" $ do
    context "rison tokens" $ do
      it "null" $ do
        (encode Null) `shouldBe` "!n"
      it "true" $ do
        (encode (Bool True)) `shouldBe` "!t"
      it "false" $ do
        (encode (Bool False)) `shouldBe` "!f"

    context "strings" $ do
      it "simple string" $ do
        encode (String "ahoj") `shouldBe` "ahoj"
      it "escaped values" $ do
        encode (String "!\\") `shouldBe` "'!!!\\'"

    context "numbers" $ do
      it "zero" $ do
        encode (Number 0) `shouldBe` "0"
      it "simple integer" $ do
        encode (Number 36589) `shouldBe` "36589"
      it "simple decimal" $ do
        encode (Number 36589.65200004) `shouldBe` "36589.65200004"

    context "object" $ do
      it "empty" $ do
        encode (Object H.empty) `shouldBe` "()"
      it "simple" $ do
        encode (Object $ H.fromList [("property", Null)]) `shouldBe`
          "(property:!n)"
      it "simple with unquoted string" $ do
        encode (Object $ H.fromList [("property", String "Off")]) `shouldBe`
          "(property:Off)"
      it "sorted properties" $ do
        encode (Object $ H.fromList [ ("d", String "d")
                                    , ("b", String "b")
                                    , ("g", String "g")
                                    , ("a", String "a")]) `shouldBe`
          "(a:a,b:b,d:d,g:g)"

    context "array" $ do
      it "empty" $ do
        encode (Array V.empty) `shouldBe` "!()"
      it "booleans" $ do
        encode (Array $ V.fromList [ Bool True
                                   , Bool True
                                   , Bool False
                                   , Bool True
                                   , Bool False]) `shouldBe`
          "!(!t,!t,!f,!t,!f)"
