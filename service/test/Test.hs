{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | API tests

module Main where


import           Control.Lens
import qualified Control.Monad.Catch     as Ex
import           Control.Monad.Logger
import           Data.Data
import           Data.Monoid
import qualified Data.Text               as Text
import           Prelude                 hiding (id)
import qualified Test.QuickCheck.Monadic as QC
import qualified Text.Microstache        as Mustache

import           Test.Hspec.Expectations
import           Test.Tasty.HUnit        hiding (assertFailure)
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Backend
import           Config                  (defaultPwResetTemplate)
import           PasswordReset
import qualified Persist.Schema          as DB
import           Types

import           Test.Common

newtype AssertionFailed = AssertionFailed String deriving (Typeable)

instance Show AssertionFailed where
  show (AssertionFailed e) = "Assertion Failed: " <>  e

instance Ex.Exception AssertionFailed

assertFailure :: Ex.MonadThrow m => String -> m a
assertFailure = Ex.throwM  . AssertionFailed

--------------------------------------------------------------------------------
-- Add User --------------------------------------------------------------------
--------------------------------------------------------------------------------

testUser :: AddUser
testUser = AddUser { addUserUuid = Nothing
                   , addUserEmail = Email "no@spam.please"
                   , addUserPassword = Password "pwd"
                   , addUserName = "Jon Doe"
                   , addUserPhone = Nothing
                   , addUserInstances = []
                   }

withUser :: AddUser -> (UserID -> (forall a. API a -> IO a) -> IO ()) -> IO ()
withUser usr f = withRunAPI $ \run -> do
  mbRes <- run $ createUser usr
  case mbRes of
    Nothing -> assertFailure "could not create user"
    Just uid -> f uid run

checkUser :: DB.User -> AddUser -> IO ()
checkUser dbUser addUser = do
  dbUser ^. email `shouldBe` addUser ^. email
  dbUser ^. name `shouldBe`  addUser ^. name
  dbUser ^. phone `shouldBe` addUser ^. phone

case_create_user :: IO ()
case_create_user = withUser testUser $ \_uid run -> do
  mbUsr <- run . getUserByEmail $ testUser ^. email
  case mbUsr of
    Nothing -> assertFailure "Did not get user"
    Just usr -> checkUser usr testUser

case_user_check_password :: IO ()
case_user_check_password = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> assertFailure "check password failed"
    Right _ -> return ()

case_user_check_password_wrong :: IO ()
case_user_check_password_wrong = withUser testUser $ \_uid run -> do
  res <- run $ checkUserPassword (testUser ^. email) (Password "bogus")
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Accepted bogus password"

--------------------------------------------------------------------------------
-- Password Changes ------------------------------------------------------------
--------------------------------------------------------------------------------

case_user_change_password :: IO ()
case_user_change_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword uid (Password "newPassword")
  res <- run $ checkUserPassword (testUser ^. email) (Password "newPassword")
  case res of
    Left _e -> assertFailure "check password fails"
    Right _ -> return ()

case_user_change_password_old_password :: IO ()
case_user_change_password_old_password = withUser testUser $ \uid run -> do
  _ <- run $ changeUserPassword uid (Password "newpassword")
  res <- run $ checkUserPassword (testUser ^. email) (testUser ^. password)
  case res of
    Left _e -> return ()
    Right _ -> assertFailure "Could still use olf password"

--------------------------------------------------------------------------------
-- Reset Password --------------------------------------------------------------
--------------------------------------------------------------------------------

case_reset_password :: IO ()
case_reset_password = withUser testUser $ \uid run -> do
  tok <- run $ createResetToken 60 uid
  _ <- run $ resetPassword tok "newPwd"
  _ <- run $ checkUserPassword (testUser ^. email) "newPwd"
  return ()

case_reset_password_wrong_token :: IO ()
case_reset_password_wrong_token =
  withRunAPI $ \run -> do
    run (resetPassword "BogusToken" "newPwd") `shouldThrow`
      (== ChangePasswordTokenError)
    return ()

case_reset_password_double_use :: IO ()
case_reset_password_double_use =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken 60 uid
    run $ resetPassword tok "newPwd"
    run (resetPassword tok "newPwd2") `shouldThrow`
      (== ChangePasswordTokenError)
    return ()

case_reset_password_expired :: IO ()
case_reset_password_expired =
  withUser testUser $ \uid run -> do
    tok <- run $ createResetToken (-60) uid
    run (resetPassword tok "newPwd") `shouldThrow`
      (== ChangePasswordTokenError)
    return ()

testEmailData :: EmailData
testEmailData =
  EmailData
  { emailDataLink = "http://localhost/reset/abc"
  , emailDataSiteName = "test.site.com"
  , emailDataExpirationTime = "24 hours"
  }

case_password_reset_render_email :: IO ()
case_password_reset_render_email = do
  renderedEmail <-
    runNoLoggingT $
    renderEmail
      testEmailConfig
      (testEmailConfig ^. pWResetTemplate)
      (Just "tok123abc")
  renderedEmail `shouldBe`
    "please click on http://localhost/reset?token=tok123abc"

case_password_reset_render_error_email :: IO ()
case_password_reset_render_error_email = do
  renderedEmail <-
    runNoLoggingT $
    renderEmail
      testEmailConfig
      (testEmailConfig ^. pWResetUnknownTemplate)
      Nothing
  renderedEmail `shouldBe`
    "Your email is unknown"

-- | Check that the render functions throws an exception when there are errors
-- during render
case_password_reset_render_email_errors :: IO ()
case_password_reset_render_email_errors = do
  let Right tmpl = Mustache.compileMustacheText "test" "{{bogus}}"
  runNoLoggingT (renderEmail testEmailConfig tmpl Nothing)
    `shouldThrow` (== EmailRenderError)

-- Check that we can render the default template (i.e. there are no missing
-- variables)
case_password_reset_default_template :: IO ()
case_password_reset_default_template = do
  _ <-
    runStderrLoggingT $ renderEmail testEmailConfig defaultPwResetTemplate Nothing
  return ()

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------

case_add_user_instance :: IO ()
case_add_user_instance = withUser testUser $ \uid run -> do
  iid <- run $ addInstance Nothing "instance1"
  run $ addUserInstance uid iid
  iids <- run $ getUserInstances uid
  iids ^.. each . id  `shouldBe` [iid]

case_remove_user_instance :: IO ()
case_remove_user_instance = withUser testUser $ \uid run -> do
  iid <- run $ addInstance Nothing "instance1"
  run $ addUserInstance uid iid
  _ <- run $ removeUserInstance uid iid

  iids <- run $ getUserInstances uid
  iids `shouldBe` []

--------------------------------------------------------------------------------
-- MkRandomString --------------------------------------------------------------
--------------------------------------------------------------------------------

prop_mkRandomString_length :: Int -> Property
prop_mkRandomString_length len = QC.monadicIO $ do
  let l = abs len
  str <- QC.run $ mkRandomString otpIDChars l
  QC.assert (Text.length str == l)

prop_mkRandomString_chars :: Int -> Int -> Property
prop_mkRandomString_chars len numChars = QC.monadicIO $ do
  let l = abs len
  let chars = take (max 1 numChars) otpIDChars
  str <- QC.run $ mkRandomString chars l
  QC.assert (Text.all (`elem` chars) str)

--------------------------------------------------------------------------------
-- Login -----------------------------------------------------------------------
--------------------------------------------------------------------------------

runLogin :: (forall a . API a -> IO a)
         -> AddUser
         -> IO ReturnLogin
runLogin run usr = do
  res <- run $ login Login{ loginUser = usr ^. email
                          , loginPassword = usr ^. password
                          , loginOtp = Nothing
                          }
  case res of
    Left _e -> assertFailure "Could not login"
    Right rl -> return rl

case_login :: IO ()
case_login = withUser testUser $ \_uid run -> do
  _ <- runLogin run testUser
  return ()

withUserToken :: AddUser
              -> (B64Token -> UserID -> (forall a. API a -> IO a) -> IO ())
              -> IO ()
withUserToken usr f = withUser usr $ \uid run -> do
  res <- runLogin run usr
  f (res ^. token) uid run


case_checkToken :: IO ()
case_checkToken = withUserToken testUser $ \tok uid run -> do
  res <- run $ checkToken tok
  res `shouldBe` Just uid

case_checkToken_bogus :: IO ()
case_checkToken_bogus = withUser testUser $ \_uid run -> do
  let tok = B64Token "bogus"
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_checkTokenInstance :: IO ()
case_checkTokenInstance = withUserToken testUser $ \tok uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  run $ addUserInstance uid iid
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Just uid

case_checkTokenInstance_not_member :: IO ()
case_checkTokenInstance_not_member = withUserToken testUser $ \tok _uid run -> do
  iid <- run $ addInstance Nothing "testInstance"
  res <- run $ checkTokenInstance "" tok iid
  res `shouldBe` Nothing


case_logout :: IO ()
case_logout = withUserToken testUser $ \tok _uid run -> do
  run $ logOut tok
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions :: IO ()
case_closeOtherSesions = withUserToken testUser $ \tok _uid run -> do
  tok2 <- view token <$> runLogin run testUser
  run $ closeOtherSessions tok2
  res <- run $ checkToken tok
  res `shouldBe` Nothing

case_closeOtherSesions_same_session :: IO ()
case_closeOtherSesions_same_session =
  withUserToken testUser $ \_tok uid run -> do
    tok2 <- view token <$> runLogin run testUser
    run $ closeOtherSessions tok2
    res <- run $ checkToken tok2
    res `shouldBe` Just uid


main :: IO ()
main = $defaultMainGenerator
