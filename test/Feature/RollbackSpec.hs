module Feature.RollbackSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

-- two helpers functions to make sure that each test can setup and cleanup properly

-- creates Item to work with for PATCH and DELETE
postItem =
  request methodPost "/items"
      [("Prefer", "tx=commit"), ("Prefer", "resolution=ignore-duplicates")]
      [json|{"id":0}|]
    `shouldRespondWith`
      ""
      { matchStatus  = 201 }

-- removes Items left over from POST, PUT, and PATCH
deleteItems =
  request methodDelete "/items?id=lte.0"
      [("Prefer", "tx=commit")]
      ""
    `shouldRespondWith`
      ""
      { matchStatus  = 204 }

preferDefault  = [("Prefer", "return=representation")]
preferCommit   = [("Prefer", "return=representation"), ("Prefer", "tx=commit")]
preferRollback = [("Prefer", "return=representation"), ("Prefer", "tx=rollback")]

withoutPreferenceApplied      = []
withPreferenceCommitApplied   = [ "Preference-Applied" <:> "tx=commit" ]
withPreferenceRollbackApplied = [ "Preference-Applied" <:> "tx=rollback" ]

shouldRespondToReads reqHeaders respHeaders = do
  it "responds to GET" $ do
    request methodGet "/items?id=eq.1"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

  it "responds to HEAD" $ do
    request methodHead "/items?id=eq.1"
        reqHeaders
        ""
      `shouldRespondWith`
        ""
        { matchHeaders = respHeaders }

  it "responds to GET on RPC" $ do
    request methodGet "/rpc/search?id=1"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

  it "responds to POST on RPC" $ do
    request methodPost "/rpc/search"
        reqHeaders
        [json|{"id":1}|]
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }

shouldPersistMutations reqHeaders respHeaders = do
  it "does persist post" $ do
    request methodPost "/items"
        reqHeaders
        [json|{"id":0}|]
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchStatus  = 201
        , matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[{"id":0}]|]
    deleteItems

  it "does persist put" $ do
    request methodPut "/items?id=eq.0"
        reqHeaders
        [json|{"id":0}|]
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[{"id":0}]|]
    deleteItems

  it "does persist patch" $ do
    postItem
    request methodPatch "/items?id=eq.0"
        reqHeaders
        [json|{"id":-1}|]
      `shouldRespondWith`
        [json|[{"id":-1}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[]|]
    get "/items?id=eq.-1"
      `shouldRespondWith`
        [json|[{"id":-1}]|]
    deleteItems

  it "does persist delete" $ do
    postItem
    request methodDelete "/items?id=eq.0"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[]|]

shouldNotPersistMutations reqHeaders respHeaders = do
  it "does not persist post" $ do
    request methodPost "/items"
        reqHeaders
        [json|{"id":0}|]
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchStatus  = 201
        , matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[]|]

  it "does not persist put" $ do
    request methodPut "/items?id=eq.0"
        reqHeaders
        [json|{"id":0}|]
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[]|]

  it "does not persist patch" $ do
    request methodPatch "/items?id=eq.1"
        reqHeaders
        [json|{"id":0}|]
      `shouldRespondWith`
        [json|[{"id":0}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.0"
      `shouldRespondWith`
        [json|[]|]
    get "items?id=eq.1"
      `shouldRespondWith`
        [json|[{"id":1}]|]

  it "does not persist delete" $ do
    request methodDelete "/items?id=eq.1"
        reqHeaders
        ""
      `shouldRespondWith`
        [json|[{"id":1}]|]
        { matchHeaders = respHeaders }
    get "/items?id=eq.1"
      `shouldRespondWith`
        [json|[{"id":1}]|]

allowed :: SpecWith ((), Application)
allowed = describe "tx-allow-override = true" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldNotPersistMutations` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withPreferenceCommitApplied
    preferCommit `shouldPersistMutations` withPreferenceCommitApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withPreferenceRollbackApplied
    preferRollback `shouldNotPersistMutations` withPreferenceRollbackApplied

disallowed :: SpecWith ((), Application)
disallowed = describe "tx-rollback-all = false, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldPersistMutations` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withoutPreferenceApplied
    preferCommit `shouldPersistMutations` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withoutPreferenceApplied
    preferRollback `shouldPersistMutations` withoutPreferenceApplied


forced :: SpecWith ((), Application)
forced = describe "tx-rollback-all = true, tx-allow-override = false" $ do
  describe "without Prefer tx" $ do
    preferDefault `shouldRespondToReads` withoutPreferenceApplied
    preferDefault `shouldNotPersistMutations` withoutPreferenceApplied

  describe "Prefer tx=commit" $ do
    preferCommit `shouldRespondToReads` withoutPreferenceApplied
    preferCommit `shouldNotPersistMutations` withoutPreferenceApplied

  describe "Prefer tx=rollback" $ do
    preferRollback `shouldRespondToReads` withoutPreferenceApplied
    preferRollback `shouldNotPersistMutations` withoutPreferenceApplied

