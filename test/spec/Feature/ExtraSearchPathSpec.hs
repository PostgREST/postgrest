module Feature.ExtraSearchPathSpec where

import Network.HTTP.Types
import Network.Wai         (Application)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "extra search path" $ do

  it "finds the ltree <@ operator on the public schema" $
    request methodGet "/ltree_sample?path=cd.Top.Science.Astronomy" [] ""
      `shouldRespondWith` [json|[
        {"path":"Top.Science.Astronomy"},
        {"path":"Top.Science.Astronomy.Astrophysics"},
        {"path":"Top.Science.Astronomy.Cosmology"}]|]
      { matchHeaders = [matchContentTypeJson] }

  it "finds the ltree nlevel function on the public schema, used through a computed column" $
    request methodGet "/ltree_sample?select=number_of_labels&path=eq.Top.Science" [] ""
      `shouldRespondWith` [json|[{"number_of_labels":2}]|]
      { matchHeaders = [matchContentTypeJson] }

  it "finds the isn = operator on the extensions schema" $
    request methodGet "/isn_sample?id=eq.978-0-393-04002-9&select=name" [] ""
      `shouldRespondWith` [json|[{"name":"Mathematics: From the Birth of Numbers"}]|]
      { matchHeaders = [matchContentTypeJson] }

  it "finds the isn is_valid function on the extensions schema" $
    request methodGet "/rpc/is_valid_isbn?input=978-0-393-04002-9" [] ""
      `shouldRespondWith` [json|true|]
      { matchHeaders = [matchContentTypeJson] }

  it "can detect fk relations through multiple views recursively when middle views are in extra search path" $
    get "/consumers_extra_view?select=*,orders_view(*)" `shouldRespondWith` 200
