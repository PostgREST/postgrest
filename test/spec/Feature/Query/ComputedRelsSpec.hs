module Feature.Query.ComputedRelsSpec where

import Network.Wai (Application)

import Network.HTTP.Types
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Protolude  hiding (get)
import SpecHelper

spec :: SpecWith ((), Application)
spec = describe "computed relationships" $ do
  it "can define a many-to-one relationship and embed" $
    get "/videogames?select=name,designers:computed_designers(name)"
    `shouldRespondWith`
      [json|[
        {"name":"Civilization I","designers":{"name":"Sid Meier"}},
        {"name":"Civilization II","designers":{"name":"Sid Meier"}},
        {"name":"Final Fantasy I","designers":{"name":"Hironobu Sakaguchi"}},
        {"name":"Final Fantasy II","designers":{"name":"Hironobu Sakaguchi"}}
      ]|] { matchHeaders = [matchContentTypeJson] }

  it "can define a one-to-many relationship and embed" $
    get "/designers?select=name,videogames:computed_videogames(name)"
    `shouldRespondWith`
      [json|[
        {"name":"Sid Meier","videogames":[{"name":"Civilization I"}, {"name":"Civilization II"}]},
        {"name":"Hironobu Sakaguchi","videogames":[{"name":"Final Fantasy I"}, {"name":"Final Fantasy II"}]}
      ]|] { matchHeaders = [matchContentTypeJson] }

  it "works with !inner and count=exact" $ do
    request methodGet "/designers?select=name,videogames:computed_videogames!inner(name)&videogames.name=eq.Civilization%20I"
      [("Prefer", "count=exact")] ""
      `shouldRespondWith`
        [json|[{"name":"Sid Meier","videogames":[{"name":"Civilization I"}]}]|]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-0/1"]
        }
    request methodGet "/videogames?select=name,designer:computed_designers!inner(name)&designer.name=like.*Hironobu*"
      [("Prefer", "count=exact")] ""
      `shouldRespondWith`
        [json|[
          {"name":"Final Fantasy I","designer":{"name":"Hironobu Sakaguchi"}},
          {"name":"Final Fantasy II","designer":{"name":"Hironobu Sakaguchi"}}
        ]|]
        { matchStatus  = 200
        , matchHeaders = ["Content-Range" <:> "0-1/2"]
        }

  it "works with rpc" $ do
    get "/rpc/getallvideogames?select=name,designer:computed_designers(name)"
      `shouldRespondWith`
      [json|[
        {"name":"Civilization I","designer":{"name":"Sid Meier"}},
        {"name":"Civilization II","designer":{"name":"Sid Meier"}},
        {"name":"Final Fantasy I","designer":{"name":"Hironobu Sakaguchi"}},
        {"name":"Final Fantasy II","designer":{"name":"Hironobu Sakaguchi"}}
      ]|] { matchHeaders = [matchContentTypeJson] }
    get "/rpc/getalldesigners?select=name,videogames:computed_videogames(name)"
      `shouldRespondWith`
      [json|[
        {"name":"Sid Meier","videogames":[{"name":"Civilization I"}, {"name":"Civilization II"}]},
        {"name":"Hironobu Sakaguchi","videogames":[{"name":"Final Fantasy I"}, {"name":"Final Fantasy II"}]}
      ]|] { matchHeaders = [matchContentTypeJson] }

  it "works with mutations" $ do
    request methodPost "/videogames?select=name,designer:computed_designers(name)"
      [("Prefer", "return=representation")]
      [json| {"id": 5, "name": "Chrono Trigger", "designer_id": 2} |]
      `shouldRespondWith`
        [json|[ {"name":"Chrono Trigger","designer":{"name":"Hironobu Sakaguchi"}} ]|]
        { matchStatus  = 201 }
    request methodPatch "/designers?select=name,videogames:computed_videogames(name)&id=eq.1"
      [("Prefer", "return=representation")]
      [json| {"name": "Sidney K. Meier"} |]
      `shouldRespondWith`
        [json|[ { "name": "Sidney K. Meier", "videogames": [{"name":"Civilization I"}, {"name":"Civilization II"}] } ]|]
        { matchStatus  = 200 }
    request methodDelete "/videogames?select=name,designer:computed_designers(name)&id=eq.3"
      [("Prefer", "return=representation")] ""
      `shouldRespondWith`
        [json|[ {"name":"Final Fantasy I","designer":{"name":"Hironobu Sakaguchi"}} ]|]
        { matchStatus  = 200 }

  it "works with self joins" $
    get "/web_content?select=name,child_web_content(name),parent_web_content(name)&id=in.(0,1)"
    `shouldRespondWith`
      [json|[
        {"name":"tardis","child_web_content":[{"name":"fezz"}, {"name":"foo"}, {"name":"bar"}],"parent_web_content":{"name":"wat"}},
        {"name":"fezz","child_web_content":[{"name":"wut"}],"parent_web_content":{"name":"tardis"}}
      ]|] { matchHeaders = [matchContentTypeJson] }

  it "can override detected relationships" $ do
    get "/videogames?select=*,designers!inner(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }
    get "/designers?select=*,videogames!inner(*)"
      `shouldRespondWith`
      [json|[]|] { matchHeaders = [matchContentTypeJson] }
