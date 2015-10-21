arr = eitherDecode "[{\"a\":10},{\"a\":20}]" :: Either String Value
ob = eitherDecode "{\"a\":10}"::Either String Value

rc :: Request
rc =  Request {
  -- | Request method such as GET.
     requestMethod = "POST"
  ,  pathInfo = ["menagerie"]
  ,  requestHeaders = [("Content-Type", "text/csv")] --      :: H.RequestHeaders
  }
bc :: BL.ByteString
bc = [str|integer->sub->sub2,double,varchar,boolean,date,money,enum
         |13,3.14159,testing!,false,1900-01-01,$3.99,foo
         |12,0.1,NULL,true,1929-10-01,12,bar
         |]

rj :: Request
rj =  Request {
 -- | Request method such as GET.
     requestMethod = "POST"
  ,  pathInfo = ["menagerie"]
  ,  requestHeaders = [("Content-Type", "application/json")] --      :: H.RequestHeaders
  }
bj :: BL.ByteString
bj = [str|{
         |  "integer->sub->>sub2": 13, "double": 3.14159, "varchar": "testing!"
         |  , "boolean": false, "date": "1900-01-01", "money": "$3.99"
         |  , "enum": "foo"
         |}
         |]
bj2 :: BL.ByteString
bj2 = [str|[
          |{
          |  "integer->sub->>sub2": 13, "double": 3.14159, "varchar": "testing!"
          |  , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          |  , "enum": "foo"
          |},
          |{
          |  "integer->sub->>sub2": 13, "double": 3.14159, "varchar": "testing!"
          |  , "boolean": false, "date": "1900-01-01", "money": "$3.99"
          |  , "enum": "foo"
          |}]
          |]
