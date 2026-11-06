"Miscellaneous tests that don't fit nicely in other test files"

from postgrest import (
    run,
)


# This test should ideally be in our spec-tests suite, but "hspec-wai"
# raises unhandled exceptions errors before matching the response, so
# the test never passes.
def test_warp_internal_server_error_response(defaultenv):
    "Warp should return an HTTP json response on exceptions"

    with run(env=defaultenv) as postgrest:
        response = postgrest.session.post(
            "/items?key=caf%E9"
        )  # \xe9 is invalid UTF-8 code for 'é', the valid one is \xc3\xa8 would be correct

        assert response.status_code == 500
        assert (
            response.text
            == '{"code":"PGRSTX00","details":null,"hint":null,"message":"Cannot decode byte \'\\\\xe9\': Data.Text.Encoding: Invalid UTF-8 stream"}'
        )
