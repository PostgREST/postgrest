{ buildToolbox
, checkedShellScript
}:
let
  release =
    checkedShellScript
      {
        name = "postgrest-release";
        docs = "Patch postgrest.cabal, CHANGELOG.md, commit and push all in one go.";
        args = [ "ARG_OPTIONAL_BOOLEAN([major], [m], [Bump to new major version (only applies on main branch).])" ];
        workingDir = "/";
      }
      ''
        current_branch="$(git rev-parse --abbrev-ref HEAD)"
        trap "echo You need to be on the main branch or a release branch to proceed. Exiting ..." ERR
        [[ "$current_branch" =~ ^main$|^v[0-9]+$ ]]
        trap "" ERR

        trap "echo You have uncommitted changes in postgrest.cabal. Exiting ..." ERR
        git diff --exit-code HEAD postgrest.cabal > /dev/null
        trap "" ERR

        bump () {
          current_version="$(grep -oP '^version:\s*\K.*' postgrest.cabal)"
          # shellcheck disable=SC2034
          IFS=. read -r major minor patch <<< "$current_version"
          echo "Current version is $current_version"

          case "$1" in
            major)
              new_version="$((major+1)).0.0"
              new_docs_version="$((major+1)).0"
              ;;
            minor)
              new_version="$major.$((minor+1)).0"
              new_docs_version="$major.$((minor+1))"
              ;;
            patch)
              new_version="$major.$minor.$((patch+1))"
              new_docs_version="$major.$minor"
              ;;
            devel)
              new_version="$major.$((minor+1))"
              new_docs_version="devel"
              ;;
          esac

          echo "Updating postgrest.cabal ..."
          sed -i -E "s/^(version:\s+).*$/\1$new_version/" postgrest.cabal > /dev/null
          echo "Updating docs/conf.py ..."
          sed -i -E "s/^(version = ).*$/\1\"$new_docs_version\"/" docs/conf.py > /dev/null

          git add postgrest.cabal docs/conf.py > /dev/null
        }

        today_date_for_changelog="$(date '+%Y-%m-%d')"
        if [[ "$current_branch" == "main" ]]; then
          if [[ "$_arg_major" == "on" ]]; then
            bump major
          else
            bump minor
          fi
        else
          bump patch
        fi

        echo "Updating CHANGELOG.md ..."
        sed -i -E "s/Unreleased/&\n\n## [$new_version] - $today_date_for_changelog/" CHANGELOG.md > /dev/null
        git add CHANGELOG.md > /dev/null

        echo "Committing ..."
        git commit -m "bump version to $new_version" > /dev/null

        if [[ "$current_branch" == "main" ]]; then
          bump devel

          # The order of operations is important here:
          # - bump devel is run and $major is upated to the new version
          # - the branch is created with the new major, but the commit before the devel bump
          # - the devel bump is committed
          git branch -f "v$major"

          echo "Committing (devel bump)..."
          git commit -m "bump version to $new_version" > /dev/null
        fi

        trap "echo Remote not found. Please push manually ..." ERR
        remote="$(git remote -v | grep 'PostgREST/postgrest' | grep push | cut -f1)"
        trap "" ERR

        if [[ "$current_branch" == "main" ]]; then
          push1="git push $remote $current_branch"
          push2="git push $remote v$major --force"
        else
          push1="git push $remote $current_branch"
          push2=""
        fi

        echo "To push the version bump(s), the following will be run:"
        echo
        echo "$push1"
        echo "$push2"
        echo

        read -r -p 'Proceed? (y/N) ' REPLY
        case "$REPLY" in
          y|Y)
            $push1
            $push2
            ;;
          *)
            echo "Aborting ..."
            ;;
        esac
      '';

in
buildToolbox
{
  name = "postgrest-release";
  tools = { inherit release; };
}
