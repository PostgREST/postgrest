# Python script to validate commit message
import sys
import re

valid_prefixes = {
    "feat",
    "fix",
    "test",
    "docs",
    "refactor",
    "chore",
    "nix",
    "ci",
    "perf",
    "revert",
    "break",
    "build",
    "deprecate",
    "drop",
    "correct",
    "changelog",
}


def is_valid(msg):

    # this pattern allows:
    #   - fix: description
    #   - chore(deps): description
    regex = r"^([a-z]+)(\([a-z]+\))?: .+"

    match = re.match(regex, msg)

    if not match:
        return False

    commit_prefix = match.group(1)

    if commit_prefix not in valid_prefixes:
        return False

    return True


def main():

    commit_msg = sys.stdin.read()

    # strip leading/trailing whitespace and newlines
    commit_msg = commit_msg.strip()

    print("Validating commit message...")
    print("")

    exit_code = 0

    if is_valid(commit_msg):
        print(f'"{commit_msg}" \033[32m✓\033[0m')
        print("Commit message is valid.")
        exit_code = 0
    else:
        print(f'"{commit_msg}" \033[31m✗\033[0m')
        print("Commit message is invalid.")
        print("")
        print("Please make sure your commit msg follows the following style:")
        print("\033[33m<prefix>: <description>\033[0m")
        print("or")
        print("\033[33m<prefix>(<scope>): <description>\033[0m")
        print("")
        print(
            "Allowed prefixes are: \033[33m"
            + ", ".join(list(valid_prefixes))
            + "\033[0m"
        )
        print("Examples:")
        print("  feat: add new feature")
        print("  chore(deps): update dependency")
        exit_code = 1

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
