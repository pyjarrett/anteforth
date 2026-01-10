"""
Run this from the top level anteforth/ directory.

Example:
python3 scripts/golden_file_tests.py
"""

import os
import subprocess


RED = "\033[31m"
GREEN = "\033[32m"
CYAN = "\033[36m"
RESET = "\033[0m"


def print_header():
    print()
    print("-" * 80)
    print()


def run_expected_file_test(actual, expected):
    print_header()
    print(f"Running anteforth on {actual}")

    process = subprocess.run(
        ["alr", "run", "--skip-build", f"--args={actual}"],
        capture_output=True,
        text=True,
    )
    stdout = process.stdout
    with open(expected, "r") as f:
        expected_output = f.read()

    if stdout.split() == expected_output.split():
        print(f"Test passed for {actual}")
    else:
        print(f"Test failed for {actual}")
        import difflib

        diff = difflib.unified_diff(
            expected_output.splitlines(keepends=True),
            stdout.splitlines(keepends=True),
            fromfile=expected,
            tofile=actual,
        )

        for line in diff:
            if line.startswith("@@"):
                print(f"{CYAN}{line}{RESET}", end="")
            elif line.startswith("+") and not line.startswith("+++"):
                print(f"{GREEN}{line}{RESET}", end="")
            elif line.startswith("-") and not line.startswith("---"):
                print(f"{RED}{line}{RESET}", end="")
            else:
                print(line, end="")


def ensure_runs_ok(file):
    print_header()
    print(f"Running anteforth on {file}")
    subprocess.run(
        ["alr", "run", "--skip-build", f"--args={file}"],
        check=True,
    )
    print(f"Test passed for {file}")


def main():
    subprocess.check_call(["alr", "build"])
    print_header()
    for root, dirs, files in os.walk("examples"):
        for file in files:
            if file.endswith(".fth"):
                full = os.path.join(root, file)
                expected = f"{full}.expected"
                if os.path.exists(expected):
                    print(f"Found an expected file {expected}")
                    run_expected_file_test(full, expected)
                else:
                    ensure_runs_ok(full)


if __name__ == "__main__":
    main()
