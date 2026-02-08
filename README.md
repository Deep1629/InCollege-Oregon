# InCollege - Oregon

## Quick Start (Docker)

1. **Build the Docker image:**
   ```sh
   docker build -t incollege-cobol .
   ```
2. **Run the container and open a shell:**
   ```sh
   docker run -it --rm -v "$PWD":/workspace incollege-cobol
   ```
3. **Inside the container, build and run the program:**
   ```sh
   ./build_and_run.sh
   ```

---

## Project Overview
COBOL-based login and account creation system.

## Files
- `src/InCollege.cob` – main COBOL source
- `input/InCollege-Input.txt` – program input which is read
- `output/InCollege-Output.txt` – program output log
- `test/Test-Input/TC*.txt` – program test input cases
- `test/Test-Output/TC*_Output.txt` – program output
- `build_and_run.sh` – script to build and run the program
- `Dockerfile` – for containerized development
- `users.dat` – user data file (created at runtime)
- `profiles.dat` – profile data file (created at runtime)
- `docs/`– supporting files

## Build and Run

Compile the COBOL source from the workspace root:

```bash
cobc -x -free src/InCollege.cob
```

Then run the program:

```bash
./InCollege
```

Behavior notes:
- The program reads menu choices, usernames, and passwords from `input/InCollege-Input.txt` using `READ InputFile`.
- All terminal output is appended to `output/InCollege-Output.txt`.
- The Job Search/Internships and "Find someone you know" options display "This feature is under construction.".
- The create profile functionality works, and profiles are saved between runs.
- The search feature works by searching for a username, and it will display the profile if found.
- The Learn-a-Skill options are present, but selecting any skill currently results in an "under construction" message.

