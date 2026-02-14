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
- `src/SendRequest.cob` – copybook for SendConnectionRequest procedure
- `src/ViewRequests.cob` – copybook for ViewPendingRequests procedure
- `src/AcceptRequest.cob` – copybook for AcceptConnectionRequest procedure
- `test/Epic1-Test-Input/InCollege-Input.txt` – program input
- `test/Epic1-Test-Output/InCollege-Output.txt` – program output
- `build_and_run.sh` – script to build and run the program
- `Dockerfile` – for containerized development
- `users.dat` – user data file (created at runtime)
- `profiles.dat` – profile data file (created at runtime)
- `connections.dat` – connection request data file (created at runtime, Epic 4)
- `docs/`, `test/`, `input/`, `output/` – supporting files

## Build and Run

**Compile** the COBOL source from the workspace root:

```bash
cobc -x -free -I./src src/InCollege.cob -o InCollege
```

**Before running**, ensure data files exist (they persist between runs):

```bash
touch users.dat profiles.dat connections.dat
```

**Run** the program:

```bash
./InCollege
```

The program reads input from `input/InCollege-Input.txt` and writes output to `output/InCollege-Output.txt`.

## Behavior Notes

- The program reads menu choices, usernames, and passwords from `input/InCollege-Input.txt` line by line
- All terminal output is displayed on stdout and by design
- The Job Search/Internships option displays "This feature is under construction."
- The create profile functionality works, and profiles are saved between runs
- The Learn-a-Skill options are present, but selecting any skill results in an "under construction" message
- **Epic 4 (Connection Requests)**: Full support for sending, viewing, and accepting connection requests persisted in `connections.dat`


