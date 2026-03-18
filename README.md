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
- `src/ViewNetwork.cob` – copybook for ViewNetwork procedure
- `src/RejectRequest.cob` – copybook for RejectConnectionRequest procedure
- `src/BrowseJobs.cob` – copybook for BrowseJobs procedure
- `src/PostJobs.cob` – copybook for PostJobs procedure
- `src/ApplyJob.cob` – copybook for viewing job details and applying
- `src/ViewApplications.cob` – copybook for viewing application summaries
- `input/InCollege-Input.txt` – program input
- `output/InCollege-Output.txt` – program output
- `build_and_run.sh` – script to build and run the program
- `Dockerfile` – for containerized development
- `users.dat` – user data file (created at runtime)
- `profiles.dat` – profile data file (created at runtime)
- `connections.dat` – connection request data file (created at runtime, Epic 4)
- `jobs.dat` – job postings data file (created at runtime)
- `applications.dat` – job applications data file (created at runtime)
- `test/` - contains test cases and outputs starting from Epic 3
- `docs/` – supporting files

## Build and Run

**Compile** the COBOL source from the workspace root:

```bash
cobc -x -free -I./src src/InCollege.cob -o InCollege
```

**Run** the program:

```bash
./InCollege
```

The program reads input from `input/InCollege-Input.txt` and writes output to `output/InCollege-Output.txt`.

## Behavior Notes

- The program reads menu choices, usernames, and passwords from `input/InCollege-Input.txt` line by line
- All terminal output is displayed on stdout and by design
- Job Search/Internships supports posting jobs, browsing listings, and viewing full job details
- Users can apply to jobs and receive a submission confirmation message
- Users can view a "My Applications" report with their total application count
- The create profile functionality works, and profiles are saved between runs
- The Learn-a-Skill options are present, but selecting any skill results in an "under construction" message
- **Epic 4 (Connection Requests)**: Full support for sending, viewing, and accepting connection requests persisted in `connections.dat`
- **Epic 5 (Network Display)**: Displays the user's network of connections, showing accepted connections and their profiles
- **Epic 6/7 (Job Board)**: Users can post jobs, browse postings, apply to jobs, and view applications persisted in data files


