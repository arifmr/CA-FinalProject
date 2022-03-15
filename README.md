# Hospital Patient List App

This app is a Haskell project for data documentation.

## Setup

```bash
cabal build

cabal run
```

## Usage

```text

Note: 
    - To Login (if you don't want to register) you may use the account below:
        username: admin
        password: admin
    - All logs are stored on log.txt file.
    - All account data are stored on account.txt file.
    - All patient data are stored on patient.txt file.
    - The username.txt file is used to check the username that has been used.
    - The currentLogin.txt file is used to store the username of the currently logged in user.

- First Menu
    1. Register If you don't have an account (enter 'r').
    2. Login to go to the second menu (enter 'l').
    3. Exit to close the app (enter 'x').

- Second Menu (After Login)
    1. Add Patient Data to a Specific File (enter 'a').
    2. Read File Data from a Specific File (enter 'r').
    3. Update Patient Data from a Specific File (enter 'u').
    4. Delete Patient Data from a Specific File (enter 'd').
    5. Logout to go back to the first menu (enter 'x').
```