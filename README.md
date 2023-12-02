```fortran
PROGRAM ADVENT_OF_CODE_2023
    IMPLICIT NONE
    CHARACTER(LEN=80) :: NOTE
    LOGICAL :: COMMIT_TIMES_SEEM_WEIRD = .TRUE.
  
    PRINT *, 'My submissions for Advent of Code 2023.'
    PRINT *, 'All solutions will be written in Fortran... for better or worse.'
    IF (COMMIT_TIMES_SEEM_WEIRD) THEN
        NOTE = 'Tweaks may be made after my initial solution for optimization, readability, etc.'
    END IF
END PROGRAM ADVENT_OF_CODE_2023
```
### Compiling
Each can be built with the GNU Fortran compiler (gfortran). 
A shell script to build the solution is present in each folder to automatically compile all the needed code.

In Bash:
```shell
chmod +x build.sh
./build.sh
./dayXpX
```
Note that each solution will need to be built locally in the folder for the given day.
