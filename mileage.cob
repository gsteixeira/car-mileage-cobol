        *> Car Mileage Calculator in Cobol
        *> Copyright (C) 2021 Gustavo Selbach Teixeira
        *> 
        *> This program is free software: you can redistribute it and/or modify
        *> it under the terms of the GNU General Public License as published by
        *> the Free Software Foundation, either version 3 of the License, or
        *> (at your option) any later version.
        *> 
        *> This program is distributed in the hope that it will be useful,
        *> but WITHOUT ANY WARRANTY; without even the implied warranty of
        *> MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        *> GNU General Public License for more details.
        *> 
        *> You should have received a copy of the GNU General Public License
        *> along with this program.  If not, see <https://www.gnu.org/licenses/>.
        *> 
        *> 
        *> Calculate car gas mileage in COBOL reading data from a csv
        *>  The csv file must have the following format:
        *>      odometer_read;gas_quantity;is_top_up?(0 or 1)
        *> 
        *> compiles like this:  cobc -x --free mileage.cob
        *> runs like this:      ./mileage path_to_data_file.csv
        *>
        IDENTIFICATION DIVISION.
        PROGRAM-ID. car-mileage.
            AUTHOR. gsteixei@gmail.com
            DATE-WRITTEN. 2021-07-04
            INSTALLATION. cobc -x --free mileage.cob
        ENVIRONMENT DIVISION.
            INPUT-OUTPUT SECTION.
                FILE-CONTROL.
                    SELECT csv_file ASSIGN TO csv_file_name
                        ORGANIZATION IS LINE SEQUENTIAL.
        DATA DIVISION.
            FILE SECTION.
                FD  csv_file.
                    01 line_record  PIC X(120) VALUE SPACES.
            WORKING-STORAGE SECTION.
                01 csv_data.
                    02 csv_file_name   PIC X(125) VALUE SPACES.
                    01 end_of_file     PIC 9 VALUE ZERO.
                01 fillup OCCURS 99 TIMES.
                    02 odometer        PIC 999999 VALUE ZEROS.
                    02 fuel            PIC 999V99 VALUE ZEROS.
                    02 topup           PIC 9 VALUE ZEROS.
                    02 refuel.
                        03 dist        PIC 9999 VALUE ZEROS.
                        03 spent       PIC 999V99 VALUE ZEROS.
                        03 mileage     PIC 99V99 VALUE ZEROS.
                01 aux_vars.
                    02 i               PIC 99 VALUE ZEROS.
                    02 j               PIC 99 VALUE ZEROS.
                    02 aux             PIC 999 VALUE ZEROS.
                    02 max_records     PIC 99  VALUE 99.
                    02 distance        PIC 9999 VALUE ZEROS.
                    02 fuel_spent      PIC 999V99 VALUE ZEROS.
                    02 last_odo        PIC 999999 VALUE ZEROS.
        PROCEDURE DIVISION.
            main.
                ACCEPT csv_file_name FROM COMMAND-LINE.
                EVALUATE csv_file_name 
                    WHEN SPACE
                        DISPLAY "no csv file supplied. Will stick to default"
                        MOVE "sample.csv" TO csv_file_name
                    WHEN "--usage"
                        DISPLAY "mileage <path_to_csv_file>"
                        STOP RUN
                END-EVALUATE
                PERFORM load_from_csv.
                PERFORM compute_mileage.
                PERFORM show_results.
            STOP RUN.
            
            compute_mileage.
                PERFORM VARYING i FROM 2 BY 1 UNTIL i >= max_records OR odometer (i) = 0
                    MOVE fuel (i) TO fuel_spent
                    IF topup (i - 1) = 1 THEN
                        MOVE odometer (i - 1) TO last_odo
                    ELSE
                        COMPUTE aux = i - 1
                        PERFORM VARYING j FROM aux BY -1 UNTIL topup (j) = 1 or j = 1
                            SUBTRACT odometer (i) FROM odometer (j) GIVING distance
                            ADD fuel(j) TO fuel_spent
                            *>DISPLAY "b" fuel(j) fuel_spent distance
                        END-PERFORM
                    END-IF
                    SUBTRACT odometer (i) FROM last_odo GIVING distance
                    DIVIDE distance BY fuel_spent GIVING mileage (i)
                    MOVE distance TO dist (i)
                    MOVE fuel_spent TO spent (i)
                    *>DISPLAY "distance " distance " fuel " fuel_spent " mileage " mileage (i)
                END-PERFORM.

            show_results.
                DISPLAY "id odometer fill  topup dist. fuel mileage"
                PERFORM VARYING i from 1 BY 1 UNTIL odometer (i) = 0 OR i >= max_records
                    DISPLAY i SPACE
                            odometer (i) SPACE SPACE SPACE SPACE
                            fuel (i) SPACE
                            topup (i) SPACE SPACE SPACE SPACE SPACE
                            dist (i) SPACE SPACE 
                            spent (i) SPACE
                            mileage (i) SPACE
                    END-DISPLAY
                END-PERFORM.
            
            load_from_csv.
                DISPLAY "Loading data from CSV..."
                MOVE 1 TO i.
                OPEN INPUT csv_file.
                MOVE 0 TO end_of_file.
                PERFORM UNTIL end_of_file = 1
                    READ csv_file
                        AT END MOVE 1 TO end_of_file
                    END-READ
                    IF end_of_file = 0 THEN
                        UNSTRING line_record DELIMITED BY ';'
                            INTO odometer (i) 
                                fuel (i)
                                topup (i)
                        END-UNSTRING
                    END-IF
                    ADD 1 TO i
                END-PERFORM
                CLOSE csv_file.
                
END PROGRAM car-mileage.
