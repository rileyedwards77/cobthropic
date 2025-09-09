IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTHROPIC.
       AUTHOR. CLAUDE.
       DATE-WRITTEN. 2025-09-09.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RESPONSE-FILE ASSIGN TO "api_response.json"
           ORGANIZATION IS LINE SEQUENTIAL.
      
       DATA DIVISION.
       FILE SECTION.
       FD RESPONSE-FILE.
       01 RESPONSE-RECORD    PIC X(1000).
      
       WORKING-STORAGE SECTION.
       01 WS-API-KEY         PIC X(100) VALUE 
           "your-api-key-here".
       
       01 WS-CURL-COMMAND    PIC X(500).
       01 WS-JSON-PAYLOAD    PIC X(300) VALUE
           '{"model":"claude-3-5-sonnet-20241022","max_tokens":100,' &
           '"messages":[{"role":"user","content":"Hello world!"}]}'.
       
       01 WS-SYSTEM-RESULT   PIC 9(3).
       01 WS-EOF-FLAG        PIC X VALUE 'N'.
           88 END-OF-FILE    VALUE 'Y'.
       
       01 WS-DISPLAY-LINE    PIC X(100).
      
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=== Cobthropic: COBOL + Anthropic API ===".
           DISPLAY " ".
           
           PERFORM BUILD-CURL-COMMAND.
           PERFORM CALL-ANTHROPIC-API.
           PERFORM READ-API-RESPONSE.
           
           DISPLAY " ".
           DISPLAY "Programme completed successfully.".
           STOP RUN.
      
       BUILD-CURL-COMMAND.
           STRING 
               'curl -X POST https://api.anthropic.com/v1/messages '
               '-H "Content-Type: application/json" '
               '-H "x-api-version: 2023-06-01" '
               '-H "Authorization: Bearer ' 
               WS-API-KEY
               '" '
               '-d ''' 
               WS-JSON-PAYLOAD
               ''' '
               '> api_response.json 2>&1'
               INTO WS-CURL-COMMAND
           END-STRING.
      
       CALL-ANTHROPIC-API.
           DISPLAY "Calling Anthropic API...".
           
           CALL "SYSTEM" USING WS-CURL-COMMAND 
               RETURNING WS-SYSTEM-RESULT.
           
           IF WS-SYSTEM-RESULT = 0
               DISPLAY "API call successful."
           ELSE
               DISPLAY "API call failed with return code: " 
                       WS-SYSTEM-RESULT
               DISPLAY "Check your API key and network connection."
               STOP RUN
           END-IF.
      
       READ-API-RESPONSE.
           DISPLAY " ".
           DISPLAY "=== API Response ===".
           
           OPEN INPUT RESPONSE-FILE.
           
           PERFORM UNTIL END-OF-FILE
               READ RESPONSE-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       MOVE RESPONSE-RECORD TO WS-DISPLAY-LINE
                       DISPLAY WS-DISPLAY-LINE
               END-READ
           END-PERFORM.
           
           CLOSE RESPONSE-FILE.
           
           DISPLAY "=== End Response ===".
      
       END PROGRAM COBTHROPIC.
