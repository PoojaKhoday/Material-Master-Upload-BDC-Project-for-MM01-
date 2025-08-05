*&
*&---------------------------------------------------------------------*
REPORT ZBDC_SESSION_REC_PPK1.
*       NO STANDARD PAGE HEADING LINE-SIZE 255.

* INCLUDE BDCRECX1_S:
* THE CALL TRANSACTION USING IS CALLED WITH AUTHORITY-CHECK!
* IF YOU HAVE OWN AUTH.-CHECKS YOU CAN USE INCLUDE BDCRECX1 INSTEAD.
*INCLUDE BDCRECX1_S.

*START-OF-SELECTION.

*PERFORM OPEN_GROUP.

TYPES: BEGIN OF TY_MM,
         MATNR TYPE MATNR,
         MTART TYPE MTART,
         MBRSH TYPE MBRSH,
         MEINS TYPE MEINS,
       END OF TY_MM.

DATA: ITAB TYPE TABLE OF TY_MM,
      WA   TYPE TY_MM.

DATA: ITAB_BDCDATA TYPE TABLE OF BDCDATA,
      WA_BDCDATA   TYPE BDCDATA.

DATA: ITAB_MSG TYPE TABLE OF BDCMSGCOLL.

PARAMETERS: IMP_FL TYPE LOCALFILE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR IMP_FL.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      PROGRAM_NAME  = SYST-CPROG       " MODULE POOL PROGRAM NAME FOR SCREEN FIELD
      DYNPRO_NUMBER = SYST-DYNNR       " DYNPRO NUMBER WHERE F4 HELP IS NEEDED
      FIELD_NAME    = SPACE            " NAME OF FIELD WHERE PATH IS TO BE ENTERED
    IMPORTING
      FILE_NAME     = IMP_FL.                " PATH NAME SELECTED BY USER WITH HELP OF FILEMNGR

START-OF-SELECTION.

  DATA: FL_STR TYPE STRING.
  FL_STR = IMP_FL.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      FILENAME                = FL_STR                " NAME OF FILE
*     FILETYPE                = 'ASC'            " FILE TYPE (ASC OR BIN)
      HAS_FIELD_SEPARATOR     = 'X' "'SPACE'            " COLUMNS SEPARATED BY TABS IN CASE OF ASCII UPLOAD
*     HEADER_LENGTH           = 0                " LENGTH OF HEADER FOR BINARY DATA
*     READ_BY_LINE            = 'X'              " THE FILE WILL BE WRITTEN TO THE INTERNAL TABLE LINE-BY-LINE
*     DAT_MODE                = SPACE            " NUMERIC AND DATE FIELDS IMPORTED IN WS_DOWNLOAD 'DAT' FORMAT
*     CODEPAGE                =                  " CHARACTER REPRESENTATION FOR OUTPUT
*     IGNORE_CERR             = ABAP_TRUE        " SPECIFIES WHETHER TO IGNORE ERRORS CONVERTING CHARACTER SETS
*     REPLACEMENT             = '#'              " REPLACEMENT CHARACTER FOR NON-CONVERTIBLE CHARACTERS
*     CHECK_BOM               = SPACE            " THE CONSISTENCY OF THE CODEPAGE AND BYTE ORDER MARK WILL BE CHECKED
*     VIRUS_SCAN_PROFILE      =                  " VIRUS SCAN PROFILE
*     NO_AUTH_CHECK           = SPACE            " SWITCH OFF CHECK FOR ACCESS RIGHTS
*  IMPORTING
*     FILELENGTH              =                  " FILE LENGTH
*     HEADER                  =                  " FILE HEADER IN CASE OF BINARY UPLOAD
    TABLES
      DATA_TAB                = ITAB               " TRANSFER TABLE FOR FILE CONTENTS
*  CHANGING
*     ISSCANPERFORMED         = SPACE            " FILE ALREADY SCANNED
    EXCEPTIONS
      FILE_OPEN_ERROR         = 1                " FILE DOES NOT EXIST AND CANNOT BE OPENED
      FILE_READ_ERROR         = 2                " ERROR WHEN READING FILE
      NO_BATCH                = 3                " FRONT-END FUNCTION CANNOT BE EXECUTED IN BACKGRND
      GUI_REFUSE_FILETRANSFER = 4                " INCORRECT FRONT END OR ERROR ON FRONT END
      INVALID_TYPE            = 5                " INCORRECT PARAMETER FILETYPE
      NO_AUTHORITY            = 6                " NO AUTHORIZATION FOR UPLOAD
      UNKNOWN_ERROR           = 7
      BAD_DATA_FORMAT         = 8                " CANNOT INTERPRET DATA IN FILE
      HEADER_NOT_ALLOWED      = 9                " INVALID HEADER
      SEPARATOR_NOT_ALLOWED   = 10               " INVALID SEPARATOR
      HEADER_TOO_LONG         = 11               " THE HEADER INFORMATION IS LIMITED TO 1023 BYTES AT PRESENT
      UNKNOWN_DP_ERROR        = 12               " ERROR WHEN CALLING DATA PROVIDER
      ACCESS_DENIED           = 13               " ACCESS TO FILE DENIED
      DP_OUT_OF_MEMORY        = 14               " NOT ENOUGH MEMORY IN DATA PROVIDER
      DISK_FULL               = 15               " STORAGE MEDIUM FULL
      DP_TIMEOUT              = 16               " TIMEOUT OF DATA PROVIDER
      OTHERS                  = 17.
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = SY-MANDT         " Client
*      dest                = FILLER8          " Target system for ODC/no longer relevant
      group               = 'SESSION_MM01' "'FILLER12'         " Session name
*      holddate            = FILLER8          " Session locked until specified date
      keep                = 'X' "'FILLER1 '         " Indicator to keep processed sessions
      user                = SY-UNAME         " Batch input user
*      record              = FILLER1          " Indicator: BI recording ('X' or ' ')
*      prog                = SY-CPROG         " Creator Program
*      dcpfm               = '%'              " Decimal Character Used
*      datfm               = '%'              " Date Format Used
*      app_area            = FILLER12         " Batch Input Application Area
*      langu               = SY-LANGU         " Language Used
*    IMPORTING
*      qid                 =                  " Unique database key
    EXCEPTIONS
      client_invalid      = 1                " Client is invalid
      destination_invalid = 2                " Target system is invalid/no longer relevant
      group_invalid       = 3                " Batch input session name is invalid
      group_is_locked     = 4                " Batch input session is protected elsewhere
      holddate_invalid    = 5                " Lock date is invalid
      internal_error      = 6                " Internal error of batch input (see SYSLOG)
      queue_error         = 7                " Error reading/writing the queue (see SYSLOG)
      running             = 8                " Session is already being processed
      system_lock_error   = 9                " System error when protecting BI session
      user_invalid        = 10               " BI user is not valid
      others              = 11
    .
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  LOOP AT ITAB INTO WA.

    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '0060'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'RMMG1-MTART'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  'ENTR'.
    PERFORM BDC_FIELD       USING 'RMMG1-MATNR'
                                  WA-MATNR.
    PERFORM BDC_FIELD       USING 'RMMG1-MBRSH'
                                  WA-MBRSH.
    PERFORM BDC_FIELD       USING 'RMMG1-MTART'
                                  WA-MTART.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '0070'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MSICHTAUSW-DYTXT(01)'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=ENTR'.
    PERFORM BDC_FIELD       USING 'MSICHTAUSW-KZSEL(01)'
                                  'X'.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '4004'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM BDC_FIELD       USING 'MAKT-MAKTX'
                                  'BDC RECORDING PPK MM01'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MARA-MEINS'.
    PERFORM BDC_FIELD       USING 'MARA-MEINS'
                                  WA-MEINS.
    PERFORM BDC_FIELD       USING 'MARA-MSTAE'
                                  '02'.
    PERFORM BDC_FIELD       USING 'MARA-MSTDE'
                                  '06.08.2025'.
    PERFORM BDC_DYNPRO      USING 'SAPLSPO1' '0300'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=CANC'.
    PERFORM BDC_DYNPRO      USING 'SAPLMGMM' '4004'.
    PERFORM BDC_FIELD       USING 'BDC_OKCODE'
                                  '=BU'.
    PERFORM BDC_FIELD       USING 'BDC_CURSOR'
                                  'MAKT-MAKTX'.
    PERFORM BDC_FIELD       USING 'MAKT-MAKTX'
                                  'BDC RECORDING PPK MM01'.
    PERFORM BDC_FIELD       USING 'MARA-MEINS'
                                  'KG'.
    PERFORM BDC_FIELD       USING 'MARA-MSTAE'
                                  '02'.
    PERFORM BDC_FIELD       USING 'MARA-MSTDE'
                                  '06.08.2025'.
*    PERFORM BDC_TRANSACTION USING 'MM01'.

*PERFORM CLOSE_GROUP.

*    CALL TRANSACTION 'MM01' USING ITAB_BDCDATA MODE 'N' UPDATE 'S' MESSAGES INTO ITAB.
     CALL FUNCTION 'BDC_INSERT'
       EXPORTING
         tcode            = 'MM01'          " Transaction code
*         post_local       = NOVBLOCAL
*         printing         = NOPRINT
*         simubatch        = space            " Simulate Background Processing (SY-BATCH='X')
*         ctuparams        = space            " CTU Recording Parameter
       TABLES
         dynprotab        =  ITAB_BDCDATA                " Table for screens of a transaction
       EXCEPTIONS
         internal_error   = 1                " Internal error of batch input (see
         not_open         = 2                " Queue was not opened (see SYSLOG)
         queue_error      = 3                " Error reading/writing the queue (see SYSLOG)
         tcode_invalid    = 4                " Transaction code is not known
         printing_invalid = 5
         posting_invalid  = 6
         others           = 7
       .
     IF SY-SUBRC <> 0.
*      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*        WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

    CLEAR ITAB_BDCDATA.
  ENDLOOP.

  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1                " Queue not opened (see SYSLOG )
      queue_error = 2                " Error reading/writing the queue (see SYSLOG)
      others      = 3
    .
  IF SY-SUBRC <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*CL_DEMO_OUTPUT=>DISPLAY( ITAB ) .

*----------------------------------------------------------------------*
*        START NEW SCREEN                                              *
*----------------------------------------------------------------------*
FORM BDC_DYNPRO USING PROGRAM DYNPRO.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-PROGRAM  = PROGRAM.
  WA_BDCDATA-DYNPRO   = DYNPRO.
  WA_BDCDATA-DYNBEGIN = 'X'.
  APPEND WA_BDCDATA TO ITAB_BDCDATA.
ENDFORM.

*----------------------------------------------------------------------*
*        INSERT FIELD                                                  *
*----------------------------------------------------------------------*
FORM BDC_FIELD USING FNAM FVAL.
*  IF FVAL <> NODATA.
  CLEAR WA_BDCDATA.
  WA_BDCDATA-FNAM = FNAM.
  WA_BDCDATA-FVAL = FVAL.
  APPEND WA_BDCDATA TO ITAB_BDCDATA.
*  ENDIF.
ENDFORM.
